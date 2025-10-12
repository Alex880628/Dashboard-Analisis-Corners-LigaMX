  install.packages("pkgbuild")
  pkgbuild::check_build_tools(debug = TRUE) 
  install.packages(c("remotes","devtools","tidyverse","usethis"))
  
  remotes::install_version("SDMTools", version = "1.1-221", upgrade = "never")
  
  devtools::install_github("statsbomb/StatsBombR")
  
  install.packages("fs")
  
  usethis::edit_r_environ()
  Sys.getenv("SB_USERNAME")
  Sys.getenv("SB_PASSWORD")
  
  library(StatsBombR)
  library(dplyr)
  library(arrow)
  library(fs)
  library(stringr)
  library(purrr)
  library(glue)
  library(ggsoccer)
  library(plotly)
  library(fuzzyjoin)
  library(fmsb)
  library(gt)
  library(cowplot)
  library(ggtext)
  
  fs::dir_create("data/events")
  
  SB_USER <- Sys.getenv("SB_USERNAME")
  SB_PASS <- Sys.getenv("SB_PASSWORD")
  
  comps <- competitions(SB_USER, SB_PASS)
  comps_mx <- comps %>% 
    filter(
      country_name == "Mexico",
      competition_name == "Liga MX" # Filtra por el nombre exacto de la competición
    )
  comps_mx %>% select(competition_id, season_id, competition_name, season_name) %>% arrange(season_id)
  
  # Asegura directorios de salida
  fs::dir_create("data/matches")
  
  # Matriz [competition_id, season_id] como requiere MultiCompMatches()
  competitionmatrix <- as.matrix(
    comps_mx[, c("competition_id", "season_id")]
  )
  
  # Descargar TODOS los partidos de todas esas temporadas
  mx_matches <- MultiCompMatches(
    username = SB_USER,
    password = SB_PASS,
    competitionmatrix = competitionmatrix
  )
  
  
  names(mx_matches)
  
  
  # 1) Ordenar
  mx_matches <- mx_matches %>%
    dplyr::arrange(`season.season_id`, match_date)
  
  # 2) Elegir columnas atómicas (sin listas/anidados)
  keep_cols <- c(
    "match_id","match_date","kick_off",
    "home_score","away_score","match_week",
    "competition.competition_id","competition.competition_name",
    "season.season_id","season.season_name",
    "home_team.home_team_id","home_team.home_team_name",
    "away_team.away_team_id","away_team.away_team_name",
    "stadium.name","referee.name","attendance",
    "behind_closed_doors","neutral_ground","collection_status",
    "play_status","match_status","match_status_360",
    "last_updated","last_updated_360"
  )
  
  mx_matches_flat <- dplyr::select(mx_matches, tidyselect::any_of(keep_cols))
  
  # (Opcional) coerciones seguras por si vienen raras
  mx_matches_flat <- mx_matches_flat %>%
    dplyr::mutate(
      behind_closed_doors = as.logical(behind_closed_doors),
      neutral_ground      = as.logical(neutral_ground)
    )
  
  # 3) Guardar parquet
  fs::dir_create("data/matches")
  arrow::write_parquet(mx_matches_flat, "data/matches/mx_matches.parquet")
  
  dplyr::glimpse(mx_matches_flat)
  
  
  
  #DESCARGAR DATOS DE EVENTOS Y 360
  # 1. Cargar la lista de partidos que ya guardaste
  partidos_a_descargar <- arrow::read_parquet("data/matches/mx_matches.parquet")
  SB_USER <- Sys.getenv("SB_USERNAME")
  SB_PASS <- Sys.getenv("SB_PASSWORD")
  
  
  # 2.Directorio de eventos 
  fs::dir_create("data/events_360") 
  for (i in 1:nrow(partidos_a_descargar)) {
    
    match_id_actual <- partidos_a_descargar$match_id[i]
    ruta_archivo <- glue::glue("data/events_360/{match_id_actual}.parquet")
    
    if (fs::file_exists(ruta_archivo)) {
      message(glue::glue("Archivo ya existe: {match_id_actual}. Saltando... ({i}/{nrow(partidos_a_descargar)})"))
      next
    }
    
    status_360 <- partidos_a_descargar$match_status_360[i]
    if (is.na(status_360) || status_360 != "available") {
      message(glue::glue("Datos 360 no disponibles para el partido {match_id_actual}. Saltando... ({i}/{nrow(partidos_a_descargar)})"))
      next
    }
    
    message(glue::glue("Descargando datos para el partido: {match_id_actual} ({i}/{nrow(partidos_a_descargar)})"))
    
    # LA FUNCIÓN CORRECTA Y VERIFICADA
    eventos_360 <- try(
      get.events(match_id = match_id_actual, username = SB_USER, password = SB_PASS)
    )
    
    if ("try-error" %in% class(eventos_360)) {
      message(glue::glue("Error al descargar el partido {match_id_actual}. Saltando..."))
      next
    }
    
    arrow::write_parquet(eventos_360, sink = ruta_archivo)
  }
  
  message("¡Descarga de todos los eventos y datos 360 completada!")
  
  
  
  # 1. Obtener la lista de todos los archivos parquet
  lista_de_archivos <- fs::dir_ls("data/events_360/", glob = "*.parquet")
  
  # 2. Leer todos los archivos y unirlos en un solo dataframe
  todos_los_eventos_mx <- arrow::open_dataset(lista_de_archivos) %>% 
    collect()
  
  # 3. ¡Inspeccionar el resultado!
  message("¡Datos cargados! El dataframe 'todos_los_eventos_mx' contiene ", 
          nrow(todos_los_eventos_mx), " eventos en total.")
  glimpse(todos_los_eventos_mx)
  

  
  # ===================================================================
  # --- BLOQUE 2: ANÁLISIS PRINCIPAL Y CREACIÓN DE OBJETOS ---
  # ===================================================================
  message("--- BLOQUE 2: Creando todos los objetos de análisis... ---")
  
  # 2.1 Crear 'ids_de_corners'
  ids_de_corners <- todos_los_eventos_mx %>%
    filter(pass.type.name == "Corner") %>%
    pull(id)
  
  # 2.2 Crear 'remates_asistidos_por_corner'
  remates_con_ff <- todos_los_eventos_mx %>%
    filter(type.name == "Shot", !sapply(shot.freeze_frame, is.null))
  remates_asistidos_por_corner <- remates_con_ff %>%
    filter(shot.key_pass_id %in% ids_de_corners)
  
  # 2.3 Crear 'corners_exitosos_conteo' y 'corners_exitosos_con_categoria'
  AREA_CHICA_X_MIN <- 114; AREA_CHICA_Y_MIN <- 30; AREA_CHICA_Y_MAX <- 50
  corners_exitosos_conteo <- remates_asistidos_por_corner %>%
    select(id, shot.outcome.name, shot.freeze_frame) %>%
    unnest(shot.freeze_frame) %>%
    mutate(
      location_x = map_dbl(location, 1, .default = NA),
      location_y = map_dbl(location, 2, .default = NA)
    ) %>%
    group_by(id) %>%
    summarise(
      atacantes_en_area_chica = sum(location_x >= AREA_CHICA_X_MIN & location_y >= AREA_CHICA_Y_MIN & location_y <= AREA_CHICA_Y_MAX & teammate == TRUE),
      fue_gol = first(shot.outcome.name) == "Goal"
    ) %>%
    ungroup()
  
  corners_exitosos_con_categoria <- corners_exitosos_conteo %>%
    mutate(categoria_atacantes = factor(case_when(
      atacantes_en_area_chica >= 4 ~ "4+ Atacantes",
      TRUE ~ paste(atacantes_en_area_chica, "Atacantes")
    ), levels = c("0 Atacantes", "1 Atacantes", "2 Atacantes", "3 Atacantes", "4+ Atacantes")))
  
  # 2.4 Crear tablas de resumen para gráficos de barras
  resultado_efectividad_remates <- corners_exitosos_con_categoria %>% group_by(categoria_atacantes) %>% summarise(total_remates_generados = n())
  resultado_goles <- corners_exitosos_con_categoria %>% group_by(categoria_atacantes) %>% summarise(total_remates = n(), total_goles = sum(fue_gol), tasa_de_gol = total_goles/total_remates)
  
  # 2.5 Crear el "diccionario" de logos
  logos_df <- tibble::tribble(
    ~team.name, ~logo_url,
    "América", "https://e01-mx-marca.uecdn.es/mx/assets/sports/logos/football/png/72x72/1292.png", "Atlas", "https://e01-mx-marca.uecdn.es/mx/assets/sports/logos/football/png/72x72/1284.png",
    "Atlético San Luis", "https://e01-mx-marca.uecdn.es/mx/assets/sports/logos/football/png/72x72/9491.png", "Cruz Azul", "https://e01-mx-marca.uecdn.es/mx/assets/sports/logos/football/png/72x72/853.png",
    "Guadalajara", "https://e01-mx-marca.uecdn.es/mx/assets/sports/logos/football/png/72x72/1283.png", "Juárez", "https://e01-mx-marca.uecdn.es/mx/assets/sports/logos/football/png/72x72/10991.png",
    "León", "https://e01-mx-marca.uecdn.es/mx/assets/sports/logos/football/png/72x72/1293.png", "Mazatlán", "https://e01-mx-marca.uecdn.es/mx/assets/sports/logos/football/png/72x72/16906.png",
    "Monterrey", "https://e01-mx-marca.uecdn.es/mx/assets/sports/logos/football/png/72x72/741.png", "Necaxa", "https://e01-mx-marca.uecdn.es/mx/assets/sports/logos/football/png/72x72/1174.png",
    "Pachuca", "https://e01-mx-marca.uecdn.es/mx/assets/sports/logos/football/png/72x72/1295.png", "Puebla", "https://e01-mx-marca.uecdn.es/mx/assets/sports/logos/football/png/72x72/1296.png",
    "Pumas UNAM", "https://e01-mx-marca.uecdn.es/mx/assets/sports/logos/football/png/72x72/1297.png", "Querétaro", "https://e01-mx-marca.uecdn.es/mx/assets/sports/logos/football/png/72x72/4047.png",
    "Santos Laguna", "https://e01-mx-marca.uecdn.es/mx/assets/sports/logos/football/png/72x72/1287.png", "Tigres UANL", "https://e01-mx-marca.uecdn.es/mx/assets/sports/logos/football/png/72x72/1294.png",
    "Tijuana", "https://e01-mx-marca.uecdn.es/mx/assets/sports/logos/football/png/72x72/4690.png", "Toluca", "https://e01-mx-marca.uecdn.es/mx/assets/sports/logos/football/png/72x72/1286.png"
  )
  
  message("--- ¡Objetos de análisis listos! Ahora puedes ejecutar cualquier bloque de visualización del BLOQUE 3. ---")
  
  
  
  
  
  

  
  # ===================================================================
  # === ANÁLISIS Y VISUALIZACIONES ===============
  # ===================================================================
  
  # --- PARTE 1: TABLAS DE RESUMEN (Remates y Goles) ---
  
  # 1.1 Tabla de resumen para TOTAL DE REMATES
  resultado_efectividad_remates <- corners_exitosos_conteo %>%
    mutate(
      categoria_atacantes = factor(case_when(
        atacantes_en_area_chica >= 4 ~ "4+ Atacantes",
        TRUE ~ paste(atacantes_en_area_chica, "Atacantes")
      ), levels = c("0 Atacantes", "1 Atacantes", "2 Atacantes", "3 Atacantes", "4+ Atacantes"))
    ) %>%
    group_by(categoria_atacantes) %>%
    summarise(total_remates_generados = n()) %>%
    tidyr::complete(categoria_atacantes, fill = list(total_remates_generados = 0))
  
  # 1.2 Tabla de resumen para GOLES Y TASA DE GOL
  resultado_goles <- corners_exitosos_conteo %>%
    mutate(
      categoria_atacantes = factor(case_when(
        atacantes_en_area_chica >= 4 ~ "4+ Atacantes",
        TRUE ~ paste(atacantes_en_area_chica, "Atacantes")
      ), levels = c("0 Atacantes", "1 Atacantes", "2 Atacantes", "3 Atacantes", "4+ Atacantes"))
    ) %>%
    group_by(categoria_atacantes) %>%
    summarise(
      total_remates = n(),
      total_goles = sum(fue_gol, na.rm = TRUE),
      tasa_de_gol = total_goles / total_remates
    ) %>%
    tidyr::complete(categoria_atacantes, fill = list(total_remates = 0, total_goles = 0, tasa_de_gol = 0))
  
  
  # --- PARTE 2: MOSTRAR GRÁFICOS DE BARRAS ---
  
  message("--- Mostrando Gráficos de Barras ---")
  # 2.1 Imprimir tabla y gráfico de TOTAL DE REMATES
  print("--- Total de Remates por Configuración ---")
  print(resultado_efectividad_remates)
  print(
    ggplot(resultado_efectividad_remates, aes(x = categoria_atacantes, y = total_remates_generados, fill = categoria_atacantes)) +
      geom_col(show.legend = FALSE) +
      geom_text(aes(label = total_remates_generados), vjust = -0.5) +
      labs(title = "Remates Generados tras Córner vs. Ocupación del Área Chica", x = "Nº de Compañeros en Área Chica", y = "Total de Remates Generados") +
      theme_minimal(base_size = 12)
  )
  
  # 2.2 Imprimir tabla y gráfico de TASA DE GOL
  print("--- Tasa de Gol por Configuración ---")
  print(resultado_goles)
  print(
    ggplot(resultado_goles, aes(x = categoria_atacantes, y = tasa_de_gol, fill = categoria_atacantes)) +
      geom_col(show.legend = FALSE) +
      geom_text(aes(label = paste0(round(tasa_de_gol * 100, 1), "%\n(", total_goles, " goles)")), vjust = -0.5) +
      scale_y_continuous(labels = scales::percent) +
      labs(title = "Tasa de Gol de Remates tras Córner", x = "Nº de Compañeros en Área Chica", y = "Tasa de Conversión a Gol") +
      theme_minimal(base_size = 12)
  )
  
  
  
 
  # --- PASO 1: PREPARAR DATOS BASE ---
  
  message("--- Reconstruyendo el dataframe base final ---")
  
  # 1.1 Preparar títulos (sin cambios)
  titulos_remates <- resultado_efectividad_remates %>%
    mutate(titulo_facet = paste0(categoria_atacantes, "\n(N = ", total_remates_generados, " Remates)")) %>%
    select(categoria_atacantes, titulo_facet)
  
  titulos_goles <- resultado_goles %>%
    mutate(titulo_facet = paste0(categoria_atacantes, "\n(N = ", total_goles, " Goles)")) %>%
    select(categoria_atacantes, titulo_facet)
  
  # 1.2 Crear el dataframe base enriquecido (CON LA CORRECCIÓN)
  datos_plot_base <- remates_asistidos_por_corner %>%
    select(
      id, match_id, shot.key_pass_id,
      equipo_atacante = team.name,
      rematador = player.name,
      minuto = minute,
      segundo = second,
      shot.outcome.name, 
      xG = shot.statsbomb_xg,
      xGoT = shot.shot_execution_xg
    ) %>%
    left_join(partidos_a_descargar %>% select(match_id, fecha = match_date, equipo_local = home_team.home_team_name, equipo_visitante = away_team.away_team_name, temporada = season.season_name), by = "match_id") %>%
    left_join(todos_los_eventos_mx %>% filter(id %in% ids_de_corners) %>% select(id, ejecutor = player.name), by = c("shot.key_pass_id" = "id")) %>%
    left_join(corners_exitosos_con_categoria, by = "id") %>%
    left_join(todos_los_eventos_mx %>% select(id, start_location = location, end_location = pass.end_location), by = c("shot.key_pass_id" = "id")) %>%
    mutate(
      fue_gol = shot.outcome.name == "Goal", 
      start_x = map_dbl(start_location, 1, .default = NA),
      start_y = map_dbl(start_location, 2, .default = NA),
      end_x = map_dbl(end_location, 1, .default = NA),
      end_y = map_dbl(end_location, 2, .default = NA)
    ) %>%
    filter(!is.na(categoria_atacantes))
  glimpse(datos_plot_base)
  
  # --- PASO 2: VISUALIZACIÓN DE TODOS LOS REMATES (CON TOOLTIP COMPLETO) ---
  
  datos_plot_remates <- datos_plot_base %>%
    left_join(titulos_remates, by = "categoria_atacantes") %>%
    mutate(
      xgot_text = if_else(!is.na(xGoT), paste0("<br><b>xGoT:</b> ", round(xGoT, 2)), ""),
      tooltip_text = paste0(
        "<b>Partido:</b> ", equipo_local, " vs ", equipo_visitante,
        "<br><b>Equipo:</b> ", equipo_atacante, # <-- ¡AHORA SÍ FUNCIONARÁ!
        "<br><b>Fecha:</b> ", as.Date(fecha),
        "<br><b>Minuto:</b> ", minuto, ":", sprintf("%02d", segundo),
        "<br><b>Ejecutó:</b> ", ejecutor,
        "<br><b>Remató:</b> ", rematador,
        "<br><b>xG:</b> ", round(xG, 2),
        xgot_text
      )
    )
  
  p_remates_final <- ggplot(datos_plot_remates) +
    annotate_pitch(dimensions = pitch_statsbomb) +
    geom_segment(aes(x = start_x, y = start_y, xend = end_x, yend = end_y, text = tooltip_text), alpha = 0.2, color = "dodgerblue", arrow = arrow(length = unit(0.1, "cm"))) +
    facet_wrap(~ titulo_facet) +
    labs(title = "Destino de los Tiros de Esquina que Terminan en Remate") +
    theme_pitch()
  
  print(ggplotly(p_remates_final, tooltip = "text"))
  
  
  # --- PASO 3: VISUALIZACIÓN DE SÓLO GOLES (CON TOOLTIP) ---
  
  datos_plot_goles <- datos_plot_base %>%
    filter(fue_gol == TRUE) %>%
    left_join(titulos_goles, by = "categoria_atacantes") %>%
    mutate(
      xgot_text = if_else(!is.na(xGoT), paste0("<br><b>xGoT:</b> ", round(xGoT, 2)), ""),
      tooltip_text = paste0(
        "<b>GOL! de ", equipo_atacante, "</b>",
        "<br><b>Partido:</b> ", equipo_local, " vs ", equipo_visitante,
        "<br><b>Fecha:</b> ", as.Date(fecha),
        "<br><b>Minuto:</b> ", minuto, ":", sprintf("%02d", segundo),
        "<br><b>Ejecutó:</b> ", ejecutor,
        "<br><b>Remató:</b> ", rematador,
        "<br><b>xG:</b> ", round(xG, 2),
        xgot_text
      )
    )
  
  p_goles_final <- ggplot(datos_plot_goles) +
    annotate_pitch(dimensions = pitch_statsbomb) +
    geom_segment(aes(x = start_x, y = start_y, xend = end_x, yend = end_y, text = tooltip_text), alpha = 0.5, color = "gold", linewidth = 1, arrow = arrow(length = unit(0.15, "cm"))) +
    facet_wrap(~ titulo_facet) +
    labs(title = "Destino de los Tiros de Esquina que Terminan en GOL") +
    theme_pitch()
  
  print(ggplotly(p_goles_final, tooltip = "text"))
  

  # --- BLOQUE A: CREAR LA TABLA DE RESUMEN COMPLETA ---
  
  resultado_xg <- remates_asistidos_por_corner %>%
    left_join(corners_exitosos_con_categoria, by = "id") %>%
    select(categoria_atacantes, xG = shot.statsbomb_xg, fue_gol) %>%
    filter(!is.na(categoria_atacantes)) %>%
    group_by(categoria_atacantes) %>%
    summarise(
      total_remates = n(),
      xG_total = sum(xG, na.rm = TRUE),
      xG_promedio_por_remate = mean(xG, na.rm = TRUE),
      total_goles = sum(fue_gol, na.rm = TRUE)
    ) %>%
    tidyr::complete(categoria_atacantes, fill = list(total_remates = 0, xG_total = 0, xG_promedio_por_remate = 0, total_goles = 0))
  
 
  print("--- Resumen Completo (Remates, Goles, xG) por Configuración ---")
  print(resultado_xg)
  
  
  
  
  # --- BLOQUE B: CREAR EL GRÁFICO COMPARATIVO  ---
  datos_grafico_comparativo <- resultado_xg %>%
    rename(total_remates_final = total_remates) %>% # Renombramos para evitar conflictos
    select(categoria_atacantes, total_remates = total_remates_final, xG_total, total_goles) %>%
    tidyr::pivot_longer(
      cols = c("total_remates", "xG_total", "total_goles"),
      names_to = "metrica",
      values_to = "valor"
    ) %>%
    mutate(
      metrica = factor(metrica, levels = c("total_remates", "xG_total", "total_goles"))
    )
  
  
  ggplot(datos_grafico_comparativo, aes(x = categoria_atacantes, y = valor, fill = metrica)) +
    
    geom_col(position = "dodge") +
    geom_text(
      aes(label = round(valor, 1)), # Redondeamos a 1 decimal para xG
      position = position_dodge(width = 0.9),
      vjust = -0.5, # Ajuste vertical para que esté encima de la barra
      size = 3.5    # Tamaño del texto
    ) +

    
    scale_fill_manual(
      name = "Métrica:",
      values = c("total_remates" = "#56B4E9", "xG_total" = "#E69F00", "total_goles" = "#009E73"),
      labels = c("Total Remates", "xG Acumulado", "Goles Reales")
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
    
    labs(
      title = "Comparación de Córners por Configuración de Ataque",
      subtitle = "Volumen (Remates) vs. Peligrosidad (xG) vs. Resultado (Goles)",
      x = "Número de Compañeros en Área Chica",
      y = "Valor Acumulado"
    ) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "top")
  
  
  
  # --- DIAGNÓSTICO: ENCONTRAR EL NOMBRE DE LA COLUMNA DE TEMPORADA ---
  glimpse(todos_los_eventos_mx)
  

 ######################################################################## 
  # ===================================================================
  # === ANÁLISIS POR TEMPORADA (CON GRÁFICO COMPARATIVO) ==============
  # ===================================================================
  
  message("--- Calculando y visualizando resumen por temporada... ---")
  
  # --- PASO 1: ENRIQUECER DATOS CON INFORMACIÓN DE LA TEMPORADA ---
  temporada_lookup <- partidos_a_descargar %>%
    select(match_id, temporada = season.season_name)
  datos_por_temporada <- remates_asistidos_por_corner %>%
    left_join(temporada_lookup, by = "match_id") %>%
    left_join(corners_exitosos_con_categoria, by = "id") %>%
    select(
      temporada,
      categoria_atacantes,
      xG = shot.statsbomb_xg,
      fue_gol
    ) %>%
    filter(!is.na(categoria_atacantes) & !is.na(temporada))
  
  # --- PASO 2: CREAR TABLA DE RESUMEN POR TEMPORADA ---
  resumen_por_temporada <- datos_por_temporada %>%
    group_by(temporada, categoria_atacantes) %>%
    summarise(
      total_remates = n(),
      xG_total = sum(xG, na.rm = TRUE),
      total_goles = sum(fue_gol, na.rm = TRUE),
      .groups = 'drop'
    )
  
  print("--- Resumen por Temporada y Configuración ---")
  print(resumen_por_temporada, n = 100) # Mostramos hasta 100 filas
  
  # --- PASO 3: PREPARAR DATOS PARA EL GRÁFICO (LA CORRECCIÓN) ---
  datos_grafico_temporada <- resumen_por_temporada %>%
    tidyr::pivot_longer(
      cols = c("total_remates", "xG_total", "total_goles"),
      names_to = "metrica",
      values_to = "valor"
    ) %>%
    mutate(
      metrica = factor(metrica, levels = c("total_remates", "xG_total", "total_goles"))
    )
  
  # --- PASO 4: VISUALIZAR LA COMPARACIÓN POR TEMPORADA (CON ETIQUETAS) ---
  print(
    ggplot(datos_grafico_temporada, aes(x = categoria_atacantes, y = valor, fill = metrica)) +
      geom_col(position = "dodge") +
      geom_text(
        aes(label = round(valor, 1)), 
        position = position_dodge(width = 0.9), 
        vjust = -0.5, 
        size = 2.5    
      ) +
      facet_wrap(~ temporada, ncol = 2, scales = "free_y") +
      scale_fill_manual(
        name = "Métrica:",
        values = c("total_remates" = "#56B4E9", "xG_total" = "#E69F00", "total_goles" = "#009E73"),
        labels = c("Total Remates", "xG Acumulado", "Goles Reales")
      ) +
      labs(
        title = "Comparación de Córners por Configuración de Ataque y Temporada",
        x = NULL,
        y = "Valor Acumulado"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        legend.position = "top",
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8)
      )
  )
  
  
  
  # ===================================================================
  # === GRÁFICOS DE BARRAS SIMPLES POR TEMPORADA ======================
  # ===================================================================
  
 
  # --- GRÁFICO 1: TOTAL DE REMATES POR TEMPORADA ---
  
  print("--- Gráfico: Total de Remates por Temporada ---")
  print(
    ggplot(resumen_por_temporada, aes(x = categoria_atacantes, y = total_remates, fill = categoria_atacantes)) +
      geom_col(show.legend = FALSE) +
      geom_text(aes(label = total_remates), vjust = -0.5, size = 2.5) +
      facet_wrap(~ temporada, ncol = 2, scales = "free_y") +
      
      labs(
        title = "Total de Remates Generados tras Córner por Temporada",
        x = NULL,
        y = "Número de Remates"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8)
      )
  )
  
  
  # --- GRÁFICO 2: TASA DE GOL POR TEMPORADA ---
  resumen_por_temporada_con_tasa <- resumen_por_temporada %>%
    mutate(tasa_de_gol = total_goles / total_remates)
  
  print("--- Gráfico: Tasa de Gol por Temporada ---")
  print(
    ggplot(resumen_por_temporada_con_tasa, aes(x = categoria_atacantes, y = tasa_de_gol, fill = categoria_atacantes)) +
      geom_col(show.legend = FALSE) +
      geom_text(
        aes(label = paste0(round(tasa_de_gol * 100, 1), "%")), 
        vjust = -0.5, 
        size = 2.5
      ) +
      facet_wrap(~ temporada, ncol = 2) +
      scale_y_continuous(labels = scales::percent) +
      
      labs(
        title = "Tasa de Gol de Remates tras Córner por Temporada",
        x = NULL,
        y = "Tasa de Conversión a Gol"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8)
      )
  )
  
  glimpse(datos_plot_base) 
  
  

  # ===================================================================
  # === VISUALIZACIONES POR TEMPORADA (CON TOOLTIPS) ========
  # ===================================================================
  
  # --- PASO 1: CREAR DATAFRAME BASE ENRIQUECIDO ---
  
  message("--- Reconstruyendo el dataframe base para el análisis por temporada ---")
  
  # 1.1 Tabla de consulta para partidos (incluyendo la temporada)
  partidos_lookup <- partidos_a_descargar %>%
    select(
      match_id, 
      fecha = match_date,
      temporada_nombre = season.season_name,
      equipo_local = home_team.home_team_name,
      equipo_visitante = away_team.away_team_name
    )
  
  # 1.2 Unimos toda la información en un dataframe final y completo
  datos_plot_base_con_todo <- remates_asistidos_por_corner %>%
    select(
      id, match_id, shot.key_pass_id,
      equipo_atacante = team.name,
      rematador = player.name,
      minuto = minute,
      segundo = second,
      shot.outcome.name,
      xG = shot.statsbomb_xg,
      xGoT = shot.shot_execution_xg
    ) %>%
    left_join(partidos_lookup, by = "match_id") %>%
    left_join(todos_los_eventos_mx %>% filter(id %in% ids_de_corners) %>% select(id, ejecutor = player.name), by = c("shot.key_pass_id" = "id")) %>%
    left_join(corners_exitosos_con_categoria, by = "id") %>%
    left_join(todos_los_eventos_mx %>% select(id, start_location = location, end_location = pass.end_location), by = c("shot.key_pass_id" = "id")) %>%
    mutate(
      fue_gol = shot.outcome.name == "Goal",
      start_x = map_dbl(start_location, 1, .default = NA),
      start_y = map_dbl(start_location, 2, .default = NA),
      end_x = map_dbl(end_location, 1, .default = NA),
      end_y = map_dbl(end_location, 2, .default = NA)
    ) %>%
    select(
      temporada = temporada_nombre,
      equipo_atacante, rematador, ejecutor,
      minuto, segundo, categoria_atacantes,
      fecha, equipo_local, equipo_visitante,
      start_x, start_y, end_x, end_y,
      fue_gol, xG, xGoT
    ) %>%
    filter(!is.na(categoria_atacantes) & !is.na(temporada))
  glimpse(datos_plot_base_con_todo)
  
  
  # --- PASO 2: BUCLE PARA GENERAR GRÁFICOS INTERACTIVOS POR TEMPORADA ---
  
  lista_temporadas <- unique(datos_plot_base_con_todo$temporada)
  message("--- Generando Gráficos de Cancha por Temporada ---")
  
  for (temporada_actual in lista_temporadas) {
    
    datos_temporada_actual <- datos_plot_base_con_todo %>%
      filter(temporada == temporada_actual)
    
    # --- Sub-gráfico A: TODOS LOS REMATES de la temporada actual ---
    
    titulos_remates_temp <- datos_temporada_actual %>%
      group_by(categoria_atacantes) %>%
      summarise(n = n(), .groups = 'drop') %>%
      mutate(titulo_facet = paste0(categoria_atacantes, "\n(N = ", n, " Remates)"))
    
    datos_plot_remates_temp <- datos_temporada_actual %>%
      left_join(titulos_remates_temp, by = "categoria_atacantes") %>%
      mutate(
        xgot_text = if_else(!is.na(xGoT), paste0("<br><b>xGoT:</b> ", round(xGoT, 2)), ""),
        tooltip_text = paste0("<b>Partido:</b> ", equipo_local, " vs ", equipo_visitante, "<br><b>Equipo:</b> ", equipo_atacante, "<br><b>Fecha:</b> ", as.Date(fecha), "<br><b>Minuto:</b> ", minuto, ":", sprintf("%02d", segundo), "<br><b>Ejecutó:</b> ", ejecutor, "<br><b>Remató:</b> ", rematador, "<br><b>xG:</b> ", round(xG, 2), xgot_text)
      )
    
    p_remates_temp <- ggplot(datos_plot_remates_temp) +
      annotate_pitch(dimensions = pitch_statsbomb) +
      geom_segment(aes(x = start_x, y = start_y, xend = end_x, yend = end_y, text = tooltip_text), alpha = 0.2, color = "dodgerblue", arrow = arrow(length = unit(0.1, "cm"))) +
      facet_wrap(~ titulo_facet) +
      labs(title = paste("Destino de Remates tras Córner - Temporada:", temporada_actual)) +
      theme_pitch()
    
    print(ggplotly(p_remates_temp, tooltip = "text"))
    
    # --- Sub-gráfico B: SÓLO GOLES de la temporada actual ---
    
    datos_goles_temp <- datos_temporada_actual %>%
      filter(fue_gol == TRUE)
    
    if (nrow(datos_goles_temp) > 0) {
      titulos_goles_temp <- datos_goles_temp %>%
        group_by(categoria_atacantes) %>%
        summarise(n = n(), .groups = 'drop') %>%
        mutate(titulo_facet = paste0(categoria_atacantes, "\n(N = ", n, " Goles)"))
      
      datos_plot_goles_temp <- datos_goles_temp %>%
        left_join(titulos_goles_temp, by = "categoria_atacantes") %>%
        mutate(
          xgot_text = if_else(!is.na(xGoT), paste0("<br><b>xGoT:</b> ", round(xGoT, 2)), ""),
          tooltip_text = paste0("<b>GOL! de ", equipo_atacante, "</b>", "<br><b>Partido:</b> ", equipo_local, " vs ", equipo_visitante, "<br><b>Fecha:</b> ", as.Date(fecha), "<br><b>Minuto:</b> ", minuto, ":", sprintf("%02d", segundo), "<br><b>Ejecutó:</b> ", ejecutor, "<br><b>Remató:</b> ", rematador, "<br><b>xG:</b> ", round(xG, 2), xgot_text)
        )
      
      p_goles_temp <- ggplot(datos_plot_goles_temp) +
        annotate_pitch(dimensions = pitch_statsbomb) +
        geom_segment(aes(x = start_x, y = start_y, xend = end_x, yend = end_y, text = tooltip_text), alpha = 0.5, color = "gold", linewidth = 1, arrow = arrow(length = unit(0.15, "cm"))) +
        facet_wrap(~ titulo_facet) +
        labs(title = paste("Destino de Goles tras Córner - Temporada:", temporada_actual)) +
        theme_pitch()
      
      print(ggplotly(p_goles_temp, tooltip = "text"))
    }
  }
  
  
  
  
  # ===================================================================
  # === ANÁLISIS POR TIPO DE COBRO  =====
  # ===================================================================
  
  # 1.1 Crear un dataframe que contenga SOLO los córners, con su tipo.
  corners_base <- todos_los_eventos_mx %>%
    filter(id %in% ids_de_corners) %>%
    mutate(
      tipo_cobro = case_when(
        pass.length < 25 ~ "Corto",
        pass.inswinging == TRUE ~ "Inswinger",
        pass.outswinging == TRUE ~ "Outswinger",
        TRUE ~ "Directo/Otro"
      )
    ) %>%
    select(id, tipo_cobro, obv_total_net)
  
  # 1.2 Crear una tabla de consulta que nos diga qué córners terminaron en remate/gol.
  resultados_lookup <- remates_asistidos_por_corner %>%
    left_join(corners_exitosos_conteo %>% select(id, fue_gol), by = "id") %>%
    select(corner_id = shot.key_pass_id, fue_gol) %>%
    mutate(
      termino_en_remate = TRUE,
      fue_gol = ifelse(is.na(fue_gol), FALSE, fue_gol)
    )
  
  # 1.3 Unir la información de los resultados a nuestra base de córners.
  analisis_tipo_cobro_df <- corners_base %>%
    left_join(resultados_lookup, by = c("id" = "corner_id")) %>%
    mutate(
      termino_en_remate = ifelse(is.na(termino_en_remate), FALSE, termino_en_remate),
      fue_gol = ifelse(is.na(fue_gol), FALSE, fue_gol)
    )
  
  # 1.4 Agrupar y resumir (este código es el mismo de antes)
  analisis_tipo_cobro <- analisis_tipo_cobro_df %>%
    group_by(tipo_cobro) %>%
    summarise(
      total_corners = n(),
      tasa_de_remate = mean(termino_en_remate, na.rm = TRUE),
      tasa_de_gol_por_corner = mean(fue_gol, na.rm = TRUE),
      valor_promedio_obv = mean(obv_total_net, na.rm = TRUE)
    ) %>%
    arrange(desc(valor_promedio_obv))
  
  # 1.5 Imprimir la tabla de resultados
  print("--- Efectividad por Tipo de Cobro de Córner ---")
  print(analisis_tipo_cobro)
  
  
  
  
  
  # ===================================================================
  # === VISUALIZACIÓN 1: GRÁFICO DE BARRAS POR TIPO DE COBRO ==========
  # ===================================================================
  
  # 1.1 Preparar los datos pivotando la tabla de resumen
  datos_grafico_cobro <- analisis_tipo_cobro %>%
    select(-total_corners) %>%
    tidyr::pivot_longer(
      cols = -tipo_cobro, 
      names_to = "metrica",
      values_to = "valor"
    ) %>%
    mutate(
      metrica = factor(metrica, levels = c("tasa_de_remate", "tasa_de_gol_por_corner", "valor_promedio_obv")),
      etiqueta = case_when(
        metrica != "valor_promedio_obv" ~ scales::percent(valor, accuracy = 0.1),
        TRUE ~ as.character(round(valor, 4))
      )
    )
  
  # 1.2 Crear el gráfico facetado
  ggplot(datos_grafico_cobro, aes(x = tipo_cobro, y = valor, fill = tipo_cobro)) +
    geom_col(show.legend = FALSE) +
    geom_text(aes(label = etiqueta), vjust = -0.5) +
    # Usamos escalas libres para que cada métrica tenga su propio eje Y
    facet_wrap(~ metrica, scales = "free_y") +
    # El eje Y necesita un formato diferente para cada faceta, lo dejamos simple
    scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) +
    labs(
      title = "Efectividad por Tipo de Cobro de Córner",
      subtitle = "Comparación de Tasas de Remate, Gol y Valor Promedio (OBV)",
      x = "Tipo de Cobro",
      y = "Valor"
    ) +
    theme_minimal(base_size = 12)
  
  
  
  
  
  
  # ===================================================================
  # === VISUALIZACIÓN FINAL: PERFILES TÁCTICOS POR EQUIPO Y TEMPORADA ==
  # ===================================================================
  
  # --- PASO 1: AGREGAR LOS DATOS POR EQUIPO Y POR TEMPORADA ---
  message("--- Calculando perfiles tácticos por equipo para cada temporada... ---")
  
  # 1.1 Unimos toda la información necesaria en un solo lugar.
  analisis_equipo_temporada <- todos_los_eventos_mx %>%
    filter(id %in% ids_de_corners) %>%
    left_join(partidos_a_descargar %>% select(match_id, temporada = season.season_name), by = "match_id") %>%
    mutate(
      tipo_cobro = case_when(
        pass.length < 25 ~ "Corto",
        pass.inswinging == TRUE ~ "Inswinger",
        pass.outswinging == TRUE ~ "Outswinger",
        TRUE ~ "Directo/Otro"
      )
    ) %>%
    left_join(analisis_tipo_cobro_df %>% select(id, termino_en_remate), by = "id") %>%
    filter(!is.na(temporada)) %>%
    group_by(temporada, team.name, tipo_cobro) %>%
    summarise(
      total_corners_temporada = n(),
      tasa_de_remate = mean(termino_en_remate, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    filter(total_corners_temporada > 20)
  
  
  # --- PASO 2: PREPARAR DATOS PARA LA VISUALIZACIÓN (NORMALIZAR Y UNIR LOGOS) ---
  
  datos_graficos_temporada <- analisis_equipo_temporada %>%
    left_join(logos_df, by = "team.name") %>%
    group_by(temporada, tipo_cobro) %>%
    mutate(tasa_normalizada = scales::rescale(tasa_de_remate)) %>%
    ungroup() %>%
    mutate(
      team_label = paste0("<img src='", logo_url, "' width='20' /> ", team.name)
    ) %>%
    filter(!is.na(logo_url))
  
  
  # --- PASO 3: BUCLE PARA GENERAR UN GRÁFICO POR CADA TEMPORADA ---
  
  # Obtenemos la lista de temporadas únicas para iterar sobre ellas
  lista_temporadas <- unique(datos_graficos_temporada$temporada)
  
  message("--- Generando un gráfico de perfiles por cada temporada... ---")
  
  for (temp in lista_temporadas) {
    
    datos_a_graficar <- datos_graficos_temporada %>%
      filter(temporada == temp)
    p <- ggplot(datos_a_graficar, aes(x = tipo_cobro, y = tasa_normalizada, fill = tipo_cobro)) +
      geom_col(show.legend = FALSE) +
      geom_text(
        aes(label = scales::percent(tasa_de_remate, accuracy = 0.1)),
        hjust = -0.1, size = 2.5
      ) +
      coord_flip() +
      facet_wrap(~ team_label, ncol = 3) + # ncol=3 se ve bien en la mayoría de las pantallas
      scale_fill_brewer(palette = "Set2") +
      scale_y_continuous(limits = c(0, 1.2), labels = NULL) + # Quitamos etiquetas del eje Y para limpiar
      labs(
        title = paste("Perfil Táctico de Córners - Temporada:", temp),
        subtitle = "Efectividad Relativa (Tasa de Remate Normalizada) por Tipo de Cobro",
        caption = "La longitud de la barra indica la efectividad del equipo en comparación con el resto de la liga en esa temporada."
      ) +
      theme_minimal(base_size = 12) +
      theme(
        panel.grid.major.y = element_blank(),
        panel.spacing = unit(1.5, "lines"),
        axis.title = element_blank(),
        axis.text.y = element_text(size = 7), 
        axis.text.x = element_blank(), 
        strip.text = element_markdown(size = 9, hjust = 0, face = "bold")
      )
    
    
    print(p)
  }
  
  
  
  
  # ===================================================================
  # === RADARES DE PERFIL TÁCTICO POR EQUIPO Y TEMPORADA ==============
  # ===================================================================
  
  # --- PASO 1: CONSOLIDAR TODAS LAS MÉTRICAS EN UNA TABLA ---
  message("--- Consolidando todas las métricas por equipo y temporada... ---")
  
  # 1.1 Empezamos con el análisis por equipo y temporada que ya teníamos
  metricas_base <- todos_los_eventos_mx %>%
    filter(id %in% ids_de_corners) %>%
    left_join(partidos_a_descargar %>% select(match_id, temporada = season.season_name), by = "match_id") %>%
    mutate(
      tipo_cobro = case_when(
        pass.length < 25 ~ "Corto",
        pass.inswinging == TRUE ~ "Inswinger",
        pass.outswinging == TRUE ~ "Outswinger",
        TRUE ~ "Directo"
      )
    ) %>%
    left_join(analisis_tipo_cobro_df %>% select(id, termino_en_remate, fue_gol), by = "id") %>%
    filter(!is.na(temporada))
  
  # 1.2 Agregamos las métricas de conteo, calidad y frecuencia
  resumen_equipos_temporada <- metricas_base %>%
    group_by(temporada, team.name) %>%
    summarise(
      Remates = sum(termino_en_remate, na.rm = TRUE),
      Goles = sum(fue_gol, na.rm = TRUE),
      xG = sum(todos_los_eventos_mx$shot.statsbomb_xg[todos_los_eventos_mx$id %in% id[termino_en_remate]], na.rm = TRUE),
      OBV = sum(obv_total_net, na.rm = TRUE),
      Outswinger = sum(tipo_cobro == "Outswinger") / n(),
      Inswinger = sum(tipo_cobro == "Inswinger") / n(),
      Corto = sum(tipo_cobro == "Corto") / n(),
      Directo = sum(tipo_cobro == "Directo") / n(),
      Tasa_Remate = mean(termino_en_remate, na.rm = TRUE),
      .groups = 'drop'
    )
  
  # 1.3 Convertir a Percentiles para que sean comparables
  datos_percentiles <- resumen_equipos_temporada %>%
    filter(Remates > 5) %>%
    group_by(temporada) %>%
    mutate(across(Remates:Tasa_Remate, ~ round(percent_rank(.) * 100, 0))) %>%
    ungroup() %>%
    select(temporada, team.name, Remates, Goles, xG, OBV, Tasa_Remate, Outswinger, Inswinger, Corto, Directo)
  
  
  
  
  # ===================================================================
  # === RADARES "PRO" POR EQUIPO Y TEMPORADA  ======
  # ===================================================================
  
  
  
  # --- PASO 1: PREPARAR DATOS (Añadir etiqueta de logo) ---
  datos_percentiles_con_logos <- datos_percentiles %>%
    left_join(logos_df, by = "team.name") %>%
    filter(!is.na(logo_url)) %>% 
    mutate(
      team_label = paste0("<img src='", logo_url, "' width='30' />")
    )
  
  # --- PASO 2: BUCLE PARA GENERAR Y GUARDAR LOS RADARES MEJORADOS ---
  fs::dir_create("graficos_radares_pro") # Nueva carpeta para no sobreescribir
  equipos_y_temporadas <- datos_percentiles_con_logos %>% distinct(temporada, team.name, team_label)
  message("--- Generando y guardando radares 'Pro' por cada equipo y temporada... ---")
  
  for (i in 1:nrow(equipos_y_temporadas)) {
    
    try({
      temp_actual <- equipos_y_temporadas$temporada[i]
      equipo_actual <- equipos_y_temporadas$team.name[i]
      label_actual <- equipos_y_temporadas$team_label[i]
      
      message(paste0("Procesando: ", i, "/", nrow(equipos_y_temporadas), " - ", equipo_actual, " (", temp_actual, ")"))
      
      datos_radar_individual <- datos_percentiles_con_logos %>%
        filter(temporada == temp_actual, team.name == equipo_actual) %>%
        tidyr::pivot_longer(
          cols = Remates:Directo, # Seleccionamos las columnas de métricas
          names_to = "metrica",
          values_to = "valor_percentil"
        )
      p_radar_pro <- ggplot(datos_radar_individual, aes(x = metrica, y = valor_percentil)) +
        # Capa base para el radar
        geom_col(aes(y = 100), fill = "#2a2a2b", width = 1, color = "#4a4a4a") + # Fondo del círculo
        # Barra principal que muestra el valor
        geom_col(aes(fill = metrica), width = 1, alpha = 0.8, show.legend = FALSE) +
        # Polígono para conectar los puntos
        #geom_polygon(aes(group = 1), fill = NA, color = "white", linewidth = 1.2) +
        # Puntos en cada vértice
        geom_point(color = "white", size = 3) +
        # Líneas guía circulares
        geom_hline(yintercept = c(25, 50, 75), color = "#4a4a4a", linetype = "dashed") +
        # Texto con el valor del percentil
        geom_text(aes(label = valor_percentil), color = "white", size = 3.5, fontface = "bold", nudge_y = 7) +
        
        coord_polar() +
        
        # Títulos con el logo del equipo
        labs(
          title = label_actual,
          subtitle = paste("Perfil Táctico de Córners |", temp_actual),
          caption = "Valores en percentiles (0-100) vs. el resto de la liga en esa temporada"
        ) +
        
        # Paleta de colores y tema oscuro
        scale_fill_brewer(palette = "Set1") +
        ylim(-20, 110) + # Damos espacio en el centro para los títulos
        theme_void() +
        theme(
          plot.background = element_rect(fill = "#1e1e1e", color = NA),
          plot.title = element_markdown(color = "white", size = 20, hjust = 0.5, margin = margin(t=10)),
          plot.subtitle = element_text(color = "white", size = 12, hjust = 0.5),
          plot.caption = element_text(color = "grey80", size = 8, hjust = 0.5, margin = margin(b=10)),
          axis.text.x = element_text(color = "white", size = 10, face = "bold")
        )
      
      # Guardar el gráfico
      nombre_archivo_seguro <- paste0(
        stringr::str_replace_all(temp_actual, "/", "-"), 
        "_", 
        stringr::str_replace_all(equipo_actual, "/", "-"), 
        ".png"
      )
      
      ggsave(
        filename = file.path("graficos_radares_pro", nombre_archivo_seguro),
        plot = p_radar_pro,
        width = 8, height = 8, dpi = 150
      )
    })
  }
  
  
  # ===================================================================
  # === MAPAS DE CALOR (POR TEMPORADA) =======
  # ===================================================================
  message("--- Preparando datos para los mapas de calor por equipo y temporada... ---")
  
  datos_heatmaps_temporada <- todos_los_eventos_mx %>%
    # Nos quedamos solo con los córners
    filter(id %in% ids_de_corners) %>%
    # Unimos la información de la temporada
    left_join(partidos_a_descargar %>% select(match_id, temporada = season.season_name), by = "match_id") %>%
    # Seleccionamos las columnas que necesitamos
    select(team.name, temporada, pass.end_location) %>%
    # Extraemos las coordenadas
    mutate(
      end_x = map_dbl(pass.end_location, 1, .default = NA),
      end_y = map_dbl(pass.end_location, 2, .default = NA)
    ) %>%
    # Quitamos filas que no tengan coordenadas o temporada
    filter(!is.na(end_x) & !is.na(temporada)) %>%
    # Unimos los logos para usarlos en los títulos
    left_join(logos_df, by = "team.name") %>%
    filter(!is.na(logo_url)) %>%
    # Creamos la etiqueta con el logo
    mutate(
      team_label = paste0("<img src='", logo_url, "' width='15' /> ", team.name)
    )
  
  # --- PASO 1: OBTENER LA LISTA DE TEMPORADAS (Sin cambios) ---
  lista_temporadas <- unique(datos_heatmaps_temporada$temporada)
  message("--- Generando un panel de mapas de calor sobre cancha esquemática por temporada... ---")
  
  # --- PASO 2: BUCLE PARA CREAR Y MOSTRAR UN GRÁFICO POR TEMPORADA (Versión Final) ---
  for (temp in lista_temporadas) {
    
    datos_a_graficar <- datos_heatmaps_temporada %>%
      filter(temporada == temp)
    p_temporada_final <- ggplot(datos_a_graficar, aes(x = end_x, y = end_y)) +
      
      # 1. Cancha esquemática clásica
      annotate_pitch(dimensions = pitch_statsbomb) +
      
      # 2. Mapa de calor
      stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = 0.6) +
      scale_fill_gradientn(colors = c("blue", "yellow", "red")) +
      
      # 3. Facetado por equipo
      facet_wrap(~ team_label, ncol = 6) +
      
      # 4. Títulos y tema
      labs(
        title = paste("Zonas de Destino de Córners - Temporada:", temp),
        subtitle = "Los mapas de calor muestran las áreas más frecuentes de recepción de centros",
        caption = "Datos: StatsBomb"
      ) +
      theme_pitch() +
      theme(
        legend.position = "none",
        # Ajustamos colores para el fondo claro por defecto de theme_pitch()
        plot.title = element_text(color = "black", size = 22, hjust = 0.5, margin = margin(b = 10)),
        plot.subtitle = element_text(color = "black", size = 14, hjust = 0.5, margin = margin(b = 10)),
        plot.caption = element_text(color = "black"),
        strip.background = element_rect(fill = "grey80"),
        strip.text = element_markdown(size = 9, color = "black", face = "bold", margin = margin(t = 5, b = 5))
      )
    
    # Imprimimos el gráfico para la temporada actual
    print(p_temporada_final)
  }
  
  message("--- ¡Proceso completado! Revisa tu pestaña 'Plots' para ver los nuevos gráficos. ---")
  
 
  # ===================================================================
  # === MAPAS DE ZONAS (VERSIÓN FINAL COMPATIBLE CON COORD_FLIP) ======
  # ===================================================================
  message("--- Iniciando la generación de mapas de zonas (versión compatible)... ---")
  
  # --- PASO 1: PREPARAR LOS DATOS (VERIFICADO) ---
  # (Este código no necesita cambios, solo nos aseguramos de que se ejecute)
  zonas_df <- tibble::tribble(
    ~zona_nombre, ~xmin, ~xmax, ~ymin, ~ymax,
    "Primer Poste - Área Chica", 114, 120, 0, 20, "Centro - Área Chica", 114, 120, 20, 60,
    "Segundo Poste - Área Chica", 114, 120, 60, 80, "Primer Poste - Área Grande", 102, 114, 0, 18,
    "Zona Punto Penal", 102, 114, 18, 62, "Segundo Poste - Área Grande", 102, 114, 62, 80,
    "Borde del Área", 90, 102, 18, 62
  )
  datos_zonas <- todos_los_eventos_mx %>%
    filter(id %in% ids_de_corners) %>%
    left_join(partidos_a_descargar %>% select(match_id, temporada = season.season_name), by = "match_id") %>%
    mutate(
      tipo_cobro = case_when(
        pass.length < 25 ~ "Corto", pass.inswinging == TRUE ~ "Inswinger",
        pass.outswinging == TRUE ~ "Outswinger", TRUE ~ "Directo/Otro"
      ),
      end_x = map_dbl(pass.end_location, 1, .default = NA),
      end_y = map_dbl(pass.end_location, 2, .default = NA)
    ) %>%
    filter(!is.na(end_x) & !is.na(temporada)) %>%
    fuzzyjoin::fuzzy_left_join(zonas_df, by = c("end_x" = "xmin", "end_x" = "xmax", "end_y" = "ymin", "end_y" = "ymax"), match_fun = list(`>=`, `<=`, `>=`, `<=`)) %>%
    mutate(zona_nombre = ifelse(is.na(zona_nombre), "Otra Zona", zona_nombre))
  
  conteos_por_zona <- datos_zonas %>%
    group_by(temporada, team.name, tipo_cobro, zona_nombre) %>%
    summarise(total_en_zona = n(), .groups = 'drop')
  
  datos_grafico_final <- zonas_df %>%
    left_join(conteos_por_zona, by = "zona_nombre") %>%
    left_join(logos_df, by = "team.name") %>%
    filter(!is.na(logo_url)) %>%
    mutate(team_label = paste0("<img src='", logo_url, "' width='25' /> ", team.name))
  
  
  # --- PASO 2: BUCLE DE VISUALIZACIÓN FINAL (CON CANCHA COMPLETA Y LOGOS) ---
  
  fs::dir_create("mapas_de_zonas_final")
  equipos_y_temporadas <- datos_grafico_final %>% distinct(temporada, team.name, team_label)
  
  for (i in 1:nrow(equipos_y_temporadas)) {
    
    temp_actual <- equipos_y_temporadas$temporada[i]
    equipo_actual <- equipos_y_temporadas$team.name[i]
    label_actual <- equipos_y_temporadas$team_label[i]
    
    message(paste0("Generando mapa de zonas para: ", equipo_actual, " (", temp_actual, ")"))
    
    datos_a_graficar <- datos_grafico_final %>%
      filter(temporada == temp_actual, team.name == equipo_actual)
    
    p_zonas_final <- ggplot(datos_a_graficar) +
      
      annotate_pitch(dimensions = pitch_statsbomb) +
      
      geom_rect(
        aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, color = tipo_cobro),
        fill = "white", alpha = 0.5, linewidth = 1.2
      ) +
      
      geom_text(
        aes(x = (xmin + xmax) / 2, y = (ymin + ymax) / 2, label = total_en_zona),
        size = 5, color = "black", fontface = "bold", na.rm = TRUE 
      ) +
      
      coord_flip(xlim = c(0, 120), ylim = c(0, 80)) +
      
      facet_wrap(~ tipo_cobro, ncol = 4) +
      
    
      labs(
        title = paste("Mapa de Zonas de Córner:", label_actual),
        subtitle = paste("Temporada:", temp_actual, "| Número de córners dirigidos a cada zona"),
        caption = "Datos: StatsBomb"
      ) +
      theme_pitch() +
      theme(
        legend.position = "none",
        plot.title = element_markdown(hjust = 0.5, size = 20), 
        plot.subtitle = element_text(hjust = 0.5, size = 14),
        strip.text = element_text(face = "bold", size = 12)
      )
 
    
    nombre_archivo_seguro <- paste0(
      stringr::str_replace_all(temp_actual, "/", "-"), "_", 
      stringr::str_replace_all(equipo_actual, "/", "-"), ".png"
    )
    
    ggsave(
      filename = file.path("mapas_de_zonas_final", nombre_archivo_seguro),
      plot = p_zonas_final,
      width = 16, height = 9, dpi = 100
    )
  }
  
  
  # ===================================================================
  # === RATIO DE CÓRNERS POR GOL ============
  # ===================================================================
  
  # --- PASO 1: ANÁLISIS AGREGADO ---
  message("--- Generando tabla de Ratio de Córners por Gol (Agregado con Logos)... ---")
  
  # 1.1 Calcular totales y unir con los logos
  ratio_agregado_logos <- todos_los_eventos_mx %>%
    filter(id %in% ids_de_corners) %>%
    left_join(analisis_tipo_cobro_df %>% select(id, fue_gol), by = "id") %>%
    group_by(team.name) %>%
    summarise(total_corners = n(), total_goles_de_corner = sum(fue_gol, na.rm = TRUE)) %>%
    ungroup() %>%
    filter(total_goles_de_corner > 0) %>%
    mutate(ratio_corners_por_gol = total_corners / total_goles_de_corner) %>%
    arrange(ratio_corners_por_gol) %>%
    left_join(logos_df, by = "team.name") %>% # Unimos la info del logo
    select(logo_url, Equipo = team.name, `Córners por Gol` = ratio_corners_por_gol)
  
  # 1.2 Crear la tabla formateada con 'gt' y logos
  tabla_agregada_gt_logos <- ratio_agregado_logos %>%
    gt() %>%
    # LA MAGIA ESTÁ AQUÍ:
    text_transform(
      locations = cells_body(columns = logo_url), # Aplicar a la columna del logo
      fn = function(x) {
        web_image(url = x, height = 30) # Convertir la URL en una imagen
      }
    ) %>%
    cols_label(logo_url = "") %>% # Dejamos el título de la columna del logo en blanco
    fmt_number(columns = `Córners por Gol`, decimals = 2) %>%
    tab_header(title = md("**Ratio de Córners por Gol Anotado**"), subtitle = "Agregado de todas las temporadas") %>%
    cols_align(align = "center", columns = everything()) %>%
    data_color(
      columns = `Córners por Gol`,
      colors = scales::col_numeric(palette = c("green", "yellow", "red"), domain = NULL)
    )
  
  print(tabla_agregada_gt_logos)
  
  
  # --- PASO 2: ANÁLISIS POR TEMPORADA ---
  message("--- Generando tablas de Ratio de Córners por Gol (por Temporada con Logos)... ---")
  
  # 2.1 Calcular totales por equipo y temporada y unir con logos
  ratio_por_temporada_logos <- todos_los_eventos_mx %>%
    filter(id %in% ids_de_corners) %>%
    left_join(partidos_a_descargar %>% select(match_id, temporada = season.season_name), by = "match_id") %>%
    left_join(analisis_tipo_cobro_df %>% select(id, fue_gol), by = "id") %>%
    filter(!is.na(temporada)) %>%
    group_by(temporada, team.name) %>%
    summarise(total_corners = n(), total_goles_de_corner = sum(fue_gol, na.rm = TRUE), .groups = 'drop') %>%
    filter(total_goles_de_corner > 0) %>%
    mutate(ratio_corners_por_gol = total_corners / total_goles_de_corner) %>%
    arrange(temporada, ratio_corners_por_gol) %>%
    left_join(logos_df, by = "team.name")
  
  # 2.2 Bucle para crear una tabla para cada temporada
  lista_temporadas_ratio <- unique(ratio_por_temporada_logos$temporada)
  for (temp in lista_temporadas_ratio) {
    
    tabla_temporada_actual <- ratio_por_temporada_logos %>%
      filter(temporada == temp) %>%
      select(logo_url, Equipo = team.name, `Córners por Gol` = ratio_corners_por_gol)
    
    tabla_temporada_gt_logos <- tabla_temporada_actual %>%
      gt() %>%
      text_transform(
        locations = cells_body(columns = logo_url),
        fn = function(x) { web_image(url = x, height = 30) }
      ) %>%
      cols_label(logo_url = "") %>%
      fmt_number(columns = `Córners por Gol`, decimals = 2) %>%
      tab_header(title = md("**Ratio de Córners por Gol Anotado**"), subtitle = paste("Temporada:", temp)) %>%
      cols_align(align = "center", columns = everything()) %>%
      data_color(
        columns = `Córners por Gol`,
        colors = scales::col_numeric(palette = c("green", "yellow", "red"), domain = NULL)
      )
    
    print(tabla_temporada_gt_logos)
  }
  
  
  
  
  
  america_2223 <- todos_los_eventos_mx %>%
    filter(id %in% ids_de_corners) %>%
    left_join(partidos_a_descargar %>% select(match_id, temporada = season.season_name), by = "match_id") %>%
    left_join(analisis_tipo_cobro_df %>% select(id, fue_gol), by = "id") %>%
    filter(temporada == "2022/2023",
           grepl("Am(é|e)rica", team.name)) %>%   # cubre "América"/"America"
    summarise(
      corners_totales = n(),
      goles_de_corner = sum(fue_gol, na.rm = TRUE)
    )
  
  america_2223
  
  # 1) Disparos que terminaron en gol y vienen de corner (StatsBomb: "From Corner")
  verif_shots <- todos_los_eventos_mx %>%
    filter(type.name == "Shot") %>%
    filter(shot.outcome.name == "Goal") %>%
    filter(shot.type.name %in% c("Corner", "From Corner") | play_pattern.name == "From Corner") %>%
    left_join(partidos_a_descargar %>% select(match_id, temporada = season.season_name), by = "match_id") %>%
    filter(temporada == "2022/2023",
           grepl("Am(é|e)rica", team.name)) %>%
    select(match_id, minute, second, team.name, player.name, shot.type.name, play_pattern.name)
  
  nrow(verif_shots) 
  verif_shots
  
  
  
  
  # ===================================================================
  # === RATIO DE CÓRNERS POR GOL =====
  # ===================================================================
  
  # --- PASO 1: IDENTIFICAR LOS GOLES QUE OCURREN EN SECUENCIA DE CÓRNER ---
  message("--- Identificando goles en secuencia de córner (1ra y 2da jugada)... ---")
  
  # 1.1 Obtenemos los córners con su información completa
  corners_info <- todos_los_eventos_mx %>%
    filter(id %in% ids_de_corners) %>%
    select(id, match_id, possession, timestamp, team.id, team.name)
  
  # 1.2 Obtenemos todos los goles con su información completa
  goles_info <- todos_los_eventos_mx %>%
    filter(shot.outcome.name == "Goal") %>%
    select(match_id, possession, timestamp, team.id, team.name)
  
  # 1.3 Unimos para encontrar goles en la misma posesión y después de un córner
  secuencias_de_gol <- corners_info %>%
    inner_join(goles_info, by = c("match_id", "possession", "team.id", "team.name"), relationship = "many-to-many") %>%
    filter(timestamp.y > timestamp.x) %>%
    distinct(id, .keep_all = TRUE) %>%
    rename(corner_id = id)
  
  # --- PASO 2: CALCULAR TOTALES Y RATIOS ---
  
  # 2.1 Tabla de Córners Totales
  corners_totales <- todos_los_eventos_mx %>%
    filter(id %in% ids_de_corners) %>%
    left_join(partidos_a_descargar %>% select(match_id, temporada = season.season_name), by = "match_id") %>%
    count(temporada, team.name, name = "total_corners")
  
  # 2.2 Tabla de Goles de Secuencia Totales
  goles_secuencia_totales <- secuencias_de_gol %>%
    left_join(partidos_a_descargar %>% select(match_id, temporada = season.season_name), by = "match_id") %>%
    count(temporada, team.name, name = "total_goles_secuencia")
  
  # 2.3 Unimos todo para el análisis POR TEMPORADA
  ratio_por_temporada <- corners_totales %>%
    left_join(goles_secuencia_totales, by = c("temporada", "team.name")) %>%
    mutate(total_goles_secuencia = ifelse(is.na(total_goles_secuencia), 0, total_goles_secuencia)) %>%
    filter(total_goles_secuencia > 0) %>%
    mutate(ratio_corners_por_gol = total_corners / total_goles_secuencia) %>%
    arrange(temporada, ratio_corners_por_gol) %>%
    left_join(logos_df, by = "team.name")
  
  # 2.4 Agregamos para el análisis TOTAL
  ratio_agregado <- ratio_por_temporada %>%
    group_by(logo_url, Equipo = team.name) %>%
    summarise(
      total_corners_agg = sum(total_corners),
      total_goles_agg = sum(total_goles_secuencia),
      .groups = 'drop'
    ) %>%
    mutate(`Córners por Gol (Secuencia)` = total_corners_agg / total_goles_agg) %>%
    arrange(`Córners por Gol (Secuencia)`) %>%
    select(logo_url, Equipo, `Córners por Gol (Secuencia)`)
  
  
  # --- PASO 3: CREAR Y MOSTRAR LAS TABLAS ---
  
  # 3.1 Tabla Agregada
  print(
    ratio_agregado %>%
      gt() %>%
      text_transform(locations = cells_body(columns = logo_url), fn = function(x) web_image(url = x, height = 30)) %>%
      cols_label(logo_url = "") %>%
      fmt_number(columns = `Córners por Gol (Secuencia)`, decimals = 2) %>%
      tab_header(title = md("**Ratio de Córners por Gol de Secuencia**"), subtitle = "Agregado de todas las temporadas (incluye 1ra y 2da jugada)") %>%
      data_color(columns = `Córners por Gol (Secuencia)`, colors = scales::col_numeric(palette = c("#63BE7B", "#FFEB84", "#F8696B"), domain = NULL))
  )
  
  # 3.2 Bucle de Tablas por Temporada
  lista_temporadas_ratio_sec <- unique(ratio_por_temporada$temporada)
  for (temp in lista_temporadas_ratio_sec) {
    tabla_temporada_gt <- ratio_por_temporada %>%
      filter(temporada == temp) %>%
      select(logo_url, Equipo = team.name, `Córners por Gol (Secuencia)` = ratio_corners_por_gol) %>%
      gt() %>%
      text_transform(locations = cells_body(columns = logo_url), fn = function(x) web_image(url = x, height = 30)) %>%
      cols_label(logo_url = "") %>%
      fmt_number(columns = `Córners por Gol (Secuencia)`, decimals = 2) %>%
      tab_header(title = md("**Ratio de Córners por Gol de Secuencia**"), subtitle = paste("Temporada:", temp)) %>%
      data_color(columns = `Córners por Gol (Secuencia)`, colors = scales::col_numeric(palette = c("#63BE7B", "#FFEB84", "#F8696B"), domain = NULL))
    
    print(tabla_temporada_gt)
  }
  
  
  
  # ===================================================================
  # === GRÁFICOS DE DISPERSIÓN (OCASIONES vs DEPENDENCIA) =============
  # ===================================================================
  
  # --- PASO 1: CALCULAR MÉTRICAS BASE POR EQUIPO Y TEMPORADA ---
  message("--- Calculando métricas para los gráficos de dispersión... ---")
  
  # 1.1 Necesitamos el total de minutos jugados por equipo en cada temporada
  minutos_jugados <- partidos_a_descargar %>%
    mutate(partidos_jugados = 1) %>% # Cada fila es un partido
    pivot_longer(
      cols = c(home_team.home_team_name, away_team.away_team_name),
      names_to = "tipo_equipo",
      values_to = "team.name"
    ) %>%
    group_by(temporada = season.season_name, team.name) %>%
    summarise(minutos_totales = sum(partidos_jugados) * 90, .groups = 'drop')
  
  # 1.2 Calcular ocasiones (remates) a balón parado y totales
  ocasiones_por_equipo <- todos_los_eventos_mx %>%
    filter(type.name == "Shot") %>%
    left_join(partidos_a_descargar %>% select(match_id, temporada = season.season_name), by = "match_id") %>%
    mutate(
      es_balon_parado = play_pattern.name %in% c("From Corner", "From Free Kick", "From Throw-in")
    ) %>%
    group_by(temporada, team.name) %>%
    summarise(
      ocasiones_abp = sum(es_balon_parado, na.rm = TRUE),
      ocasiones_totales = n(),
      .groups = 'drop'
    )
  
  # 1.3 Unir todo y calcular las métricas finales
  metricas_dispersion <- ocasiones_por_equipo %>%
    left_join(minutos_jugados, by = c("temporada", "team.name")) %>%
    filter(!is.na(minutos_totales) & minutos_totales > 0) %>% # Asegurarse de que hay minutos jugados
    mutate(
      ocasiones_abp_por_90 = (ocasiones_abp / minutos_totales) * 90,
      pct_ocasiones_abp = ocasiones_abp / ocasiones_totales
    ) %>%
    left_join(logos_df, by = "team.name") %>%
    filter(!is.na(logo_url)) %>%
    mutate(
      team_label = paste0("<img src='", logo_url, "' width='25' />")
    )
  
  # --- PASO 2: VISUALIZACIÓN AGREGADA (Todas las Temporadas) ---
  message("--- Generando gráfico de dispersión agregado... ---")
  
  # 2.1 Calcular métricas agregadas
  metricas_agregadas <- metricas_dispersion %>%
    group_by(team.name, team_label) %>%
    summarise(
      ocasiones_abp = sum(ocasiones_abp),
      ocasiones_totales = sum(ocasiones_totales),
      minutos_totales = sum(minutos_totales),
      .groups = 'drop'
    ) %>%
    mutate(
      ocasiones_abp_por_90 = (ocasiones_abp / minutos_totales) * 90,
      pct_ocasiones_abp = ocasiones_abp / ocasiones_totales
    )
  
  # 2.2 Crear el gráfico
  p_agregado <- ggplot(metricas_agregadas, aes(x = ocasiones_abp_por_90, y = pct_ocasiones_abp)) +
    geom_richtext(aes(label = team_label), fill = NA, label.color = NA) + # Usamos geom_richtext para los logos
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(
      title = "Eficacia y Dependencia del Balón Parado (Agregado)",
      x = "Ocasiones de Gol Generadas a Balón Parado por 90'",
      y = "% de Ocasiones Generadas a Balón Parado"
    ) +
    theme_minimal()
  
  print(p_agregado)
  
  
  # --- PASO 3: BUCLE DE VISUALIZACIONES POR TEMPORADA ---
  message("--- Generando gráficos de dispersión por temporada... ---")
  lista_temporadas_dispersion <- unique(metricas_dispersion$temporada)
  
  for (temp in lista_temporadas_dispersion) {
    
    datos_a_graficar <- metricas_dispersion %>%
      filter(temporada == temp)
    
    p_temporada <- ggplot(datos_a_graficar, aes(x = ocasiones_abp_por_90, y = pct_ocasiones_abp)) +
      geom_richtext(aes(label = team_label), fill = NA, label.color = NA) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      labs(
        title = paste("Eficacia y Dependencia del Balón Parado - Temporada:", temp),
        x = "Ocasiones de Gol Generadas a Balón Parado por 90'",
        y = "% de Ocasiones a Balón Parado"
      ) +
      theme_minimal()
    
    print(p_temporada)
  }
  
  
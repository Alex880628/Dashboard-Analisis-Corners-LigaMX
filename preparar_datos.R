# ===================================================================
# SCRIPT: preparar_datos.R (VERSIÓN FINAL UNIFICADA)
# OBJETIVO: Leer, procesar y guardar todos los datos para la app,
# asegurando que TODOS los dataframes usen nombres de equipo consistentes.
# ===================================================================

# --- 1) LIBRERÍAS ---
suppressPackageStartupMessages({
  library(tidyverse)
  library(arrow)
  library(fs)
  library(glue)
  library(ggsoccer)
  library(fuzzyjoin)
  library(scales)
})

# --- 2) CREAR CARPETA DE SALIDA ---
dir_create("shiny_data")

# --- 3) CARGA DE DATOS CRUDOS ---
print("Iniciando la carga de datos crudos...")
eventos_crudos <- open_dataset("data/events_360/", format = "parquet") |> collect()
partidos_crudos <- read_parquet("data/matches/mx_matches.parquet")
print("Datos crudos cargados exitosamente.")

# --- 4) !! CORRECCIÓN UNIFICADA DE NOMBRES !! ---
# Este es el paso más importante. Corregimos los nombres en la fuente.
print("Corrigiendo nombres de equipos en los datos crudos...")
eventos_crudos_corregidos <- eventos_crudos %>%
  mutate(
    team.name = recode(team.name,
                       "América" = "Club América", "Juárez" = "FC Juárez", "Mazatlán" = "Mazatlán FC",
                       "Necaxa" = "Club Necaxa", "Tijuana" = "Club Tijuana", "León" = "Club León",
                       "Puebla" = "Club Puebla"
    )
  )
partidos_crudos_corregidos <- partidos_crudos %>%
  mutate(
    home_team.home_team_name = recode(home_team.home_team_name,
                                      "América" = "Club América", "Juárez" = "FC Juárez", "Mazatlán" = "Mazatlán FC",
                                      "Necaxa" = "Club Necaxa", "Tijuana" = "Club Tijuana", "León" = "Club León",
                                      "Puebla" = "Club Puebla"
    ),
    away_team.away_team_name = recode(away_team.away_team_name,
                                      "América" = "Club América", "Juárez" = "FC Juárez", "Mazatlán" = "Mazatlán FC",
                                      "Necaxa" = "Club Necaxa", "Tijuana" = "Club Tijuana", "León" = "Club León",
                                      "Puebla" = "Club Puebla"
    )
  )

# --- 5) CREACIÓN DE DATOS BASE (usando datos corregidos) ---
# ... (La función construir_objetos_base es la misma, no necesita cambios)
construir_objetos_base <- function(todos_los_eventos_mx, partidos_df){
  ids_de_corners <- todos_los_eventos_mx |> filter(pass.type.name == "Corner") |> pull(id)
  remates_con_ff <- todos_los_eventos_mx |> filter(type.name == "Shot", !sapply(shot.freeze_frame, is.null))
  remates_asistidos_por_corner <- remates_con_ff |> filter(shot.key_pass_id %in% ids_de_corners)
  AREA_CHICA_X_MIN <- 114; AREA_CHICA_Y_MIN <- 30; AREA_CHICA_Y_MAX <- 50
  corners_exitosos_conteo <- remates_asistidos_por_corner |> select(id, shot.outcome.name, shot.freeze_frame) |> unnest(shot.freeze_frame) |> mutate(location_x = map_dbl(location, 1, .default = NA), location_y = map_dbl(location, 2, .default = NA)) |> group_by(id) |> summarise(atacantes_en_area_chica = sum(location_x >= AREA_CHICA_X_MIN & location_y >= AREA_CHICA_Y_MIN & location_y <= AREA_CHICA_Y_MAX & teammate == TRUE), fue_gol = first(shot.outcome.name) == "Goal", .groups = "drop")
  corners_exitosos_con_categoria <- corners_exitosos_conteo |> mutate(categoria_atacantes = factor(case_when(atacantes_en_area_chica >= 4 ~ "4+ Atacantes", TRUE ~ paste0(atacantes_en_area_chica, " Atacantes")), levels = c("0 Atacantes","1 Atacantes","2 Atacantes","3 Atacantes","4+ Atacantes")))
  datos_plot_base <- remates_asistidos_por_corner |> select(id, match_id, shot.key_pass_id, equipo_atacante = team.name, rematador = player.name, minuto = minute, segundo = second, shot.outcome.name, xG = shot.statsbomb_xg, xGoT = shot.shot_execution_xg) |> left_join(partidos_df |> select(match_id, fecha = match_date, equipo_local = home_team.home_team_name, equipo_visitante = away_team.away_team_name, temporada = season.season_name), by = "match_id") |> left_join(todos_los_eventos_mx |> filter(id %in% ids_de_corners) |> select(id, ejecutor = player.name), by = c("shot.key_pass_id" = "id")) |> left_join(corners_exitosos_con_categoria, by = "id") |> left_join(todos_los_eventos_mx |> select(id, start_location = location, end_location = pass.end_location), by = c("shot.key_pass_id" = "id")) |> mutate(fue_gol = shot.outcome.name == "Goal", start_x = map_dbl(start_location, 1, .default = NA), start_y = map_dbl(start_location, 2, .default = NA), end_x   = map_dbl(end_location, 1, .default = NA), end_y   = map_dbl(end_location, 2, .default = NA)) |> filter(!is.na(categoria_atacantes))
  corners_base <- todos_los_eventos_mx |> filter(id %in% ids_de_corners) |> mutate(tipo_cobro = case_when(pass.length < 25 ~ "Corto", pass.inswinging == TRUE ~ "Inswinger", pass.outswinging == TRUE ~ "Outswinger", TRUE ~ "Directo/Otro")) |> select(id, match_id, team.name, tipo_cobro, obv_total_net)
  resultados_lookup <- remates_asistidos_por_corner |> left_join(corners_exitosos_conteo |> select(id, fue_gol), by = "id") |> select(corner_id = shot.key_pass_id, fue_gol) |> mutate(termino_en_remate = TRUE, fue_gol = ifelse(is.na(fue_gol), FALSE, fue_gol))
  analisis_tipo_cobro_df <- corners_base |> left_join(resultados_lookup, by = c("id" = "corner_id")) |> mutate(termino_en_remate = ifelse(is.na(termino_en_remate), FALSE, termino_en_remate), fue_gol = ifelse(is.na(fue_gol), FALSE, fue_gol))
  list(ids_de_corners = ids_de_corners, remates_asistidos_por_corner = remates_asistidos_por_corner, corners_exitosos_conteo = corners_exitosos_conteo, corners_exitosos_con_categoria = corners_exitosos_con_categoria, datos_plot_base = datos_plot_base, analisis_tipo_cobro_df = analisis_tipo_cobro_df, eventos_crudos = todos_los_eventos_mx, partidos_crudos = partidos_df)
}
print("Procesando y construyendo objetos base...")
datos_base = construir_objetos_base(eventos_crudos_corregidos, partidos_crudos_corregidos)
print("Objetos base creados.")

# --- 6) PREPARACIÓN DE DATOS ESPECÍFICOS (usando datos corregidos) ---
print("Preparando datos específicos para las visualizaciones...")

# --- Tarea A: Preparar datos para S13 (Dispersión) ---
minutos <- datos_base$partidos_crudos |>
  mutate(partidos_jugados = 1) |>
  pivot_longer(cols = c(home_team.home_team_name, away_team.away_team_name),
               names_to = "tipo", values_to = "team.name") |>
  group_by(temporada = season.season_name, team.name) |>
  summarise(minutos_totales = sum(partidos_jugados)*90, .groups = 'drop')
ocasiones <- datos_base$eventos_crudos |>
  filter(type.name == "Shot") |>
  left_join(datos_base$partidos_crudos |> select(match_id, temporada = season.season_name), by = "match_id") |>
  mutate(es_balon_parado = play_pattern.name %in% c("From Corner","From Free Kick","From Throw-in")) |>
  group_by(temporada, team.name) |>
  summarise(ocasiones_abp = sum(es_balon_parado, na.rm = TRUE),
            ocasiones_totales = n(), .groups = 'drop')
datos_s13 <- left_join(ocasiones, minutos, by = c("temporada","team.name")) |>
  filter(!is.na(minutos_totales) & minutos_totales > 0) |>
  mutate(ocasiones_abp_por_90 = (ocasiones_abp/minutos_totales)*90,
         pct_ocasiones_abp = ocasiones_abp/ocasiones_totales)

# --- Tarea B: Crear el Dataframe de Logos ---
# ... (el código de logos_df es el mismo y está correcto)
datos_logos_vector <- c("América", "https://e01-mx-marca.uecdn.es/mx/assets/sports/logos/football/png/72x72/1292.png", "Atlas", "https://e01-mx-marca.uecdn.es/mx/assets/sports/logos/football/png/72x72/1284.png", "Atlético San Luis", "https://e01-mx-marca.uecdn.es/mx/assets/sports/logos/football/png/72x72/9491.png", "Cruz Azul", "https://e01-mx-marca.uecdn.es/mx/assets/sports/logos/football/png/72x72/853.png", "Guadalajara", "https://e01-mx-marca.uecdn.es/mx/assets/sports/logos/football/png/72x72/1283.png", "Juárez", "https://e01-mx-marca.uecdn.es/mx/assets/sports/logos/football/png/72x72/10991.png", "León", "https://e01-mx-marca.uecdn.es/mx/assets/sports/logos/football/png/72x72/1293.png", "Mazatlán", "https://e01-mx-marca.uecdn.es/mx/assets/sports/logos/football/png/72x72/16906.png", "Monterrey", "https://e01-mx-marca.uecdn.es/mx/assets/sports/logos/football/png/72x72/741.png", "Necaxa", "https://e01-mx-marca.uecdn.es/mx/assets/sports/logos/football/png/72x72/1174.png", "Pachuca", "https://e01-mx-marca.uecdn.es/mx/assets/sports/logos/football/png/72x72/1295.png", "Puebla", "https://e01-mx-marca.uecdn.es/mx/assets/sports/logos/football/png/72x72/1296.png", "Pumas UNAM", "https://e01-mx-marca.uecdn.es/mx/assets/sports/logos/football/png/72x72/1297.png", "Querétaro", "https://e01-mx-marca.uecdn.es/mx/assets/sports/logos/football/png/72x72/4047.png", "Santos Laguna", "https://e01-mx-marca.uecdn.es/mx/assets/sports/logos/football/png/72x72/1287.png", "Tigres UANL", "https://e01-mx-marca.uecdn.es/mx/assets/sports/logos/football/png/72x72/1294.png", "Tijuana", "https://e01-mx-marca.uecdn.es/mx/assets/sports/logos/football/png/72x72/4690.png", "Toluca", "https://e01-mx-marca.uecdn.es/mx/assets/sports/logos/football/png/72x72/1286.png")
logos_df_inicial <- tibble(nombre_corto = datos_logos_vector[seq(1, length(datos_logos_vector), by = 2)], logo_url = datos_logos_vector[seq(2, length(datos_logos_vector), by = 2)])
logos_df <- logos_df_inicial |> mutate(team.name = case_when(nombre_corto == "América" ~ "Club América", nombre_corto == "Juárez" ~ "FC Juárez", nombre_corto == "Mazatlán" ~ "Mazatlán FC", nombre_corto == "Necaxa" ~ "Club Necaxa", nombre_corto == "Tijuana" ~ "Club Tijuana", nombre_corto == "León" ~ "Club León", nombre_corto == "Puebla" ~ "Club Puebla", TRUE ~ nombre_corto)) |> select(team.name, logo_url)

# --- Tarea C: Preparar Datos para los Radares (usando datos corregidos) ---
metricas_base_radar <- datos_base$eventos_crudos %>% # Ya viene con nombres corregidos
  filter(id %in% datos_base$ids_de_corners) %>%
  left_join(datos_base$partidos_crudos %>% select(match_id, temporada = season.season_name), by = "match_id") %>%
  mutate(tipo_cobro = case_when(pass.length < 25 ~ "Corto", pass.inswinging == TRUE ~ "Inswinger", pass.outswinging == TRUE ~ "Outswinger", TRUE ~ "Directo")) %>%
  left_join(datos_base$analisis_tipo_cobro_df %>% select(id, termino_en_remate, fue_gol), by = "id") %>%
  filter(!is.na(temporada))
resumen_equipos_temporada <- metricas_base_radar %>%
  group_by(temporada, team.name) %>%
  summarise(Remates = sum(termino_en_remate, na.rm = TRUE), Goles = sum(fue_gol, na.rm = TRUE), xG = sum(datos_base$eventos_crudos$shot.statsbomb_xg[datos_base$eventos_crudos$id %in% id[termino_en_remate]], na.rm = TRUE), OBV = sum(obv_total_net, na.rm = TRUE), Outswinger = sum(tipo_cobro == "Outswinger") / n(), Inswinger = sum(tipo_cobro == "Inswinger") / n(), Corto = sum(tipo_cobro == "Corto") / n(), Directo = sum(tipo_cobro == "Directo") / n(), Tasa_Remate = mean(termino_en_remate, na.rm = TRUE), .groups = 'drop')
datos_radares <- resumen_equipos_temporada %>%
  filter(Remates > 0) %>%
  group_by(temporada) %>%
  mutate(across(Remates:Tasa_Remate, ~ round(percent_rank(.) * 100, 0))) %>%
  ungroup() %>%
  select(temporada, team.name, Remates, Goles, xG, OBV, Tasa_Remate, Outswinger, Inswinger, Corto, Directo)

# --- 7) GUARDADO DE ARCHIVOS FINALES ---
print("Guardando archivos .rds finales...")
saveRDS(datos_base, file = "shiny_data/datos_base.rds")
saveRDS(datos_s13, file = "shiny_data/datos_s13.rds")
saveRDS(logos_df, file = "shiny_data/logos_df.rds")
saveRDS(datos_radares, file = "shiny_data/datos_radares.rds")

print("---------------------------------------------------------------------")
print("¡ÉXITO! Se han creado los archivos .rds con nombres de equipo corregidos.")
print("Ya puedes ejecutar 'app.R'.")
print("---------------------------------------------------------------------")
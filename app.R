
# ===================================================================
# Shiny App: Liga MX — ABP Corners (Hackathon 2025)
# AUTOR: Alejandro Hernández Toriz
# VERSIÓN: Optimizada con datos pre-procesados y todas las visualizaciones.
# ===================================================================

# ---------- 1) LIBRERÍAS ----------
suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(tidyverse)
  library(arrow)
  library(fs)
  library(glue)
  library(ggtext)
  library(plotly)
  library(ggsoccer)
  library(fuzzyjoin)
  library(scales)
  library(gt)
  library(ggrepel)
  library(ggimage)
})

# ---------- 2) UTILIDADES GLOBALES ----------
plot_theme <- function(base = 12) theme_minimal(base_size = base) + theme(legend.position = "top")
TEMPORADAS_OBJ <- c("2021/2022","2022/2023","2023/2024","2024/2025")
imgPath <- "www/portada.png"
if (!file.exists(imgPath)) {
  stop("¡¡ERROR CRÍTICO!!: La imagen 'portada.png' no se encuentra en la carpeta 'www'.")
}


# ---------- 3) UI (CON NUEVOS SELECTORES) ----------
app_theme <- bs_theme(
  version = 5, bootswatch = "cosmo",
  primary = "#D71920", secondary = "#76B82A"
)

ui <- page_navbar(
  title = span("Hackathon 2025 · Liga MX — ABP Corners", style = "font-weight:700"),
  theme = app_theme, window_title = "ABP Corners — Liga MX",
  
  # --- BLOQUE DE LA PORTADA ---
  nav_panel(
    title = "Inicio",
    # Usamos HTML puro para forzar la estructura
    tags$div(
      class = "container-fluid",
      style = "padding: 0; margin: 0;",
      tags$div(
        # Contenedor con la imagen de fondo
        style = paste0(
          "position: relative; ",
          "width: 100%; ",
          "height: 80vh; ", # Aumentamos un poco la altura
          "background-image: url('portada.png'); ", # ¡VOLVEMOS AL MÉTODO SIMPLE!
          "background-size: cover; ",
          "background-position: center; ",
          "display: flex; ",
          "align-items: flex-end; ", # Alinea el contenido hijo abajo
          "justify-content: center; ", # Centra el contenido hijo horizontalmente
          "border-radius: 12px; ",
          "box-shadow: 0 6px 24px rgba(0,0,0,.3);"
        ),
        # Contenedor del texto
        tags$div(
          style = paste0(
            "color: white; ",
            "text-align: center; ",
            "padding-bottom: 50px; ", # Espacio desde el borde inferior
            "text-shadow: 0 0 10px rgba(0,0,0,0.8);"
          ),
          h1("Hackathon 2025", style="font-weight: 900;"), # Más grueso
          h3("Autor: Alejandro Hernández Toriz", style="font-weight: 700;"),
          p("Analítica de balón parado (córners) para Liga MX — temporadas 2021/22 a 2024/25.", style="font-weight: 500;")
        )
      )
    )
  ), 
  
  # --- BLOQUE DEL DASHBOARD DE ANÁLISIS ---
  nav_panel(
    title = "Análisis",
    layout_sidebar(
      sidebar = sidebar(
        h5("Secciones"),
        actionButton("s1", "1) Barras: Remates en córners", width = "100%"),
        actionButton("s2", "2) Remates con tooltips", width = "100%"),
        actionButton("s3", "3) Goles con tooltips", width = "100%"),
        actionButton("s4", "4) Comparación de córners", width = "100%"),
        actionButton("s5", "5) Tasa de gol por remate", width = "100%"),
        actionButton("s6", "6) Visualización por temporada", width = "100%"),
        actionButton("s7", "7) Efectividad por tipo de córner", width = "100%"),
        actionButton("s8", "8) Perfil táctico de córners", width = "100%"),
        actionButton("s9", "9) Radares de perfil táctico", width = "100%"),
        actionButton("s10", "10) Mapas de calor", width = "100%"),
        actionButton("s11", "11) Mapas de zonas", width = "100%"),
        actionButton("s12", "12) Ratios de córners por gol", width = "100%"),
        actionButton("s13", "13) Dispersión: Ocasiones vs Dependencia", width = "100%"),
        hr(),
        selectInput("temporada_sel", "Filtrar temporada:", choices = c("Todas", TEMPORADAS_OBJ), selected = "2021/2022"),
        selectInput("equipo_sel_s11", "Filtrar equipo (para Mapas):", choices = NULL),
        selectInput("equipo_sel_s9", "Filtrar equipo (para Radares):", choices = NULL)
      ),
      uiOutput("panel_contenido")
    )
  )
)

# ---------- 4) SERVER (COMPLETO Y FINAL) ----------
server <- function(input, output, session){
  
  # --- Carga de datos pre-procesados ---
  base <- reactive({ readRDS("shiny_data/datos_base.rds") })
  datos_s13 <- reactive({ readRDS("shiny_data/datos_s13.rds") })
  logos_df <- reactive({ readRDS("shiny_data/logos_df.rds") })
  datos_radares <- reactive({ readRDS("shiny_data/datos_radares.rds") })
  
  # --- Lógica de la App ---
  observeEvent(input$go_dash, { bslib::update_navs(session, selected = "Análisis") })
  filtrar_por_temporada <- function(df, col = "temporada"){
    if (isTruthy(input$temporada_sel) && input$temporada_sel != "Todas") df |> filter(.data[[col]] == input$temporada_sel) else df
  }
  seccion_activa <- reactiveVal("s1")
  for (id in paste0("s", 1:13)) { local({ myid <- id; observeEvent(input[[myid]], { seccion_activa(myid) }) }) }
  
  observe({
    req(base())
    equipos <- sort(unique(base()$eventos_crudos$team.name))
    updateSelectInput(session, "equipo_sel_s11", choices = equipos, selected = equipos[1])
  })
  observe({
    req(datos_radares())
    equipos <- sort(unique(datos_radares()$team.name))
    updateSelectInput(session, "equipo_sel_s9", choices = equipos, selected = equipos[1])
  })
  
  # --- Renderizado de Contenido ---
  output$panel_contenido <- renderUI({
    req(seccion_activa())
    card(
      full_screen = TRUE,
      card_header(h4(switch(seccion_activa(),
                            s1 = "Barras: Remates en córners (Todas las temporadas)", s2 = "Remates con tooltips", s3 = "Goles con tooltips",
                            s4 = "Comparación de córners Todas las temporadas", s5 = "Tasa de gol por remate (Todas las temporadas)", s6 = "Visualización por temporada",
                            s7 = "Efectividad por tipo de córner (Todas las temporadas)", s8 = "Perfil táctico de córners", s9 = "Radares de perfil táctico",
                            s10 = "Mapas de calor", s11 = "Mapas de zonas", s12 = "Ratios de córners por gol",
                            s13 = "Dispersión: Ocasiones a BP por 90' vs % dependencia"
      ))),
      uiOutput("contenido_real")
    )
  })
  output$contenido_real <- renderUI({
    switch(seccion_activa(),
           s1 = plotOutput("p_s1"), s2 = plotlyOutput("p_s2"), s3 = plotlyOutput("p_s3"),
           s4 = plotOutput("p_s4"), s5 = plotOutput("p_s5"), s6 = plotOutput("p_s6a"),
           s7 = plotOutput("p_s7"), s8 = plotOutput("p_s8"), s9 = plotOutput("p_s9", height = 700),
           s10 = plotOutput("p_s10"), s11 = plotOutput("p_s11"), s12 = tagList(gt_output("gt_s12a"), br(), gt_output("gt_s12b")),
           s13 = plotOutput("p_s13")
    )
  })
  
  # ---------- S1 ----------
  output$p_s1 <- renderPlot({
    req(base())
    corners_exitosos_conteo <- base()$corners_exitosos_conteo
    df <- corners_exitosos_conteo |>
      mutate(categoria_atacantes = factor(case_when(
        atacantes_en_area_chica >= 4 ~ "4+ Atacantes",
        TRUE ~ paste0(atacantes_en_area_chica, " Atacantes")
      ), levels = c("0 Atacantes","1 Atacantes","2 Atacantes","3 Atacantes","4+ Atacantes"))) |>
      count(categoria_atacantes, name = "total_remates_generados") |>
      complete(categoria_atacantes, fill = list(total_remates_generados = 0))
    
    ggplot(df, aes(categoria_atacantes, total_remates_generados, fill = categoria_atacantes)) +
      geom_col(show.legend = FALSE) +
      geom_text(aes(label = total_remates_generados), vjust = -0.5) +
      labs(title = "Remates generados tras córner vs ocupación del área chica",
           x = "Nº compañeros en área chica", y = "Total remates") +
      plot_theme()
  })
  
  # ---------- S2 ----------
  output$p_s2 <- renderPlotly({
    req(base())
    df <- base()$datos_plot_base |> filtrar_por_temporada(col = "temporada")
    temporada_seleccionada <- input$temporada_sel
    titulo_dinamico <- paste(
      "Destino de centros de córner que terminan en remate | Temporada:", 
      temporada_seleccionada
    )
    titulos <- df |> count(categoria_atacantes, name = "n") |>
      mutate(titulo_facet = paste0(categoria_atacantes, "\n(N = ", n, " Remates)"))
    df <- df |> left_join(titulos, by = "categoria_atacantes") |>
      mutate(
        xgot_text = if_else(!is.na(xGoT), paste0("<br><b>xGoT:</b> ", round(xGoT,2)), ""),
        tooltip_text = paste0(
          "<b>Partido:</b> ", equipo_local, " vs ", equipo_visitante,
          "<br><b>Equipo:</b> ", equipo_atacante,
          "<br><b>Fecha:</b> ", as.Date(fecha),
          "<br><b>Minuto:</b> ", minuto, ":", sprintf("%02d", segundo),
          "<br><b>Ejecutó:</b> ", ejecutor,
          "<br><b>Remató:</b> ", rematador,
          "<br><b>xG:</b> ", round(xG,2),
          xgot_text
        )
      )
    
    p <- ggplot(df) +
      annotate_pitch(dimensions = pitch_statsbomb) +
      geom_segment(aes(x = start_x, y = start_y, xend = end_x, yend = end_y, text = tooltip_text),
                   alpha = .25, color = "dodgerblue",
                   arrow = arrow(length = unit(0.1, "cm"))) +
      facet_wrap(~ titulo_facet, ncol = min(4, ceiling(sqrt(length(unique(df$titulo_facet)) )))) +
      labs(title = titulo_dinamico) +
      theme_pitch() + 
      theme( strip.text = element_text(margin = margin(t = 8, b = 8)),panel.spacing.y = unit(1.5, "lines") # Aumenta el espaciado vertical
      )
    
    ggplotly(p, tooltip = "text") |>
      layout(margin = list(t = 80))
  })
  
  # ---------- S3 ----------
  output$p_s3 <- renderPlotly({
    req(base())
    df <- base()$datos_plot_base |> filter(fue_gol) |> filtrar_por_temporada(col = "temporada")
    if (nrow(df) == 0) return(NULL)
    
    temporada_seleccionada <- input$temporada_sel
    titulo_dinamico <- paste(
      "Destino de los córners que terminan en GOL | Temporada:", 
      temporada_seleccionada
    )
    
    tit <- df |> count(categoria_atacantes, name = "n") |>
      mutate(titulo_facet = paste0(categoria_atacantes, "\n(N = ", n, " Goles)"))
    df <- df |> left_join(tit, by = "categoria_atacantes") |>
      mutate(
        xgot_text = if_else(!is.na(xGoT), paste0("<br><b>xGoT:</b> ", round(xGoT,2)), ""),
        tooltip_text = paste0(
          "<b>GOL! de ", equipo_atacante, "</b>",
          "<br><b>Partido:</b> ", equipo_local, " vs ", equipo_visitante,
          "<br><b>Fecha:</b> ", as.Date(fecha),
          "<br><b>Minuto:</b> ", minuto, ":", sprintf("%02d", segundo),
          "<br><b>Ejecutó:</b> ", ejecutor,
          "<br><b>Remató:</b> ", rematador,
          "<br><b>xG:</b> ", round(xG,2),
          xgot_text
        )
      )
    
    p <- ggplot(df) +
      annotate_pitch(dimensions = pitch_statsbomb) +
      geom_segment(aes(x = start_x, y = start_y, xend = end_x, yend = end_y, text = tooltip_text),
                   alpha = .6, color = "gold", linewidth = 1,
                   arrow = arrow(length = unit(0.15, "cm"))) +
      facet_wrap(~ titulo_facet) +
      labs(title = titulo_dinamico) +
      theme_pitch() + 
      theme(
        strip.text = element_text(margin = margin(t = 8, b = 8)),
        panel.spacing.y = unit(1.5, "lines")
      )
    
    ggplotly(p, tooltip = "text") |>
      layout(margin = list(t = 80))
  })
  
  # ---------- S4 ----------
  output$p_s4 <- renderPlot({
    req(base())
    df <- base()$remates_asistidos_por_corner |>
      left_join(base()$corners_exitosos_con_categoria, by = "id") |>
      select(categoria_atacantes, xG = shot.statsbomb_xg, fue_gol) |>
      filter(!is.na(categoria_atacantes)) |>
      group_by(categoria_atacantes) |>
      summarise(
        total_remates = n(),
        xG_total = sum(xG, na.rm = TRUE),
        total_goles = sum(fue_gol, na.rm = TRUE),
        .groups = 'drop'
      ) |>
      pivot_longer(cols = c(total_remates, xG_total, total_goles),
                   names_to = "metrica", values_to = "valor") |>
      mutate(metrica = factor(metrica, levels = c("total_remates","xG_total","total_goles")))
    
    ggplot(df, aes(categoria_atacantes, valor, fill = metrica)) +
      geom_col(position = position_dodge()) +
      geom_text(aes(label = round(valor,1)),
                position = position_dodge(.9), vjust = -0.5, size = 3.5) +
      labs(title = "Comparación por configuración", x = NULL, y = "Valor acumulado") +
      plot_theme(14)
  })
  
  # ---------- S5 ----------
  output$p_s5 <- renderPlot({
    req(base())
    df <- base()$corners_exitosos_conteo |>
      mutate(categoria_atacantes = factor(case_when(
        atacantes_en_area_chica >= 4 ~ "4+ Atacantes",
        TRUE ~ paste0(atacantes_en_area_chica, " Atacantes")
      ), levels = c("0 Atacantes","1 Atacantes","2 Atacantes","3 Atacantes","4+ Atacantes"))) |>
      group_by(categoria_atacantes) |>
      summarise(total_remates = n(),
                total_goles = sum(fue_gol),
                tasa_de_gol = total_goles/total_remates)
    
    ggplot(df, aes(categoria_atacantes, tasa_de_gol, fill = categoria_atacantes)) +
      geom_col(show.legend = FALSE) +
      geom_text(aes(label = paste0(round(tasa_de_gol*100,1), "%")), vjust = -0.5) +
      scale_y_continuous(labels = percent) +
      labs(title = "Tasa de gol de remates tras córner", x = NULL, y = "Tasa") +
      plot_theme()
  })
  
  # ---------- S6 ----------
  output$p_s6a <- renderPlot({
    req(base())
    temporada_seleccionada <- input$temporada_sel
    titulo_dinamico <- paste(
      "Comparación por configuración | Temporada:", 
      temporada_seleccionada
    )
    lookup <- base()$partidos_crudos |> select(match_id, temporada = season.season_name)
    df <- base()$remates_asistidos_por_corner |>
      left_join(lookup, by = "match_id") |>
      left_join(base()$corners_exitosos_con_categoria, by = "id") |>
      select(temporada, categoria_atacantes, xG = shot.statsbomb_xg, fue_gol) |>
      filter(!is.na(categoria_atacantes) & !is.na(temporada)) |>
      group_by(temporada, categoria_atacantes) |>
      summarise(total_remates = n(),
                xG_total = sum(xG, na.rm = TRUE),
                total_goles = sum(fue_gol, na.rm = TRUE),
                .groups = 'drop') |>
      pivot_longer(cols = c(total_remates, xG_total, total_goles),
                   names_to = "metrica", values_to = "valor") |>
      mutate(metrica = factor(metrica, levels = c("total_remates","xG_total","total_goles")))
    
    df <- filtrar_por_temporada(df, col = "temporada")
    
    ggplot(df, aes(categoria_atacantes, valor, fill = metrica)) +
      geom_col(position = position_dodge()) +
      geom_text(aes(label = round(valor,1)),
                position = position_dodge(.9), vjust = -0.5, size = 2.8) +
      facet_wrap(~ temporada, ncol = 2, scales = "free_y") +
      labs(title = titulo_dinamico, x = NULL, y = "Valor acumulado") +
      plot_theme(12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))
  })
  
  # ---------- S7 ----------
  output$p_s7 <- renderPlot({
    req(base())
    df <- base()$analisis_tipo_cobro_df |>
      group_by(tipo_cobro) |>
      summarise(
        total_corners = n(),
        tasa_de_remate = mean(termino_en_remate, na.rm = TRUE),
        tasa_de_gol_por_corner = mean(fue_gol, na.rm = TRUE),
        valor_promedio_obv = mean(obv_total_net, na.rm = TRUE),
        .groups = 'drop'
      ) |>
      select(-total_corners) |>
      pivot_longer(cols = -tipo_cobro, names_to = "metrica", values_to = "valor") |>
      mutate(
        metrica = factor(metrica, levels = c("tasa_de_remate","tasa_de_gol_por_corner","valor_promedio_obv")),
        etiqueta = case_when(
          metrica != "valor_promedio_obv" ~ percent(valor, .1),
          TRUE ~ as.character(round(valor,4))
        )
      )
    
    ggplot(df, aes(tipo_cobro, valor, fill = tipo_cobro)) +
      geom_col(show.legend = FALSE) +
      geom_text(aes(label = etiqueta), vjust = -0.5) +
      facet_wrap(~ metrica, scales = "free_y") +
      labs(title = "Efectividad por tipo de cobro", x = NULL, y = "Valor") +
      plot_theme()
  })
  
  # ---------- S8 (CON LOGOS EN FACETAS) ----------
  output$p_s8 <- renderPlot({
    req(base(), logos_df())
    lookup <- base()$partidos_crudos |> select(match_id, temporada = season.season_name)
    analisis_con_logos <- base()$eventos_crudos |>
      filter(id %in% base()$ids_de_corners) |>
      left_join(lookup, by = "match_id") |>
      mutate(tipo_cobro = case_when(
        pass.length < 25 ~ "Corto", pass.inswinging == TRUE ~ "Inswinger",
        pass.outswinging == TRUE ~ "Outswinger", TRUE ~ "Directo/Otro"
      )) |>
      left_join(base()$analisis_tipo_cobro_df |> select(id, termino_en_remate), by = "id") |>
      filter(!is.na(temporada)) |>
      group_by(temporada, team.name, tipo_cobro) |>
      summarise(total_corners_temporada = n(), tasa_de_remate = mean(termino_en_remate, na.rm = TRUE), .groups = 'drop') |>
      # --- CAMBIO AQUÍ: Reducimos el filtro de 20 a 5 ---
      filter(total_corners_temporada > 5) |>
      # --------------------------------------------------
    left_join(logos_df(), by = "team.name") |>
      filter(!is.na(logo_url)) |>
      mutate(facet_title = glue("<img src='{logo_url}' width='20'/> {team.name}"))
    analisis_filtrado <- filtrar_por_temporada(analisis_con_logos, col = "temporada") |>
      group_by(temporada, tipo_cobro) |>
      mutate(tasa_normalizada = rescale(tasa_de_remate)) |>
      ungroup()
    if (nrow(analisis_filtrado) == 0) return(NULL)
    subtitulo_dinamico <- if (input$temporada_sel == "Todas") {
      "Efectividad relativa por tipo de cobro (promedio de todas las temporadas)"
    } else {
      paste("Efectividad relativa por tipo de cobro | Temporada:", input$temporada_sel)
    }
    ggplot(analisis_filtrado, aes(x = tipo_cobro, y = tasa_normalizada, fill = tipo_cobro)) +
      geom_col(show.legend = FALSE) +
      geom_text(aes(label = percent(tasa_de_remate, .1)), hjust = -0.1, size = 2.7) +
      coord_flip() +
      facet_wrap(~ facet_title, ncol = 3) +
      labs(
        title = "Perfil táctico de córners", 
        subtitle = subtitulo_dinamico
      ) +
      plot_theme(12) +
      theme(strip.text.x = element_markdown(size = 10), axis.title = element_blank(), axis.text.x = element_blank(),
            axis.text.y = element_text(
              size = 9,
              face = "bold"
            ))
  })
  
  # ---------- S9 (Radar "Pro" dinámico) ----------
  output$p_s9 <- renderPlot({
    req(datos_radares(), logos_df(), input$equipo_sel_s9)
    
    # VALIDACIÓN 1: El usuario DEBE seleccionar una temporada específica
    if (input$temporada_sel == "Todas") {
      return(
        ggplot() + theme_void() + 
          labs(title = "Por favor, selecciona una temporada específica en el menú lateral.") +
          theme(plot.title = element_text(hjust = 0.5, size = 14))
      )
    }
    
    datos_filtrados <- datos_radares() |>
      filter(temporada == input$temporada_sel, team.name == input$equipo_sel_s9)
    
    # VALIDACIÓN 2: Verificar si hay datos DESPUÉS de filtrar
    if(nrow(datos_filtrados) == 0){
      return(
        ggplot() + theme_void() +
          labs(
            title = "No hay datos suficientes para este equipo en la temporada seleccionada.",
            subtitle = "El equipo no cumple el umbral mínimo de remates generados para crear un perfil."
          ) +
          theme(
            plot.title = element_text(hjust = 0.5, size = 14, color="white"),
            plot.subtitle = element_text(hjust = 0.5, size = 10, color="white"),
            plot.background = element_rect(fill = "#1e1e1e", color = NA) # Fondo oscuro consistente
          )
      )
    }
    
    # ... (el resto del código del radar es el mismo) ...
    datos_radar_individual <- datos_filtrados |>
      left_join(logos_df(), by = "team.name") |>
      filter(!is.na(logo_url)) |>
      mutate(team_label = paste0("<img src='", logo_url, "' width='40' />")) |>
      pivot_longer(cols = Remates:Directo, names_to = "metrica", values_to = "valor_percentil")
    label_actual <- datos_radar_individual$team_label[1]
    temp_actual <- datos_radar_individual$temporada[1]
    ggplot(datos_radar_individual, aes(x = metrica, y = valor_percentil)) +
      geom_col(aes(y = 100), fill = "#2a2a2b", width = 1, color = "#4a4a4a") +
      geom_col(aes(fill = metrica), width = 1, alpha = 0.8, show.legend = FALSE) +
      geom_point(color = "white", size = 3) +
      geom_hline(yintercept = c(25, 50, 75), color = "#4a4a4a", linetype = "dashed") +
      geom_text(aes(label = valor_percentil), color = "white", size = 3.5, fontface = "bold", nudge_y = 7) +
      coord_polar() +
      labs(title = label_actual, subtitle = paste("Perfil Táctico de Córners |", temp_actual), caption = "Valores en percentiles (0-100) vs. el resto de la liga en esa temporada") +
      scale_fill_brewer(palette = "Set1") +
      ylim(-20, 110) + theme_void() +
      theme(
        plot.background = element_rect(fill = "#1e1e1e", color = NA),
        plot.title = element_markdown(color = "white", size = 20, hjust = 0.5, margin = margin(t=10)),
        plot.subtitle = element_text(color = "white", size = 12, hjust = 0.5),
        plot.caption = element_text(color = "grey80", size = 8, hjust = 0.5, margin = margin(b=10)),
        axis.text.x = element_text(color = "white", size = 10, face = "bold")
      )
  })
  
  # ---------- S10 ----------
  output$p_s10 <- renderPlot({
    req(base())
    titulo_dinamico <- paste(
      "Zonas de destino de córners — mapas de calor | Temporada:", 
      input$temporada_sel
    )
    
    lookup <- base()$partidos_crudos |> select(match_id, temporada = season.season_name)
    d <- base()$eventos_crudos |>
      filter(id %in% base()$ids_de_corners) |>
      left_join(lookup, by = "match_id") |>
      transmute(team.name, temporada,
                end_x = map_dbl(pass.end_location, 1, .default = NA),
                end_y = map_dbl(pass.end_location, 2, .default = NA)) |>
      filter(!is.na(end_x) & !is.na(temporada)) |>
      filtrar_por_temporada(col = "temporada")
    
    ggplot(d, aes(end_x, end_y)) +
      annotate_pitch(dimensions = pitch_statsbomb) +
      stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = .6) +
      scale_fill_gradientn(colors = c("blue","yellow","red")) +
      facet_wrap(~ team.name, ncol = 6) +
      labs(title = titulo_dinamico) +
      theme_pitch()
  })
  
  # ---------- S11 (CON FILTRO DE EQUIPO Y LOGO) ----------
  output$p_s11 <- renderPlot({
    req(base(), logos_df(), input$equipo_sel_s11)
    
    # --- PASO 1: FILTRAR LOS DATOS ---
    # Primero filtramos el gran dataframe de eventos para la selección actual
    lookup <- base()$partidos_crudos |> select(match_id, temporada = season.season_name)
    datos_filtrados <- base()$eventos_crudos |>
      filter(
        team.name == input$equipo_sel_s11,
        id %in% base()$ids_de_corners
      ) |>
      left_join(lookup, by = "match_id") |>
      filtrar_por_temporada(col = "temporada")
    
    # --- PASO 2: VALIDACIÓN CRUCIAL ---
    # Si después de filtrar no queda NINGUNA fila, mostramos un mensaje y paramos.
    if (nrow(datos_filtrados) == 0) {
      return(
        ggplot() +
          theme_void() +
          labs(
            title = "No hay datos de córners registrados para esta selección.",
            subtitle = "Por favor, elige otra combinación de equipo y temporada."
          ) +
          theme(
            plot.title = element_text(hjust = 0.5, size = 14),
            plot.subtitle = element_text(hjust = 0.5, size = 10)
          )
      )
    }
    # --- FIN DE LA VALIDACIÓN ---
    
    # --- PASO 3: PROCESAR Y GRAFICAR (solo si hay datos) ---
    zonas_df <- tibble(
      zona_nombre = c("Primer Poste - Área Chica","Centro - Área Chica","Segundo Poste - Área Chica", "Primer Poste - Área Grande","Zona Punto Penal","Segundo Poste - Área Grande","Borde del Área"),
      xmin = c(114,114,114,102,102,102,90), xmax = c(120,120,120,114,114,114,102),
      ymin = c(0,20,60,0,18,62,18), ymax = c(20,60,80,18,62,80,62)
    )
    
    datos_contados <- datos_filtrados |>
      mutate(
        tipo_cobro = case_when(pass.length < 25 ~ "Corto", pass.inswinging == TRUE ~ "Inswinger", pass.outswinging == TRUE ~ "Outswinger", TRUE ~ "Directo/Otro"),
        end_x = map_dbl(pass.end_location, 1, .default = NA), 
        end_y = map_dbl(pass.end_location, 2, .default = NA)
      ) |>
      filter(!is.na(end_x) & !is.na(temporada)) |>
      fuzzy_left_join(zonas_df, by = c("end_x"="xmin","end_x"="xmax","end_y"="ymin","end_y"="ymax"), match_fun = list(`>=`,`<=`,`>=`,`<=`)) |>
      mutate(zona_nombre = ifelse(is.na(zona_nombre), "Otra Zona", zona_nombre)) |>
      count(team.name, tipo_cobro, zona_nombre, name = "total_en_zona")
    
    logo_actual <- logos_df() |> filter(team.name == input$equipo_sel_s11) |> pull(logo_url)
    
    ggplot(left_join(zonas_df, datos_contados, by = "zona_nombre")) +
      annotate_pitch(dimensions = pitch_statsbomb) +
      geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, color = tipo_cobro), fill = "white", alpha = .5, linewidth = 1.2) +
      geom_text(aes(x = (xmin+xmax)/2, y = (ymin+ymax)/2, label = total_en_zona), size = 4, fontface = "bold", na.rm = TRUE) +
      coord_flip(xlim = c(80,120), ylim = c(0,80)) +
      facet_wrap(~ tipo_cobro, ncol = 4) +
      theme_pitch() +
      labs(
        title = glue("Mapa de Zonas de Córner: <img src='{logo_actual}' width='30'/> {input$equipo_sel_s11}"), 
        subtitle = glue("Temporada: {input$temporada_sel}")
      ) +
      theme(plot.title = element_markdown())
  })
  
  # ---------- S12a y S12b (CON LOGOS) ----------
  output$gt_s12a <- render_gt({
    req(base(), logos_df())
    if (input$temporada_sel == "Todas") { df <- base()$eventos_crudos |> filter(id %in% base()$ids_de_corners) |> left_join(base()$analisis_tipo_cobro_df |> select(id, fue_gol), by = "id") |> group_by(team.name) |> summarise(total_corners = n(), total_goles_de_corner = sum(fue_gol, na.rm = TRUE), .groups = 'drop') |> filter(total_goles_de_corner > 0) |> mutate(`Córners por Gol` = total_corners / total_goles_de_corner) |> arrange(`Córners por Gol`) |> select(Equipo = team.name, `Córners por Gol`) } else { lookup <- base()$partidos_crudos |> select(match_id, temporada = season.season_name); df <- base()$eventos_crudos |> filter(id %in% base()$ids_de_corners) |> left_join(lookup, by = "match_id") |> left_join(base()$analisis_tipo_cobro_df |> select(id, fue_gol), by = "id") |> filter(temporada == input$temporada_sel) |> group_by(Equipo = team.name) |> summarise(total_corners = n(), total_goles_de_corner = sum(fue_gol, na.rm = TRUE), .groups = 'drop') |> filter(total_goles_de_corner > 0) |> mutate(`Córners por Gol` = total_corners / total_goles_de_corner) |> arrange(`Córners por Gol`) |> select(Equipo, `Córners por Gol`) }
    df |> left_join(logos_df(), by = c("Equipo" = "team.name")) |> select(logo_url, Equipo, `Córners por Gol`) |> gt() |> text_transform(locations = cells_body(columns = logo_url), fn = function(x) { web_image(x, height = 30) }) |> cols_label(logo_url = "") |> fmt_number(columns = `Córners por Gol`, decimals = 2) |> tab_header(title = md("**Ratio de Córners por Gol Anotado**"), subtitle = if (input$temporada_sel=="Todas") "Todas las temporadas" else paste("Temporada:", input$temporada_sel))
  })
  output$gt_s12b <- render_gt({
    req(base(), logos_df())
    corners_info <- base()$eventos_crudos |> filter(id %in% base()$ids_de_corners) |> select(id, match_id, possession, timestamp, team.id, team.name); goles_info <- base()$eventos_crudos |> filter(shot.outcome.name == "Goal") |> select(match_id, possession, timestamp, team.id, team.name); sec <- inner_join(corners_info, goles_info, by = c("match_id","possession","team.id","team.name")) |> filter(timestamp.y > timestamp.x) |> distinct(id, .keep_all = TRUE) |> rename(corner_id = id); if (input$temporada_sel == "Todas"){ df <- sec |> count(team.name, name = "total_goles_secuencia") |> left_join(base()$eventos_crudos |> filter(id %in% base()$ids_de_corners) |> count(team.name, name = "total_corners"), by = "team.name") |> mutate(`Córners por Gol (Secuencia)` = total_corners / total_goles_secuencia) |> arrange(`Córners por Gol (Secuencia)`) |> select(Equipo = team.name, `Córners por Gol (Secuencia)`) } else { lookup <- base()$partidos_crudos |> select(match_id, temporada = season.season_name); df <- sec |> left_join(lookup, by = "match_id") |> filter(temporada == input$temporada_sel) |> count(team.name, name = "total_goles_secuencia") |> left_join(base()$eventos_crudos |> filter(id %in% base()$ids_de_corners) |> left_join(lookup, by = "match_id") |> filter(temporada == input$temporada_sel) |> count(team.name, name = "total_corners"), by = "team.name") |> mutate(`Córners por Gol (Secuencia)` = total_corners / total_goles_secuencia) |> arrange(`Córners por Gol (Secuencia)`) |> select(Equipo = team.name, `Córners por Gol (Secuencia)`) }
    df |> left_join(logos_df(), by = c("Equipo" = "team.name")) |> select(logo_url, Equipo, `Córners por Gol (Secuencia)`) |> gt() |> text_transform(locations = cells_body(columns = logo_url), fn = function(x) { web_image(x, height = 30) }) |> cols_label(logo_url = "") |> fmt_number(columns = `Córners por Gol (Secuencia)`, decimals = 2) |> tab_header(title = md("**Ratio de Córners por Gol de Secuencia**"), subtitle = if (input$temporada_sel=="Todas") "Todas las temporadas" else paste("Temporada:", input$temporada_sel))
  })
  
  # ---------- S13 (CON LOGOS Y DEPURACIÓN) ----------
  output$p_s13 <- renderPlot({
    req(datos_s13(), logos_df())
    titulo_dinamico <- paste(
      "Ocasiones a balón parado por 90' vs % de dependencia | Temporada:", 
      input$temporada_sel
    )
    
    
    df_filtrado <- datos_s13() |> filtrar_por_temporada(col = "temporada")
    df_con_logos <- df_filtrado |> left_join(logos_df(), by = "team.name")
    equipos_sin_logo <- df_con_logos |> filter(is.na(logo_url)) |> pull(team.name)
    if (length(equipos_sin_logo) > 0) { print("Equipos sin logo encontrado:"); print(unique(equipos_sin_logo)) }
    df_final <- df_con_logos |> filter(!is.na(logo_url))
    if(nrow(df_final) == 0) return(NULL)
    ggplot(df_final, aes(x = ocasiones_abp_por_90, y = pct_ocasiones_abp)) +
      geom_image(aes(image = logo_url), size = 0.07, asp = 1.6) +
      scale_y_continuous(labels = percent_format(accuracy = 1)) +
      labs(
        title = titulo_dinamico,
        x = "ABP por 90'", 
        y = "% ABP de todas las ocasiones"
      ) +
      plot_theme(12)
  })
}

# --- Ejecutar la App ---
shinyApp(ui, server)



















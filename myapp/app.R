library(shiny)
library(shinydashboard)
library(leaflet)
library(sf)
library(dplyr)
library(ggplot2)

# Cargar datos
vulnerabilidad <- st_read("../data/vulnerabilidad-barrios.geojson")
ayuda_adiccion <- st_read("../data/addiccions-adicciones.geojson")
ayuda_discapacidad <- st_read("../data/discapacitat-discapacidad.geojson")
ayuda_sinhogar <- st_read("../data/sense-llar-sin-hogar.geojson")
ayuda_mujer <- st_read("../data/dona-mujer.geojson")

# Agregar la columna de barrio a los dataframes de ayudas
ayuda_adiccion <- st_join(ayuda_adiccion, vulnerabilidad, join = st_within)
ayuda_discapacidad <- st_join(ayuda_discapacidad, vulnerabilidad, join = st_within)
ayuda_sinhogar <- st_join(ayuda_sinhogar, vulnerabilidad, join = st_within)
ayuda_mujer <- st_join(ayuda_mujer, vulnerabilidad, join = st_within)

ui <- dashboardPage(
  dashboardHeader(title = "Vulnerabilidad por barrios"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Mapa", tabName = "mapa", icon = icon("map")),
      menuItem("Gráfico de Barras", tabName = "barras", icon = icon("bar-chart"))
    ),
    selectInput(
      "barrio",
      "Seleccionar Barrio:",
      choices = c("Todos", unique(vulnerabilidad$nombre)),
      selected = "Todos"
    ),
    selectInput(
      "indice",
      "Seleccionar Índice de Vulnerabilidad:",
      choices = c("Global" = "ind_global", "Equipamiento" = "ind_equip", "Demográfico" = "ind_dem", "Económico" = "ind_econom"),
      selected = "ind_global"
    ),
    sliderInput(
      "vul_range",
      "Rango de Vulnerabilidad:",
      min = min(vulnerabilidad$ind_global, na.rm = TRUE),
      max = max(vulnerabilidad$ind_global, na.rm = TRUE),
      value = range(vulnerabilidad$ind_global, na.rm = TRUE),
      step = 0.1
    ),
    checkboxGroupInput(
      "ayudas",
      "Puntos de ayuda a Mostrar:",
      choices = c(
        "Ayuda Adicción" = "ayuda_adiccion",
        "Ayuda a Discapacitados" = "ayuda_discapacidad",
        "Ayuda Sin Hogar" = "ayuda_sinhogar",
        "Ayuda a Mujeres" = "ayuda_mujer"
      ),
      selected = c("ayuda_adiccion", "ayuda_discapacidad", "ayuda_sinhogar", "ayuda_mujer")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "mapa",
              leafletOutput("vulnerabilidades")
      ),
      tabItem(tabName = "barras",
              plotOutput("barplot")
      )
    )
  )
)

server <- function(input, output, session) {
  # Función para añadir puntos al mapa
  add_points_to_map <- function(map, points_data, color, group_name) {
    leafletProxy(map) %>%
      addCircleMarkers(
        data = points_data,
        color = color,
        fillOpacity = 0.7,
        radius = 5,
        group = group_name,
        popup = ~paste("Nombre:", equipamien)
      )
  }
  
  # Actualizar el rango del slider basado en el índice seleccionado
  observe({
    updateSliderInput(session, "vul_range",
                      min = min(vulnerabilidad[[input$indice]], na.rm = TRUE),
                      max = max(vulnerabilidad[[input$indice]], na.rm = TRUE),
                      value = range(vulnerabilidad[[input$indice]], na.rm = TRUE),
                      step = 0.1)
  })
  
  output$vulnerabilidades <- renderLeaflet({
    leaflet(data = vulnerabilidad) %>%
      setView(lng = -0.3763, lat = 39.469, zoom = 12) %>%
      addTiles()
  })
  
  observe({
    selected_data <- vulnerabilidad
    
    # Filtrar por barrio si no es "Todos"
    if (input$barrio != "Todos") {
      selected_data <- subset(selected_data, nombre == input$barrio)
    }
    
    # Filtrar por rango de vulnerabilidad
    selected_data <- subset(selected_data, selected_data[[input$indice]] >= input$vul_range[1] & selected_data[[input$indice]] <= input$vul_range[2])
    
    pal <- colorBin(
      palette = "YlOrRd",
      domain = vulnerabilidad[[input$indice]],
      bins = 7,
      reverse = TRUE
    )
    
    leafletProxy("vulnerabilidades", data = selected_data) %>%
      clearShapes() %>%
      clearControls() %>%  # Limpiar leyendas anteriores
      addPolygons(
        fillColor = ~pal(selected_data[[input$indice]]),
        fillOpacity = 0.7,
        color = "white",
        weight = 1,
        popup = ~paste("Barrio:", nombre, "<br>", "Vulnerabilidad:", selected_data[[input$indice]])
      ) %>%
      addLegend(
        pal = pal,
        values = ~selected_data[[input$indice]],
        opacity = 0.7,
        title = "Vulnerabilidad",
        position = "bottomright"
      )
    
    # Filtrar puntos de ayuda por barrio
    if (input$barrio != "Todos") {
      ayuda_adiccion_filtered <- subset(ayuda_adiccion, nombre == input$barrio)
      ayuda_discapacidad_filtered <- subset(ayuda_discapacidad, nombre == input$barrio)
      ayuda_sinhogar_filtered <- subset(ayuda_sinhogar, nombre == input$barrio)
      ayuda_mujer_filtered <- subset(ayuda_mujer, nombre == input$barrio)
    } else {
      ayuda_adiccion_filtered <- ayuda_adiccion
      ayuda_discapacidad_filtered <- ayuda_discapacidad
      ayuda_sinhogar_filtered <- ayuda_sinhogar
      ayuda_mujer_filtered <- ayuda_mujer
    }
    
    # Limpiar los grupos de puntos de ayuda anteriores
    leafletProxy("vulnerabilidades") %>%
      clearGroup("Ayuda Adicción") %>%
      clearGroup("Ayuda a Discapacitados") %>%
      clearGroup("Ayuda Sin Hogar") %>%
      clearGroup("Ayuda a Mujeres")
    
    # Agregar los puntos de ayuda seleccionados
    if ("ayuda_adiccion" %in% input$ayudas) {
      add_points_to_map("vulnerabilidades", ayuda_adiccion_filtered, "red", "Ayuda Adicción")
    }
    if ("ayuda_discapacidad" %in% input$ayudas) {
      add_points_to_map("vulnerabilidades", ayuda_discapacidad_filtered, "blue", "Ayuda a Discapacitados")
    }
    if ("ayuda_sinhogar" %in% input$ayudas) {
      add_points_to_map("vulnerabilidades", ayuda_sinhogar_filtered, "green", "Ayuda Sin Hogar")
    }
    if ("ayuda_mujer" %in% input$ayudas) {
      add_points_to_map("vulnerabilidades", ayuda_mujer_filtered, "purple", "Ayuda a Mujeres")
    }
  })
  
  output$barplot <- renderPlot({
    selected_data <- vulnerabilidad
    if (input$barrio != "Todos"){
    selected_data = subset(selected_data, nombre== input$barrio)}
    selected_data <- subset(selected_data, selected_data[[input$indice]] >= input$vul_range[1] & selected_data[[input$indice]] <= input$vul_range[2])
    
    ggplot(selected_data, aes(x = reorder(nombre, -selected_data[[input$indice]]), y = selected_data[[input$indice]])) +
      geom_bar(stat = "identity",fill = "#98ff98") +
      theme_minimal() +
      labs(
        title = "Índice de Vulnerabilidad por Barrio",
        x = "Barrio",
        y = "Índice de Vulnerabilidad"
      ) +
      theme(axis.text.y = element_text(angle = 45, hjust = 1))+
      coord_flip()  # Gira el gráfico para facilitar la lectura
  })
}
shinyApp(ui = ui, server = server)

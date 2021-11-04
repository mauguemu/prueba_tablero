# carga de librerías
library(shiny)
library(shinydashboard)
library( shinyWidgets )
library(dplyr)
library(sf)
library(terra)
library(raster)
library(rgdal)
library(DT)
library(plotly)
library(leaflet)
library(leaflet.extras)
library(leafem)
library(ggplot2)
library(graphics)
library(tidyverse)

#Lectura datos zonas
zonas <-
  st_read("https://raw.githubusercontent.com/mauguemu/prueba_tablero/master/Datos/capas/zonas_delimitadas.geojson",
          quiet = TRUE
  )

# Transformación del CRS del objeto zonas
zonas <-
  zonas %>%
  st_transform(4326)

#Lectura datos cuadrantes
cuadrantes <-
  st_read("https://raw.githubusercontent.com/mauguemu/prueba_tablero/master/Datos/capas/cuadrantes.geojson",
          quiet = TRUE
  )

# Transformación del CRS de cuadrantes 
cuadrantes <-
  cuadrantes %>%
  st_transform(4326)

#Lectura datos recursos patimoniales  
recursos_patrimoniales <-
  st_read("https://raw.githubusercontent.com/mauguemu/prueba_tablero/master/Datos/capas/recursos_patrimonio_material.geojson",
          quiet = TRUE
  )

# Transformación del CRS de recursos patrimoniales

recursos_patrimoniales <-
  recursos_patrimoniales %>%
  st_transform(4326)

#lectura patrimonio_inmaterial
patrimonio_inmaterial <-
  st_read(
    "/vsicurl/https://raw.githubusercontent.com/mauguemu/prueba_tablero/master/Datos/tablas/recursos_patrimonio_inmat.csv",
    quiet = TRUE
  )

# Lista ordenada de estado + "Todos"
lista_estado <- unique(recursos_patrimoniales$estado)
lista_estado <- sort(lista_estado)
lista_estado <- c("Todas", lista_estado)

# Lista ordenada de subcategorias + "Todas"
lista_subcategorias <- unique(recursos_patrimoniales$subcategoria)
lista_subcategorias <- sort(lista_subcategorias)
lista_subcategorias <- c("Todas", lista_subcategorias)

# Componentes de la aplicación Shiny
# Definición del objeto ui

ui <- dashboardPage(
  dashboardHeader(title = "Patrimonio material"),
  dashboardSidebar(sidebarMenu(
    menuItem(
      text = "Filtros",
      selectInput(
        inputId = "estado",
        label = "Estado",
        choices = lista_estado,
        selected = "Todas"
      ),
      selectInput(
        inputId = "subcategoria",
        label = "Subcategoría",
        choices = lista_subcategorias,
        selected = "Todas"
      ),
      
      numericRangeInput(
       inputId = "evaluacion_multicriterio",
      label = "Evaluación multicriterio",
      value = c(3,6.5),
      width = NULL,
      separator = " a ",
      min = 3,
      max = 6.5,
      step = NA
      ),
      startExpanded = TRUE
    )
  )),
  dashboardBody(fluidRow(
    box(
      title = "Mapa registros del patrimonio material",
      leafletOutput(outputId = "mapa"),
      width = 6
    ),
    box(
      title = "Registros del patrimonio material",
      DTOutput(outputId = "tabla"),
      width = 6
    )
  ),
  fluidRow(
    box(
      title = "Valoración de los recursos del patrimonio material",
      plotlyOutput(outputId = "grafico_evaluacion"),
      width = 12
    )
  ))
)


server <- function(input, output, session) {

  filtrarRegistros <- reactive({
    # Remoción de geometrías y selección de columnas
    patrimonio_filtrado <-
      recursos_patrimoniales %>%
      dplyr::select(codigo,denominacion,dominio,subcategoria,estado,economico,disponibilidad,identidad_territorial,condicion,evaluacion_multicriterio,foto,ficha,id_recurso)
    
    # Filtrado de felidae por rango
    patrimonio_filtrado <-
      patrimonio_filtrado %>%
      filter(
        evaluacion_multicriterio >= input$evaluacion_multicriterio[1] &
          evaluacion_multicriterio <= input$evaluacion_multicriterio[2]
      )
    # Filtrado de registros por estado
    if (input$estado != "Todas") {
      patrimonio_filtrado <-
        patrimonio_filtrado %>%
        filter(estado == input$estado)
    }
    # Filtrado de registros por subcategoría
    if (input$subcategoria != "Todas") {
      patrimonio_filtrado <-
        patrimonio_filtrado %>%
        filter(subcategoria == input$subcategoria)
    }
    
    return(patrimonio_filtrado)
  })  

  output$tabla <- renderDT({
    registros <- filtrarRegistros()
    
    registros %>%
      st_drop_geometry() %>%
      select(codigo,denominacion, subcategoria, estado,evaluacion_multicriterio)%>%
      datatable(registros, options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.11.3/i18n/es_es.json'), pageLength = 8))
  }) 
  
  output$mapa <- renderLeaflet({
    registros <-
      filtrarRegistros()
    
    colores <- c('red', 'orange', 'yellow')
    
    c_zona <- levels(as.factor(zonas$id_zona))
    
    paleta <- colorFactor(palette = colores, domain = c_zona)
    
    # Mapa leaflet básico con capas de zonas y recursos patrimoniales 
    leaflet() %>%
      addTiles() %>%
      setView(-83.0232, 9.9952, 15) %>%
      
      addProviderTiles(
        providers$CartoDB.Positron, group = "carto_DB") %>%
      addProviderTiles(
        providers$Esri.WorldImagery, group = "Esri") %>%
      
      addPolygons(
        data = zonas,
        color = ~paleta(id_zona),
        smoothFactor = 0.3,
        fillOpacity = 0.3,
        popup =  ~nombre,
        label= ~id_zona,
        stroke = TRUE,
        weight = 2.0,
        group = "Zonas delimitadas"
      )  %>%
      
      addPolygons(
        data = cuadrantes,
        color = "black",
        smoothFactor = 0.3,
        stroke = TRUE,
        weight = 1.0,
        group = "Cuadrantes"
      ) %>%
      
      addCircleMarkers(
        data = registros,
        stroke = F,
        radius = 4,
        popup = paste0("<strong>Recurso: </strong>",
                       registros$denominacion,
                       "<br>",
                       "<strong>Subcategoría: </strong>",
                       registros$subcategoria,
                       "<br>",
                       "<strong>Estado de conservación: </strong>",
                       registros$estado,
                       "<br>",
                       "<img src='https://raw.githubusercontent.com/mauguemu/prueba_tablero/master/Datos/Imagenes/",registros$foto,".png","'width='200'/>",
                       "<br>",
                       "<a href='https://raw.githubusercontent.com/mauguemu/prueba_tablero/master/Datos/Imagenes/",registros$ficha,".png", "'>Ficha</a>"),
        label = ~codigo,
        fillColor = 'orange',
        fillOpacity = 1,
        group = "Recursos patrimoniales"
      )%>%
      addSearchOSM()%>%
      addResetMapButton()%>%
      addMouseCoordinates()%>%
      addLayersControl(
        baseGroups = c("Carto_DB","Esri"),
        overlayGroups = c("Zonas delimitadas","Cuadrantes", "Recursos patrimoniales"),
        options = layersControlOptions(collapsed = T)
      )
  })
  
  output$grafico_evaluacion <- renderPlotly({
    registros <- filtrarRegistros()
    
    registros %>%
      st_drop_geometry() %>%
      select(denominacion,economico,disponibilidad,identidad_territorial,condicion)%>%
      pivot_longer(c("economico","disponibilidad","identidad_territorial","condicion"), names_to = "criterio",values_to = "valoracion")%>%
      ggplot(aes(x = valoracion, y = denominacion, fill = criterio)) +
      ggtitle("Valoración de los recursos patrimoniales") +
      ylab("Recurso") +
      xlab("Valoración multicriterio") +
      geom_col()%>%
      config(locale = "es")
    
  })

  
  
}

shinyApp(ui, server)
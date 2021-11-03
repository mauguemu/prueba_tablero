# carga de librerías
library(shiny)
library(shinydashboard)
library(dplyr)
library(sf)
library(terra)
library(raster)
library(rgdal)
library(DT)
library(plotly)
library(leaflet)

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
    "/vsicurl/https://raw.githubusercontent.com/mauguemu/prueba_tablero/master/Datos/tablas/recursos_patrimonio_inmaterial.csv",
    quiet = TRUE
  )


ui <- fluidPage(
  
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)
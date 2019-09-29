library(shiny)
library(shinydashboard)
library(shinyjs)
library(leaflet)

provincias <- read.csv("datos_csv/codprov.csv", fileEncoding = "UTF-8")
SXnacional <- readRDS("SXnacional2019.rds") # saveRDS(SXnacional, "SXnacional2019.rds")

dashboardPage(
  dashboardHeader(title = "Foreign Statix"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Municipios", tabName = "tab1", icon = shiny::icon("bulding")),
        menuItem("Provincias", tabName = "tab2", icon = shiny::icon("binoculars"))
    )
  ),
  dashboardBody(
    fluidRow(
      tabItems(
        tabItem(tabName = "tab1",
                box(width = 3, title = "Selecciona Municipio y Nacionalidad", status = "primary", solidHeader = TRUE,
                    selectizeInput("selectProvincia", "Selecciona Provincia", choices = provincias$Nombre, multiple = F, selected = "Murcia"),
                    selectizeInput("selectMunicipio", label = "Selecciona Municipio", choices = c("Cargando..."), multiple = T),
                    selectizeInput("selectNacionalidad", label = "Selecciona Nacionalidad", choices = levels(SXnacional$nacionalidad)),
                    checkboxInput("porcentaje", "Aplicar porcentaje sobre población total", value = F),
                    disabled(checkboxInput("hombreMujer", "Distinguir entre hombre/mujer", value = F)),
                    useShinyjs(),
                    disabled(downloadButton("descargaKMZ", label = "Descargar seleccionados")),
                    htmlOutput("info")
                ),
                box(width = 9, title = "Mapa", status = "warning", solidHeader = F, 
                    tags$style(type = "text/css", "#mapa {height: calc(100vh - 150px) !important;}"),
                    leafletOutput("mapa", width = "auto")
                )
        ),
        tabItem(tabName = "tab2",
                box(width = 2, title = "Selecciona Provincia y Nacionalidad", status = "primary", solidHeader = TRUE
                    
                    ),
                box()
                )
      )
    )
  )
)
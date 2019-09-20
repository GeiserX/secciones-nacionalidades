library(shiny)
library(shinydashboard)

provincias <- read.csv("datos_csv/codprov.csv", fileEncoding = "UTF-8")
SXnacional <- readRDS("SXnacional2019.rds") # saveRDS(SXnacional, "SXnacional2019.rds")

ui <- dashboardPage(
  dashboardHeader(title = "Extranjeros por Secciones Censales España"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Municipios", tabName = "tab1", icon = shiny::icon("atom"))
    )
  ),
  dashboardBody(
    fluidRow(
      tabItems(
        tabItem(tabName = "tab1",
                box(width = 2, title = "Selecciona Municipio y Nacionalidad", status = "primary", solidHeader = TRUE,
                    selectizeInput("selectProvincia", "Selecciona Provincia", choices = provincias$Nombre, multiple = F, selected = "Murcia"),
                    selectizeInput("selectMunicipio", label = "Selecciona Municipio", choices = c("Cargando...")),
                    selectizeInput("selectNacionalidad", label = "Selecciona Nacionalidad", choices = levels(SXnacional$nacionalidad)),
                    actionButton("calcularSecciones", label = "Generar mapa")
                ),
                box(width = 10, title = "Mapa", status = "warning", solidHeader = F,
                    leafletOutput("mapa", width = "auto")
                )
        )
      )
    )
  )
)
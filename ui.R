dashboardPage(
  dashboardHeader(title = "Foreign Statix"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Map it", tabName = "tab1", icon = shiny::icon("building")),
        menuItem("Statistics by Province", tabName = "tab2", icon = shiny::icon("building")),
        menuItem("Historic data", tabName = "tab3", icon = shiny::icon("binoculars"))
    )
  ),
  dashboardBody(
    fluidRow(
      tabItems(
        tabItem(tabName = "tab1",
                box(width = 3, title = "Selecciona Municipio y Nacionalidad", status = "primary", solidHeader = TRUE,
                    selectizeInput("selectProvincia", "Selecciona Provincia", choices = provincias$Nombre, multiple = F, selected = "Murcia"),
                    selectizeInput("selectMunicipio", label = "Selecciona Municipio", choices = c(""), multiple = T),
                    selectizeInput("selectNacionalidad", label = "Selecciona Nacionalidad", choices = levels(SXnacionalAmbos$nacionalidad)),
                    checkboxInput("porcentaje", "Aplicar porcentaje sobre población total", value = F),
                    checkboxInput("hombreMujer", "Distinguir entre hombre/mujer", value = F),
                    useShinyjs(),
                    disabled(downloadButton("descargaKML", label = "Descargar seleccionados")),
                    htmlOutput("mention")
                ),
                box(width = 9, title = "Mapa", status = "warning", solidHeader = F, 
                    tags$style(type = "text/css", "#mapa {height: calc(100vh - 150px) !important;}"),
                    leafletOutput("mapa", width = "auto")
                )
        )
      )
    )
  )
)
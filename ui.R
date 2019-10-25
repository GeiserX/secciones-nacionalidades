dashboardPage(
  dashboardHeader(title = "Foreign Statix"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Map it", tabName = "tab1", icon = shiny::icon("building")),
        menuItem("Statistics by Province", tabName = "tab2", icon = shiny::icon("brain")),
        menuItem("Historic data", tabName = "tab3", icon = shiny::icon("binoculars"))
    )
  ),
  dashboardBody(
    fluidRow(
      tabItems(
        tabItem(tabName = "tab1",
                box(width = 3, title = "Select Municipality and Nationality", status = "primary", solidHeader = TRUE,
                    selectizeInput("selectProvincia", "Select Province", choices = provincias$Nombre, multiple = F, selected = "Murcia"),
                    selectizeInput("selectMunicipio", label = "Select Municipality", choices = c(""), multiple = T),
                    selectizeInput("selectNacionalidad", label = "Select Nationality", choices = levels(SXnacionalAmbos$nacionalidad)),
                    checkboxInput("porcentaje", "Apply percentage over total population", value = F),
                    checkboxInput("hombreMujer", "Distinguish between man/womanr", value = F),
                    useShinyjs(),
                    disabled(downloadButton("descargaKML", label = "Download selected (blue) areas")),
                    htmlOutput("mention")
                ),
                box(width = 9, title = "Map", status = "info", solidHeader = F, 
                    tags$style(type = "text/css", "#mapa {height: calc(100vh - 150px) !important;}"),
                    leafletOutput("mapa", width = "auto")
                )
        ),
        tabItem(tabName = "tab2",
                box(width = 3, title = "Select Province", status = "primary", solidHeader = TRUE,
                    selectizeInput("selectProvincia2", "Select Province", choices = provincias$Nombre, multiple = F, selected = "Murcia"),
                    selectizeInput("selectNacionalidad2", label = "Select Nationality", choices = levels(SXnacionalAmbos$nacionalidad)),
                    checkboxInput("sort", "Sort data?", value = F)
                    ),
                box(width = 9, title = "Chart", status = "info", solidHeader = F, 
                    tags$style(type = "text/css", "#chart {height: calc(100vh - 150px) !important;}"),
                    highchartOutput(outputId = "chart", width = "auto")
                )
                
        )
      )
    )
  )
)
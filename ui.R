dashboardPage(
  dashboardHeader(title = "Foreign Insight"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Map it", tabName = "tab1", icon = shiny::icon("building")),
        menuItem("Statistics by Province", tabName = "tab2", icon = shiny::icon("brain")),
        menuItem("Spain Map", tabName = "tab3", icon = shiny::icon("binoculars")),
        menuItem("Historic data", tabName = "tab4", icon = shiny::icon(""))
        
    )
  ),
  dashboardBody(
    fluidRow(
      tabItems(
        tabItem(tabName = "tab1",
                box(width = 3, title = "Census Sections", status = "primary", solidHeader = TRUE,
                    selectizeInput("selectProvincia", "Select Province", choices = provincias$Nombre, multiple = F, selected = "Murcia"),
                    selectizeInput("selectMunicipio", label = "Select Municipality", choices = c(""), multiple = T),
                    selectizeInput("selectNacionalidad", label = "Select Nationality", choices = levels(SXnacionalAmbos$nacionalidad)),
                    checkboxInput("porcentaje", "Apply percentage", value = F),
                    checkboxInput("hombreMujer", "Distinguish man/woman", value = F),
                    useShinyjs(),
                    disabled(downloadButton("descargaKML", label = "Download selected areas")),
                    htmlOutput("mention")
                ),
                box(width = 9, title = "Map", status = "info", solidHeader = F, 
                    tags$style(type = "text/css", "#mapa {height: calc(100vh - 163px) !important;}"),
                    leafletOutput("mapa", width = "auto")
                )
        ),
        tabItem(tabName = "tab2",
                box(width = 2, title = "Provinces", status = "primary", solidHeader = TRUE,
                    selectizeInput("selectProvincia2", "Select Province", choices = provincias$Nombre, multiple = F, selected = "Murcia"),
                    selectizeInput("selectNacionalidad2", label = "Select Nationality", choices = levels(SXnacionalAmbos$nacionalidad)),
                    checkboxInput("sort", "Sort data", value = F),
                    checkboxInput("manWoman", "Men/women column", value = F),
                    checkboxInput("percentage2", "Percentage column", value = F)
                    ),
                box(width = 10, title = "Chart", status = "info", solidHeader = F, 
                    tags$style(type = "text/css", "#chart {height: calc(100vh - 163px) !important;}"),
                    highchartOutput(outputId = "chart", width = "auto")
                )
                
        ),
        tabItem(tabName = "tab3",
                box(width = 2, title = "Spain", status = "primary", solidHeader = TRUE,
                    selectizeInput("selectNacionalidad3", label = "Select Nationality", choices = levels(SXnacionalAmbos$nacionalidad))
                ),
                box(width = 10, title = "Population", status = "info", solidHeader = F, 
                    tags$style(type = "text/css", "#spainmap {height: calc(100vh - 163px) !important;}"),
                    highchartOutput(outputId = "spainmap", width = "auto")
                )
                
        )
      )
    )
  )
)
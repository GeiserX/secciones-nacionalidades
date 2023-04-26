dashboardPage(skin = "blue", title = "Foreign Insight",
  dashboardHeader(title = "Foreign Insight"),
    dashboardSidebar(
      sidebarMenu(id = "tabs",
        menuItem("Statistics by Sections", tabName = "tab1", icon = shiny::icon("building")),
        menuItem("Graphs by Province", tabName = "tab2", icon = shiny::icon("brain")),
        menuItem("National Map", tabName = "tab3", icon = shiny::icon("binoculars")),
        menuItem("Historic data", tabName = "tab4", icon = shiny::icon("history")),
        menuItem("References", tabName = "ref", icon = shiny::icon("copyright")),
        sliderInput("selectYear", "", min = 2012, max = 2021, value = 2021, sep = "")
        # radioButtons("NationalityOrBirth", label="Select source of statistics",
        #              choices = list("Per Nationality" = 1, "Per Country of Birth" = 2), selected = 1)
    )
  ),
  dashboardBody(
    fluidRow(
      tabItems(
        tabItem(tabName = "tab1",
                box(width = 3, title = "Census Sections", status = "primary", solidHeader = TRUE,
                    selectizeInput("selectProvincia", "Select Province", choices = provincias$Nombre, multiple = F, selected = "Murcia"),
                    selectizeInput("selectMunicipio", label = "Select Municipality",
                                   choices = c("Loading... please wait"), multiple = T, selected = "Madrid (Madrid)"),
                    selectInput("selectNacionalidad", label = "Select Nationality",
                                choices = levels(Nacionalidad_Ambos$nacionalidad), selected = "Total Población"),
                    checkboxInput("porcentaje", "Apply percentage", value = F),
                    checkboxInput("hombreMujer", "Distinguish man/woman", value = F),
                    useShinyjs(),
                    disabled(downloadButton("descargaKML", label = "Download selected areas"))
                ),
                box(width = 9, title = "Map", status = "info", solidHeader = F, 
                    tags$style(type = "text/css", "#mapa {height: calc(100vh - 163px) !important;}"),
                    leafletOutput("mapa", width = "auto")
                )
        ),
        tabItem(tabName = "tab2",
                box(width = 2, title = "Provinces", status = "primary", solidHeader = TRUE,
                    selectizeInput("selectProvincia2", "Select Province", choices = provincias$Nombre, multiple = F, selected = "Murcia"),
                    selectizeInput("selectNacionalidad2", label = "Select Nationality",
                                   choices = levels(Nacionalidad_Ambos$nacionalidad), selected = "Total Población"),
                    checkboxInput("sort", "Sort data", value = T),
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
                    selectizeInput("selectNacionalidad3", label = "Select Nationality",
                                   choices = levels(Nacionalidad_Ambos$nacionalidad), selected = "Total Población")
                ),
                box(width = 10, title = "Population", status = "info", solidHeader = F, 
                    tags$style(type = "text/css", "#spainmap {height: calc(100vh - 163px) !important;}"),
                    highchartOutput(outputId = "spainmap", width = "auto")
                )
        ),
        tabItem(tabName = "tab4",
                box(width = 2, title = "Historic data & Analytics", status = "primary", solidHeader = TRUE,
                    radioButtons("metricsByAggregate", label = "Select Aggregation",
                                 choices = c("National", "Province", "Municipality"), selected = "National"),
                    selectizeInput("selectNacionalidad4", label = "Select Nationality",
                                   choices = levels(Nacionalidad_Ambos$nacionalidad), selected = "Total Población"),
                    uiOutput("provinces"),
                    uiOutput("municipalities")
                ),
                box(width = 10, title = "Chart", status = "info", solidHeader = F, 
                    highchartOutput(outputId = "historicChart", width = "auto")
                )
        ),
        tabItem(tabName = "ref", 
                box(width = 12, title = "References", status = "info", solidHeader = F,
                    HTML(paste0("Cartography extracted from <a ",
                           "href=http://www.ine.es/ss/Satellite?L=es_ES&c=Page&cid=1259952026632&p=1259952026632&pagename=", 
                           "ProductosYServicios%2FPYSLayout target=_blank> data publicly available at the Spanish",
                           " \'Instituto Nacional de Estadística\'</a><hr>",
                           " Population data extracted thanks to <a href=https://www.ine.es/dyngs/INEbase/es/operacion.htm?c=Estadistica_C",
                           "&cid=1254736177012&menu=resultados&secc=1254736195461&idp=1254734710990 target=_blank>",
                           "the publicly available information at the Spanish \'Instituto Nacional de Estadística\'</a><hr>",
                           "Source code available at <a href=https://github.com/GeiserX/secciones-nacionalidades target=_blank>GitHub</a>,",
                           " so if you find any errors, open an issue there. PRs are welcome. ",
                           "Docker container avaialble at <a href=https://hub.docker.com/r/drumsergio/secciones-nacionalidades",
                           " target=_blank>DockerHub</a>."))
                                     ))
      )
    )
  )
)

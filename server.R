#install.packages("rgdal", "raster", "sp")

library(shiny)
library(shinydashboard)
library(rgdal)
library(raster) 
library(sp)
library(ggplot2)
library(pxR)

#comunidades <- read.csv("datos_csv/codccaa.csv", fileEncoding = "UTF-8")
provincias <- read.csv("datos_csv/codprov.csv", fileEncoding = "UTF-8")
municipios <- read.csv("datos_csv/Municipios_Censo_2011.csv", fileEncoding = "UTF-8")
#secciones <- readOGR(dsn = "cartografia_censo2011_nacional/", layer = "SECC_CPV_E_20111101_01_R_INE")
secciones <- readRDS("cartografia_censo2011_nacional/secciones.rds") # saveRDS(secciones, "cartografia_censo2011_nacional/secciones.RDS")
#SXnacionalTodosSexos <- as.data.frame(read.px("scPrincNacionalidades.px"))
#SXnacional <- SXnacionalTodosSexos[which(SXnacionalTodosSexos$sexo == "Ambos Sexos"), ]
SXnacional <- readRDS("SXnacional2019.rds") # saveRDS(SXnacional, "SXnacional2019.rds")

shinyServer(function(input, output, session) {
  
  observeEvent(input$selectProvincia,{
    provincia <- provincias$ID[which(input$selectProvincia == provincias$Nombre)]
    municipiosElegibles <- municipios$NOMBRE[which(provincia == municipios$CPRO)]
    updateSelectInput(session = session, "selectMunicipio", choices = municipiosElegibles)
  })

  observeEvent(input$calcularSecciones,{
    output$mapa <- renderPlot({
      municipio <- sprintf("%05d", municipios$COD_MUN[which(input$selectMunicipio == municipios$NOMBRE)])
      capa <- secciones[secciones@data$CUMUN == municipio,]
      
      capa@data$seccionCensal <- paste0(capa@data$CUMUN, capa@data$CDIS, capa@data$CSEC)
      capa@data <- subset(capa@data, select = -c(CDIS, CSEC, CUMUN, CMUN, CPRO, CCA)) # Eliminamos la info extra ya condensada
      
      nacionalidad <- SXnacional[which(input$selectNacionalidad == SXnacional$nacionalidad), ]
      
      capa@data$numPoblacionElegida <- nacionalidad[match(capa@data$seccionCensal, nacionalidad$sección), "value"]

      
      plot(capa, col=capa@data$numPoblacionElegida, border="black", main=paste0("Sección Censal para ", input$selectMunicipio))
      #ggplot(capa) 
    })
  })
  
})


#murcia <- secciones[secciones@data$CPRO == 30,]

#plot(murcia, col="cyan1", border="black", main="Secciones Censales España")

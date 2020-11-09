#install.packages(c('shiny', 'shinydashboard', 'shinyjs', 'rgdal', 'raster', 'sp', 'pxR', 'leaflet', 'plotKML', 'highcharter', 'shinytest', 'testthat', 'rjson'))
suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(shinyjs)
  library(rgdal)
  library(raster)
  library(sp)
  library(pxR)
  library(leaflet)
  library(plotKML)
  library(highcharter)
})

#options(shiny.port=8080)

#comunidades <- read.csv("datos_csv/codccaa.csv", fileEncoding = "UTF-8")
provincias <- read.csv("datos_csv/codprov.csv", fileEncoding = "UTF-8")
municipios <- read.csv("datos_csv/Municipios_Censo_2011.csv", fileEncoding = "UTF-8")

year <- 2019
SXnacionalAmbos <<- readRDS(paste0("poblacion/", year, "/SXnacional", year, "ambos.rds"))
SXnacionalHombres <<- readRDS(paste0("poblacion/", year, "/SXnacional", year, "hombres.rds"))
SXnacionalMujeres <<- readRDS(paste0("poblacion/", year, "/SXnacional", year, "mujeres.rds"))
secciones <<- readRDS(paste0("seccionado/", year, "/secciones.rds"))

poblacionAñoAmbos <<- list()
for(i in 1:length(list.files("poblacion/"))){
  año <- list.files("poblacion/")
  poblacionAñoAmbos <<- append(poblacionAñoAmbos, 
                         list(cbind(readRDS(paste0("poblacion/", año[i], "/SXnacional", año[i], "ambos.rds")), año[i])))
}

samePopulationPrintYellow <- function(capa_sp){
  return(max(capa_sp@data$numPoblacionElegida,  na.rm = T) - min(capa_sp@data$numPoblacionElegida,  na.rm = T) == 0)
}

simplyMapIt <- function(porcentaje, hombreMujer, municipioSelected, nacionalidadSelected, SXnacionalAmbos, SXnacionalHombres, SXnacionalMujeres, Year){
  return({renderLeaflet({
    municipio <<- sprintf("%05d", municipios$COD_MUN[municipios$NOMBRE %in% municipioSelected])
    capa <- secciones[secciones@data$CUMUN %in% municipio,]
    
    capa@data$seccionCensal <- paste0(capa@data$CUMUN, capa@data$CDIS, capa@data$CSEC)
    capa@data$download <- paste0("download-", capa@data$CUMUN, capa@data$CDIS, capa@data$CSEC)
    
    nacionalidad <- SXnacionalAmbos[which(nacionalidadSelected == SXnacionalAmbos$nacionalidad), ]
    totalPoblacion <- SXnacionalAmbos[which("Total Población" == SXnacionalAmbos$nacionalidad), ]
    nacionalidadPorSeccion <- nacionalidad[match(capa@data$seccionCensal, nacionalidad$sección), "value"]
    capa@data$numPoblacionElegida <- nacionalidadPorSeccion
    if(hombreMujer == T){
      nacionalidadHombres <- SXnacionalHombres[which(nacionalidadSelected == SXnacionalHombres$nacionalidad), ]
      nacionalidadMujeres <- SXnacionalMujeres[which(nacionalidadSelected == SXnacionalMujeres$nacionalidad), ]
      nacionalidadPorSeccionHombres <- nacionalidadHombres[match(capa@data$seccionCensal, nacionalidadHombres$sección), "value"]
      nacionalidadPorSeccionMujeres <- nacionalidadMujeres[match(capa@data$seccionCensal, nacionalidadMujeres$sección), "value"]
      capa@data$numPoblacionElegidaHombres <- nacionalidadPorSeccionHombres
      capa@data$numPoblacionElegidaMujeres <- nacionalidadPorSeccionMujeres
    }
    if(porcentaje == T){
      capa@data$porcentajePoblacion <- 100 * as.numeric(nacionalidadPorSeccion) /
        as.numeric(totalPoblacion[match(capa@data$seccionCensal, totalPoblacion$sección), "value"])
      
      min <- floor(min(capa@data$porcentajePoblacion, na.rm = T))
      max <- ceiling(max(capa@data$porcentajePoblacion, na.rm = T))
    } else {
      min <- min(capa@data$numPoblacionElegida, na.rm = T)
      max <- max(capa@data$numPoblacionElegida, na.rm = T)
    }
    pal <- colorQuantile(colorRamp(c("#00FF00", "#FF0000")), domain = min:max)
    
    capa_sp <<- spTransform(capa, CRS("+proj=longlat +datum=WGS84 +no_defs"))
    
    leaflet(capa_sp, options = leafletOptions(minZoom = 9, maxZoom = 18)) %>% 
      addTiles() %>% 
      setView(lat = round(mean(coordinates(capa_sp)[,2]), digits = 7),
              lng = round(mean(coordinates(capa_sp)[,1]), digits = 7), zoom=11) %>% 
      addPolygons(weight = 2, fillColor = ~ if(samePopulationPrintYellow(capa_sp)){ "#FFFF00" } else {
        pal(if(porcentaje == T) {capa_sp@data$porcentajePoblacion} else {capa_sp@data$numPoblacionElegida})},
        fillOpacity = "0.4",
        stroke = T, color = "black", opacity = 0.8, highlightOptions = highlightOptions(color = "white", weight = 4, bringToFront = TRUE),
        popup = paste0("Sección Censal: <b>", paste0(capa_sp@data$CUMUN, "-", capa_sp@data$CDIS, "-", capa_sp@data$CSEC), "</b><br>",
                       "Población: <b>", capa_sp@data$numPoblacionElegida,
                       if(porcentaje == T) {
                              if(hombreMujer == T){ 
                                     paste0(
                                       "</b><br>",
                                       "Porcentaje de población: <b>", round(capa_sp@data$porcentajePoblacion, digits = 2), "%</b><br>",
                                       "Hombres: <b>", capa_sp@data$numPoblacionElegidaHombres, "</b><br>",
                                       "Mujeres: <b>", capa_sp@data$numPoblacionElegidaMujeres, "</b><br>",
                                       "Fecha: <b>", Year, "</b>")} else {
                                     paste0(
                                       if(samePopulationPrintYellow(capa_sp)){
                                              paste0(" </b> Fecha: <b>", Year, "</b><br>")} else{
                                              paste0("</b><br>", "Porcentaje de población: <b>", round(capa_sp@data$porcentajePoblacion, digits = 2),
                                                     "%</b><br>", "Fecha: <b>", Year, "</b>")})}} else {
                              paste0("</b><br>", if(hombreMujer == T) paste0("Hombres: <b>", capa_sp@data$numPoblacionElegidaHombres, "</b><br>",
                                                                             "Mujeres: <b>", capa_sp@data$numPoblacionElegidaMujeres, "</b><br>"),
                                     "Fecha: <b>", Year, "</b>")}
        ),
        layerId = capa_sp@data$seccionCensal, group = "censussections", label = capa_sp@data$seccionCensal) %>% 
      addLegend(colors = if(samePopulationPrintYellow(capa_sp)){ "#FFFF00" } else { c(pal(max), pal((3*max+2*min)/5), pal((2*max+3*min)/5), pal(min))},
                labels = if(porcentaje == T){
                                if(samePopulationPrintYellow(capa_sp)){ 
                                       paste0(min(capa_sp@data$porcentajePoblacion,  na.rm = T),
                                              "% - ", max(capa_sp@data$porcentajePoblacion,  na.rm = T), "%")} else {
                                       c(paste0(round((3*max+min)/4, digits = 2), " - <b>", max, "%</b>"),
                                         paste0(round((max+min)/2, digits = 2), " - ", round((3*max+min)/4, digits = 2), "%"),
                                         paste0(round((max+3*min)/4, digits = 2), " - ", round((max+min)/2, digits = 2), "%"),
                                         paste0("<b>", min, "</b> - ", round((max+3*min)/4, digits = 2), "%"))}} else{
                                if(samePopulationPrintYellow(capa_sp)){ 
                                       paste0(min(capa_sp@data$numPoblacionElegida,  na.rm = T), " - ", max(capa_sp@data$numPoblacionElegida,  na.rm = T))} else {
                                       c(paste0(round((3*max+min)/4, digits = 2), " - <b>", max, "</b>"),
                                         paste0(round((max+min)/2, digits = 2), " - ", round((3*max+min)/4, digits = 2)),
                                         paste0(round((max+3*min)/4, digits = 2), " - ", round((max+min)/2, digits = 2)),
                                         paste0("<b>", min, "</b> - ", round((max+3*min)/4, digits = 2)))}},
                na.label = "Valor no disponible", title = "Población", opacity = "0.4", bins = 2)
  })
  })
  samePopulationPrintYellow(capa_sp)
}
#install.packages(c('shiny', 'shinydashboard', 'shinyjs', 'rgdal', 'raster', 'sp', 'pxR', 'leaflet', 'plotKML', 'highcharter'))
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

#comunidades <- read.csv("datos_csv/codccaa.csv", fileEncoding = "UTF-8")
provincias <- read.csv("datos_csv/codprov.csv", fileEncoding = "UTF-8")
municipios <- read.csv("datos_csv/Municipios_Censo_2011.csv", fileEncoding = "UTF-8")

year <- 2019
SXnacionalAmbos <<- readRDS(paste0("poblacion/", year, "/SXnacional", year, "ambos.rds"))
SXnacionalHombres <<- readRDS(paste0("poblacion/", year, "/SXnacional", year, "hombres.rds"))
SXnacionalMujeres <<- readRDS(paste0("poblacion/", year, "/SXnacional", year, "mujeres.rds"))
secciones <<- readRDS(paste0("seccionado/", year, "/secciones.rds"))

simplyMapIt <- function(porcentaje, hombreMujer, municipioSelected, nacionalidadSelected, SXnacionalAmbos, SXnacionalHombres, SXnacionalMujeres, Year){
  if(porcentaje == T){
    if(hombreMujer == T){
      
      ####################################################
      ## Mapa con porcentajes y Distinción Hombre/Mujer ##
      ####################################################
      
      return({renderLeaflet({
        municipio <<- sprintf("%05d", municipios$COD_MUN[municipios$NOMBRE %in% municipioSelected])
        capa <- secciones[secciones@data$CUMUN %in% municipio,]
        
        capa@data$seccionCensal <- paste0(capa@data$CUMUN, capa@data$CDIS, capa@data$CSEC)
        capa@data$download <- paste0("download-", capa@data$CUMUN, capa@data$CDIS, capa@data$CSEC)
        
        nacionalidad <- SXnacionalAmbos[which(nacionalidadSelected == SXnacionalAmbos$nacionalidad), ]
        nacionalidadHombres <- SXnacionalHombres[which(nacionalidadSelected == SXnacionalHombres$nacionalidad), ]
        nacionalidadMujeres <- SXnacionalMujeres[which(nacionalidadSelected == SXnacionalMujeres$nacionalidad), ]
        totalPoblacion <- SXnacionalAmbos[which("Total Población" == SXnacionalAmbos$nacionalidad), ]
        nacionalidadPorSeccion <- nacionalidad[match(capa@data$seccionCensal, nacionalidad$sección), "value"]
        nacionalidadPorSeccionHombres <- nacionalidadHombres[match(capa@data$seccionCensal, nacionalidadHombres$sección), "value"]
        nacionalidadPorSeccionMujeres <- nacionalidadMujeres[match(capa@data$seccionCensal, nacionalidadMujeres$sección), "value"]
        
        capa@data$numPoblacionElegida <- nacionalidadPorSeccion
        capa@data$numPoblacionElegidaHombres <- nacionalidadPorSeccionHombres
        capa@data$numPoblacionElegidaMujeres <- nacionalidadPorSeccionMujeres
        capa@data$porcentajePoblacion <- 100 * as.numeric(nacionalidadPorSeccion) / as.numeric(totalPoblacion[match(capa@data$seccionCensal,
                                                                                                                    totalPoblacion$sección), "value"])
        
        min <- floor(min(capa@data$porcentajePoblacion, na.rm = T))
        max <- ceiling(max(capa@data$porcentajePoblacion, na.rm = T))
        pal <- colorQuantile(colorRamp(c("#00FF00", "#FF0000")), domain = min:max)
        
        capa_sp <<- spTransform(capa, CRS("+proj=longlat +datum=WGS84 +no_defs"))
        
        if(max(capa_sp@data$numPoblacionElegida,  na.rm = T) - min(capa_sp@data$numPoblacionElegida,  na.rm = T) == 0) {
          leaflet(capa_sp, options = leafletOptions(minZoom = 9, maxZoom = 18)) %>% 
            addTiles() %>% 
            setView(lat = round(mean(coordinates(capa_sp)[,2]), digits = 7),
                    lng = round(mean(coordinates(capa_sp)[,1]), digits = 7), zoom=11) %>% 
            addPolygons(weight = 2, fillColor = "#FFFF00", fillOpacity = "0.4", stroke = T, color = "black", opacity = 0.8,
                        highlightOptions = highlightOptions(color = "white", weight = 4, bringToFront = TRUE),
                        popup = paste0("Sección Censal: <b>", paste0(capa_sp@data$CUMUN, "-", capa_sp@data$CDIS, "-", capa_sp@data$CSEC), "</b><br>",
                                       "Población: <b>", capa_sp@data$numPoblacionElegida, "</b><br>",
                                       "Porcentaje de población: <b>", round(capa_sp@data$porcentajePoblacion, digits = 2), "%</b><br>",
                                       "Hombres: <b>", capa_sp@data$numPoblacionElegidaHombres, "</b><br>",
                                       "Mujeres: <b>", capa_sp@data$numPoblacionElegidaMujeres, "</b><br>",
                                       "Fecha: <b>", Year, "</b>"),
                        layerId = capa_sp@data$seccionCensal, group = "censussections", label = capa_sp@data$seccionCensal) %>% 
            addLegend(colors = "#FFFF00",
                      labels = paste0(min(capa_sp@data$porcentajePoblacion,  na.rm = T), "% - ", max(capa_sp@data$porcentajePoblacion,  na.rm = T), "%"),
                      na.label = "Valor no disponible", title = "Población", opacity = "0.4", bins = 2)
        }
        else {
          leaflet(capa_sp, options = leafletOptions(minZoom = 9, maxZoom = 18)) %>% 
            addTiles() %>% 
            setView(lat = round(mean(coordinates(capa_sp)[,2]), digits = 7),
                    lng = round(mean(coordinates(capa_sp)[,1]), digits = 7), zoom=11) %>% 
            addPolygons(weight = 2, fillColor = ~pal(porcentajePoblacion), fillOpacity = "0.4", stroke = T, color = "black", opacity = 0.8,
                        highlightOptions = highlightOptions(color = "white", weight = 4, bringToFront = TRUE),
                        popup = paste0("Sección Censal: <b>", paste0(capa_sp@data$CUMUN, "-", capa_sp@data$CDIS, "-", capa_sp@data$CSEC), "</b><br>",
                                       "Población: <b>", capa_sp@data$numPoblacionElegida, "</b><br>",
                                       "Porcentaje de población: <b>", round(capa_sp@data$porcentajePoblacion, digits = 2), "%</b><br>",
                                       "Hombres: <b>", capa_sp@data$numPoblacionElegidaHombres, "</b><br>",
                                       "Mujeres: <b>", capa_sp@data$numPoblacionElegidaMujeres, "</b><br>",
                                       "Fecha: <b>", Year, "</b>"),
                        layerId = capa_sp@data$seccionCensal, group = "censussections", label = capa_sp@data$seccionCensal)  %>% 
            addLegend(colors = c(pal(max), pal((3*max+2*min)/5), pal((2*max+3*min)/5), pal(min)),
                      labels = c(paste0(round((3*max+min)/4, digits = 2), " - <b>", max, "%</b>"),
                                 paste0(round((max+min)/2, digits = 2), " - ", round((3*max+min)/4, digits = 2), "%"),
                                 paste0(round((max+3*min)/4, digits = 2), " - ", round((max+min)/2, digits = 2), "%"),
                                 paste0("<b>", min, "</b> - ", round((max+3*min)/4, digits = 2), "%")),
                      na.label = "Valor no disponible", title = "Población", opacity = "0.4", bins = 2)
        }
        
        
      })
      })
    }
    else{
      
      ###############################################
      ## Mapa únicamente con valores y porcentajes ##
      ###############################################
      
      return({renderLeaflet({
        municipio <<- sprintf("%05d", municipios$COD_MUN[municipios$NOMBRE %in% municipioSelected])
        capa <- secciones[secciones@data$CUMUN %in% municipio,]
        
        capa@data$seccionCensal <- paste0(capa@data$CUMUN, capa@data$CDIS, capa@data$CSEC)
        capa@data$download <- paste0("download-", capa@data$CUMUN, capa@data$CDIS, capa@data$CSEC)
        
        nacionalidad <- SXnacionalAmbos[which(nacionalidadSelected == SXnacionalAmbos$nacionalidad), ]
        totalPoblacion <- SXnacionalAmbos[which("Total Población" == SXnacionalAmbos$nacionalidad), ]
        nacionalidadPorSeccion <- nacionalidad[match(capa@data$seccionCensal, nacionalidad$sección), "value"]
        
        capa@data$numPoblacionElegida <- nacionalidadPorSeccion
        capa@data$porcentajePoblacion <- 100 * as.numeric(nacionalidadPorSeccion) / as.numeric(totalPoblacion[match(capa@data$seccionCensal,
                                                                                                                    totalPoblacion$sección), "value"])
        
        min <- floor(min(capa@data$porcentajePoblacion, na.rm = T))
        max <- ceiling(max(capa@data$porcentajePoblacion, na.rm = T))
        pal <- colorQuantile(colorRamp(c("#00FF00", "#FF0000")), domain = min:max)
        
        capa_sp <<- spTransform(capa, CRS("+proj=longlat +datum=WGS84 +no_defs"))
        
        if(max(capa_sp@data$numPoblacionElegida,  na.rm = T) - min(capa_sp@data$numPoblacionElegida,  na.rm = T) == 0) {
          leaflet(capa_sp, options = leafletOptions(minZoom = 9, maxZoom = 18)) %>% 
            addTiles() %>% 
            setView(lat = round(mean(coordinates(capa_sp)[,2]), digits = 7),
                    lng = round(mean(coordinates(capa_sp)[,1]), digits = 7), zoom=11) %>% 
            addPolygons(weight = 2, fillColor = "#FFFF00", fillOpacity = "0.4", stroke = T, color = "black", opacity = 0.8,
                        highlightOptions = highlightOptions(color = "white", weight = 4, bringToFront = TRUE),
                        popup = paste0("Sección Censal: <b>", paste0(capa_sp@data$CUMUN, "-", capa_sp@data$CDIS, "-", capa_sp@data$CSEC), "</b><br>",
                                       "Población: <b>", capa_sp@data$numPoblacionElegida, " </b> Fecha: <b>", Year, "</b><br>"),
                        layerId = capa_sp@data$seccionCensal, group = "censussections", label = capa_sp@data$seccionCensal) %>% 
            addLegend(colors = "#FFFF00",
                      labels = paste0(min(capa_sp@data$porcentajePoblacion,  na.rm = T), "% - ", max(capa_sp@data$porcentajePoblacion,  na.rm = T), "%"),
                      na.label = "Valor no disponible", title = "Población", opacity = "0.4", bins = 2)
        }
        else {
          leaflet(capa_sp, options = leafletOptions(minZoom = 9, maxZoom = 18)) %>% 
            addTiles() %>% 
            setView(lat = round(mean(coordinates(capa_sp)[,2]), digits = 7),
                    lng = round(mean(coordinates(capa_sp)[,1]), digits = 7), zoom=11) %>% 
            addPolygons(weight = 2, fillColor = ~pal(porcentajePoblacion), fillOpacity = "0.4", stroke = T, color = "black", opacity = 0.8,
                        highlightOptions = highlightOptions(color = "white", weight = 4, bringToFront = TRUE),
                        popup = paste0("Sección Censal: <b>", paste0(capa_sp@data$CUMUN, "-", capa_sp@data$CDIS, "-", capa_sp@data$CSEC), "</b><br>",
                                       "Población: <b>", capa_sp@data$numPoblacionElegida, "</b><br>",
                                       "Porcentaje de población: <b>", round(capa_sp@data$porcentajePoblacion, digits = 2), "%</b><br>",
                                       "Fecha: <b>", Year, "</b>"),
                        layerId = capa_sp@data$seccionCensal, group = "censussections", label = capa_sp@data$seccionCensal)  %>% 
            addLegend(colors = c(pal(max), pal((3*max+2*min)/5), pal((2*max+3*min)/5), pal(min)),
                      labels = c(paste0(round((3*max+min)/4, digits = 2), " - <b>", max, "%</b>"),
                                 paste0(round((max+min)/2, digits = 2), " - ", round((3*max+min)/4, digits = 2), "%"),
                                 paste0(round((max+3*min)/4, digits = 2), " - ", round((max+min)/2, digits = 2), "%"),
                                 paste0("<b>", min, "</b> - ", round((max+3*min)/4, digits = 2), "%")),
                      na.label = "Valor no disponible", title = "Población", opacity = "0.4", bins = 2)
        }
        
        
      })
      })
      
    }
    
    
    
    
  }
  else{
    if(hombreMujer == T){
      ###############################################
      ## Mapa únicamente con valores y hombreMujer ##
      ###############################################
      
      return({renderLeaflet({
        municipio <<- sprintf("%05d", municipios$COD_MUN[municipios$NOMBRE %in% municipioSelected])
        capa <- secciones[secciones@data$CUMUN %in% municipio,]
        
        capa@data$seccionCensal <- paste0(capa@data$CUMUN, capa@data$CDIS, capa@data$CSEC)
        capa@data$download <- paste0("download-", capa@data$CUMUN, capa@data$CDIS, capa@data$CSEC)
        
        nacionalidad <- SXnacionalAmbos[which(nacionalidadSelected == SXnacionalAmbos$nacionalidad), ]
        nacionalidadHombres <- SXnacionalHombres[which(nacionalidadSelected == SXnacionalHombres$nacionalidad), ]
        nacionalidadMujeres <- SXnacionalMujeres[which(nacionalidadSelected == SXnacionalMujeres$nacionalidad), ]
        totalPoblacion <- SXnacionalAmbos[which("Total Población" == SXnacionalAmbos$nacionalidad), ]
        nacionalidadPorSeccion <- nacionalidad[match(capa@data$seccionCensal, nacionalidad$sección), "value"]
        nacionalidadPorSeccionHombres <- nacionalidadHombres[match(capa@data$seccionCensal, nacionalidadHombres$sección), "value"]
        nacionalidadPorSeccionMujeres <- nacionalidadMujeres[match(capa@data$seccionCensal, nacionalidadMujeres$sección), "value"]
        
        capa@data$numPoblacionElegida <- nacionalidadPorSeccion
        capa@data$numPoblacionElegidaHombres <- nacionalidadPorSeccionHombres
        capa@data$numPoblacionElegidaMujeres <- nacionalidadPorSeccionMujeres
        
        min <- min(capa@data$numPoblacionElegida, na.rm = T)
        max <- max(capa@data$numPoblacionElegida, na.rm = T)
        pal <- colorQuantile(colorRamp(c("#00FF00", "#FF0000")), domain = min:max)
        
        capa_sp <<- spTransform(capa, CRS("+proj=longlat +datum=WGS84 +no_defs"))
        
        if(max(capa_sp@data$numPoblacionElegida,  na.rm = T) - min(capa_sp@data$numPoblacionElegida,  na.rm = T) == 0) {
          leaflet(capa_sp, options = leafletOptions(minZoom = 9, maxZoom = 18)) %>% 
            addTiles() %>% 
            setView(lat = round(mean(coordinates(capa_sp)[,2]), digits = 7),
                    lng = round(mean(coordinates(capa_sp)[,1]), digits = 7), zoom=11) %>% 
            addPolygons(weight = 2, fillColor = "#FFFF00", fillOpacity = "0.4", stroke = T, color = "black", opacity = 0.8,
                        highlightOptions = highlightOptions(color = "white", weight = 4, bringToFront = TRUE),
                        popup = paste0("Sección Censal: <b>", paste0(capa_sp@data$CUMUN, "-", capa_sp@data$CDIS, "-", capa_sp@data$CSEC), "</b><br>",
                                       "Población: <b>", capa_sp@data$numPoblacionElegida, "</b><br>",
                                       "Hombres: <b>", capa_sp@data$numPoblacionElegidaHombres, "</b><br>",
                                       "Mujeres: <b>", capa_sp@data$numPoblacionElegidaMujeres, "</b><br>",
                                       "Fecha: <b>", Year, "</b>"),
                        layerId = capa_sp@data$seccionCensal, group = "censussections", label = capa_sp@data$seccionCensal) %>% 
            addLegend(colors = "#FFFF00",
                      labels = paste0(min(capa_sp@data$numPoblacionElegida,  na.rm = T), " - ", max(capa_sp@data$numPoblacionElegida,  na.rm = T)),
                      na.label = "Valor no disponible", title = "Población", opacity = "0.4", bins = 2)
        }
        else {
          leaflet(capa_sp, options = leafletOptions(minZoom = 9, maxZoom = 18)) %>% 
            addTiles() %>% 
            setView(lat = round(mean(coordinates(capa_sp)[,2]), digits = 7),
                    lng = round(mean(coordinates(capa_sp)[,1]), digits = 7), zoom=11) %>% 
            addPolygons(weight = 2, fillColor = ~pal(numPoblacionElegida), fillOpacity = "0.4", stroke = T, color = "black", opacity = 0.8,
                        highlightOptions = highlightOptions(color = "white", weight = 4, bringToFront = TRUE),
                        popup = paste0("Sección Censal: <b>", paste0(capa_sp@data$CUMUN, "-", capa_sp@data$CDIS, "-", capa_sp@data$CSEC), "</b><br>",
                                       "Población: <b>", capa_sp@data$numPoblacionElegida, "</b><br>",
                                       "Hombres: <b>", capa_sp@data$numPoblacionElegidaHombres, "</b><br>",
                                       "Mujeres: <b>", capa_sp@data$numPoblacionElegidaMujeres, "</b><br>",
                                       "Fecha: <b>", Year, "</b>"),
                        layerId = capa_sp@data$seccionCensal, group = "censussections", label = capa_sp@data$seccionCensal)  %>% 
            addLegend(colors = c(pal(max), pal((3*max+2*min)/5), pal((2*max+3*min)/5), pal(min)),
                      labels = c(paste0(round((3*max+min)/4, digits = 2), " - <b>", max, "</b>"),
                                 paste0(round((max+min)/2, digits = 2), " - ", round((3*max+min)/4, digits = 2)),
                                 paste0(round((max+3*min)/4, digits = 2), " - ", round((max+min)/2, digits = 2)),
                                 paste0("<b>", min, "</b> - ", round((max+3*min)/4, digits = 2))),
                      na.label = "Valor no disponible", title = "Población", opacity = "0.4", bins = 2)
        }
        
        
      })
      })
      
    }
    else{
      #################################
      ## Mapa únicamente con valores ##
      #################################
      
      return({renderLeaflet({
        municipio <<- sprintf("%05d", municipios$COD_MUN[municipios$NOMBRE %in% municipioSelected])
        capa <- secciones[secciones@data$CUMUN %in% municipio,]
        
        capa@data$seccionCensal <- paste0(capa@data$CUMUN, capa@data$CDIS, capa@data$CSEC)
        capa@data$download <- paste0("download-", capa@data$CUMUN, capa@data$CDIS, capa@data$CSEC)
        #capa@data <- subset(capa@data, select = -c(CDIS, CSEC, CUMUN, CMUN, CPRO, CCA)) # Eliminamos la info extra ya condensada
        
        nacionalidad <- SXnacionalAmbos[which(nacionalidadSelected == SXnacionalAmbos$nacionalidad), ]
        
        capa@data$numPoblacionElegida <- nacionalidad[match(capa@data$seccionCensal, nacionalidad$sección), "value"]
        
        min <- min(capa@data$numPoblacionElegida, na.rm = T)
        max <- max(capa@data$numPoblacionElegida, na.rm = T)
        pal <- colorQuantile(colorRamp(c("#00FF00", "#FF0000")), domain = min:max)
        
        capa_sp <<- spTransform(capa, CRS("+proj=longlat +datum=WGS84 +no_defs"))
        
        if(max(capa_sp@data$numPoblacionElegida,  na.rm = T) - min(capa_sp@data$numPoblacionElegida,  na.rm = T) == 0) {
          leaflet(capa_sp, options = leafletOptions(minZoom = 9, maxZoom = 18)) %>% 
            addTiles() %>% 
            setView(lat = round(mean(coordinates(capa_sp)[,2]), digits = 7),
                    lng = round(mean(coordinates(capa_sp)[,1]), digits = 7), zoom=11) %>% 
            addPolygons(weight = 2, fillColor = "#FFFF00", fillOpacity = "0.4", stroke = T, color = "black", opacity = 0.8,
                        highlightOptions = highlightOptions(color = "white", weight = 4, bringToFront = TRUE),
                        popup = paste0("Sección Censal: <b>", paste0(capa_sp@data$CUMUN, "-", capa_sp@data$CDIS, "-", capa_sp@data$CSEC), "</b><br>",
                                       "Población: <b>", capa_sp@data$numPoblacionElegida, "</b><br>",
                                       "Fecha: <b>", Year, "</b>"),
                        layerId = capa_sp@data$seccionCensal, group = "censussections", label = capa_sp@data$seccionCensal) %>% 
            addLegend(colors = "#FFFF00",
                      labels = paste0(min(capa_sp@data$numPoblacionElegida,  na.rm = T), " - ", max(capa_sp@data$numPoblacionElegida,  na.rm = T)),
                      na.label = "Valor no disponible", title = "Población", opacity = "0.4", bins = 2)
        }
        else {
          leaflet(capa_sp, options = leafletOptions(minZoom = 9, maxZoom = 18)) %>% 
            addTiles() %>% 
            setView(lat = round(mean(coordinates(capa_sp)[,2]), digits = 7),
                    lng = round(mean(coordinates(capa_sp)[,1]), digits = 7), zoom=11) %>% 
            addPolygons(weight = 2, fillColor = ~pal(numPoblacionElegida), fillOpacity = "0.4", stroke = T, color = "black", opacity = 0.8,
                        highlightOptions = highlightOptions(color = "white", weight = 4, bringToFront = TRUE),
                        popup = paste0("Sección Censal: <b>", paste0(capa_sp@data$CUMUN, "-", capa_sp@data$CDIS, "-", capa_sp@data$CSEC), "</b><br>",
                                       "Población: <b>", capa_sp@data$numPoblacionElegida, "</b><br>",
                                       "Fecha: <b>", Year, "</b>"),
                        layerId = capa_sp@data$seccionCensal, group = "censussections", label = capa_sp@data$seccionCensal) %>% 
            addLegend(colors = c(pal(max), pal((3*max+2*min)/5), pal((2*max+3*min)/5), pal(min)),
                      labels = c(paste0(round((3*max+min)/4, digits = 2), " - <b>", max, "</b>"),
                                 paste0(round((max+min)/2, digits = 2), " - ", round((3*max+min)/4, digits = 2)),
                                 paste0(round((max+3*min)/4, digits = 2), " - ", round((max+min)/2, digits = 2)),
                                 paste0("<b>", min, "</b> - ", round((max+3*min)/4, digits = 2))),
                      na.label = "Valor no disponible", title = "Población", opacity = "0.4", bins = 2)
        }
        
      })
      })
    }
  }
}
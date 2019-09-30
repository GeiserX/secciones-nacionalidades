#install.packages("rgdal", "raster", "sp")

library(shiny)
library(shinydashboard)
library(shinyjs)
library(rgdal)
library(raster) 
library(sp)
library(pxR)
library(leaflet)
library(plotKML)

#comunidades <- read.csv("datos_csv/codccaa.csv", fileEncoding = "UTF-8")
provincias <- read.csv("datos_csv/codprov.csv", fileEncoding = "UTF-8")
municipios <- read.csv("datos_csv/Municipios_Censo_2011.csv", fileEncoding = "UTF-8")
#secciones <- readOGR(dsn = "seccionado_2018/", layer = "SECC_CE_20180101") # Datos a 2018
secciones <- readRDS("seccionado_2018/secciones.rds") # saveRDS(secciones, "seccionado_2018/secciones.rds")
#secciones_json <- geojson_json(secciones)
#seccionesTransform <- spTransform(seccionesRaw, CRS("+proj=longlat +datum=WGS84"))
#seccionesGoogleMapsz <- fortify(seccionesTransform)

#SXnacional <- as.data.frame(read.px("scPrincNacionalidades.px"))
#SXnacionalAmbos <- SXnacionalTodosSexos[which(SXnacional$sexo == "Ambos Sexos"), ]
#SXnacionalHombres <- SXnacionalTodosSexos[which(SXnacional$sexo == "Hombres"), ]
#SXnacionalMujeres <- SXnacionalTodosSexos[which(SXnacional$sexo == "Mujeres"), ]
SXnacionalAmbos <- readRDS("SXnacional2019ambos.rds") # saveRDS(SXnacionalAmbos, "SXnacional2019ambos.rds")
SXnacionalHombres <- readRDS("SXnacional2019hombres.rds") # saveRDS(SXnacionalHombres, "SXnacional2019hombres.rds")
SXnacionalMujeres <- readRDS("SXnacional2019mujeres.rds") # saveRDS(SXnacionalMujeres, "SXnacional2019mujeres.rds")

shinyServer(function(input, output, session) {
  
  observe({
    if(input$porcentaje == T){
      updateSelectizeInput(session, "selectNacionalidad", choices = levels(SXnacionalAmbos$nacionalidad)[-1])
      enable("hombreMujer")
    }
    else{
      updateSelectizeInput(session, "selectNacionalidad", choices = levels(SXnacionalAmbos$nacionalidad))
      disable("hombreMujer")
    }
  })
  
  output$info <- renderText({
    paste0("<hr>Cartografía obtenida de <a href=http://www.ine.es/ss/Satellite?L=es_ES&c=Page&cid=1259952026632&p=1259952026632&pagename=ProductosYServicios%2FPYSLayout>",
           "los datos disponibles públicamente en la web del Instituto Nacional de Estadística</a><hr> Datos sobre población obtenida gracias a la ",
    "<a href=https://www.ine.es/dyngs/INEbase/es/operacion.htm?c=Estadistica_C&cid=1254736177012&menu=resultados&secc=1254736195461&idp=1254734710990>",
      "información disponible públicamente en la web del Istituto Nacional de Estadística</a>")
  })
  
  clickedIds <- reactiveValues(ids = vector())
  
  observeEvent(input$selectMunicipio,{
    disable("descargaKML")
  })
    
  
  observeEvent(input$selectProvincia,{
    provincia <- provincias$ID[which(input$selectProvincia == provincias$Nombre)]
    municipiosElegibles <<- municipios$NOMBRE[which(provincia == municipios$CPRO)]
    #trimmedMunicipios <- trimws(gsub("\\([^()]*\\)", "", municipiosElegibles))
    updateSelectizeInput(session = session, "selectMunicipio", choices = municipiosElegibles)
  })
  
  observe({
    if(!is.null(input$selectMunicipio)){
      if(input$selectMunicipio != "Cargando..." && input$selectMunicipio %in% municipiosElegibles){
        if(input$porcentaje == T){
          if(input$hombreMujer == T){
            
            ####################################################
            ## Mapa con porcentajes y Distinción Hombre/Mujer ##
            ####################################################
            
            output$mapa <- renderLeaflet({
              municipio <<- sprintf("%05d", municipios$COD_MUN[municipios$NOMBRE %in% input$selectMunicipio])
              capa <- secciones[secciones@data$CUMUN %in% municipio,]
              
              capa@data$seccionCensal <- paste0(capa@data$CUMUN, capa@data$CDIS, capa@data$CSEC)
              capa@data$download <- paste0("download-", capa@data$CUMUN, capa@data$CDIS, capa@data$CSEC)
              
              nacionalidad <- SXnacionalAmbos[which(input$selectNacionalidad == SXnacionalAmbos$nacionalidad), ]
              nacionalidadHombres <- SXnacionalHombres[which(input$selectNacionalidad == SXnacionalHombres$nacionalidad), ]
              nacionalidadMujeres <- SXnacionalMujeres[which(input$selectNacionalidad == SXnacionalMujeres$nacionalidad), ]
              totalPoblacion <- SXnacionalAmbos[which("Total Población" == SXnacionalAmbos$nacionalidad), ]
              nacionalidadPorSeccion <- nacionalidad[match(capa@data$seccionCensal, nacionalidad$sección), "value"]
              nacionalidadPorSeccionHombres <- nacionalidadHombres[match(capa@data$seccionCensal, nacionalidadHombres$sección), "value"]
              nacionalidadPorSeccionMujeres <- nacionalidadMujeres[match(capa@data$seccionCensal, nacionalidadMujeres$sección), "value"]
              
              capa@data$numPoblacionElegida <- nacionalidadPorSeccion
              capa@data$numPoblacionElegidaHombres <- nacionalidadPorSeccionHombres
              capa@data$numPoblacionElegidaMujeres <- nacionalidadPorSeccionMujeres
              capa@data$porcentajePoblacion <- 100 * as.numeric(nacionalidadPorSeccion) / as.numeric(totalPoblacion[match(capa@data$seccionCensal, totalPoblacion$sección), "value"])
              
              min <- floor(min(capa@data$porcentajePoblacion, na.rm = T))
              max <- ceiling(max(capa@data$porcentajePoblacion, na.rm = T))
              pal <- colorQuantile(colorRamp(c("#00FF00", "#FF0000")), domain = min:max)
              
              capa_sp <<- spTransform(capa, CRS("+proj=longlat +datum=WGS84 +no_defs"))
              
              if(max(capa_sp@data$numPoblacionElegida,  na.rm = T) - min(capa_sp@data$numPoblacionElegida,  na.rm = T) == 0) {
                leaflet(capa_sp, options = leafletOptions(minZoom = 9, maxZoom = 18)) %>% 
                  addTiles() %>% 
                  setView(lat = mean(coordinates(capa_sp)[,2]), lng=mean(coordinates(capa_sp)[,1]), zoom=11) %>% 
                  addPolygons(weight = 2, fillColor = "#FFFF00", fillOpacity = "0.4", stroke = T, color = "black", opacity = 0.8,
                              highlightOptions = highlightOptions(color = "white", weight = 4, bringToFront = TRUE),
                              popup = paste0("Sección Censal: <b>", paste0(capa_sp@data$CUMUN, "-", capa_sp@data$CDIS, "-", capa_sp@data$CSEC), "</b><br>",
                                             "Población: <b>", capa_sp@data$numPoblacionElegida, "</b><br>",
                                             "Porcentaje de población: <b>", round(capa_sp@data$porcentajePoblacion, digits = 2), "%</b><br>",
                                             "Hombres: <b>", capa_sp@data$numPoblacionElegidaHombres, "</b><br>",
                                             "Mujeres: <b>", capa_sp@data$numPoblacionElegidaMujeres, "</b>"),
                              layerId = capa_sp@data$seccionCensal, group = "censussections", label = capa_sp@data$seccionCensal) %>% 
                  addLegend(colors = "#FFFF00",
                            labels = paste0(min(capa_sp@data$porcentajePoblacion,  na.rm = T), "% - ", max(capa_sp@data$porcentajePoblacion,  na.rm = T), "%"),
                            na.label = "Valor no disponible", title = "Población", opacity = "0.4", bins = 2)
              }
              else {
                leaflet(capa_sp, options = leafletOptions(minZoom = 9, maxZoom = 18)) %>% 
                  addTiles() %>% 
                  setView(lat = mean(coordinates(capa_sp)[,2]), lng=mean(coordinates(capa_sp)[,1]), zoom=11) %>% 
                  addPolygons(weight = 2, fillColor = ~pal(porcentajePoblacion), fillOpacity = "0.4", stroke = T, color = "black", opacity = 0.8,
                              highlightOptions = highlightOptions(color = "white", weight = 4, bringToFront = TRUE),
                              popup = paste0("Sección Censal: <b>", paste0(capa_sp@data$CUMUN, "-", capa_sp@data$CDIS, "-", capa_sp@data$CSEC), "</b><br>",
                                             "Población: <b>", capa_sp@data$numPoblacionElegida, "</b><br>",
                                             "Porcentaje de población: <b>", round(capa_sp@data$porcentajePoblacion, digits = 2), "%</b><br>",
                                             "Hombres: <b>", capa_sp@data$numPoblacionElegidaHombres, "</b><br>",
                                             "Mujeres: <b>", capa_sp@data$numPoblacionElegidaMujeres, "</b>"),
                              layerId = capa_sp@data$seccionCensal, group = "censussections", label = capa_sp@data$seccionCensal)  %>% 
                  addLegend(colors = c(pal(max), pal((3*max+2*min)/5), pal((2*max+3*min)/5), pal(min)),
                            labels = c(paste0(round((3*max+min)/4, digits = 2), " - <b>", max, "%</b>"),
                                       paste0(round((max+min)/2, digits = 2), " - ", round((3*max+min)/4, digits = 2), "%"),
                                       paste0(round((max+3*min)/4, digits = 2), " - ", round((max+min)/2, digits = 2), "%"),
                                       paste0("<b>", min, "</b> - ", round((max+3*min)/4, digits = 2), "%")),
                            na.label = "Valor no disponible", title = "Población", opacity = "0.4", bins = 2)
              }
              
              
            })
            
          }
          else{
            
            ###############################################
            ## Mapa únicamente con valores y porcentajes ##
            ###############################################
            
            output$mapa <- renderLeaflet({
              municipio <<- sprintf("%05d", municipios$COD_MUN[municipios$NOMBRE %in% input$selectMunicipio])
              capa <- secciones[secciones@data$CUMUN %in% municipio,]
              
              capa@data$seccionCensal <- paste0(capa@data$CUMUN, capa@data$CDIS, capa@data$CSEC)
              capa@data$download <- paste0("download-", capa@data$CUMUN, capa@data$CDIS, capa@data$CSEC)
              
              nacionalidad <- SXnacionalAmbos[which(input$selectNacionalidad == SXnacionalAmbos$nacionalidad), ]
              totalPoblacion <- SXnacionalAmbos[which("Total Población" == SXnacionalAmbos$nacionalidad), ]
              nacionalidadPorSeccion <- nacionalidad[match(capa@data$seccionCensal, nacionalidad$sección), "value"]
              
              capa@data$numPoblacionElegida <- nacionalidadPorSeccion
              capa@data$porcentajePoblacion <- 100 * as.numeric(nacionalidadPorSeccion) / as.numeric(totalPoblacion[match(capa@data$seccionCensal, totalPoblacion$sección), "value"])
              
              min <- floor(min(capa@data$porcentajePoblacion, na.rm = T))
              max <- ceiling(max(capa@data$porcentajePoblacion, na.rm = T))
              pal <- colorQuantile(colorRamp(c("#00FF00", "#FF0000")), domain = min:max)
              
              capa_sp <<- spTransform(capa, CRS("+proj=longlat +datum=WGS84 +no_defs"))
              
              if(max(capa_sp@data$numPoblacionElegida,  na.rm = T) - min(capa_sp@data$numPoblacionElegida,  na.rm = T) == 0) {
                leaflet(capa_sp, options = leafletOptions(minZoom = 9, maxZoom = 18)) %>% 
                  addTiles() %>% 
                  setView(lat = mean(coordinates(capa_sp)[,2]), lng=mean(coordinates(capa_sp)[,1]), zoom=11) %>% 
                  addPolygons(weight = 2, fillColor = "#FFFF00", fillOpacity = "0.4", stroke = T, color = "black", opacity = 0.8,
                              highlightOptions = highlightOptions(color = "white", weight = 4, bringToFront = TRUE),
                              popup = paste0("Sección Censal: <b>", paste0(capa_sp@data$CUMUN, "-", capa_sp@data$CDIS, "-", capa_sp@data$CSEC), "</b><br>",
                                             "Población: <b>", capa_sp@data$numPoblacionElegida, "</b><br>",
                                             "Porcentaje de población: <b>", round(capa_sp@data$porcentajePoblacion, digits = 2), "%</b>"),
                              layerId = capa_sp@data$seccionCensal, group = "censussections", label = capa_sp@data$seccionCensal) %>% 
                  addLegend(colors = "#FFFF00",
                            labels = paste0(min(capa_sp@data$porcentajePoblacion,  na.rm = T), "% - ", max(capa_sp@data$porcentajePoblacion,  na.rm = T), "%"),
                            na.label = "Valor no disponible", title = "Población", opacity = "0.4", bins = 2)
              }
              else {
                leaflet(capa_sp, options = leafletOptions(minZoom = 9, maxZoom = 18)) %>% 
                  addTiles() %>% 
                  setView(lat = mean(coordinates(capa_sp)[,2]), lng=mean(coordinates(capa_sp)[,1]), zoom=11) %>% 
                  addPolygons(weight = 2, fillColor = ~pal(porcentajePoblacion), fillOpacity = "0.4", stroke = T, color = "black", opacity = 0.8,
                              highlightOptions = highlightOptions(color = "white", weight = 4, bringToFront = TRUE),
                              popup = paste0("Sección Censal: <b>", paste0(capa_sp@data$CUMUN, "-", capa_sp@data$CDIS, "-", capa_sp@data$CSEC), "</b><br>",
                                             "Población: <b>", capa_sp@data$numPoblacionElegida, "</b><br>",
                                             "Porcentaje de población: <b>", round(capa_sp@data$porcentajePoblacion, digits = 2), "%</b>"),
                              layerId = capa_sp@data$seccionCensal, group = "censussections", label = capa_sp@data$seccionCensal)  %>% 
                  addLegend(colors = c(pal(max), pal((3*max+2*min)/5), pal((2*max+3*min)/5), pal(min)),
                            labels = c(paste0(round((3*max+min)/4, digits = 2), " - <b>", max, "%</b>"),
                                       paste0(round((max+min)/2, digits = 2), " - ", round((3*max+min)/4, digits = 2), "%"),
                                       paste0(round((max+3*min)/4, digits = 2), " - ", round((max+min)/2, digits = 2), "%"),
                                       paste0("<b>", min, "</b> - ", round((max+3*min)/4, digits = 2), "%")),
                            na.label = "Valor no disponible", title = "Población", opacity = "0.4", bins = 2)
              }
              
              
            })
            
          }
          

          

        }
        else{
          
          #################################
          ## Mapa únicamente con valores ##
          #################################
          
          output$mapa <- renderLeaflet({
            municipio <<- sprintf("%05d", municipios$COD_MUN[municipios$NOMBRE %in% input$selectMunicipio])
            capa <- secciones[secciones@data$CUMUN %in% municipio,]
            
            capa@data$seccionCensal <- paste0(capa@data$CUMUN, capa@data$CDIS, capa@data$CSEC)
            capa@data$download <- paste0("download-", capa@data$CUMUN, capa@data$CDIS, capa@data$CSEC)
            #capa@data <- subset(capa@data, select = -c(CDIS, CSEC, CUMUN, CMUN, CPRO, CCA)) # Eliminamos la info extra ya condensada
            
            nacionalidad <- SXnacionalAmbos[which(input$selectNacionalidad == SXnacionalAmbos$nacionalidad), ]
            
            capa@data$numPoblacionElegida <- nacionalidad[match(capa@data$seccionCensal, nacionalidad$sección), "value"]
            
            min <- min(capa@data$numPoblacionElegida, na.rm = T)
            max <- max(capa@data$numPoblacionElegida, na.rm = T)
            pal <- colorQuantile(colorRamp(c("#00FF00", "#FF0000")), domain = min:max)
            
            capa_sp <<- spTransform(capa, CRS("+proj=longlat +datum=WGS84 +no_defs"))
            
            if(max(capa_sp@data$numPoblacionElegida,  na.rm = T) - min(capa_sp@data$numPoblacionElegida,  na.rm = T) == 0) {
              leaflet(capa_sp, options = leafletOptions(minZoom = 9, maxZoom = 18)) %>% 
                addTiles() %>% 
                setView(lat = mean(coordinates(capa_sp)[,2]), lng=mean(coordinates(capa_sp)[,1]), zoom=11) %>% 
                addPolygons(weight = 2, fillColor = "#FFFF00", fillOpacity = "0.4", stroke = T, color = "black", opacity = 0.8,
                            highlightOptions = highlightOptions(color = "white", weight = 4, bringToFront = TRUE),
                            popup = paste0("Sección Censal: <b>", paste0(capa_sp@data$CUMUN, "-", capa_sp@data$CDIS, "-", capa_sp@data$CSEC), "</b><br>",
                                           "Población: <b>", capa_sp@data$numPoblacionElegida, "</b>"),
                            layerId = capa_sp@data$seccionCensal, group = "censussections", label = capa_sp@data$seccionCensal) %>% 
                addLegend(colors = "#FFFF00",
                          labels = paste0(min(capa_sp@data$numPoblacionElegida,  na.rm = T), " - ", max(capa_sp@data$numPoblacionElegida,  na.rm = T)),
                          na.label = "Valor no disponible", title = "Población", opacity = "0.4", bins = 2)
            }
            else {
              leaflet(capa_sp, options = leafletOptions(minZoom = 9, maxZoom = 18)) %>% 
                addTiles() %>% 
                setView(lat = mean(coordinates(capa_sp)[,2]), lng=mean(coordinates(capa_sp)[,1]), zoom=11) %>% 
                addPolygons(weight = 2, fillColor = ~pal(numPoblacionElegida), fillOpacity = "0.4", stroke = T, color = "black", opacity = 0.8,
                            highlightOptions = highlightOptions(color = "white", weight = 4, bringToFront = TRUE),
                            popup = paste0("Sección Censal: <b>", paste0(capa_sp@data$CUMUN, "-", capa_sp@data$CDIS, "-", capa_sp@data$CSEC), "</b><br>",
                                           "Población: <b>", capa_sp@data$numPoblacionElegida, "</b>"),
                            layerId = capa_sp@data$seccionCensal, group = "censussections", label = capa_sp@data$seccionCensal) %>% 
                addLegend(colors = c(pal(max), pal((3*max+2*min)/5), pal((2*max+3*min)/5), pal(min)),
                          labels = c(paste0(round((3*max+min)/4, digits = 2), " - <b>", max, "</b>"),
                                     paste0(round((max+min)/2, digits = 2), " - ", round((3*max+min)/4, digits = 2)),
                                     paste0(round((max+3*min)/4, digits = 2), " - ", round((max+min)/2, digits = 2)),
                                     paste0("<b>", min, "</b> - ", round((max+3*min)/4, digits = 2))),
                          na.label = "Valor no disponible", title = "Población", opacity = "0.4", bins = 2)
            }
            
          })
          
        }
        
        }
    }
  })
  
  observeEvent(input$mapa_shape_click, {
    click <- input$mapa_shape_click
    
    if(is.null(click$id))
      return
    
    proxy <- leafletProxy("mapa")
    clickedIds$ids <- c(clickedIds$ids, click$id)
    clickedPolys <<- capa_sp[capa_sp@data$seccionCensal %in% clickedIds$ids, ]
    
    
    
    if(click$id %in% clickedPolys@data$download){
      
      nameMatch <- clickedPolys@data$seccionCensal[clickedPolys@data$download == click$id]
      clickedIds$ids <- clickedIds$ids[!clickedIds$ids %in% click$id] 
      clickedIds$ids <- clickedIds$ids[!clickedIds$ids %in% nameMatch]
      proxy %>% removeShape(layerId = click$id)
      
      if(length(clickedIds$ids) == 0) {
        disable("descargaKML")
      }
      
    } else {
      
      enable("descargaKML")
      
      proxy %>% addPolygons(data = clickedPolys,
                            fillColor = "blue",
                            fillOpacity = 0.8,
                            weight = 1,
                            color = "black",
                            stroke = T,
                            label = clickedPolys@data$download,
                            group = "censussections",
                            layerId = clickedPolys@data$download)
      
    }
  })
  
  output$descargaKML <- downloadHandler(
    filename = function() {
      paste0(input$selectMunicipio,".kml")
    },
    content = function(file) {
      
      if("porcentajePoblacion" %in% colnames(clickedPolys@data)){
        if("numPoblacionElegidaHombres" %in% colnames(clickedPolys@data)){
          plotKML::kml(obj = clickedPolys, file = file, kmz = F, colour = "green", alpha = 0.5,
                       html.table = paste0("Poblacion - ", clickedPolys@data$numPoblacionElegida, "<br>",
                                           "Porcentaje - ", round(clickedPolys@data$porcentajePoblacion, digits = 2), "%<br>",
                                           "Hombres - ", clickedPolys@data$numPoblacionElegidaHombres, "<br>",
                                           "Mujeres - ", clickedPolys@data$numPoblacionElegidaMujeres),
                       labels = paste0("Seccion Censal ", clickedPolys@data$CDIS, clickedPolys@data$CSEC)) 
        }
        else{
          plotKML::kml(obj = clickedPolys, file = file, kmz = F, colour = "green", alpha = 0.5,
                       html.table = paste0("Poblacion - <b>", clickedPolys@data$numPoblacionElegida, "</b><br>",
                                           "Porcentaje - <b>", round(clickedPolys@data$porcentajePoblacion, digits = 2), "%</b>"),
                       labels = paste0("Seccion Censal ", clickedPolys@data$CDIS, clickedPolys@data$CSEC))  
        }
      }
      else{
        plotKML::kml(obj = clickedPolys, file = file, kmz = F, colour = "green", alpha = 0.5,
                     html.table = paste0("Poblacion - ", clickedPolys@data$numPoblacionElegida),
                     labels = paste0("Seccion Censal ", clickedPolys@data$CDIS, clickedPolys@data$CSEC))
      }
        
      # kmlPolygons(obj = clickedPolys["seccionCensal"], kmlfile = file, name = paste0("Sección Censal ", clickedPolys@data$CDIS, clickedPolys@data$CSEC),
      #             description = clickedPolys@data$numPoblacionElegida, col = "Green", visibility = 0.5, lwd = 0, kmlname = "Polígonos búsqueda")
      
      #writeOGR(clickedPolys, file, layer = paste0("Secciones Censales ", input$selectMunicipio), driver = "KML")
    }
  )
  
})

# http://r-sig-geo.2731867.n2.nabble.com/reading-kmz-file-in-R-td5148622.html KMZ

#murcia <- secciones[secciones@data$CPRO == 30,]

#plot(murcia, col="cyan1", border="black", main="Secciones Censales España")







#
# capa@data <- capa@data[!is.na(capa@data$numPoblacionElegida), ] # Quitar NA's
# 
# capa_dataframe <- fortify(capa)
# capa@data$id <- capa_dataframe$id[1]:(strtoi(capa_dataframe$id[1]) + (dim(capa@data)[1]-1))
# 
# capa_join = plyr::join(x = capa_dataframe, y = capa@data, by="id")
# 
# g <- ggplot(capa_join) +
#   geom_polygon_interactive(color='black', 
#                            aes(long, lat, group=group, fill=numPoblacionElegida, tooltip=sprintf("%s<br/>%s", seccionCensal, numPoblacionElegida))) +
#   hrbrthemes::theme_ipsum() +
#   colormap::scale_fill_colormap(colormap=colormap::colormaps$copper, reverse = T) +
#   labs(title='Nacionalidad por municipio', subtitle='', caption='Fuente: ine.es')
# 
# #frameWidget(ggiraph(code=print(g)))
# 
# ggplotly(g)
# 
# p <- plot_geo(capa_join, locationmode = 'USA-states') %>%
#   add_trace(
#     z = ~total.exports, text = ~hover, locations = ~code,
#     color = ~total.exports, colors = 'Purples'
#  )
#install.packages("rgdal", "raster", "sp")

library(shiny)
library(shinydashboard)
library(rgdal)
library(raster) 
library(sp)
library(pxR)
library(leaflet)
library(plotKML)

#comunidades <- read.csv("datos_csv/codccaa.csv", fileEncoding = "UTF-8")
provincias <- read.csv("datos_csv/codprov.csv", fileEncoding = "UTF-8")
municipios <- read.csv("datos_csv/Municipios_Censo_2011.csv", fileEncoding = "UTF-8")
#secciones <- readOGR(dsn = "cartografia_censo2011_nacional/", layer = "SECC_CPV_E_20111101_01_R_INE")
secciones <- readRDS("cartografia_censo2011_nacional/secciones.rds") # saveRDS(secciones, "cartografia_censo2011_nacional/secciones.RDS")
#secciones_json <- geojson_json(secciones)
#seccionesTransform <- spTransform(seccionesRaw, CRS("+proj=longlat +datum=WGS84"))
#seccionesGoogleMapsz <- fortify(seccionesTransform)

#SXnacionalTodosSexos <- as.data.frame(read.px("scPrincNacionalidades.px"))
#SXnacional <- SXnacionalTodosSexos[which(SXnacionalTodosSexos$sexo == "Ambos Sexos"), ]
SXnacional <- readRDS("SXnacional2019.rds") # saveRDS(SXnacional, "SXnacional2019.rds")

shinyServer(function(input, output, session) {
  
  clickedIds <- reactiveValues(ids = vector())
  
  observeEvent(input$selectProvincia,{
    provincia <- provincias$ID[which(input$selectProvincia == provincias$Nombre)]
    municipiosElegibles <- municipios$NOMBRE[which(provincia == municipios$CPRO)]
    updateSelectizeInput(session = session, "selectMunicipio", choices = municipiosElegibles)
  })
  
  observeEvent(input$calcularSecciones, {
    output$mapa <- renderLeaflet({
      municipio <<- sprintf("%05d", municipios$COD_MUN[which(input$selectMunicipio == municipios$NOMBRE)])
      capa <- secciones[secciones@data$CUMUN == municipio,]
      
      capa@data$seccionCensal <- paste0(capa@data$CUMUN, capa@data$CDIS, capa@data$CSEC)
      capa@data$download <- paste0("download-", capa@data$CUMUN, capa@data$CDIS, capa@data$CSEC)
      #capa@data <- subset(capa@data, select = -c(CDIS, CSEC, CUMUN, CMUN, CPRO, CCA)) # Eliminamos la info extra ya condensada
      
      nacionalidad <- SXnacional[which(input$selectNacionalidad == SXnacional$nacionalidad), ]
      
      capa@data$numPoblacionElegida <- nacionalidad[match(capa@data$seccionCensal, nacionalidad$sección), "value"]
      
      min <- min(capa@data$numPoblacionElegida, na.rm = T)
      max <- max(capa@data$numPoblacionElegida, na.rm = T)
      pal <- colorQuantile(colorRamp(c("#00FF00", "#FF0000")), domain = min:max)
      
      capa_sp <<- spTransform(capa, CRS("+proj=longlat +datum=WGS84 +no_defs"))
      
      if(max(capa_sp@data$numPoblacionElegida,  na.rm = T) - min(capa_sp@data$numPoblacionElegida,  na.rm = T) == 0) {
        leaflet(capa_sp, options = leafletOptions(minZoom = 10, maxZoom = 18)) %>% 
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
        leaflet(capa_sp, options = leafletOptions(minZoom = 10, maxZoom = 18)) %>% 
          addTiles() %>% 
          setView(lat = mean(coordinates(capa_sp)[,2]), lng=mean(coordinates(capa_sp)[,1]), zoom=11) %>% 
          addPolygons(weight = 2, fillColor = ~pal(numPoblacionElegida), fillOpacity = "0.4", stroke = T, color = "black", opacity = 0.8,
                      highlightOptions = highlightOptions(color = "white", weight = 4, bringToFront = TRUE),
                      popup = paste0("Sección Censal: <b>", paste0(capa_sp@data$CUMUN, "-", capa_sp@data$CDIS, "-", capa_sp@data$CSEC), "</b><br>",
                                     "Población: <b>", capa_sp@data$numPoblacionElegida, "</b>"),
                      layerId = capa_sp@data$seccionCensal, group = "censussections", label = capa_sp@data$seccionCensal) %>% 
          addLegend(colors = c(pal(max), pal((3*max+2*min)/5), pal((2*max+3*min)/5), pal(min)),
                    labels = c(paste0(round((2*max+min)/3, digits = 2), " - <b>", max, "</b>"),
                               paste0(round((max+min)/2, digits = 2), " - ", round((2*max+min)/3, digits = 2)),
                               paste0(round((max+2*min)/3, digits = 2), " - ", round((max+min)/2, digits = 2)),
                               paste0("<b>", min, "</b> - ", round((max+2*min)/3, digits = 2))),
                    na.label = "Valor no disponible", title = "Población", opacity = "0.4", bins = 2)
      }
      
      #pal=pal, values=~numPoblacionElegida
    })
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
      
    } else {
      
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
      # kmlPolygons(obj = clickedPolys["seccionCensal"], kmlfile = file, name = paste0("Sección Censal ", clickedPolys@data$CDIS, clickedPolys@data$CSEC),
      #             description = clickedPolys@data$numPoblacionElegida, col = "Green", visibility = 0.5, lwd = 0, kmlname = "Polígonos búsqueda")
      plotKML::kml(obj = clickedPolys, file = file, kmz = F)
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
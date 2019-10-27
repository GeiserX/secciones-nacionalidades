shinyServer(function(input, output, session) {

  output$mention <- renderText({
    paste0("<hr>Cartography extracted from <a ",
    "href=http://www.ine.es/ss/Satellite?L=es_ES&c=Page&cid=1259952026632&p=1259952026632&pagename=ProductosYServicios%2FPYSLayout target=_blank>",
    "data publicly available at the Spanish \'Instituto Nacional de Estadística\'</a><hr> Population data extracted thanks to ",
    "<a href=https://www.ine.es/dyngs/INEbase/es/operacion.htm?c=Estadistica_C&cid=1254736177012&menu=resultados&secc=1254736195461&idp=1254734710990 target=_blank>",
    "the publicly available information at the Spanish \'Instituto Nacional de Estadística\'</a><hr>",
    "Source code available at <a href=https://github.com/DrumSergio/secciones-nacionalidades target=_blank>GitHub</a><br>",
    "Docker container avaialble at <a href=https://hub.docker.com/r/drumsergio/secciones-nacionalidadess target=_blank>DockerHub</a>")
  })
  
  clickedIds <- reactiveValues(ids = vector())
  nationalitySaved <- reactiveValues(ids = vector())
  
  observeEvent(input$selectMunicipio,{
    disable("descargaKML")
  })
    
  
  observeEvent(input$selectProvincia,{
    provincia <- provincias$ID[which(input$selectProvincia == provincias$Nombre)]
    municipiosElegibles <<- municipios$NOMBRE[which(provincia == municipios$CPRO)]
    updateSelectizeInput(session = session, "selectMunicipio", choices = municipiosElegibles)
  })
  
  observeEvent(input$porcentaje, {
    if(input$porcentaje == T){
      if(nationalitySaved$n != "Total Población")
        updateSelectizeInput(session, "selectNacionalidad", choices = levels(SXnacionalAmbos$nacionalidad)[-1], selected = nationalitySaved$n)
      else
        updateSelectizeInput(session, "selectNacionalidad", choices = levels(SXnacionalAmbos$nacionalidad)[-1])
    }
    else{
      
      updateSelectizeInput(session, "selectNacionalidad", choices = levels(SXnacionalAmbos$nacionalidad), selected = nationalitySaved$n)
    }
  })
  
  observe({
    nationalitySaved$n <- input$selectNacionalidad
    if(!is.null(input$selectMunicipio)){
      #if(input$selectMunicipio %in% municipiosElegibles){
        
        output$mapa <- simplyMapIt(porcentaje = input$porcentaje, hombreMujer = input$hombreMujer, municipioSelected = input$selectMunicipio,
                                   nacionalidadSelected = input$selectNacionalidad, SXnacionalAmbos = SXnacionalAmbos, SXnacionalHombres = SXnacionalHombres,
                                   SXnacionalMujeres = SXnacionalMujeres)
       # }
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
        if("numPoblacionElegidaHombres" %in% colnames(clickedPolys@data)){
          plotKML::kml(obj = clickedPolys, file = file, kmz = F, colour = "green", alpha = 0.5,
                       html.table = paste0("Poblacion - ", clickedPolys@data$numPoblacionElegida, "<br>",
                                           "Hombres - ", clickedPolys@data$numPoblacionElegidaHombres, "<br>",
                                           "Mujeres - ", clickedPolys@data$numPoblacionElegidaMujeres),
                       labels = paste0("Seccion Censal ", clickedPolys@data$CDIS, clickedPolys@data$CSEC)) 
        }
        else{
          plotKML::kml(obj = clickedPolys, file = file, kmz = F, colour = "green", alpha = 0.5,
                       html.table = paste0("Poblacion - ", clickedPolys@data$numPoblacionElegida),
                       labels = paste0("Seccion Censal ", clickedPolys@data$CDIS, clickedPolys@data$CSEC)) 
        }
      }
    }
  )
  
  
  #############
  ### TAB 2 ###
  #############

  observe({
    provincia2 <<- as.numeric(provincias$ID[which(input$selectProvincia2 == provincias$Nombre)])
    municipiosElegibles2 <<- municipios$NOMBRE[which(provincia2 == municipios$CPRO)]
    
    output$chart <- renderHighchart({
      
      nacionalidad <- SXnacionalAmbos[which(input$selectNacionalidad2 == SXnacionalAmbos$nacionalidad), ]
      
      capa <- secciones[secciones@data$CPRO %in% sprintf("%02d", provincia2),]
      capa@data$seccionCensal <- paste0(capa@data$CUMUN, capa@data$CDIS, capa@data$CSEC)
      capa@data$numPoblacionElegida <- nacionalidad[match(capa@data$seccionCensal, nacionalidad$sección), "value"]
      
      if(input$manWoman){
        nacionalidadHombres <- SXnacionalHombres[which(input$selectNacionalidad2 == SXnacionalHombres$nacionalidad), ]
        nacionalidadMujeres <- SXnacionalMujeres[which(input$selectNacionalidad2 == SXnacionalMujeres$nacionalidad), ]

        capa@data$hombres <- nacionalidadHombres[match(capa@data$seccionCensal, nacionalidadHombres$sección), "value"]
        capa@data$mujeres <- nacionalidadMujeres[match(capa@data$seccionCensal, nacionalidadMujeres$sección), "value"]
        
        datos_agregadosH <- aggregate(capa@data$hombres, by = list(Municipio=capa@data$NMUN), FUN = sum)
        datos_agregadosM <- aggregate(capa@data$mujeres, by = list(Municipio=capa@data$NMUN), FUN = sum)
      }
      
      datos_agregados <- aggregate(capa@data$numPoblacionElegida, by = list(Municipio=capa@data$NMUN), FUN = sum)      
      if(input$sort) {
        datos_agregados <- datos_agregados[order(datos_agregados$x, decreasing = T),]
        if(input$manWoman){
          datos_agregadosH <- datos_agregadosH[order(datos_agregadosH$x, decreasing = T),]
          datos_agregadosM <- datos_agregadosM[order(datos_agregadosM$x, decreasing = T),]
        }
      }
      
      if(input$manWoman){
        highchart() %>% 
          hc_chart(type = "column", zoomType = "x") %>% 
          hc_title(text = "Population") %>% 
          hc_xAxis(categories = datos_agregados$Municipio) %>% 
          hc_add_series(data = datos_agregados$x, name = "Total Population") %>% 
          hc_add_series(data = datos_agregadosH$x, name = "Men") %>% 
          hc_add_series(data = datos_agregadosM$x, name = "Women")
      } else {
        highchart() %>% 
          hc_chart(type = "column", zoomType = "x") %>% 
          hc_title(text = "Population") %>% 
          hc_xAxis(categories = datos_agregados$Municipio) %>% 
          hc_add_series(data = datos_agregados$x, name = "Total Population")
      }
    })
  })
  

    
})
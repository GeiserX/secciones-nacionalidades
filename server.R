shinyServer(function(input, output, session) {
  
  observeEvent(input$selectYear,{
    year <- input$selectYear
    
    # SXnacional <- as.data.frame(read.px(paste0("poblacion/", year, "/0003.px")))
    # 
    # SXnacionalAmbos <- SXnacional[which(SXnacional$sexo == "Ambos Sexos"), ]
    # SXnacionalHombres <- SXnacional[which(SXnacional$sexo == "Hombres"), ]
    # SXnacionalMujeres <- SXnacional[which(SXnacional$sexo == "Mujeres"), ]
    # 
    # saveRDS(SXnacionalAmbos, paste0("poblacion/", year, "/SXnacional", year, "ambos.rds"))
    # saveRDS(SXnacionalHombres, paste0("poblacion/", year, "/SXnacional", year, "hombres.rds"))
    # saveRDS(SXnacionalMujeres, paste0("poblacion/", year, "/SXnacional", year, "mujeres.rds"))
    
    SXnacionalAmbos <<- readRDS(paste0("poblacion/", year, "/SXnacional", year, "ambos.rds"))
    SXnacionalHombres <<- readRDS(paste0("poblacion/", year, "/SXnacional", year, "hombres.rds"))
    SXnacionalMujeres <<- readRDS(paste0("poblacion/", year, "/SXnacional", year, "mujeres.rds"))
    
    # secciones <- readOGR(dsn = paste0("seccionado/", year, "/"), layer = paste0("SECC_CE_", year, "0101"))
    # secciones <- readOGR(dsn = paste0("seccionado/", year, "/"), layer = paste0("SECC_CE_", year, "0101_01_R_INE"))
    # secciones <- readOGR(dsn = paste0("seccionado/", year, "/"), layer = paste0("SECC_CE_", year, "0101_00_R_INE"))
    # saveRDS(secciones, paste0("seccionado/", year, "/secciones.rds")) 
    # git lfs track ..(FILE)..
    secciones <<- readRDS(paste0("seccionado/", year, "/secciones.rds"))
    
    # es_map <- download_map_data(url = "countries/es/es-all", showinfo = F)
    # saveRDS(es_map, "es-all.rds")
    es_map <<- readRDS("es-all.rds")
  })

  # output$mention <- renderText({
  #   paste0("<hr>Cartography extracted from <a ",
  #   "href=http://www.ine.es/ss/Satellite?L=es_ES&c=Page&cid=1259952026632&p=1259952026632&pagename=ProductosYServicios%2FPYSLayout target=_blank>",
  #   "data publicly available at the Spanish \'Instituto Nacional de Estadística\'</a><hr> Population data extracted thanks to ",
  #   "<a href=https://www.ine.es/dyngs/INEbase/es/operacion.htm?c=Estadistica_C&cid=1254736177012&menu=resultados&secc=1254736195461&idp=1254734710990 target=_blank>",
  #   "the publicly available information at the Spanish \'Instituto Nacional de Estadística\'</a><hr>",
  #   "Source code available at <a href=https://github.com/DrumSergio/secciones-nacionalidades target=_blank>GitHub</a>, so if you find any errors, please, kindly open an issue there.<br>",
  #   "Docker container avaialble at <a href=https://hub.docker.com/r/drumsergio/secciones-nacionalidades target=_blank>DockerHub</a>.")
  # })
  
  clickedIds <- reactiveValues(ids = vector())
  nationalitySaved <- reactiveValues(ids = vector())
  
  observeEvent(input$selectMunicipio,{
    disable("descargaKML")
    nationalitySaved$n <- input$selectNacionalidad
    if(!is.null(input$selectMunicipio)){
      año <- input$selectYear
      output$mapa <- simplyMapIt(porcentaje = input$porcentaje, hombreMujer = input$hombreMujer, municipioSelected = input$selectMunicipio,
                                 nacionalidadSelected = input$selectNacionalidad, SXnacionalAmbos = SXnacionalAmbos, SXnacionalHombres = SXnacionalHombres,
                                 SXnacionalMujeres = SXnacionalMujeres, Year = año)
    }
  })
    
  
  observeEvent(input$selectProvincia,{
    provincia <- provincias$ID[which(input$selectProvincia == provincias$Nombre)]
    municipiosElegibles <<- municipios$NOMBRE[which(provincia == municipios$CPRO)]
    updateSelectInput(session = session, "selectMunicipio", choices = municipiosElegibles)
  })
  
  observeEvent(input$porcentaje, {
    
    nationalitySaved$n <- input$selectNacionalidad
    
    if(input$porcentaje == T){
      if(input$selectNacionalidad == "Total Población")
        updateSelectizeInput(session, "selectNacionalidad", choices = levels(SXnacionalAmbos$nacionalidad)[-1])
      else
        updateSelectizeInput(session, "selectNacionalidad", choices = levels(SXnacionalAmbos$nacionalidad)[-1], selected = nationalitySaved$n)
    } else {
      updateSelectizeInput(session, "selectNacionalidad", choices = levels(SXnacionalAmbos$nacionalidad), selected = nationalitySaved$n)
    }
    
    if(!is.null(input$selectMunicipio)){
      año <- input$selectYear
      output$mapa <- simplyMapIt(porcentaje = input$porcentaje, hombreMujer = input$hombreMujer, municipioSelected = input$selectMunicipio,
                                 nacionalidadSelected = ifelse(input$porcentaje == T && input$selectNacionalidad == "Total Población",
                                                               "Españoles", input$selectNacionalidad), 
                                 SXnacionalAmbos = SXnacionalAmbos, SXnacionalHombres = SXnacionalHombres,
                                 SXnacionalMujeres = SXnacionalMujeres, Year = año)
    }
    
  })
  
  observeEvent(input$hombreMujer,{
    nationalitySaved$n <- input$selectNacionalidad
    if(!is.null(input$selectMunicipio)){
      año <- input$selectYear
      output$mapa <- simplyMapIt(porcentaje = input$porcentaje, hombreMujer = input$hombreMujer, municipioSelected = input$selectMunicipio,
                               nacionalidadSelected = input$selectNacionalidad, SXnacionalAmbos = SXnacionalAmbos, SXnacionalHombres = SXnacionalHombres,
                               SXnacionalMujeres = SXnacionalMujeres, Year = año)
    }
  })
  
  # observe({
  #   nationalitySaved$n <- input$selectNacionalidad
  #   if(!is.null(input$selectMunicipio)){
  #     año <- input$selectYear
  #     output$mapa <- simplyMapIt(porcentaje = input$porcentaje, hombreMujer = input$hombreMujer, municipioSelected = input$selectMunicipio,
  #                                  nacionalidadSelected = input$selectNacionalidad, SXnacionalAmbos = SXnacionalAmbos, SXnacionalHombres = SXnacionalHombres,
  #                                  SXnacionalMujeres = SXnacionalMujeres, Year = año)
  #   }
  # })
  # 
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
      
      year <- input$selectYear
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
      
      if(input$percentage2){
        nacionalidad <- SXnacionalAmbos[which("Total Población" == SXnacionalAmbos$nacionalidad), ]
        capa@data$numPoblacionTOTAL <- nacionalidad[match(capa@data$seccionCensal, nacionalidad$sección), "value"]
        datos_agregadosT <- aggregate(capa@data$numPoblacionTOTAL, by = list(Municipio=capa@data$NMUN), FUN = sum)
        datos_agregadosTP <- datos_agregadosT
        datos_agregadosTP$x <- round(100 * as.numeric(datos_agregados$x) / as.numeric(datos_agregadosT$x), digits = 2)
      }
      
      
      
      if(input$sort) {
        if(input$manWoman){
          datos_agregadosH <- datos_agregadosH[order(datos_agregados$x, decreasing = T),]
          datos_agregadosM <- datos_agregadosM[order(datos_agregados$x, decreasing = T),]
        }
        if(input$percentage2){
          datos_agregadosTP <- datos_agregadosTP[order(datos_agregados$x, decreasing = T),]
        }
        datos_agregados <- datos_agregados[order(datos_agregados$x, decreasing = T),]
      }
      
      if(input$manWoman){
        if(input$percentage2){
          highchart() %>% 
            hc_chart(type = "column", zoomType = "x") %>% 
            hc_title(text = paste0("Population ", year)) %>% 
            hc_xAxis(categories = datos_agregados$Municipio) %>% 
            hc_yAxis_multiples(list(title = list(text = paste0("Population ", year))), list(opposite = TRUE, title = list(text = "Percentage"))) %>% 
            hc_add_series(data = datos_agregados$x, name = paste0("Total Population ", year)) %>% 
            hc_add_series(data = datos_agregadosH$x, name = "Men") %>% 
            hc_add_series(data = datos_agregadosM$x, name = "Women") %>%
            hc_add_series(data = datos_agregadosTP$x, name = "Percentage", yAxis = 1)
        } else {
          highchart() %>% 
            hc_chart(type = "column", zoomType = "x") %>% 
            hc_title(text = paste0("Population ", year)) %>% 
            hc_xAxis(categories = datos_agregados$Municipio) %>% 
            hc_add_series(data = datos_agregados$x, name = paste0("Total Population ", year)) %>% 
            hc_add_series(data = datos_agregadosH$x, name = "Men") %>% 
            hc_add_series(data = datos_agregadosM$x, name = "Women")
        }
      } else {
        if(input$percentage2){
          highchart() %>%
            hc_chart(type = "column", zoomType = "x") %>% 
            hc_title(text = paste0("Population ", year)) %>% 
            hc_xAxis(categories = datos_agregados$Municipio) %>% 
            hc_yAxis_multiples(list(title = list(text = paste0("Population ", year))), list(opposite = TRUE, title = list(text = "Percentage"))) %>% 
            hc_add_series(data = datos_agregados$x, name = paste0("Total Population ", year)) %>% 
            hc_add_series(data = datos_agregadosTP$x, name = "Percentage", yAxis = 1)
        } else {
          highchart() %>% 
            hc_chart(type = "column", zoomType = "x") %>% 
            hc_title(text = paste0("Population ", year)) %>% 
            hc_xAxis(categories = datos_agregados$Municipio) %>% 
            hc_add_series(data = datos_agregados$x, name = paste0("Total Population ", year)) 
        }
      }
    })
  })
  
  #############
  ### TAB 3 ###
  #############
  
  observeEvent(input$selectNacionalidad3,{
    output$spainmap <- renderHighchart({
      year <- input$selectYear
      nacionalidad <- SXnacionalAmbos[which(input$selectNacionalidad3 == SXnacionalAmbos$nacionalidad), ]
      total <- nacionalidad$value[nacionalidad$sección == "TOTAL"]
      secciones@data$seccionCensal <- paste0(secciones@data$CUMUN, secciones@data$CDIS, secciones@data$CSEC)
      secciones@data$poblacion <- nacionalidad[match(secciones@data$seccionCensal, nacionalidad$sección), "value"]
      
      datos_agregados <- aggregate(secciones@data$poblacion, by = list(Provincia=secciones@data$CPRO), FUN = sum)     
      
      codes <- sprintf('%02d', seq(1,52))
      names <- c('es-vi', 'es-ab', 'es-a', 'es-al', 'es-av', 'es-ba', 'es-pm', 'es-b', 'es-bu', 'es-cc',
                 'es-ca', 'es-cs', 'es-cr', 'es-co', 'es-c', 'es-cu', 'es-gi', 'es-gr', 'es-gu', 'es-ss',
                 'es-h', 'es-hu', 'es-j', 'es-le', 'es-l', 'es-lo', 'es-lu', 'es-m', 'es-ma', 'es-mu',
                 'es-na', 'es-or', 'es-o', 'es-p', 'es-gc', 'es-po', 'es-sa', 'es-tf', 'es-s', 'es-sg',
                 'es-se', 'es-so', 'es-t', 'es-te', 'es-to', 'es-v', 'es-va', 'es-bi', 'es-za', 'es-z',
                 'es-ce', 'es-me')
      codenames <- data.frame(codes, names)
      datos_agregados$Provincia <- codenames$names[match(datos_agregados$Provincia, codenames$codes)]
      colnames(datos_agregados) <- c("Province", "Value")
      
      highchart(type = "map") %>% 
        hc_add_series_map(map = es_map, df = datos_agregados, joinBy = c("hc-key", "Province"),
                          value = "Value", name = paste0("Population ", year)) %>%
        hc_subtitle(text = paste0("Population: ", suppressWarnings(format(total, big.mark = "."))))
      # hcmap(map = es_map, data = datos_agregados, joinBy = c("hc-key", "Province"), download_map_data = F,
      #       value = "Value", name = paste0("Population ", year))
    })
  })
  
  #############
  ### TAB 4 ###
  #############
  
  observeEvent(input$selectNacionalidad4,{
    output$historicChart <- renderHighchart({
      nacionalidad <- list()
      total <- data.frame(years = list.files("poblacion/"))
      for(i in 1:length(list.files("poblacion/"))){
        nacionalidad <-  append(nacionalidad, list(poblacionAñoAmbos[[i]][which(input$selectNacionalidad4 == poblacionAñoAmbos[[i]]$nacionalidad), ]))
        total$population[i] <- nacionalidad[[i]]$value[nacionalidad[[i]]$sección == "TOTAL"]
      }
      
      highchart() %>% 
        hc_chart(type = "line", zoomType = "x") %>% 
        hc_title(text = "Population trend") %>% 
        hc_xAxis(categories = total$years) %>% 
        hc_add_series(data = total$population, name = "Total population")
    })
  })
  
  output$provinces <- renderUI({
    if(input$metricsByAggregate != "National"){
      selectizeInput("chartProvincia", label = "Select Province", choices = provincias$Nombre, multiple = T, selected = c())
    }
  })
  
  observeEvent(input$metricsByAggregate,{
    if(input$metricsByAggregate == "Province"){
      output$historicChart <- renderHighchart({
        nacionalidad <- list()
        total <- data.frame(years = list.files("poblacion/"))
        for(i in 1:length(list.files("poblacion/"))){
          nacionalidad <-  append(nacionalidad, list(poblacionAñoAmbos[[i]][which(input$selectNacionalidad4 == poblacionAñoAmbos[[i]]$nacionalidad), ]))
          total$population[i] <- nacionalidad[[i]]$value[nacionalidad[[i]]$sección == "TOTAL"]
        }
        
        highchart() %>% 
          hc_chart(type = "line", zoomType = "x") %>% 
          hc_title(text = "Population trend") %>% 
          hc_xAxis(categories = total$years) %>% 
          hc_add_series(data = total$population, name = "Total population")
      })
    }
  })
    
})
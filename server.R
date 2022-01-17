shinyServer(function(input, output, session) {
  
  # observeEvent(input$NationalityOrBirth, {
  #   if(input$NationalityOrBirth == 1){
  #     Nacionalidad_Ambos <<- readRDS(paste0("poblacion/", year, "/Nacionalidad", year, "ambos.rds"))
  #     Nacionalidad_Hombres <<- readRDS(paste0("poblacion/", year, "/Nacionalidad", year, "hombres.rds"))
  #     Nacionalidad_Mujeres <<- readRDS(paste0("poblacion/", year, "/Nacionalidad", year, "mujeres.rds"))
  #   } else {
  #     Nacionalidad_Ambos <<- readRDS(paste0("poblacion/", year, "/Nacimiento", year, "ambos.rds"))
  #     colnames(Nacionalidad_Ambos)[colnames(Nacionalidad_Ambos) == "país.de.nacimiento"] <- "nacionalidad" 
  #     Nacionalidad_Hombres <<- readRDS(paste0("poblacion/", year, "/Nacimiento", year, "hombres.rds"))
  #     colnames(Nacionalidad_Hombres)[colnames(Nacionalidad_Ambos) == "país.de.nacimiento"] <- "nacionalidad" 
  #     Nacionalidad_Mujeres <<- readRDS(paste0("poblacion/", year, "/Nacimiento", year, "mujeres.rds"))
  #     colnames(Nacionalidad_Mujeres)[colnames(Nacionalidad_Ambos) == "país.de.nacimiento"] <- "nacionalidad" 
  #   }
  # })
  
  observeEvent(input$selectYear,{
    year <- input$selectYear
    
    # for(year in 2012:2021){
      # Nacionalidad <- as.data.frame(read.px(paste0("poblacion/", year, "/0003.px")))
      # Nacionalidad_Ambos <- Nacionalidad[which(Nacionalidad$sexo == "Ambos Sexos"), ]
      # Nacionalidad_Hombres <- Nacionalidad[which(Nacionalidad$sexo == "Hombres"), ]
      # Nacionalidad_Mujeres <- Nacionalidad[which(Nacionalidad$sexo == "Mujeres"), ]
      # saveRDS(Nacionalidad_Ambos, paste0("poblacion/", year, "/Nacionalidad", year, "ambos.rds"))
      # saveRDS(Nacionalidad_Hombres, paste0("poblacion/", year, "/Nacionalidad", year, "hombres.rds"))
      # saveRDS(Nacionalidad_Mujeres, paste0("poblacion/", year, "/Nacionalidad", year, "mujeres.rds"))
    #   
    #   Nacimiento <- as.data.frame(read.px(paste0("poblacion/", year, "/0006.px")))
    #   Nacimiento_Ambos <- Nacimiento[which(Nacimiento$sexo == "Ambos Sexos"), ]
    #   Nacimiento_Hombres <- Nacimiento[which(Nacimiento$sexo == "Hombres"), ]
    #   Nacimiento_Mujeres <- Nacimiento[which(Nacimiento$sexo == "Mujeres"), ]
    #   saveRDS(Nacimiento_Ambos, paste0("poblacion/", year, "/Nacimiento", year, "ambos.rds"))
    #   saveRDS(Nacimiento_Hombres, paste0("poblacion/", year, "/Nacimiento", year, "hombres.rds"))
    #   saveRDS(Nacimiento_Mujeres, paste0("poblacion/", year, "/Nacimiento", year, "mujeres.rds"))
    # }

    # if(input$NationalityOrBirth == 1){
      Nacionalidad_Ambos <<- readRDS(paste0("poblacion/", year, "/Nacionalidad", year, "ambos.rds"))
      Nacionalidad_Hombres <<- readRDS(paste0("poblacion/", year, "/Nacionalidad", year, "hombres.rds"))
      Nacionalidad_Mujeres <<- readRDS(paste0("poblacion/", year, "/Nacionalidad", year, "mujeres.rds"))
    # } else {
      # Nacionalidad_Ambos <<- readRDS(paste0("poblacion/", year, "/Nacimiento", year, "ambos.rds"))
      # Nacionalidad_Hombres <<- readRDS(paste0("poblacion/", year, "/Nacimiento", year, "hombres.rds"))
      # Nacionalidad_Mujeres <<- readRDS(paste0("poblacion/", year, "/Nacimiento", year, "mujeres.rds"))
    # }

    # secciones <- readOGR(paste0("seccionado/", year, "/SECC_CE_", year, "0101.shp"))
    ## secciones <- readOGR(dsn = paste0("seccionado/", year, "/"), layer = paste0("SECC_CE_", year, "0101_01_R_INE"))
    ## secciones <- readOGR(dsn = paste0("seccionado/", year, "/"), layer = paste0("SECC_CE_", year, "0101_00_R_INE"))
    # saveRDS(secciones, paste0("seccionado/", year, "/secciones.rds"))
    ## git lfs track ..(FILE)..
    secciones <<- readRDS(paste0("seccionado/", year, "/secciones.rds"))
    
    # es_map <- download_map_data(url = "countries/es/es-all", showinfo = F)
    # saveRDS(es_map, "es-all.rds")
    es_map <<- readRDS("es-all.rds")
  })
  
  clickedIds <- reactiveValues(ids = vector())
  nationalitySaved <- reactiveValues(ids = vector())
  
  observeEvent(input$selectMunicipio,{
    disable("descargaKML")
    nationalitySaved$n <- input$selectNacionalidad
    if(!is.null(input$selectMunicipio)){
      año <- input$selectYear
      output$mapa <- simplyMapIt(porcentaje = input$porcentaje, hombreMujer = input$hombreMujer, municipioSelected = input$selectMunicipio,
                                 nacionalidadSelected = input$selectNacionalidad, Nacionalidad_Ambos = Nacionalidad_Ambos, Nacionalidad_Hombres = Nacionalidad_Hombres,
                                 Nacionalidad_Mujeres = Nacionalidad_Mujeres, Year = año)
    }
  })
  
  observeEvent(input$selectProvincia,{
    provincia <- provincias$ID[which(input$selectProvincia == provincias$Nombre)]
    municipiosElegibles <<- municipios$NOMBRE[which(provincia == municipios$CPRO)]
    updateSelectInput(session = session, "selectMunicipio", choices = municipiosElegibles)
  })
  
  observeEvent(input$selectNacionalidad,{
    if(!is.null(input$selectMunicipio)){
      año <- input$selectYear
      output$mapa <- simplyMapIt(porcentaje = input$porcentaje, hombreMujer = input$hombreMujer, municipioSelected = input$selectMunicipio,
                               nacionalidadSelected = input$selectNacionalidad, Nacionalidad_Ambos = Nacionalidad_Ambos, Nacionalidad_Hombres = Nacionalidad_Hombres,
                               Nacionalidad_Mujeres = Nacionalidad_Mujeres, Year = año)
    }
  })
  
  observeEvent(input$porcentaje, {
    
    nationalitySaved$n <- input$selectNacionalidad
    
    if(input$porcentaje == T){
      if(input$selectNacionalidad == "Total Población")
        updateSelectizeInput(session, "selectNacionalidad", choices = levels(Nacionalidad_Ambos$nacionalidad)[-1])
      else
        updateSelectizeInput(session, "selectNacionalidad", choices = levels(Nacionalidad_Ambos$nacionalidad)[-1], selected = nationalitySaved$n)
    } else {
      updateSelectizeInput(session, "selectNacionalidad", choices = levels(Nacionalidad_Ambos$nacionalidad), selected = nationalitySaved$n)
    }
    
    if(!is.null(input$selectMunicipio)){
      año <- input$selectYear
      output$mapa <- simplyMapIt(porcentaje = input$porcentaje, hombreMujer = input$hombreMujer, municipioSelected = input$selectMunicipio,
                                 nacionalidadSelected = ifelse(input$porcentaje == T && input$selectNacionalidad == "Total Población",
                                                               "Españoles", input$selectNacionalidad), 
                                 Nacionalidad_Ambos = Nacionalidad_Ambos, Nacionalidad_Hombres = Nacionalidad_Hombres,
                                 Nacionalidad_Mujeres = Nacionalidad_Mujeres, Year = año)
    }
    
  })
  
  observeEvent(input$hombreMujer,{
    nationalitySaved$n <- input$selectNacionalidad
    if(!is.null(input$selectMunicipio)){
      año <- input$selectYear
      output$mapa <- simplyMapIt(porcentaje = input$porcentaje, hombreMujer = input$hombreMujer, municipioSelected = input$selectMunicipio,
                               nacionalidadSelected = input$selectNacionalidad, Nacionalidad_Ambos = Nacionalidad_Ambos, Nacionalidad_Hombres = Nacionalidad_Hombres,
                               Nacionalidad_Mujeres = Nacionalidad_Mujeres, Year = año)
    }
  })
  
  # observe({
  #   nationalitySaved$n <- input$selectNacionalidad
  #   if(!is.null(input$selectMunicipio)){
  #     año <- input$selectYear
  #     output$mapa <- simplyMapIt(porcentaje = input$porcentaje, hombreMujer = input$hombreMujer, municipioSelected = input$selectMunicipio,
  #                                  nacionalidadSelected = input$selectNacionalidad, Nacionalidad_Ambos = Nacionalidad_Ambos, Nacionalidad_Hombres = Nacionalidad_Hombres,
  #                                  Nacionalidad_Mujeres = Nacionalidad_Mujeres, Year = año)
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
      nacionalidad <- Nacionalidad_Ambos[which(input$selectNacionalidad2 == Nacionalidad_Ambos$nacionalidad), ]
      
      capa <- secciones[secciones@data$CPRO %in% sprintf("%02d", provincia2),]
      capa@data$seccionCensal <- paste0(capa@data$CUMUN, capa@data$CDIS, capa@data$CSEC)
      capa@data$numPoblacionElegida <- nacionalidad[match(capa@data$seccionCensal, nacionalidad$sección), "value"]
      
      if(input$manWoman){
        nacionalidadHombres <- Nacionalidad_Hombres[which(input$selectNacionalidad2 == Nacionalidad_Hombres$nacionalidad), ]
        nacionalidadMujeres <- Nacionalidad_Mujeres[which(input$selectNacionalidad2 == Nacionalidad_Mujeres$nacionalidad), ]

        capa@data$hombres <- nacionalidadHombres[match(capa@data$seccionCensal, nacionalidadHombres$sección), "value"]
        capa@data$mujeres <- nacionalidadMujeres[match(capa@data$seccionCensal, nacionalidadMujeres$sección), "value"]
        
        datos_agregadosH <- aggregate(capa@data$hombres, by = list(Municipio=capa@data$NMUN), FUN = sum)
        datos_agregadosM <- aggregate(capa@data$mujeres, by = list(Municipio=capa@data$NMUN), FUN = sum)
      }
      
      datos_agregados <- aggregate(capa@data$numPoblacionElegida, by = list(Municipio=capa@data$NMUN), FUN = sum)      
      
      if(input$percentage2){
        nacionalidad <- Nacionalidad_Ambos[which("Total Población" == Nacionalidad_Ambos$nacionalidad), ]
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
      yearSelected <- input$selectYear
      
      nacionalidad <- Nacionalidad_Ambos[which(input$selectNacionalidad3 == Nacionalidad_Ambos$nacionalidad), ]
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
                          value = "Value", name = paste0("Population ", "")) %>%
        hc_subtitle(text = paste0("Population: ", suppressWarnings(format(total, big.mark = "."))))
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
      selectizeInput("selectProvincia4", label = "Select Province", choices = provincias$Nombre, multiple = T, selected = c("Murcia"))
    }
  })
  
  observe({
    if(input$metricsByAggregate == "Province"){
      if(!is.null(input$selectProvincia4)){
        
        output$historicChart <- renderHighchart({
          
          nacionalidad <- list()
          datosAgregados <- list()
          #total <- list(data.frame(years = list.files("poblacion/")))
          years <- list.files("poblacion/")
          
          for(i in 1:length(years)){
            nacionalidad <-  append(nacionalidad, list(poblacionAñoAmbos[[i]][which(input$selectNacionalidad4 == poblacionAñoAmbos[[i]]$nacionalidad), ]))
            #total$population[i] <- nacionalidad[[i]]$value[nacionalidad[[i]]$sección == "TOTAL"]
            seccionadoAño[[i]]@data <- seccionadoAño[[i]]@data[seccionadoAño[[i]]@data$NPRO %in% input$selectProvincia4, ]
            seccionadoAño[[i]]@data$seccionCensal <- paste0(seccionadoAño[[i]]@data$CUMUN, seccionadoAño[[i]]@data$CDIS, seccionadoAño[[i]]@data$CSEC)
            seccionadoAño[[i]]@data$poblacion <- nacionalidad[[i]][match(seccionadoAño[[i]]@data$seccionCensal, nacionalidad[[i]]$sección), "value"]
            datosAgregados <- append(datosAgregados, list(cbind(aggregate(seccionadoAño[[i]]@data$poblacion, by = list(Provincia=seccionadoAño[[i]]@data$NPRO), FUN = sum, na.rm = T), years[i])))
          }
         
          result <- do.call(rbind, datosAgregados) %>% 
            group_by(Provincia) %>% 
            summarise(years, x)
          
          hc <- highchart() %>% 
            hc_chart(type = "line", zoomType = "x") %>% 
            hc_title(text = "Population trend") %>% 
            hc_xAxis(categories = years)
          
          for(i in input$selectProvincia4){
            hc <- hc %>% 
            hc_add_series(result$x[which(result$Provincia==i)], name=i)
          }
          hc
        })
      }
    }
  })
    
})
shinyServer(function(input, output, session) {
  
  observe({
    if(input$porcentaje == T){
      updateSelectizeInput(session, "selectNacionalidad", choices = levels(SXnacionalAmbos$nacionalidad)[-1])
    }
    else{
      updateSelectizeInput(session, "selectNacionalidad", choices = levels(SXnacionalAmbos$nacionalidad))
    }
  })
  
  output$mention <- renderText({
    paste0("<hr>Cartografía obtenida de <a ",
    "href=http://www.ine.es/ss/Satellite?L=es_ES&c=Page&cid=1259952026632&p=1259952026632&pagename=ProductosYServicios%2FPYSLayout target=_blank>",
    "los datos disponibles públicamente en la web del Instituto Nacional de Estadística</a><hr> Datos sobre población obtenida gracias a la ",
    "<a href=https://www.ine.es/dyngs/INEbase/es/operacion.htm?c=Estadistica_C&cid=1254736177012&menu=resultados&secc=1254736195461&idp=1254734710990 target=_blank>",
    "información disponible públicamente en la web del Istituto Nacional de Estadística</a><hr>",
    "Código fuente disponible en<a href=https://github.com/DrumSergio/secciones-nacionalidades target=_blank> GitHub</a><br>",
    "Contenedor Docker disponible  en <a href=https://hub.docker.com/r/drumsergio/secciones-nacionalidadess target=_blank>DockerHub</a>")
  })
  
  clickedIds <- reactiveValues(ids = vector())
  
  observeEvent(input$selectMunicipio,{
    disable("descargaKML")
  })
    
  
  observeEvent(input$selectProvincia,{
    provincia <- provincias$ID[which(input$selectProvincia == provincias$Nombre)]
    municipiosElegibles <<- municipios$NOMBRE[which(provincia == municipios$CPRO)]
    updateSelectizeInput(session = session, "selectMunicipio", choices = municipiosElegibles)
  })
  
  observe({
    if(!is.null(input$selectMunicipio)){
      if(input$selectMunicipio != "Cargando..." && input$selectMunicipio %in% municipiosElegibles){
        
        output$mapa <- simplyMapIt(porcentaje = input$porcentaje, hombreMujer = input$hombreMujer, municipioSelected = input$selectMunicipio,
                                   nacionalidadSelected = input$selectNacionalidad, SXnacionalAmbos = SXnacionalAmbos, SXnacionalHombres = SXnacionalHombres,
                                   SXnacionalMujeres = SXnacionalMujeres)
        
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
  
})
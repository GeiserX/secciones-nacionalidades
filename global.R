#install.packages('shiny', 'shinydashboard', 'shinyjs', 'rgdal', 'raster', 'sp', 'pxR', 'leaflet', 'plotKML')

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
#secciones <- readOGR(dsn = "seccionado/", layer = "SECC_CE_20180101") # Datos a 2018
secciones <- readRDS("seccionado/secciones.rds") # saveRDS(secciones, "seccionado/secciones.rds") # git lfs track ..(FILE)..

# SXnacional <- as.data.frame(read.px("poblacion/2018/0003.px"))
# 
# SXnacionalAmbos <- SXnacional[which(SXnacional$sexo == "Ambos Sexos"), ]
# SXnacionalHombres <- SXnacional[which(SXnacional$sexo == "Hombres"), ]
# SXnacionalMujeres <- SXnacional[which(SXnacional$sexo == "Mujeres"), ]
# 
# saveRDS(SXnacionalAmbos, "poblacion/2018/SXnacional2018ambos.rds")
# saveRDS(SXnacionalHombres, "poblacion/2018/SXnacional2018hombres.rds")
# saveRDS(SXnacionalMujeres, "poblacion/2018/SXnacional2018mujeres.rds")

SXnacionalAmbos <- readRDS("poblacion/2018/SXnacional2018ambos.rds") 
SXnacionalHombres <- readRDS("poblacion/2018/SXnacional2018hombres.rds")
SXnacionalMujeres <- readRDS("poblacion/2018/SXnacional2018mujeres.rds")

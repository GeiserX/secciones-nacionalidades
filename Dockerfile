FROM openanalytics/r-base
MAINTAINER Sergio Fernández "acsdesk@protonmail.com"

RUN apt-get update && apt-get install -y libgdal-dev

RUN R -e "install.packages(c('shiny', 'shinydashboard', 'shinyjs', 'rgdal', 'raster', 'sp', 'pxR', 'leaflet', 'plotKML', 'highcharter'), repos='https://cloud.r-project.org/')"

COPY Rprofile.site /usr/lib/R/etc/
RUN mkdir -p /root/secciones_censales
COPY . /root/secciones_censales
VOLUME /root/secciones_censales/cache

EXPOSE 8080
CMD ["R", "-e", "shiny::runApp('/root/secciones_censales', port = 8080, host = '0.0.0.0')"]

## To see shiny logs -- docker logs [container-name]
## To know container-name -- docker ps (last column)
## To stop all running containers -- docker stop $(docker ps -a -q)
## To remove all running containers -- docker rm $(docker ps -a -q)
## To remove all images -- docker rmi $(docker images -q)
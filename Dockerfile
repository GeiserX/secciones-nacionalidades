FROM openanalytics/r-base
MAINTAINER Sergio Fernández "acsdesk@protonmail.com"

RUN apt-get update && apt-get install -y libgdal-dev

RUN R -e "install.packages(c('shiny', 'shinydashboard', 'rgdal', 'raster', 'sp', 'pxR', 'leaflet', 'plotKML'), repos='https://cloud.r-project.org/')"

RUN mkdir -p /root/secciones_censales
COPY . /root/secciones_censales
COPY Rprofile.site /usr/lib/R/etc/

VOLUME /root/secciones_censales/cache ## To improve read/write speeds on this directory and serve data quicker. Also to allow persistence

EXPOSE 8080
CMD ["R", "-e", "shiny::runApp('/root/secciones_censales', port = 8080, host = '0.0.0.0')"]

## To see shiny logs -- docker logs [container-name]
## To know container-name -- docker ps (last column)
## To stop all running containers -- docker stop $(docker ps -a -q)
## To remove all running containers -- docker rm $(docker ps -a -q)
## To remove all images -- docker rmi $(docker images -q)
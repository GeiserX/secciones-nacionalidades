FROM openanalytics/r-base
MAINTAINER Sergio Fernández "acsdesk@protonmail.com"

RUN apt-get update && apt-get install -y libgdal-dev libudunits2-dev curl

RUN R -e "install.packages('remotes', repos='https://cloud.r-project.org/')"  # Install remotes to install github s2 package, otherwise broken. Delete next time these two lines. (Not working 09/01/21. Delete lines in the future)
RUN R -e "remotes::install_github('r-spatial/s2')"
RUN R -e "install.packages(c('shiny', 'shinydashboard', 'shinyjs', 'rgdal', 'raster', 'sp', 'pxR', 'leaflet', 'plotKML', 'highcharter', 'dplyr'), repos='https://cloud.r-project.org/')"

COPY Rprofile.site /usr/lib/R/etc/
RUN mkdir -p /root/secciones_censales
COPY . /root/secciones_censales

EXPOSE 8080
HEALTHCHECK --interval=60s --timeout=3s CMD curl -f localhost || exit 1
CMD ["R", "-e", "shiny::runApp('/root/secciones_censales', port = 8080, host = '0.0.0.0')"]

## To run -- docker run --name secciones-nacionalidades -p 8080:8080 drumsergio/secciones-nacionalidades:latest
## To see shiny logs -- docker logs [container-name]
## To know container-name -- docker ps (last column)
## To stop all running containers -- docker stop $(docker ps -a -q)
## To remove all running containers -- docker rm $(docker ps -a -q)
## To remove all images -- docker rmi $(docker images -q)

FROM debian:bookworm
MAINTAINER Sergio Fernández "acsdesk@protonmail.com"
ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get install -y libgdal-dev r-base locales libudunits2-dev libgeos-dev libproj-dev curl && rm -rf /var/lib/apt/lists/* && localedef -i es_ES -c -f UTF-8 -A /usr/share/locale/locale.alias es_ES.UTF-8
ENV LANG es_ES.utf8

RUN R -e "install.packages(c('shiny','shinydashboard', 'shinyjs', 'rgdal', 'raster', 'sp', 'pxR', 'leaflet', 'devtools', 'highcharter', 'dplyr'), repos='https://cloud.r-project.org/')"
RUN R -e "devtools::install_github('cran/RandomFields')"
RUN R -e "devtools::install_github('cran/geoR')"
RUN R -e "devtools::install_github('gearslaboratory/gdalUtils')"
RUN R -e "devtools::install_github('Envirometrix/landmap')"
RUN R -e "devtools::install_github('cran/plotKML')"
RUN R -e "devtools::install_github('Appsilon/shiny.stats')"

COPY Rprofile.site /usr/lib/R/etc/
RUN mkdir -p /root/secciones_censales
COPY . /root/secciones_censales

EXPOSE 8080
HEALTHCHECK CMD curl --fail http://localhost:8080 || exit 1   
CMD ["R", "-e", "shiny::runApp('/root/secciones_censales', port = 8080, host = '0.0.0.0')"]


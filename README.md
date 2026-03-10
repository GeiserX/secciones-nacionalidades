<p align="center">
  <img src="docs/images/banner.svg" alt="secciones-nacionalidades banner" width="900"/>
</p>

<h1 align="center">secciones-nacionalidades</h1>

<p align="center">
  <a href="https://github.com/GeiserX/secciones-nacionalidades/actions"><img src="https://github.com/GeiserX/secciones-nacionalidades/workflows/Test%20and%20Deployment%20to%20K3s%20cluster/badge.svg?branch=main" alt="Test and Deployment"></a>
</p>

<p align="center">
  <strong>Foreign Insight — Nationalities in Spain from INE census data</strong>
</p>

---

Proyecto unificador en R - Shiny de datos sobre el Instituto Nacional de Estadistica Espanol sobre Secciones Censales y Nacionalidades

## Actualizacion anual:
1. Descargar datos sobre [poblacion](https://www.ine.es/dyngs/INEbase/es/operacion.htm?c=Estadistica_C&cid=1254736177012&menu=resultados&secc=1254736195461&idp=1254734710990) y [seccionado](http://www.ine.es/ss/Satellite?L=es_ES&c=Page&cid=1259952026632&p=1259952026632&pagename=ProductosYServicios%2FPYSLayout).
2. Ejecutar los comentarios en `server.R` y borrar los datos para dejar unicamente los ficheros RDS, con menor peso.
3. Anadir el nuevo ano en la app en `ui.R` y tambien en `global.R`.

## Author

[@GeiserX](https://github.com/GeiserX)

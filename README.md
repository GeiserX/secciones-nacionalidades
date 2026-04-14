<p align="center">
  <img src="docs/images/banner.svg" alt="secciones-nacionalidades banner" width="900"/>
</p>

<h1 align="center">secciones-nacionalidades</h1>

<p align="center">
  <a href="https://github.com/GeiserX/secciones-nacionalidades/actions/workflows/ci.yml"><img src="https://img.shields.io/github/actions/workflow/status/GeiserX/secciones-nacionalidades/ci.yml?label=CI" alt="CI"></a>
  <a href="https://github.com/GeiserX/secciones-nacionalidades/blob/main/LICENSE"><img src="https://img.shields.io/github/license/GeiserX/secciones-nacionalidades" alt="License"></a>
  <a href="https://hub.docker.com/r/drumsergio/secciones-nacionalidades"><img src="https://img.shields.io/docker/pulls/drumsergio/secciones-nacionalidades" alt="Docker Pulls"></a>
  <a href="https://github.com/GeiserX/secciones-nacionalidades/stargazers"><img src="https://img.shields.io/github/stars/GeiserX/secciones-nacionalidades" alt="GitHub Stars"></a>
  <a href="https://github.com/GeiserX/awesome-spain#readme"><img src="https://img.shields.io/badge/listed%20on-awesome--spain-c60b1e?style=flat-square&logo=data:image/svg%2bxml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIyMCIgaGVpZ2h0PSIxNCIgdmlld0JveD0iMCAwIDIwIDE0Ij48cmVjdCB3aWR0aD0iMjAiIGhlaWdodD0iMTQiIGZpbGw9IiNjNjBiMWUiLz48cmVjdCB5PSIzLjUiIHdpZHRoPSIyMCIgaGVpZ2h0PSI3IiBmaWxsPSIjZmZjNDAwIi8+PC9zdmc+&labelColor=ffc400" alt="listed on awesome-spain"></a>
</p>

<p align="center">
  <strong>Foreign Insight — Nacionalidades en España por sección censal (INE)</strong>
</p>

---

## Descripción

Aplicación web interactiva construida con R y Shiny que permite explorar datos demográficos de nacionalidades por **sección censal** en España, utilizando datos públicos del [Instituto Nacional de Estadística (INE)](https://www.ine.es/).

### Funcionalidades

- **Mapa interactivo por secciones censales**: selecciona provincia, municipio y nacionalidad para visualizar la distribución en el mapa con código de colores
- **Descarga a KML**: selecciona las áreas de interés en el mapa y descárgalas como archivo KML, listo para importar en Google Earth o Google Maps
- **Gráficos por provincia**: visualización de barras con datos de población por municipio, con opciones de porcentaje y distinción hombre/mujer
- **Mapa nacional**: vista general de España con datos agregados por provincia
- **Datos históricos**: evolución temporal a nivel nacional, provincial o municipal

> **⚠️ Datos disponibles: 2012–2021.** El INE dejó de publicar los microdatos de nacionalidad por sección censal a partir del año 2022. Por tanto, esta aplicación contiene datos únicamente del periodo 2012–2021 y no recibirá actualizaciones de nuevos años salvo que el INE reanude la publicación.

## Ejecución

```bash
docker run -p 3838:3838 drumsergio/secciones-nacionalidades
```

## Actualización anual (archivado)

1. Descargar datos sobre [población](https://www.ine.es/dyngs/INEbase/es/operacion.htm?c=Estadistica_C&cid=1254736177012&menu=resultados&secc=1254736195461&idp=1254734710990) y [seccionado](http://www.ine.es/ss/Satellite?L=es_ES&c=Page&cid=1259952026632&p=1259952026632&pagename=ProductosYServicios%2FPYSLayout).
2. Ejecutar los comentarios en `server.R` y borrar los datos para dejar únicamente los ficheros RDS, con menor peso.
3. Añadir el nuevo año en la app en `ui.R` y también en `global.R`.

## Autor

[@GeiserX](https://github.com/GeiserX)

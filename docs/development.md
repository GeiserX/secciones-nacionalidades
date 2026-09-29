# Desarrollo

El INE dejó de publicar los microdatos de nacionalidad por sección censal después de 2021, así que la aplicación ya no recibe años nuevos. Este procedimiento se conserva como registro de cómo se añadía cada año.

## Actualización anual (archivado)

1. Descargar datos sobre [población](https://www.ine.es/dyngs/INEbase/es/operacion.htm?c=Estadistica_C&cid=1254736177012&menu=resultados&secc=1254736195461&idp=1254734710990) y [seccionado](http://www.ine.es/ss/Satellite?L=es_ES&c=Page&cid=1259952026632&p=1259952026632&pagename=ProductosYServicios%2FPYSLayout).
2. Ejecutar los comentarios en `server.R` y borrar los datos para dejar únicamente los ficheros RDS, con menor peso.
3. Añadir el nuevo año en la app en `ui.R` y también en `global.R`.

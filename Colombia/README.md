# Clasificación_Género

### Documentos clave

**Código Clasificación Género**
: Script de R que contiene todo el código para realizar la clasificación de listas de género basado en listas y paqueterías.

### Metodología

El código está escrito en el lenguaje de programación de R. El objetivo del código es poder hacer un conteo de si las patentes están hechas solo por hombres, solo por mujeres o mixto (es decir hombres y mujeres que aparecen como inventores) para después contar la cantidad total de hombres y mujeres. 

Para esto se usó la base de datos de nombres con géneros versión 1.1 de la WIPO (Organización Mundial de la Propiedad Intelectual) clonada de su repositorio en Github. Posteriormente, esa misma base fue limpiada para su utilización separando únicamente los nombres en una nueva base de datos para trabajar de mejor manera.

Para realizar la clasificación de inventores de la base de datos se siguieron los pasos descritos a continuación:

* Se realizó una limpieza y estandarización de los nombres. Este paso es el que personaliza el código para cada país.
* Una vez teniendo los nombres limpios, el código se encarga de comparar los nombres con la base de datos WIPO para conocer cuál es el género de los nombres obtenidos previamente. Además de cotejar y ver si el género es masculino o femenino, también se realiza un condicional para aquellos nombres neutros, es decir que puede ser tanto masculino como femenino; dichos nombres fueron incluidos manualmente. Una vez que se hace la comparación con la base de datos de WIPO, se procede a obtener el género de los nombres con la función género.
* Los géneros obtenidos con ambos métodos se guardan en columnas diferentes para después poder compararse para luego quedarse con una sola opción, ya que algunos nombres no tuvieron género con ambos métodos o los resultados eran ambiguos. El código le da prioridad a los resultados obtenidos con la base de datos de WIPO. En caso de ser neutral, se queda con el género asignado por la base de datos de WIPO. 
* Tras obtener un solo resultado de género para cada nombre, el código crea un condicional para saber si existen solo hombres, solo mujeres o si es mixto (Cabe recalcar que estas etiquetas se usaron para mantener el mismo orden que se especificaba en la base de datos). Una vez hecha esa evaluación se procede a hacer un conteo del número total de hombres y el número total de mujeres. 
* Finalmente se pegan las columnas de esta nueva base de datos a la original para posteriormente exportar dicha base de datos.

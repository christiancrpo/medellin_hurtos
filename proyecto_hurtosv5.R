# Cargando librerías
library(dplyr)
library(ggplot2)

# crear una copia de hurto_a_persona en variable hap
hap <- hurto_a_persona

# eliminar columnas que no son relevantes para este análisis
hap <- hap %>%
  select(-cantidad, -estado_civil, -grupo_actor, -actividad_delictiva,
         -parentesco, -ocupacion, -discapacidad, -grupo_especial, -nivel_academico,
         -testigo, -caracterizacion, -articulo_penal, -categoria_penal, -conducta_especial,
         -sede_receptora, -modelo, -color, -permiso, -unidad_medida)

# ver la tabla hap creada sin las columnas que no se requieren
View(hap)

# Agrupar los datos por barrio y contar la cantidad de hurtos
hurtos_por_barrio <- hap %>%
  group_by(nombre_barrio) %>%
  summarise(cantidad_hurtos = n()) %>%
  filter(cantidad_hurtos > 2000)  # Filtrar barrios con más de 1000 hurtos

# Crear el gráfico de barras
ggplot(hurtos_por_barrio, aes(y = reorder(nombre_barrio, cantidad_hurtos), x = cantidad_hurtos)) +
  geom_bar(stat = "identity", fill = "skyblue") +  # Usamos 'stat = "identity"' para usar la variable y el valor
  labs(title = "Cantidad de Hurtos por Barrio", 
       x = "nombre_barrio", 
       y = "Cantidad de Hurtos") +
  theme(axis.text.y = element_text(size = 8)) 

# encontré después de graficar que existe en el top 10 de barrios "sin dato"

# Verificar cuántos registros con "sin dato" hay ahora en la columna barrio
sum(hap$nombre_barrio == "Sin dato")

# Filtrar los registros que tienen "sin dato" en la columna barrio
hap_sin_dato <- hap %>% filter(nombre_barrio == "Sin dato")

# Ver todos los registros que contienen "sin dato"
View(hap_sin_dato)  # Si estás usando RStudio, esto abrirá una vista interactiva en la ventana de datos

# Verificar cuántos registros con "sin dato" hay ahora en la columna codigo_barrio
sum(hap$codigo_barrio == "SIN DATO")

# Limpieza de filas de nombre_barrio y codigo_barrio "sin dato"

# Eliminar las filas donde nombre_barrio o codigo_barrio tienen el valor "sin dato"
hap_limpio <- hap %>% filter(nombre_barrio != "Sin dato" & codigo_barrio != "SIN DATO")

# Verificar que las filas han sido eliminadas
sum(hap_limpio$nombre_barrio == "sin dato")
sum(hap_limpio$codigo_barrio == "SIN DATO")

#información
# que nos permita identificar el barrio, adicional a que estos casos en su mayoría son
# hurtos o estafas virtuales, se decide borrar las 5171 filas.

# Eliminar las filas donde nombre_barrio o codigo_barrio tienen el valor "sin dato"
hap_limpio <- hap %>% filter(nombre_barrio != "Sin dato" & codigo_barrio != "SIN DATO")

# Verificar que las filas han sido eliminadas
sum(hap_limpio$barrio == "Sin dato")
sum(hap_limpio$codigo_barrio == "SIN DATO")

# nuevo gráfico

# Agrupar los datos por barrio y contar la cantidad de hurtos
hurtos_por_barrio <- hap_limpio %>%
  group_by(nombre_barrio) %>%
  summarise(cantidad_hurtos = n()) %>%
  filter(cantidad_hurtos > 2000)  # Filtrar barrios con más de 2000 hurtos

# crear el gráfico de barras horizontales
ggplot(hurtos_por_barrio, aes(y = reorder(nombre_barrio, cantidad_hurtos), x = cantidad_hurtos)) +
  geom_bar(stat = "identity", fill = "skyblue") +  # Usamos 'stat = "identity"' para usar la variable y el valor
  labs(title = "Cantidad de Hurtos por Barrio", 
       x = "nombre_barrio", 
       y = "Cantidad de Hurtos") +
  theme(axis.text.y = element_text(size = 8)) 

colnames(hap_limpio)
View(hap_limpio)

# cálculo de tops

# categoría de bienes preferida por ladrones?
top_categoria <- sort(table(hap_limpio$categoria_bien), decreasing = TRUE)
print("Top 'categoria_bien':")
head(top_categoria)

# cuales son los bienes que más se roban en medellín?
top_bien <- sort(table(hap_limpio$bien), decreasing = TRUE)
print("Top 'bien':")
head(top_bien)

# cuales son los lugares en los que más roban en medellín?
top_lugar <- sort(table(hap_limpio$lugar), decreasing = TRUE)
print("Top 'lugar':")
head(top_lugar)

# cual es el "sexo" al que más atacan?
top_sexo <- sort(table(hap_limpio$sexo), decreasing = TRUE)
print("Top 'sexo':")
head(top_sexo)

# cual es la edad que más atacan?
top_edad <- sort(table(hap_limpio$edad), decreasing = TRUE)
print("Top 'edad':")
head(top_edad)

# se encuentran 26250 filas que no tienen datos para las categorías de tipos
# de bienes y bienes robados, sin embargo se decide no eliminar estas filas
# ya que contienen información relevante sobre los barrios que son víctimas de
# robos, los cuales son prioridad en este análisis

# gráfico de tops

# Función para graficar el top 5 de una categoría
graficar_top5 <- function(data, columna, titulo) {
  # Obtener el top 5
  top5 <- sort(table(data[[columna]]), decreasing = TRUE) |> head(5)
  top5_df <- as.data.frame(top5) # Convertir a data frame
  colnames(top5_df) <- c("Categoria", "Frecuencia")
  
  # Crear el gráfico
  ggplot(top5_df, aes(x = reorder(Categoria, Frecuencia), y = Frecuencia)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    coord_flip() +
    labs(title = titulo, x = "Categoría", y = "Frecuencia") +
    theme_minimal()
}

# Gráfico 1: Categoría de bienes
grafico_categoria <- graficar_top5(hap_limpio, "categoria_bien", "Top 5 Categorías de Bienes Robados")
print(grafico_categoria)

# Gráfico 2: Bienes más robados
grafico_bien <- graficar_top5(hap_limpio, "bien", "Top 5 Bienes más Robados")
print(grafico_bien)

# Gráfico 3: Lugares con más robos
grafico_lugar <- graficar_top5(hap_limpio, "lugar", "Top 5 Lugares con Más Robos")
print(grafico_lugar)

# Gráfico 4: Sexo más atacado
grafico_sexo <- graficar_top5(hap_limpio, "sexo", "Sexo Más Atacado")
print(grafico_sexo)

# Gráfico 5: Edad más atacada
grafico_edad <- graficar_top5(hap_limpio, "edad", "Edades Más Atacadas")
print(grafico_edad)

# Sacar estadística de meses con más hurtos al año

# Asegurarte de que la columna "fecha_hecho" sea de tipo Date-Time
hap_limpio$fecha_hecho <- as.POSIXct(hap_limpio$fecha_hecho, format = "%Y-%m-%d %H:%M:%S")

# Extraer el mes de cada fecha
hap_limpio$mes <- format(hap_limpio$fecha_hecho, "%m") # Extrae el mes en formato "01", "02", etc.

# Contar la frecuencia de hurtos por mes
hurto_por_mes <- table(hap_limpio$mes)
hurto_por_mes <- as.data.frame(hurto_por_mes)  # Convertir a data frame para graficar
colnames(hurto_por_mes) <- c("Mes", "Frecuencia")

# Ordenar por frecuencia
hurto_por_mes <- hurto_por_mes[order(-hurto_por_mes$Frecuencia), ]

# Graficar los meses con más hurtos
library(ggplot2)

ggplot(hurto_por_mes, aes(x = reorder(Mes, -Frecuencia), y = Frecuencia)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Meses con Más Hurtos", x = "Mes", y = "Frecuencia") +
  theme_minimal()

# Exportar hap_limpio a un archivo CSV

write.csv(hap_limpio, "hap_limpio.csv", row.names = FALSE)
getwd()

View(hap_limpio)

# Remover el carácter '#' al inicio de los valores de la columna "codigo_barrio"
hap_limpio$codigo_barrio <- gsub("^#", "", hap_limpio$codigo_barrio)

# Exportar la tabla limpia como un archivo CSV
write.csv(hap_limpio, "hap_limpio_sin_hash.csv", row.names = FALSE)

cat("Archivo exportado como 'hap_limpio_sin_hash.csv'")



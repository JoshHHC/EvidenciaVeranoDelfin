
library(tidyverse)

sr <- read_csv('master.csv')

glimpse(sr)

# Eliminando valores perdidos
sr_no_na <- sr %>%
  na.omit()

# Seleccionando los atributos de segmentación.
sr1 <- sr %>%
  na.omit() %>%
  select(suicides_no, `suicides/100k pop`, `gdp_for_year ($)`, `gdp_per_capita ($)`)

sum <- sr_no_na %>% select(-country, -age, -sex, -year) %>% summary()
sum

# Análisis de cluster jerárquico

# La agrupación jerárquica agrupa las observaciones en función de sus distancias en pares. 
# Primero, se escalarán los valores ya que hay varias unidades de medida entre las variables. 
# Luego, las distancias en pares se calcularán utilizando el método euclidiano.

# Visualizando las distancias de los pares

# El siguiente histograma muestra que una distancia euclidiana de dos es la distancia más frecuente
# entre las distancias por pares.

# Escala sr1 y distancia euclidiana de observaciones sr1
dist_sr1 <- dist(scale(sr1))

# Visualizando distancias en pares
hist(dist_sr1)

# Análisis de vinculación de clusters.
hclust_sr1 <- hclust(dist_sr1, method = "complete")

library(dendextend)

sr1_dendrogram <- as.dendrogram(hclust_sr1)

sr1_dendrogram_color <- color_branches(sr1_dendrogram, h = 8)

# Extraer grupo de cluster
cluster <- cutree(hclust_sr1, k=4)

# Agregar valores de grupo de cluster a observaciones
sr2 <- sr_no_na %>% mutate(cluster = cluster)

# Dendrograma de grupos

# Usando hclust (), se completó un análisis de vinculación utilizando el método "complete" para vincular distancias. 
# Después de una inspección visual del dendrograma, se probó con varios números de clúster y también se utilizó 
# varias relaciones bivariadas para determinar qué cantidad de clúster segmenta mejor los datos. 
# Se consideró el método tradicional de selección de k mediante el trazado de las distancias en función del número
# de conglomerados; sin embargo, los datos son demasiado grandes y se identificaron más de 900 conglomerados.

plot(sr1_dendrogram_color)

# Analizando las relaciones bivariadas

# Se analizó las diversas combinaciones de distancias de pares utilizando gráficos 2D. 
# Esto se hizo para seleccionar las variables que mejor visualizan los segmentos del clúster. 
# Se seleccionó variables analizando la forma y la fuerza de varias relaciones bivariadas. 
# Se colocó una línea de regresión en la gráfica para comprender mejor las relaciones.

# La relación entre las variables 'Suicides_no' y 'GDP for Year' tuvo la asociación más fuerte 
# de todas las demás relaciones bivariadas, como se muestra a continuación.

# Caracterizando y visualizando relación bivariada
sr2_lr <- ggplot(sr2, aes(x = `gdp_for_year ($)`, y = suicides_no)) +
              geom_point() +
              geom_smooth(method = "lm", se = FALSE) +
              scale_y_log10() +
              scale_x_log10()

sr2_lr

# Visualización de agrupaciones

# Usando las variables seleccionadas (Número de suicidios y PIB por año), se construyeron diagramas 2D 
# para visualizar los grupos. Se agregaron líneas de regresión para cada grupo para demostrar cómo se relacionan
# las variables dentro de cada grupo. Se quitaron las escalas de registro de cada eje para distinguir los grupos.

# Visualización de agrupamientos y su línea de regresión.
sr2_plot <- ggplot(sr2, aes(y=suicides_no, x=`gdp_for_year ($)`, color = factor(cluster))) +
                geom_point() +
                geom_smooth(method = "lm", se = FALSE)

sr2_plot

# Perfilando e interpretando cada grupo

# Para comprender lo que sucede en cada grupo, se visualiza cada grupo y se describe en función del país, 
# el sexo y la edad. Sus perfiles se visualizan por color.

# Cluster 1: incluye la mayor parte de los puntos de datos. La mayoría de las víctimas de suicidio 
# se encuentran en este grupo y no existe un elemento de perfil único basado en los datos que tenemos. 

cluster1 <- sr2 %>%
                filter(cluster ==  1) %>% 
                     ggplot(aes(x = suicides_no, y = `gdp_for_year ($)`, color = sex)) +
                         geom_point() +
                         scale_y_log10() +
                         scale_x_log10()

cluster1

# Cluster 2

# Las víctimas en el Grupo 2 son de Japón y los Estados Unidos.
#     Todos son hombres.
#     Sus edades van desde los 15 a los 74 años, y el grupo de edades de 35 a 54 años tiene el mayor valor
#     en Número de suicidios.

# El grupo 2 se visualiza a continuación; perfilado por país

cluster2_country <- sr2 %>%
                filter(cluster ==  2) %>%
                    ggplot(aes(y = suicides_no, x = `gdp_for_year ($)`, color = `country`)) +
                        geom_point()+
                        scale_y_log10() +
                        scale_x_log10() +
                        ggtitle("Cluster 2 PIB ~ No. de Suicidios por País")

cluster2_country

# Grupo 3

#     Las víctimas en el grupo 3 son todas de los Estados Unidos.
#     Todos son hombres.
#     Sus edades van desde los 35 a los 74 años, y el grupo de edades de 35 a 54 años tiene el mayor número 
#     de observaciones.

# El grupo 3 se visualiza a continuación; perfilado por grupo de edad

cluster3_age <- sr2 %>% 
                filter(cluster ==  3) %>%
                    ggplot(aes(y = suicides_no, x = `gdp_for_year ($)`, color = `age`)) +
                        geom_point() +
                          scale_y_log10() +
                          ggtitle("Cluster 3 PIB ~ No. de Suicidios por edad")

cluster3_age

# Cluster 4

# Las víctimas en el grupo 4 son todas de los Estados Unidos.
# Están compuestos por hombres y mujeres, sin embargo, la mayoría de las víctimas en este grupo son mujeres.

# El grupo 4 se visualiza a continuación; perfilado por año

cluster4_country <- sr2 %>%
                filter(cluster ==  4) %>%
                    ggplot(aes(y = suicides_no, x = `gdp_for_year ($)`, color = `year`)) +
                        geom_point()+
                        scale_y_log10() +
                        scale_x_log10() +
                        ggtitle("Cluster 4 GDP ~ No. of Suicides por año")

cluster4_country

# Resumen de los promedios por grupo

# En este segundo análisis, se resumiran los promedios de cada variable: número de suicidios, 
# suicidios por cada 100,000 personas, PIB por año y PIB per cápita. Los promedios 
# para cada par de clúster / variable se muestran en una tabla. Luego se visualiza en un diagrama de serpiente.

# Resumen del promedio de la población
pop <- sr2 %>% summarize(
  suicides_no = mean(suicides_no),
  `suicides/100k pop` = mean(`suicides/100k pop`), 
  `gdp_for_year ($)` = mean(`gdp_for_year ($)`),
  `gdp_per_capita ($)` = mean(`gdp_per_capita ($)`),
  `HDI for year` = mean(`HDI for year`)) %>%
  mutate(cluster = "pop") 

# reordenar columnas
pop <- pop[,c(6,1,2,3,4,5)]

# resumen de la media para cada grupo
summary <- sr2 %>% group_by(cluster) %>% summarize(
          suicides_no = mean(suicides_no),
          `suicides/100k pop` = mean(`suicides/100k pop`), 
          `gdp_for_year ($)` = mean(`gdp_for_year ($)`),
          `gdp_per_capita ($)` = mean(`gdp_per_capita ($)`),
          `HDI for year` = mean(`HDI for year`))

# combinando la población y los promedios de los conglomerados en una tabla
summary <- add_row(summary, cluster = "pop",
                suicides_no = 206.1243,
               `suicides/100k pop`= 11.99194,
               `gdp_for_year ($)`= 547663851141,
               `gdp_per_capita ($)` = 21074.37,
               `HDI for year` = 0.7766011,
                    .before = 1)

# Resumen rotativo con cada agrupación como columnas.
summary <- as.data.frame(t(summary))
summary$profiling_var <- rownames(summary)
summary <- summary[-c(1), ]

# volver a agregar nombres de columnas
colnames(summary) <- c("Population", "Cluster 1", " Cluster 2", "Cluster 3", "Cluster 4")
colnames(summary)[6] <- "profiling_var"

# convertir el resumen en un tabla y cambiar la clase de col 1-4 como numérico
summary <- tbl_df(summary)
summary <- summary %>% mutate_at(vars(1:5), as.character)
summary <- summary %>% mutate_at(vars(1:5), as.numeric)

summary

# Análisis del diagrama de serpiente por grupo

#     Los grupos 3 y 4 se empatan para obtener el PIB promedio más alto por año.
#     El grupo 4 tiene el promedio más alto de suicidios.
#     El grupo 2, sin embargo, tiene la tasa de suicidio promedio más alta (por cada 100 mil personas), 
#     mientras que tiene el segundo PIB promedio más alto por año y el número de suicidios.
#     Los grupos 2 y 3 tienen el promedio más alto de suicidios y la tasa de suicidios.

# Transformando resumen a un largo tbl
library(reshape2)
summary_melt <- melt(summary, id = "profiling_var")

# Nombrar columnas y convertir de nuevo a tbl
colnames(summary_melt)[3] <- "mean"
colnames(summary_melt)[2] <- "cluster"
summary_melt <- tbl_df(summary_melt)

snake_plot <- summary_melt %>%
                    ggplot(aes(x=profiling_var, y= mean, group = cluster, color = cluster)) + 
                    geom_line() +
                    geom_point() +
                    scale_y_log10() +
                    ggtitle("Promedios de clúster por variable") +
                    theme(legend.position = c(0.8, 0.8))
snake_plot

# Para una mejor visualización, proporcioné una segunda versión del gráfico de serpientes a continuación, 
# sin PIB por año ni IDH por año.

summary_melt2 <- summary_melt %>% filter(profiling_var != "gdp_for_year ($)", profiling_var != "HDI for year")

snake_plot2 <- summary_melt2 %>%
                    ggplot(aes(x=profiling_var, y= mean, group = cluster, color = cluster)) + 
                    geom_line() +
                    geom_point() +
                    scale_y_log10() +
                    ggtitle("Promedios de clúster por variable") +
                    theme(legend.position = c(0.8, 0.8))

snake_plot2

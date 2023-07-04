library("tidyverse")
library("cluster")
library("NbClust")
library("factoextra")
library(extrafont)


theme_set(theme_classic() +
            theme(plot.background = element_rect(fill = "transparent", color = "transparent"),
                  legend.background = element_rect(fill = "transparent"),
                  panel.background = element_rect(fill = "transparent")))

#   Dataset sobre casos, muertes y vacunas
coviddata <- read.csv("C:/Users/gusta/Desktop/rProyect/Covid/Datasets/All countries covid cases.csv")
coviddata <- subset(select(coviddata, Country_code, everything())[,2:8])
coviddata$Date_reported <- as.Date(coviddata$Date_reported)

# Selecciona la ultima fecha de cada pais, y remueve dos importantes outliers
coviddata <- coviddata %>%
  arrange(desc(Date_reported)) %>%
  group_by(Country) %>%
  filter(Country != "United States of America" & Country != "China") %>%
  top_n(1, wt = Date_reported) %>%
  ungroup()

datos <- coviddata %>%
  select(Country, Cumulative_deaths, Cumulative_cases)

caracteristicas <- datos[, c("Cumulative_deaths", "Cumulative_cases")]

k <- 3

# Aplicar k-means a la matriz de características
kmeans_result <- kmeans(caracteristicas, centers = k)

# Agregar la asignación de cluster al dataframe 
datos$Cluster <- kmeans_result$cluster

# Crear el gráfico de dispersión (scatterplot) con colores según el cluster
ggplot(datos, aes(x = Cumulative_deaths, y = Cumulative_cases, color = factor(Cluster))) +
  geom_point() +
  labs(x = "Cumulative Deaths", y = "Cumulative Cases", color = "Cluster") 


######## PAM ##################
covid.scaled <-  scale(datos[,c(2,3)])
rownames(covid.scaled) <-  datos$Country

# Choose number of clusters
nc <- NbClust(covid.scaled, distance = "euclidean",
              min.nc = 2, max.nc = 15, method = "kmeans")

# PAM: Partiniong around medoid
set.seed(1234)
fit.pam <- pam(covid.scaled, k = 2, stand = TRUE) #Elegimos 2 clusters
clusplot(fit.pam, col.p = "#08306B")
fit.pam[["clusinfo"]] 

fit.pam$clustering # a que cluster pertenece cada banco

datos <- mutate(
  datos,
  cluster_assignment = as.character(fit.pam$clustering)
)

# Graficamos ambas variables y vemos la distribucion de los medoides
ggplot(data = datos) +
  geom_point(aes(x = Cumulative_cases, y = Cumulative_deaths, color = cluster_assignment), size = 2.5) +
  ggrepel::geom_text_repel(
    data = subset(datos, cluster_assignment == "2"),
    aes(x = Cumulative_cases, y = Cumulative_deaths, label = Country),
    vjust = -1.5, force = 10,
    family = "Times New Roman" ) +
  labs(x = "Casos acumulados", y = "Muertes acumuladas", title = "") +
  scale_color_manual(values = c("1" = "#08306B", "2" = "#33CCFF")) +
  guides(color = guide_legend(title = "")) +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) 
# Podemos ver que estan bastante bien agrupados por cluster.


### HIEARCHICAL CLUSTERING 
distance <- dist(covid.scaled)
class(distance)

hc.covid <- hclust(distance, method = "average")
plot(hc.covid, hang = -1, cex = 0.2, main = "")









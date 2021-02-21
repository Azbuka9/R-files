library(tidyverse)  # lib za data manipulaciju
library(cluster)    # lib za algoritme klasterovanja # lib for clustering algorithms
library(factoextra)# lib for clustering algorithms and data visualization
library(gridExtra) # koristimo za grid.arrange funkciju

#Uèitavamo fajl USArrest iz datasets paketa
#Ovaj data set govori o broju kriviènih dela na osnovu kojih su ljudi uhapšeni po Amerièkim saveznim državama.
df <- USArrests 
df <- na.omit(df)# uklanjanje nedostajucih vrednosti
df <- scale(df)# sklairanje
head(df)

distance <- get_dist(df)?
k2 <- kmeans(df, centers = 2, nstart = 25)
str(k2)
k2
fviz_cluster(k2, data = df)

df %>%
  as_tibble() %>%
  mutate(cluster = k2$cluster,
         state = row.names(USArrests)) %>%
  ggplot(aes(UrbanPop, Murder, color = factor(cluster), label = state)) +
  geom_text()

k3 <- kmeans(df, centers = 3, nstart = 25)
k4 <- kmeans(df, centers = 4, nstart = 25)
k5 <- kmeans(df, centers = 5, nstart = 25)

# plots to compare
p1 <- fviz_cluster(k2, geom = "point", data = df) + ggtitle("k = 2")
p1
p2 <- fviz_cluster(k3, geom = "point",  data = df) + ggtitle("k = 3")
p2
p3 <- fviz_cluster(k4, geom = "point",  data = df) + ggtitle("k = 4")
p3
p4 <- fviz_cluster(k5, geom = "point",  data = df) + ggtitle("k = 5")
p4

grid.arrange(p1, p2, p3, p4, nrow = 2)

set.seed(123)

# funkcija za izracunavanje ukupnog broja klastera unutar klastera
wss <- function(k) {
  kmeans(df, k, nstart = 10 )$tot.withinss
}

# izracunavanje i iscrtavanje wss for k = 1 to k = 15
k.values <- 1:15

# trazimo optimalan broj klastera
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Broj klastera K",
     ylab="Total within-clusters sum of squares") # sum of squares unutar klastera

set.seed(123)

fviz_nbclust(df, kmeans, method = "wss")


# funkcija za izracunavanje prosecne siluete za k klastera
avg_sil <- function(k) {
  km.res <- kmeans(df, centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(df))
  mean(ss[, 3])
}
avg_sil(5)
avg_sil(2)






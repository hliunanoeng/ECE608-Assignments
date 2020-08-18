#5.1
#a
library("dplyr")
simdat = lapply(c(0, 8), function(mx) {
  lapply(c(0,8), function(my) {
    tibble(x = rnorm(100, mean = mx, sd = 2),
           y = rnorm(100, mean = my, sd = 2),
           class = paste(mx, my, sep = ":"))
  }) %>% bind_rows
}) %>% bind_rows
simdatxy = simdat[, c("x", "y")]

library("cluster")
pam4 <- pam(simdatxy, 4)
sil <- silhouette(pam4, 4)
index <- order(as.array(as.numeric(row.names(sil))))
sil <- sil[index,]
plot(sil, col=c("red","green","blue","purple"), main="Silhouette")


#b
sil_wid <- function(k){
  p <- pam(simdatxy,k)
  s <- silhouette(p,k)
  return(mean(s[,"sil_width"]))
}

k_vector <- c(2:11)
width_vector <- c()

for (val in k_vector){
  w <- sil_wid(val)
  width_vector <- c(width_vector,w)
}
plot(k_vector,width_vector,xlab = "K", ylab = "silhouette index")

#c
library(ggplot2)
simdatxy2 <- tibble(x = runif(400,min=-10,max=10), y = runif(400,min=-10,max=10))
ggplot(simdatxy2, aes(x = x, y = y)) + geom_point() +
  coord_fixed()

sil_wid2 <- function(k){
  p <- pam(simdatxy2,k)
  s <- silhouette(p,k)
  return(mean(s[,"sil_width"]))
}

k_vector <- c(2:20)
width_vector <- c()

for (val in k_vector){
  val
  w <- sil_wid2(val)
  width_vector <- c(width_vector,w)
}
plot(k_vector,width_vector,xlab = "K", ylab = "silhouette index")

#5.3
#a
library(kernlab)
data(spirals)
s_data <- tibble(x = spirals[,1], y = spirals[,2],cluster = as.factor(kmeans(spirals, centers=2)$cluster))
ggplot(s_data, aes(x = x, y = y, col = cluster)) + geom_point() + coord_fixed()

#b
library(dbscan)
s_data2 <- tibble(x = spirals[,1], y = spirals[,2],cluster = as.factor(dbscan::dbscan(spirals, eps = 0.15, minPts = 3)$cluster))
ggplot(s_data2, aes(x = x, y = y, col = cluster)) + geom_point() + coord_fixed()

#c
s_data3 <- tibble(x = spirals[,1], y = spirals[,2],cluster = as.factor(dbscan::dbscan(spirals, eps = 0.15, minPts = 3)$cluster))
ggplot(s_data3, aes(x = x, y = y, col = cluster)) + geom_point() + coord_fixed()

e_vector <- seq(0.01,1,0.01)
ncluster_vector <- vapply(e_vector, function(x){length(unique(as.factor(dbscan::dbscan(spirals, eps = x, minPts = 3)$cluster)))},numeric(1))
plot(e_vector,ncluster_vector, xlab="Parameter eps", ylab="Number of Clusters")

P_vector <- seq(1,20,1)
ncluster_vector2 <- vapply(P_vector, function(x){length(unique(as.factor(dbscan::dbscan(spirals, eps = 0.15, minPts = x)$cluster)))},numeric(1))
plot(P_vector,ncluster_vector2, xlab="Parameter minPts", ylab="Number of Clusters")

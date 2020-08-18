#4.1
#library(mixtools)
#yvar <- readRDS("../data/Myst.rds")$yvar
#results <- mixtools::normalmixEM(yvar)
#t(data.frame(results$mu,results$sigma))

#4.2
#original distribution
#lambda <- rgamma(10000, shape = 10, rate = 3/2)
#distribution1 <- rpois(length(lambda), lambda = lambda)

#theoretical distribution generated from parameters obtained from fitting in 4.4.3
#p_size <- 10.06276
#p_prob <- 0.6038492
#distribution2 <- rnbinom(10000, p_size, p_prob)

#qqplot(distribution1,distribution2, asp = 1, xlab = "simulated", ylab = "theoretical")
#abline(a = 0, b = 1, col="blue")

#4.3
library("flexmix")
data("NPreg")

#a
NPreg1 <- dplyr::filter(NPreg,class==1)
NPreg2 <- dplyr::filter(NPreg,class==2)

#plot(NPreg1$x,NPreg1$yn,col="blue",xlab = "x", ylab = "y")
#points(NPreg2$x,NPreg2$yn,col="green")
#hist(NPreg$yn,main = "",xlab="y")

#b
m1 <- flexmix(yn ~ x + I(x^2), data = NPreg, k = 2)
#m1

#c
#m1@components
NPreg_clustered <- data.frame(NPreg, "cluster" = as.factor(clusters(m1)))
truthTable <- NPreg_clustered[,c("x","yn","class","cluster")]
#truthTable
#mean(abs(truthTable$class - truthTable$cluster))
#summary(m1)

#d
#library("ggplot2")
#ggplot(data = truthTable, aes(x = x, y = yn)) + geom_point(aes(color=cluster)) + labs(x="x",y="y")

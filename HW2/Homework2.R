#2.1
#generate data
sequence_length <- 1000
mutation_rate <- 10^-4
trial_size <- 10000
l <- replicate(trial_size,{sum(rbinom(sequence_length,1,mutation_rate))})
#fit data
library("vcd")
f <- goodfit( l, "poisson")
#check goodness of Poisson fit
summary(f)
rootogram(f, main="Poisson Rootgram",xlab = "",ylab="Frequency", rect_gp = gpar(fill = "chartreuse4"))

#simulate many Poisson trials using the fitted lambda parameter
lambda <- f$par
simulated = rpois(trial_size,lambda[[1]])
table(l)
table(simulated)

#2.2
#generate data 
generator <- function(n=25,min=0,max=7){
  return(max(runif(n,0,7)))
}
B <- 100
l <- replicate(B,generator())
plot data
hist(l,xlab="",main="Distribution Of Maxima")

#2.3
#a
mtb = read.table("~/Desktop/M_tuberculosis.txt",header=TRUE)
table(mtb$AmAcid)
table(mtb$Codon)

#b
(mtb$Number/sum(mtb$Number))*1000

#c
library(dplyr)

bias_transform <- function(t = mtb){
  new_mtb <- t %>%
    group_by(AmAcid) %>%
    mutate(freq = Number/sum(Number), redundant = length(factor(Codon))) %>%
    mutate(bias = abs(freq-(1/redundant)))

  return(new_mtb[which.max(new_mtb$bias),])}

bias_transform()

#2.4
library("Biostrings")

#a
staph = readDNAStringSet("~/Desktop/staphsequence.ffn.txt", "fasta")
staph[1:3]

#b
staph <- readDNAStringSet("~/Desktop/staphsequence.ffn.txt", "fasta")

window <- 100
l <- length(staph[[1]])
start <- (c(1:as.integer(l/window))-1)*window
end <- start + window

view <- Views(staph[[1]],start=start,end=end)
gc_fixed_window <- rowSums(alphabetFrequency(view)[, c(2,3)]/window)
plot(gc_fixed_window, type = 'l')


#c
window <- 100
GC_content <- letterFrequencyInSlidingView(staph[[1]], window, c("G","C"))
GC_content
GC_fraction <- GC_content/window
GC_fraction

#d
GC_roll <- rowSums(GC_fraction)
plot(GC_roll, type = 'l',ylab="GC %",xlab="Window")

#2.5
s <- seq(0,1,by=0.005)
d_10_30 <- dbeta(s, 10, 30)
d_20_60 <- dbeta(s, 20, 60)
d_50_150 <- dbeta(s, 50, 150)
d_1_1 <- dbeta(s, 1, 1)
d_h_h <- dbeta(s, 1/2, 1/2)

plot(s,d_10_30,type="l",lty=2,ylab="",ylim=c(0,15),col="red")
lines(s,d_20_60,col="green",lty=2)
lines(s,d_50_150,col="blue",lty=2)
lines(s,d_1_1, col="purple",lty=2)
lines(s,d_h_h, col="orange",lty=2)

legend( "topright", c("(10,30)","(20,60)","(50,150)","(1,1)","(1/2,1/2)"),text.col=c("red","green","blue","purple","orange"))

#2.6
#the posterior distribution from textbook where alpha = 50, beta = 350
posterior_theta_textbook = rbeta(n = 1e6, 90, 610)
#my own posterior distribution generated from alpha = 20, beta= 50
rtheta = rbeta(100000, 20, 50)
y = vapply(rtheta, function(th) {rbinom(1, prob = th, size = 300)}, numeric(1))
posterior_theta_new = rtheta[ y == 40 ]
qqplot(posterior_theta_new, posterior_theta_textbook, type = "l", asp = 1)
abline(a = 0, b = 1, col = "blue")
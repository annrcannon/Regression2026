# Chapter 3 in Class

library(gridExtra)  
library(knitr) 
library(kableExtra)
library(tidyverse)


# Show don't code
plotBinomial=function(n,p){
  y1 <- 0:n  # possible values
  prob <- dbinom(y1,n,p)  # P(Y=y)
  BBGdf <- data.frame(y1,prob)
  ggplot(data = BBGdf, aes(x = y1, xend = y1, y = 0, yend = prob)) + 
    geom_segment() + 
    xlab("number of successes") + ylab("probability") + labs(title=paste("n = ", n, " p = ", p))
  

}
Binom1 <- plotBinomial(10,.25) + xlim(0, 10) + scale_y_continuous(breaks = c(0.00, 0.05, 0.10, 0.15, 0.20, 0.25))
Binom2 <- plotBinomial(20,.2) + xlim(0, 15)
Binom3 <- plotBinomial(10,.5) + xlim(0, 10)
Binom4 <- plotBinomial(50,.2) + xlim(0, 25)


grid.arrange(Binom1,Binom2,Binom3,Binom4,ncol=2)


# Binomial example
?dbinom
dbinom(1,size=4,.5)
outcomes=c(0,1,2,3,4)
probs = dbinom(outcomes,size=4,.5)
barplot(probs)

dbinom(500,size=1000,.5)

# suppose we want the probability of getting at least 500
# heads out of 1000'
pbinom(500000,1000000,.5,lower.tail = F)

# Test example
dbinom(2, size = 10, prob = .25)

# probability of getting 2 or less correct
p1 <- dbinom(0, size = 10, prob = .25)
p2 <-dbinom(1, size = 10, prob = .25)
p3 <-dbinom(2, size = 10, prob = .25)
p3

# prob of 2 or less
# P(Y<=2)
p1+p2+p3

pbinom(2,10,.25,lower.tail = T)

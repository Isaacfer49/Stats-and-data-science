## Statistical Methods.
## The following is just a simple demonstration of the bootstraping statistical method implemented in R. 
## Using the same dataset its original creator used, Bradley Efron. 

library(bootstrap)
data(law)

law_sample <- law

LSAT <- law_sample$LSAT
GPA <- law_sample$GPA

cor(LSAT,GPA)

# create a scatter plot
plot(LSAT, GPA, main = "Scatter plot of x and y")

# add a line for the correlation
abline(lm(GPA ~ LSAT), col = "red")

##Bootstrapping over the same sample.

B <- 10000
n <- 15
CorBootstrap <- rep(0,B)

for (i in 1:B) {
  
  sample_est <- law_sample[sample(c(1:n), size = n, replace = TRUE),]
  
  CorBootstrap[i] <- cor(sample_est$LSAT,sample_est$GPA)
  
}

hist(CorBootstrap)

alphaaa <- 0.05

c(quantile(CorBootstrap, alphaaa/2), quantile(CorBootstrap, 1 - alphaaa/2))

##Confirming our parameter estimation.

data("law82")
population <- law82

# create a scatter plot
plot(population$LSAT, population$GPA, main = "Scatter plot of x and y")

# add a line for the correlation
abline(lm(population$GPA ~ population$LSAT), col = "red")

cor(population$LSAT,population$GPA)



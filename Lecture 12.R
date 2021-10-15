# Austin Ash
# ACTS 190
# Lecture 12 - Discrete Distributions notes
# 10-11-21

# Page 21
library(fitdistrplus)
library(stats4)
library(MASS)
# for other necessary test or graphical tools
library(survival)
library(actuar)
library(distrMod)

# Loading in data (same from lecture 11)
cyber.data = read.csv(file.choose(), header = T)
summary(cyber.data)
head(cyber.data,10)

# Cullen and Frey graph
descdist(cyber.data[,2], boot = 1000,discrete=TRUE)
# From the plot, negative binomial and poisson both look very good. Also try normal 
#   and exponential

# Trying to fit probability distributions to the # of cyber attacks
# Negative binomial
nbinom.f<- fitdist(cyber.data$event.count, "nbinom")

# Poisson
pois.f<- fitdist(cyber.data$event.count, "pois")# Poisson

# Normal
norm.f <- fitdist(cyber.data$event.count, "norm") # Normal

# Exponential
exp.f <- fitdist(cyber.data$event.count, "exp") # Exponential

summary(nbinom.f)
summary(pois.f)
summary(norm.f)
summary(exp.f)

# From AIC and BIC, negative binomial and exponential look the best. We will compare these 2

# Comparing the distributions:
par(mfrow = c(2, 2))
plot.legend <- c("Binomial neg","Exponential")
denscomp(list(nbinom.f,exp.f), legendtext = plot.legend)
qqcomp(list(nbinom.f,exp.f), legendtext = plot.legend)
cdfcomp(list(nbinom.f,exp.f), legendtext = plot.legend)
ppcomp(list(nbinom.f,exp.f), legendtext = plot.legend)

# From these plots, choose negative binomial for this dataset.
#   We can go one step further. Generate random dataset from distribution and find
#   correlation with existing data

par(mfrow = c(1, 1))
#  set.seed(512)
# Exponential:
Ex=rexp(length(cyber.data$event.count),0.03934638)# estimated rate= 0.03934638
plot(cyber.data$event.count,Ex)# scatterplot observed vs fitted
cor(cyber.data$event.count,Ex)

# Norm
No.r=rnorm(length(cyber.data$event.count),25.41530,19.82332) # estimated parameters
plot(cyber.data$event.count,No.r)# scatterplot observed vs fitted
cor(cyber.data$event.count,No.r)

# Poisson
Po=rpois(length(cyber.data$event.count),25.4153)# estimated parameters
plot(cyber.data$event.count,Po)# scatterplot observed vs fitted
cor(cyber.data$event.count,Po)

# Negative binomial
neg.bin=rnbinom(length(cyber.data$event.count),size=1.658441, mu=25.415193)# estimated parameters from fitdist
plot(cyber.data$event.count,neg.bin)# scatterplot observed vs fitted
cor(cyber.data$event.count,neg.bin)


# Page 36- Ord plots
library(vcd)
data(Butterfly, package="vcd")
ord <- Ord_plot(Butterfly, main = "Butterfly species collected in Malaya",
                gp=gpar(cex=1), pch=16)

# Repeating process for horsekicks
data(HorseKicks, package="vcd")
nk <- as.vector(HorseKicks)
k <- as.numeric(names(HorseKicks))
nk1 <- c(NA, nk[-length(nk)])
y <- k * nk/nk1
weight = sqrt(pmax(nk, 1) - 1)
(ord.df <- data.frame(k, nk, nk1, y, weight))

coef(lm(y ~ k, weights=weight, data=ord.df))

Ord_plot(HorseKicks,
         main = "Death by horse kicks", gp=gpar(cex=1), pch=16)

# THIS ALSO DOESN'T WORK
nbinom.horse<- fitdist(HorseKicks, "nbinom")

# Poisson
pois.horse<- fitdist(HorseKicks, "pois")# Poisson






# Negative binomial and poisson for butterfly COULD NOT GET IT TO WORK (Butterfly needs to be vector)
nbinom.b<- fitdist(Butterfly, "nbinom")

# Poisson
pois.b<- fitdist(Butterfly, "pois")# Poisson


# Poisson
Po=rpois(length(cyber.data$event.count),25.4153)# estimated parameters
plot(cyber.data$event.count,Po)# scatterplot observed vs fitted
cor(cyber.data$event.count,Po)

# Negative binomial
neg.bin=rnbinom(length(cyber.data$event.count),size=1.658441, mu=25.415193)# estimated parameters from fitdist
plot(cyber.data$event.count,neg.bin)# scatterplot observed vs fitted
cor(cyber.data$event.count,neg.bin)

# Try with new data
Ord_plot(Federalist, main = "Instances of 'may' in Federalist papers",
         gp=gpar(cex=1), pch=16)


# Generate 1000 RV, poisson w/ lambda = 2
pois.test=rpois(1000,2)# estimated parameters
Ord_plot(pois.test, main = "Test: Poisson",
         gp=gpar(cex=1), pch=16)

# Generate 10000 RV, negative binomial
neg.bin.test=rnbinom(10000, size=1.658441, mu=25.415193)
Ord_plot(neg.bin.test, main = "Test: Neg Bin",
         gp=gpar(cex=1), pch=16)

# How to find mean and variance of distribution without estimating from parameters?
#   generate 1000s of RVs for the distribution and find mean and variance

# Austin Ash & Zac Johnson
# ACTS 190- Project
# 10-14-21


# 10-15-21 update- Austin:
#   VERY rough draft of much of the content for the first presentation. much more interpretation is 
#   needed. I began the content for presentation 2 so we can have a backup in case we can't make 
#   the content last 30 mins. Experiencing errors with fitdist() corresponding to the gamma and
#   exponential distributions. I don't think it is related to the numbers, but more likely the
#   data/ data format itself.



# Importing necessary libraries
library(fitdistrplus)
library(actuar)


insurance <- read.csv(file.choose(), header = T)
# file name is insurance.csv
# Source: https://www.kaggle.com/mirichoi0218/insurance

head(insurance)
attach(insurance)

# data is continuous

# We will primarily be looking at the "charges" variable for univariate analysis

# Checking for missing values
sum(is.na(charges)) # No missing values in dataset



### DATA DESCRIPTION

# Data size: 7 columns of data, 1338 rows
# Columns: age, sex, bmi, children, smoker, region, charges

# Data type of "charges" variable- these are the recorded claim amounts
typeof(charges) # Data type is double, it numeric data

# Computing summary statistics for "charges" variable
summary(insurance)

##   charges     
# Min.   : 1122  
# 1st Qu.: 4740  
# Median : 9382  
# Mean   : 13270  
# 3rd Qu.: 16640  
# Max.   : 63770

#   Notably, the values in the "charges" variable are all > 0
#       We will keep this in mind when deciding on a distribution to fit

# mean of 13270 > median of 9382... data is potential right skewed

### PLOTTING "CHARGES" VARIABLE

## histogram
hist(charges,col="purple",main="Histogram Claims",xlab="Amount of Claims",ylab="Freq")
# Data is heavily right skewed

## boxplot
boxplot(charges,col="green",main="BoxPlot of Claims",xlab="Amount")
# more evidence of right skewed

## QQ-plot
qqnorm(charges, col="purple")
qqline(charges)

## PP-plot
# FORGOT HOW TO DO, WILL COME BACK LATER

plotdist(charges, histo = TRUE, demp = TRUE)

descdist(charges, boot = 1000,discrete=FALSE)
## estimated skewness:  1.51588 
## estimated kurtosis:  4.606299 
# Right skewed and heavier tailed than the normal distribution
# According to Cullen and Frey graph, the beta, gamma, and lognormal distributions
#   look like potential options

# We can rule out beta because the beta distribution is constrained to the range of (0,1) and
#   our x values reach far larger than 1

### FITTING DISTRIBUTIONS:
# We will begin by fitting the pareto, gamma, lognormal, and exponential distributions

weib.ins <- fitdist(charges, "weibull")
gam.ins <- fitdist(charges, "gamma")
lnorm.ins <- fitdist(charges, "lnorm")
exp.ins <- fitdist(charges, "exp")

summary(weib.ins)
summary(gam.ins)
summary(lnorm.ins)
summary(exp.ins)



# Error in fitdist(charges, "gamma") : 
#   the function mle failed to estimate the parameters, 
#           with the error code 100
#ALSO
# Error in fitdist(charges, "exp") : 
#   the function mle failed to estimate the parameters, 
#           with the error code 100

# Not sure why these errors are occurring, the data should fit a gamma or exponential



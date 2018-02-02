#Ariq Chowdhury
#BAX 452
#2/1/18
#HW4

#1.22.1
# In Section 1.12.1.2, the reader was reminded that the results of a cross-
# validation are random, due to the random partitioning into training and
# test sets. Try doing several runs of the linear and k-NN code in that 
# section, comparing results.
library(freqparcoord)
data(mlb)

xvalpart <- function(data,p) {
  n <- nrow(data)
  ntrain <- round(p*n)
  trainidxs <- sample(1:n, ntrain, replace=FALSE)
  list(train = data[trainidxs,], valid = data[-trainidxs,])
}

xvallm <- function(data, ycol, predvars, p, meanabs = TRUE) {
  tmp <- xvalpart(data, p)
  train <- tmp$train
  valid <- tmp$valid
  trainy <- train[,ycol]
  trainpreds <- train[,predvars]
  trainpreds <- as.matrix(trainpreds)
  lmout <- lm(trainy ~ trainpreds)
  validpreds <- as.matrix(valid[,predvars])
  predy <- cbind(1,validpreds) %*% coef(lmout)
  realy <- valid[,ycol]
  if (meanabs) return(mean(abs(predy - realy)))
  list(predy = predy, realy = realy)
}

lin1 <- xvallm(mlb,5,c(4,6),2/3)
lin2 <- xvallm(mlb,5,c(4,6),2/3)
lin3 <- xvallm(mlb,5,c(4,6),2/3)
lin4 <- xvallm(mlb,5,c(4,6),2/3)

lin1; lin2; lin3; lin4

#ranges between 13 - 15, off by about 13.4 lbs on average

xvalknn <- function(data, ycol, predvars, k, p, meanabs = TRUE) {
  data <- data[,c(predvars, ycol)]
  ycol <- length(predvars) + 1
  tmp <- xvalpart(data, p)
  train <- tmp$train
  valid <- tmp$valid
  valid <- as.matrix(valid)
  xd <- preprocessx(train[,-ycol],k)
  kout <- knnest(train[,ycol], xd, k)
  predy <- predict(kout, valid[,-ycol], TRUE)
  realy <- valid[,ycol]
  if (meanabs) return(mean(abs(predy - realy)))
  list(predy = predy, realy = realy)
}

library(regtools)
knn1 <- xvalknn(mlb, 5, c(4,6), 25, 2/3)
knn2 <- xvalknn(mlb, 5, c(4,6), 25, 2/3)
knn3 <- xvalknn(mlb, 5, c(4,6), 25, 2/3)
knn4 <- xvalknn(mlb, 5, c(4,6), 25, 2/3)

knn1; knn2; knn3; knn4

#ranges between 13 - 15, off by about 14 lbs on average

#1.22.2
# Extend (1.28) to include interaction terms for age and gender, and age^2
# and gender. Run the new model, and find the estimated effect of being 
# female, for a 32-year-old person with a Master's degree.
data(prgeng)
prgeng$age2 <- prgeng$age^2
edu <- prgeng$educ
prgeng$ms <- as.integer(edu == 14)
prgeng$phd <- as.integer(edu == 16)
prgeng$fem <- prgeng$sex - 1
tmp <- prgeng[edu >= 13,]
pe <- tmp[,c(1,12,9,13,14,15,8)]
pe <- as.matrix(pe)

lm(wageinc ~ age + age2 + wkswrkd + ms + phd + fem, data = prgeng)

femms32 <- lm(wageinc ~ age + age2 + wkswrkd + ms + phd + fem + phd:fem + ms:fem + age:fem + age2:fem, data = prgeng)

femms32$coef[7] + femms32$coef[9] + femms32$coef[10]*32 + femms32$coef[11]*32

# The estimated effect of being female for a 32 yr old w/ a masters degree
# is #36,211.60

#1.22.3
# Consider the bodyfat data mentioned in Section 1.2 Use lm() to form a
# prediction equation for density from the other variables (skipping the)
# first three, and comment on whether the use of indirect methods in this 
# way seems feasible.

bodyfat <- read.csv("bodyfat.csv")
bodyfat2 <- bodyfat[,c(5:18)]
fatlm <- lm(density ~ ., data = bodyfat2)
summary(fatlm)
plot(predict(fatlm),bodyfat2$density, xlab="predicted",ylab="actual")
abline(a=0,b=1)

# scatterplot of predicted vs actuals show that predicting bodyfat from
# indirect varibales is fairly feasible. We have an R^2 of .72, so it's
# relatively accurate, but with some error.

#1.22.4
# In Section 1.19.5.2, we gave this intuitive explanation:
#
#   In other words, the national mean height is a weighted average of the 
#   state means, with the weight for each state being its proportion of
#   the national population. Replace state by gender in the following.
#   
# (a) Write Eglish prose that relates the overall mean height of people
#   and the gender-specific mean heights.
#
# The national mean height is a weighted average of the mean for each gender,
# with the weight for each gender being its proportion of the national population.
# If we were to calculate the national average height and knew the average
# height of females is 64 in and the average weight of males is 70 in, and that
# 51% of the population is female, the national avg would be 64*.51 + 70*.49
#
# (b) Write English prose that relates the overall proportion of people
#   taller than 70 inches to the gender-specific proportions.
#
# The national mean height is a weighted average of the proportion of >70 for each gender,
# with the weight for each gender being its proportion of the national population.
# If we were to calculate the national proportion of >70 and knew the average
# proportion of females >70 is 10% and the proportion of males >70 is 50%, and that
# 51% of the population is female, the national avg would be .1*.51 + .5*.49
#

#2.14.1
# Consider the census data in Section 1.16.1.
# (a) Form an approximate 95% confidence interval for B6 in the model (1.28).
# (b) Form an approximate 95% confidence interval for the gender effect
# for Master's degree holders, B6 + B7, in the model (1.28).

#a
model1 <- lm(wageinc ~ age + age2 + wkswrkd + ms + phd + fem + ms:fem + phd:fem, data = prgeng)
summary(model1)
confint(model1, 'fem', level=0.95)

# 2.5 %    97.5 %
#   fem -11853.68 -8699.915

#b
confint(model1, 'fem', level=0.95) + confint(model1, 'ms:fem', level=0.95)

# 2.5 %    97.5 %
#   fem -19398.6 -9469.501

#2.14.2
# The full bikeshare dataset spans 3 years' time. Our analyses here have
# only used the first year. Extend the analysis in Section 2.8.5 to the full
# data set, adding dummy variables indicating the second and third year.
# Form an approximate 95% confidence interval for the difference between
# the coefficients of these two dummies.
shar <- read.csv("day.csv")
names(shar)[15] <- "reg"
shar$temp2 <- shar$temp^2
shar$clearday <- as.integer(shar$weathersit == 1)
shar$spring <- as.integer(shar$season == 1)
shar$summer <- as.integer(shar$season == 2)
shar$fall <- as.integer(shar$season == 3)
shar$winter <- as.integer(shar$season == 4)
bike <- lm(reg ~ temp+temp2+workingday+clearday+spring+summer+fall,data=shar)
summary(bike)
confint(bike, 'spring', level=0.95) - confint(bike, 'fall', level=0.95)

# This dataset only includes 2 years... so I'll do this for seasons instead
# The difference between spring and fall is somewhere between 544.84 and 716.21 less bikes

#2.14.3
# Suppose we are studying growth patterns in children, at k particular
# ages. Denote the height of the ith child in our sample data at age j by
# Hij , with Hi = (Hi1, ...,Hik)' denoting the data for child i. Suppose the
# population distribution of each Hi is k-variate normal with mean vector mu
# and covariance matrix summation. Say we are interested in successive differences
# in heights, Dij = Hi,j+1 - Hij, j = 1,2, ...,k-1. Define Di = (Di1, ...,Dik)'.
# Explain why each Di is (k-1)-variate normal, and derive matrix expressions
# for the mean vector and covariance matrices.

#2.14.4
# In the simulation in Section 2.9.3, it is claimed that p^2 = 0.50.
# Confirm this through derivation.

simr2 <- function(n,p,nreps) {
  r2s <- vector(length = nreps)
  for (i in 1:nreps) {
    x <- matrix(rnorm(n*p),ncol=p)
    y <- x %*% rep(1,p) + rnorm(n,sd=sqrt(p))
    r2s[i] <- getr2(x,y)
  }
  hist(r2s)
}

getr2 <- function(x,y) {
  smm <- summary(lm(y~x))
  smm$r.squared
}

simr2(250,8,1000)

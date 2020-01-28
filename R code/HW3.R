#Section 13: Regression Analysis

x = c(18,23,25,35,65,54,34,56,72,19,23,42,18,39,37)
y = c(202,186,187,180,156,169,174,172,153,199,193,174,198,183,178)
plot(x,y) 				# make a plot
abline(lm(y ~ x))		 	# plot the regression line
lm(y ~ x) 

#library(UsingR)
lm.result=simple.lm(x,y)
summary(lm.result)

coef(lm.result) 			# or use lm.result[[’coef’]]
lm.res = resid(lm.result) 		# or lm.result[[’resid’]]
summary(lm.res)

#par(mfrow=c(2,2))
plot(lm.result)

es = resid(lm.result) 		# the residuals lm.result
b1 =(coef(lm.result))[['x']] 		# the x part of the coefficients
s = sqrt( sum( es^2 ) / (15-2) )
SE = s/sqrt(sum((x-mean(x))^2))
t = (b1 - (-1) )/SE 			# of course - (-1) = +1
pt(t,13,lower.tail=FALSE) 		# find the right tail for this value of t
				# and 15-2 d.f.

SE = s * sqrt( sum(x^2)/( 15*sum((x-mean(x))^2)))
b0 = 210.04846 			# copy or use
t = (b0 - 220)/SE 			# (coef(lm.result))[[’(Intercept)’]]
pt(t,13,lower.tail=TRUE) 		# use lower tail (220 or less)

## call simple.lm again
simple.lm(x,y,show.ci=TRUE,conf.level=0.90)

lm.result = lm(y ~ x)

summary(lm.result)

plot(x,y)
abline(lm.result)

resid(lm.result)

coef(lm.result)

coef(lm.result)[1]

coef(lm.result)[’x’]

fitted(lm.result) 			# you can abbreviate to just fitted

coefficients(lm.result)

coefficients(summary(lm.result))

coefficients(summary(lm.result))[2,2]

coefficients(summary(lm.result))[’x’,’Std. Error’]

predict(lm.result,data.frame(x= c(50,60)))

predict(lm.result,data.frame(x=sort(x)), 	# as before
  level=.9, interval="confidence")	 	# what is new

plot(x,y)
abline(lm.result)
ci.lwr = predict(lm.result,data.frame(x=sort(x)), level=.9,interval="confidence")[,2]
points(sort(x), ci.lwr,type="l") 		# or use lines()

curve(predict(lm.result,data.frame(x=x), interval="confidence")[,3],add=T)

#Section 14: Multiple Linear Regression

x = 1:10
y = sample(1:100,10)
z = x+y 				# notice no error term -- sigma = 0
lm(z ~ x+y) 			# we use lm() as before
z = x+y + rnorm(10,0,2) 		# now sigma = 2
lm(z ~ x+y)
z = x+y + rnorm(10,0,10) 		# more noise -- sigma = 10
lm(z ~ x+y)

lm(z ~ x+y -1) 			# no intercept beta_0

summary(lm(z ~ x+y ))

library(lattice); data(homeprice);attach(homeprice)
panel.lm = function(x,y) {
  panel.xyplot(x,y)
  panel.abline(lm(y~x)) }
xyplot(sale ~ rooms | neighborhood,panel= panel.lm,data=homeprice)
## too few points in some of the neighborhoods, let’s combine
nbd = as.numeric(cut(neighborhood,c(0,2,3,5),labels=c(1,2,3)))
table(nbd) 			# check that we partitioned well
xyplot(sale ~ rooms | nbd, panel= panel.lm,layout=c(3,1))

summary(lm(sale ~ bedrooms + nbd))

-58.9 + 115.32*(1:3) 		# nbd is 1, 2 or 3

summary(lm(sale ~ bedrooms + nbd + full))

SE = 18.19
t = (28.51 - 15)/SE
t
pt(t,df=25,lower.tail=F)

dist = c(253, 337,395,451,495,534,574)
height = c(100,200,300,450,600,800,1000)
lm.2 = lm(dist ~ height + I(height^2))
lm.3 = lm(dist ~ height + I(height^2) + I(height^3))
lm.2
lm.3

#pts = seq(min(height),max(height),length=100)
quad.fit = 200.211950 + .706182 * pts -0.000341 * pts^2
cube.fit = 155.5 + 1.119 * pts - .001234 * pts^2 + .000000555 * pts^3
plot(height,dist)
lines(pts,quad.fit,lty=1,col="blue")
lines(pts,cube.fit,lty=2,col="red")
legend(locator(1),c("quadratic fit","cubic fit"),lty=1:2,col=c("blue","red"))

summary(lm.3)

pts = seq(min(height),max(height),length=100)
makecube = sapply(pts,function(x) coef(lm.3) %*% x^(0:3))
makesquare = sapply(pts,function(x) coef(lm.2) %*% x^(0:2))
lines(pts,makecube,lty=1)
lines(pts,makesquare,lty=2)

#Section 15: Analysis of Variance

x = c(4,3,4,5,2,3,4,5)
y = c(4,4,5,5,4,5,4,4)
z = c(3,4,2,4,5,5,4,4)
scores = data.frame(x,y,z)
boxplot(scores)

scores = stack(scores) 		# look at scores if not clear
names(scores)

oneway.test(values ~ ind, data=scores, var.equal=T)

df = stack(data.frame(x,y,z)) 	# prepare the data
oneway.test(values ~ ind, data=df,var.equal=T)

anova(lm(values ~ ind, data=df))

kruskal.test(values ~ ind, data=df)

#2. Understanding Naive Bayes Classifier Using R

#Getting started with Naive Bayes
#Install the package
install.packages("e1071")

#Loading the library
library(e1071)
?naiveBayes 		#Ccontains an example implementation of Titanic dataset
#Next load the Titanic dataset
data("Titanic")
str(Titanic)
#Save into a data frame and view it
Titanic_df=as.data.frame(Titanic)
str(Titanic_df)

#Creating data from table
#This will repeat each combination with the frequency of each combination
sum(Titanic_df$Freq)
repeating_sequence=rep.int(seq_len(nrow(Titanic_df)), Titanic_df$Freq)
#Create the dataset by row repetition created
Titanic_dataset=Titanic_df[repeating_sequence,]
#We no longer need the frequency, drop the feature
Titanic_dataset$Freq=NULL

#Fitting the Naive Bayes model
Naive_Bayes_Model=naiveBayes(Survived ~., data=Titanic_dataset)
#What does the model say? Print the model summary
Naive_Bayes_Model

#Prediction on the dataset
NB_Predictions=predict(Naive_Bayes_Model,Titanic_dataset)
#Confusion matrix to check accuracy
table(NB_Predictions,Titanic_dataset$Survived)

#Getting started with Naive Bayes in mlr
#Install the package
install.packages("mlr")
#Loading the library
library(mlr)

#Create a classification task for learning on Titanic Dataset
#and specify the target feature
task = makeClassifTask(data = Titanic_dataset, target = "Survived")
task
selected_model = makeLearner("classif.naiveBayes")
#Train the model
NB_mlr = train(selected_model, task)

#Read the model learned
NB_mlr$learner.model

#Predict on the dataset without passing the target feature
predictions_mlr = as.data.frame(predict(NB_mlr, newdata = Titanic_dataset[,1:3]))
##Confusion matrix to check accuracy
table(predictions_mlr[,1],Titanic_dataset$Survived)

#3. Fitting a Model by Maximum Likelihood

set.seed(1001)
N <- 100
x <- rnorm(N, mean = 3, sd = 2)
mean(x)
sd(x)

LL <- function(mu, sigma) {
  R = dnorm(x, mu, sigma)
  #
  -sum(log(R))
}

library(stats4)
mle(LL, start = list(mu = 1, sigma=1))

dnorm(x, 1, -1)

mle(LL, start = list(mu = 1, sigma=1), method = "L-BFGS-B", lower = c(-Inf, 0), upper = c(Inf, Inf))

LL <- function(mu, sigma) {
  R = suppressWarnings(dnorm(x, mu, sigma))
  #
  -sum(log(R))
}
mle(LL, start = list(mu = 1, sigma=1))

mle(LL, start = list(mu = 0, sigma=1))

x <- runif(N)
y <- 5 * x + 3 + rnorm(N)

fit <- lm(y ~ x)
summary(fit)

plot(x, y)
abline(fit, col = "red")

LL <- function(beta0, beta1, mu, sigma) {
  # Find residuals
  #
  R = y - x * beta1 - beta0
  #
  # Calculate the likelihood for the residuals (with mu and sigma as parameters)
  #
  R = suppressWarnings(dnorm(R, mu, sigma))
  #
  # Sum the log likelihoods for all of the data points
  #
  -sum(log(R))
}

LL <- function(beta0, beta1, mu, sigma) {
  R = y - x * beta1 - beta0
  #
  R = suppressWarnings(dnorm(R, mu, sigma, log = TRUE))
  #
  -sum(R)
}

fit <- mle(LL, start = list(beta0 = 3, beta1 = 1, mu = 0, sigma=1))

fit <- mle(LL, start = list(beta0 = 5, beta1 = 3, mu = 0, sigma=1))
# OK
fit

fit <- mle(LL, start = list(beta0 = 4, beta1 = 2, mu = 0, sigma=1))
fit

summary(fit)

fit <- mle(LL, start = list(beta0 = 2, beta1 = 1.5, sigma=1), fixed = list(mu = 0), nobs = length(y))
summary(fit)

AIC(fit)
BIC(fit)
logLik(fit)

install.packages("bbmle")
library(bbmle)
fit <- mle2(LL, start = list(beta0 = 3, beta1 = 1, mu = 0, sigma = 1))
summary(fit)

#4. Density Estimation: “과제 03. Density Estimation Using R.pdf” 튜토리얼

#7.1 Introduction
#7.2 Density Estimation
x <- c(0, 1, 1.1, 1.5, 1.9, 2.8, 2.9, 3.5)
n <- length(x)
xgrid <- seq(from = min(x) - 1, to = max(x) + 1, by = 0.01)
h <- 0.4
bumps <- sapply(x, function(a) gauss((xgrid - a)/h)/(n * h))

#7.3 Analysis Using R
logL <- function(param, x) {
  d1 <- dnorm(x, mean = param[2], sd = param[3])
  d2 <- dnorm(x, mean = param[4], sd = param[5])
  -sum(log(param[1] * d1 + (1 - param[1]) * d2))
}
startparam <- c(p = 0.5, mu1 = 50, sd1 = 3, mu2 = 80, sd2 = 3)
opp <- optim(startparam, logL, x = faithful$waiting)
rec <- function(x) (abs(x) < 1) * 0.5
tri <- function(x) (abs(x) < 1) * (1 - abs(x))
gauss <- function(x) 1/sqrt(2*pi) * exp(-(x^2)/2)
x <- seq(from = -3, to = 3, by = 0.001)
plot(x, rec(x), type = "l", ylim = c(0,1), lty = 1, ylab = expression(K(x)))
lines(x, tri(x), lty = 2)
lines(x, gauss(x), lty = 3)
legend(-3, 0.8, legend = c("Rectangular", "Triangular", "Gaussian"), lty = 1:3,
  title = "kernel functions", bty = "n")

plot(xgrid, rowSums(bumps), ylab = expression(hat(f)(x)), type = "l", xlab = "x", lwd = 2)
rug(x, lwd = 2)
out <- apply(bumps, 2, function(b) lines(xgrid, b))
  #method = "L-BFGS-B",
  #lower = c(0.01, rep(1, 4)),
  #upper = c(0.99, rep(200, 4)))
opp

epa <- function(x, y)
  ((x^2 + y^2) < 1) * 2/pi * (1 - x^2 - y^2)
x <- seq(from = -1.1, to = 1.1, by = 0.05)
epavals <- sapply(x, function(a) epa(a, x))
persp(x = x, y = x, z = epavals, xlab = "x", ylab = "y",
  zlab = expression(K(x, y)), theta = -35, axes = TRUE,
  box = TRUE)

data("faithful", package = "datasets")
x <- faithful$waiting
layout(matrix(1:3, ncol = 3))
hist(x, xlab = "Waiting times (in min.)", ylab = "Frequency",
  probability = TRUE, main = "Gaussian kernel",
  border = "gray")
lines(density(x, width = 12), lwd = 2)
rug(x)
hist(x, xlab = "Waiting times (in min.)", ylab = "Frequency",
  probability = TRUE, main = "Rectangular kernel",
  border = "gray")
lines(density(x, width = 12, window = "rectangular"), lwd = 2)
rug(x)
hist(x, xlab = "Waiting times (in min.)", ylab = "Frequency",
  probability = TRUE, main = "Triangular kernel",
  border = "gray")
lines(density(x, width = 12, window = "triangular"), lwd = 2)
rug(x)

#install.packages("KernSmooth")
#install.packages("HSAUR")
library("KernSmooth")
data("CYGOB1", package = "HSAUR")
CYGOB1d <- bkde2D(CYGOB1, bandwidth = sapply(CYGOB1, dpik))
contour(x = CYGOB1d$x1, y = CYGOB1d$x2, z = CYGOB1d$fhat,
  xlab = "log surface temperature",
  ylab = "log light intensity")

persp(x = CYGOB1d$x1, y = CYGOB1d$x2, z = CYGOB1d$fhat,
  xlab = "log surface temperature",
  ylab = "log light intensity",
  zlab = "estimated density",
  theta = -35, axes = TRUE, box = TRUE)

#install.packages("mclust")
library("mclust")
mc <- Mclust(faithful$waiting)
mc

mc$parameters$mean

sqrt(mc$parameters$variance$sigmasq)

#install.packages("flexmix")
library("flexmix")
fl <- flexmix(waiting ~ 1, data = faithful, k = 2)

parameters(fl, component = 1)

parameters(fl, component = 2)

library("boot")
fit <- function(x, indx) {
  a <- Mclust(x[indx], minG = 2, maxG = 2)$parameters
  if (a$pro[1] < 0.5) return(c(p = a$pro[1], mu1 = a$mean[1]))
}

opar <- as.list(opp$par)
rx <- seq(from = 40, to = 110, by = 0.1)
d1 <- dnorm(rx, mean = opar$mu1, sd = opar$sd1)
d2 <- dnorm(rx, mean = opar$mu2, sd = opar$sd2)
f <- opar$p * d1 + (1 - opar$p) * d2
hist(x, probability = TRUE, xlab = "Waiting times (in min.)",
  border = "gray", xlim = range(rx), ylim = c(0, 0.06),
  main = "")
lines(rx, f, lwd = 2)
lines(rx, dnorm(rx, mean = mean(x), sd = sd(x)), lty = 2, lwd = 2)
legend(50, 0.06, lty = 1:2, bty = "n",
  legend = c("Fitted two-component mixture density", "Fitted single normal density"))

  #mu2 = a$mean[2]))
  #return(c(p = 1 - a$pro[1], mu1 = a$mean[2],
  #mu2 = a$mean[1]))
#}

bootpara <- boot(faithful$waiting, fit, R = 1000)

boot.ci(bootpara, type = "bca", index = 1)

boot.ci(bootpara, type = "bca", index = 2)

boot.ci(bootpara, type = "bca", index = 3)

bootplot <- function(b, index, main = "") {
  dens <- density(b$t[,index])
  ci <- boot.ci(b, type = "bca", index = index)$bca[4:5]

layout(matrix(1:2, ncol = 2))
bootplot(bootpara, 2, main = expression(mu[1]))
bootplot(bootpara, 3, main = expression(mu[2]))

  est <- b$t0[index]
  plot(dens, main = main)
  y <- max(dens$y) / 10
  segments(ci[1], y, ci[2], y, lty = 2)
  points(ci[1], y, pch = "(") 
  points(ci[2], y, pch = ")")
  points(est, y, pch = 19)
}

#5. Understanding the EM (Expectation Maximization) Algorithm

set.seed(123) ## ensures we all see the same output
trueMean <- 10 ## suppose this true mean is unknown
n <- 20
x <- rnorm(n, mean = trueMean) ## sample data from a Normal distribution
print(x)
hist(x, col = "lavender")
abline(v = mean(x), col = "red", lwd = 2) ## highlight sample mean

dat <- c(1,2,3) # mean of ‘dat’ is 2
rbind(prod(dnorm(dat, mean=1.5, sd=1)),
  prod(dnorm(dat, mean=2, sd=1)),
  prod(dnorm(dat, mean=2.5, sd=1)))

dat <- c(1,2,3)
mean_grid <- seq(0, 4, by=0.1) ## values of the mean to check the likelihood at
myLikelihood <- rep(0, length(mean_grid) )
for( i in seq_along( myLikelihood ) ) {
  myLikelihood[i] <- prod( dnorm( dat, mean = mean_grid[i], sd=1 ) )
  }
plot( myLikelihood ~ mean_grid, type="b" )

set.seed(123)
tau_1_true <- 0.25
x <- y <- rep(0,1000)
for( i in 1:1000 ) {
  if( runif(1) < tau_1_true ) {
    x[i] <- rnorm(1, mean=1) # 25%의 확률로 head가 나오면 정규분포 D1에서 샘플링
    y[i] <- "heads"
  } else {
    x[i] <- rnorm(1, mean=7) # 75%의 확률로 tail이 나오면 정규분포 D2에서 샘플링
    y[i] <- "tails"
  }
}
library(“lattice”)
densityplot( ~x, par.settings = list( plot.symbol = list(col=as.factor(y))))

print( x[1] )			## 랜덤변수 값으로 변동
dnorm( x[1], mean=0 )		## x[1]의 값에 따라 변동
dnorm( x[1], mean=1 ) 		## x[1]의 값에 따라 변동

tau_1 <- 0.5 ## our initial believed proportion from D1, chosen arbitrarily
tau_2 <- 0.5 ## our initial believed proportion from D2, chosen arbitrarily
T_1 <- tau_1 * dnorm( x[1], mean=0 )
T_2 <- tau_2 * dnorm( x[1], mean=1 )
print( T_1 ) ## x[1]의 값에 따라 변동
print( T_2 ) ## x[1]의 값에 따라 변동
T_1 / (T_1 + T_2) ## x[1]의 값에 따라 변동

T_1 <- tau_1 * dnorm( x, mean=0 )
T_2 <- tau_2 * dnorm( x, mean=1 )
head( T_1 / (T_1 + T_2) )

P_1 <- T_1 / (T_1 + T_2)
P_2 <- T_2 / (T_1 + T_2)
mu_1 <- sum( P_1 * x ) / sum(P_1)
mu_2 <- sum( P_2 * x ) / sum(P_2)
c(mu_1, mu_2)

## set the initial guesses for the distribution parameters
mu_1 <- 0
mu_2 <- 1
## as well as the latent variable parameters
tau_1 <- 0.5
tau_2 <- 0.5
for( i in 1:10 ) {
  ## Given the observed data, as well as the distribution parameters,
  ## what are the latent variables?
  T_1 <- tau_1 * dnorm( x, mu_1 )
  T_2 <- tau_2 * dnorm( x, mu_2 )
  P_1 <- T_1 / (T_1 + T_2)
  P_2 <- T_2 / (T_1 + T_2) ## note: P_2 = 1 - P_1
  tau_1 <- mean(P_1)
  tau_2 <- mean(P_2)
  ## Given the observed data, as well as the latent variables,
  ## what are the population parameters?
  mu_1 <- sum( P_1 * x ) / sum(P_1)
  mu_2 <- sum( P_2 * x ) / sum(P_2)
  ## print the current estimates
  print( c(mu_1, mu_2, mean(P_1)) )
}

#install.packages("mixtools") ## if you don't have it already.
library("mixtools")
myEM <- normalmixEM( x, mu = c(0,1), sigma=c(1,1), sd.constr=c(1,1) )
## number of iterations= 7
myEM$mu ## the distribution means
## [1] 0.9866 7.0059
myEM$lambda ## the mixing probabilities
## [1] 0.2435 0.7565

set.seed(123)
tau_true <- 0.25
x <- y <- rep(0,1000)
for( i in 1:1000 ) {
  if( runif(1) < tau_true ) {
    x[i] <- rnorm(1, mean=1)
    y[i] <- "heads"
  } else {
    x[i] <- rnorm(1, mean=4)
    y[i] <- "tails"
  }
}
densityplot( ~x, par.settings =list( plot.symbol=list(col=as.factor(y))))

mu_1 <- 0
mu_2 <- 1
tau_1 <- 0.5
tau_2 <- 0.5
for( i in 1:30 ) {
  ## Given the observed data, as well as the distribution parameters,
  ## what are the latent variables?
  T_1 <- tau_1 * dnorm( x, mu_1 )
  T_2 <- tau_2 * dnorm( x, mu_2 )
  P_1 <- T_1 / (T_1 + T_2)
  P_2 <- T_2 / (T_1 + T_2) ## note: P_2 = 1 - P_1
  tau_1 <- mean(P_1)
  tau_2 <- mean(P_2)
  ## Given the observed data, as well as the latent variables,
  ## what are the population parameters?
  mu_1 <- sum( P_1 * x ) / sum(P_1)
  mu_2 <- sum( P_2 * x ) / sum(P_2)
  ## print the current estimates
  print( c(mu_1, mu_2, mean(P_1)) )
}
myEM <- normalmixEM( x, mu = c(0,1), sigma=c(1,1), sd.constr=c(1,1) )
## number of iterations= 21
myEM$mu ## the means of the two distributions
myEM$lambda ## the mixing probabilities
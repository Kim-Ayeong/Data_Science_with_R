#< Section 3: Univariate Data >

x=c("Yes","No","No","Yes","Yes")
table(x)

x=c("Yes","No","No","Yes","Yes")
x 					# print out values in x
factor(x)				# print out value in factor(x)

beer = scan()
3 4 1 1 3 4 3 3 1 3 2 1 2 1 2 3 2 3 1 1 1 1 4 3 1
 
barplot(beer) 				# this isn't correct
barplot(table(beer)) 			# Yes, call with summarized data
barplot(table(beer)/length(beer)) 	# divide by n for proportion

table(beer)/length(beer)

beer.counts = table(beer) 		# store the table result
pie(beer.counts) 			# first pie -- kind of dull
names(beer.counts) = c("domestic\n can","Domestic\n bottle", "Microbrew","Import") 	# give names
pie(beer.counts) 			# prints out names
pie(beer.counts,col=c("purple","green2","cyan","white")) 				# now with colors

sals = scan() 				# read in with scan
12 .4 5 2 50 8 3 1 4 0.25
 
mean(sals) 				# the average
var(sals) 				# the variance
sd(sals) 				# the standard deviation
median(sals) 				# the median
fivenum(sals) 				# min, lower hinge, Median, upper hinge, max
summary(sals)

data=c(10, 17, 18, 25, 28, 28)
summary(data)
quantile(data,.25)
quantile(data,c(.25,.75)) 		# two values of p at once

sort(sals)
fivenum(sals) 				# note 1 is the 3rd value, 8 the 8th.
summary(sals) 				# note 3.25 value is 1/4 way between 1 and 2

mean(sals,trim=1/10) 			# trim 1/10 off top and bottom
mean(sals,trim=2/10)

IQR(sals)

mad(sals)

median(abs(sals - median(sals))) 	# without normalizing constant
median(abs(sals - median(sals)))*1.4826

scores = scan()
2 3 16 23 14 12 4 13 2 0 0 0 6 28 31 14 4 8 2 5
 
apropos("stem") 			# What exactly is the name?
stem(scores)

stem(scores,scale=2)

sals = c(12, .4, 5, 2, 50, 8, 3, 1, 4, .25) 	# enter data
cats = cut(sals,breaks=c(0,1,5,max(sals))) 	# specify the breaks
cats 						# view the values
table(cats) 					# organize
levels(cats) = c("poor","rich","rolling in it") # change labels
table(cats)

x=scan()
29.6 28.2 19.6 13.7 13.0 7.8 3.4 2.0 1.9 1.0 0.7 0.4 0.4 0.3 0.3
0.3 0.3 0.3 0.2 0.2 0.2 0.1 0.1 0.1 0.1 0.1
 
hist(x) 					# frequencies
hist(x,probability=TRUE) 			# proportions (or probabilities)
rug(jitter(x)) 					# add tick marks

hist(x,breaks=10) 				# 10 breaks, or just hist(x,10)
hist(x,breaks=c(0,1,2,3,4,5,10,20,max(x))) 	# specify break points

library("UsingR") 				# read in library for these notes
data(movies) 					# read in data set for gross.
names(movies)
attach(movies) 					# to access the names above
boxplot(current,main="current receipts",horizontal=TRUE)
boxplot(gross,main="gross receipts",horizontal=TRUE)
detach(movies) 					# tidy up

#install.packages("ts")
library("ts") 					# load the library
data("lynx") 					# load the data
summary(lynx) 					# Just what is lynx?

x = c(.314,.289,.282,.279,.275,.267,.266,.265,.256,.250,.249,.211,.161)
tmp = hist(x) 					# store the results
lines(c(min(tmp$breaks),tmp$mids,max(tmp$breaks)),c(0,tmp$counts,0),type="l")

data(faithful)
attach(faithful) 				# make eruptions visible
hist(eruptions,15,prob=T) 			# proportions, not frequencies
lines(density(eruptions)) 			# lines makes a curve, default bandwidth
lines(density(eruptions,bw="SJ"),col='red') 	# Use SJ bandwidth, in red

#< Section 4: Bivariate Data >

smokes = c("Y","N","N","Y","N","Y","Y","Y","N","Y")
amount = c(1,2,2,3,3,1,2,1,3,2)
table(smokes,amount)

tmp=table(smokes,amount) 			# store the table
old.digits = options("digits") 			# store the number of digits
options(digits=3) 				# only print 3 decimal places
prop.table(tmp,1) 				# the rows sum to 1 now
prop.table(tmp,2) 				# the columns sum to 1 now
prop.table(tmp)					# all the numbers sum to 1
options(digits=old.digits) 			# restore the number of digits

barplot(table(smokes,amount))
barplot(table(amount,smokes))
smokes=factor(smokes) 				# for names
barplot(table(smokes,amount), 
  beside=TRUE, 					# put beside not stacked
  legend.text=T) 				# add legend
barplot(table(amount,smokes),main="table(amount,smokes)", 
  beside=TRUE,
  legend.text=c("less than 5","5-10","more than 10"))

prop = function(x) x/sum(x)

apply(x,2,prop)

t(apply(x,1,prop))

x = c(5, 5, 5, 13, 7, 11, 11, 9, 8, 9)
y = c(11, 8, 4, 5, 9, 5, 10, 5, 4, 10)
boxplot(x,y)

amount = scan()
5 5 5 13 7 11 11 9 8 9 11 8 4 5 9 5 10 5 4 10
 
category = scan()
1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2
 
boxplot(amount ~ category) 			# note the tilde

library("UsingR"); data(home) 			# read in dataset home
attach(home)
names(home)
boxplot(scale(old),scale(new)) 			# make boxplot after scaling each
detach(home)

stripchart(scale(old),scale(new))

simple.violinplot(scale(old),scale(new))

data(home); attach(home)
plot(old,new)
detach(home)

data(homedata)
attach(homedata)
plot(old,new)
detach(homedata)

x = 1:2; y = c(2,4); df = data.frame(x=x,y=y)
ls() 				# list all the varibles known
rm(y) 				# delete the y variable
attach(df) 			# attach the data frame
ls()		 		# y is visible, but doesn't show up
ls(pos=2) 			# y is in position 2 from being attached
y 				# y is visible because df is attached
x 				# which x did we find, x or df[['x']]
x=c(1,3) 			# assign to x
df 				# not the x in df
detach(df)
x 				# assigned to real x variable
y

data(home); attach(home)
x = old 			# use generic variable names
y = new	 			# for illustration only.
plot(x,y)
abline(lm(y ~ x))
detach(home)

data(home); attach(home)
x = old; y = new
simple.lm(x,y)
detach(home)

lm.res = simple.lm(x,y) 	# store the answers in lm.res
coef(lm.res)
coef(lm.res)[1] 		# first one, use [2] for second

simple.lm(x,y,show.residuals=TRUE)

lm.res = simple.lm(x,y)
the.residuals = resid(lm.res) 	# how to get residuals
plot(the.residuals)

cor(x,y) 			# to find R
cor(x,y)^2 			# to find R^2

rank(c(2,3,5,7,11)) 		# already in order
rank(c(5,3,2,7,11)) 		# for example, 5 is 3rd largest
rank(c(5,5,2,7,5)) 		# ties have ranks averaged (2+3+4)/3=3

cor(rank(x),rank(y))

cor.sp <- function(x,y) cor(rank(x),rank(y))

cor.sp(x,y)

data("florida") 		# or read.table on florida.txt
names(florida)
attach(florida) 		# so we can get at the names BUSH, ...
simple.lm(BUSH,BUCHANAN)
detach(florida) 		# clean up

identify(BUSH,BUCHANAN,n=2) 	# n=2 gives two points

BUSH[50]
BUCHANAN[50]
florida[50,]

simple.lm(BUSH[-50],BUCHANAN[-50])

65.57350 + 0.00348 * BUSH[50]

simple.lm(BUSH[-50],BUCHANAN[-50],pred=BUSH[50])

simple.lm(BUSH,BUCHANAN)
abline(65.57350,0.00348) 	# numbers from above

library(MASS) 			# read in the external library
attach(florida)
plot(BUSH,BUCHANAN) 		# a scatter plot
abline(lm(BUCHANAN ~ BUSH),lty="1") 			# lty sets line type
abline(rlm(BUCHANAN ~ BUSH),lty="2")
legend(locator(1),legend=c('lm','rlm'),lty=1:2)  	# add legend
detach(florida) 		# tidy up

plot(BUSH,BUCHANAN)
abline(rlm(BUCHANAN ~ BUSH),lty='1')
abline(rlm(BUCHANAN[-50] ~ BUSH[-50]),lty='2')

x=seq(0,4,by=.1) 		# create the x values
plot(x,x^2,type="l") 		# type="l" to make line

curve(x^2,0,4)

miles = (0:8)*4 		# 0 4 8 ... 32
tread = scan()
394 329 291 255 229 204 179 163 150
 
plot(miles,tread) 		# make the scatterplot
abline(lm(tread ~ miles))
abline(360,-7.3)
points(miles,360 - 7.3*miles,type="l")
lines(miles,360 - 7.3*miles)
curve(360 - 7.3*x,add=T) 	# add a function of x

#< Section 5: Multivariate Data >

weight = c(150, 135, 210, 140)
height = c(65, 61, 70, 65)
gender = c("Fe","Fe","M","Fe")
study = data.frame(weight,height,gender) 	# make the data frame
study

study = data.frame(w=weight,h=height,g=gender)

row.names(study)<-c("Mary","Alice","Bob","Judy")

study
rm(weight) 			# clean out an old copy
weight
attach(study)
weight

study[,'weight'] 		# all rows, just the weight column
study[,1] 			# all rows, just the first column
study[,1:2]

study['Mary',]
study['Mary','weight']

study$weight 			# using $
study[['weight']] 		# using the name.
study[['w']] 			# unambiguous shortcuts are okay
study[[1]] 			# by position

study[study$gender == 'Fe', ] 	# use $ to access gender via a list

data(PlantGrowth)
PlantGrowth

attach(PlantGrowth)
weight.ctrl = weight[group == "ctrl"]

unstack(PlantGrowth)

boxplot(unstack(PlantGrowth))

boxplot(weight ~ group)

library(MASS); data(Cars93); attach(Cars93) 
## make some categorical variables using cut
price = cut(Price,c(0,12,20,max(Price)))
levels(price)=c("cheap","okay","expensive")
mpg = cut(MPG.highway,c(0,20,30,max(MPG.highway)))
levels(mpg) = c("gas guzzler","okay","miser")
## now look at the relationships
table(Type)
table(price,Type)
table(price,Type,mpg)

barplot(table(price,Type),beside=T) 		# the price by different types
barplot(table(Type,price),beside=T) 		# type by different prices

y=rnorm(1000) 					# 1000 random numbers
f=factor(rep(1:10,100)) 			# the number 1,2...10 100 times
boxplot(y ~ f,main="Boxplot of normal random data with model notation")

x = rnorm(100)
y = factor(rep(1:10,10))
stripchart(x ~ y)

par(mfrow=c(1,3)) 				# 3 graphs per page
data(InsectSprays) 				# load in the data
boxplot(count ~ spray, data = InsectSprays, col = "lightgray")
simple.violinplot(count ~ spray, data = InsectSprays, col = "lightgray")
simple.densityplot(count ~ spray, data = InsectSprays)

plot(x,y) 					# simple scatterplot
points(x,y,pch="2") 				# plot these with a triangle

data("ToothGrowth")
attach(ToothGrowth)
plot(len ~ dose,pch=as.numeric(supp))
## click mouse to add legend.
tmp = levels(supp) 				# store for a second
legend(locator(1),legend=tmp,pch=1:length(tmp))
detach(ToothGrowth)

data(emissions) 				# or read in from dataset
attach(emissions)
simple.scatterplot(perCapita,CO2)
title("GDP/capita vs. CO2 emissions 1999")
detach(emissions)

pairs(emissions)

histogram( ~ Max.Price | Cylinders , data = Cars93)

bwplot( ~ Max.Price | Cylinders , data = Cars93)

attach(Cars93) 					# don't need data = Cars93 now
xyplot(MPG.highway ~ Fuel.tank.capacity | Type)
## plot with a regression line
## first define a regression line drawing function
plot.regression = function(x,y) {
  panel.xyplot(x,y)
  panel.abline(lm(y~x))
}
trellis.device(bg="white") 			# set background to white.
xyplot(MPG.highway ~ Fuel.tank.capacity | Type, panel = plot.regression)

#< Section 6: Random Data >

sample(1:6,10,replace=T)

RollDie = function(n) sample(1:6,n,replace=T)
RollDie(5)

runif(1,0,2) 					# time at light
runif(5,0,2) 					# time at 5 lights
runif(5) 					# 5 random numbers in [0,1]

x=runif(100) 					# get the random numbers
hist(x,probability=TRUE,col=gray(.9),main="uniform on [0,1]")
curve(dunif(x,0,1),add=T)

rnorm(1,100,16) 				# an IQ score
rnorm(1,mean=280,sd=10)				# how long for a baby (10 days early)

x=rnorm(100)
hist(x,probability=TRUE,col=gray(.9),main="normal mu=0,sigma=1")
curve(dnorm(x),add=T)
## also for IQs using rnorm(100,mean=100,sd=16)

n=1; p=.5 					# set the probability
rbinom(1,n,p) 					# different each time
rbinom(10,n,p) 					# 10 different such numbers

n = 10; p=.5
rbinom(1,n,p) 					# 6 successes in 10 trials
rbinom(5,n,p) 					# 5 binomial number

n=5; p=.25 					# change as appropriate
x=rbinom(100,n,p) 				# 100 random numbers
hist(x,probability=TRUE,)
## use points, not curve as dbinom wants integers only for x
xvals=0:n;points(xvals,dbinom(xvals,n,p),type="h",lwd=3)
points(xvals,dbinom(xvals,n,p),type="p",lwd=3)

x=rexp(100,1/2500)
hist(x,probability=TRUE,col=gray(.9),main="exponential mean=2500")
curve(dexp(x,1/2500),add=T)

## Roll a die
sample(1:6,10,replace=TRUE)  		# no sixes!
## toss a coin
sample(c("H","T"),10,replace=TRUE)
## pick 6 of 54 (a lottery)
sample(1:54,6) 				# no replacement
## pick a card. (Fancy! Uses paste, rep)
cards = paste(rep(c("A",2:10,"J","Q","K"),4),c("H","D","S","C"))
sample(cards,5) 			# a pair of jacks, no replacement
## roll 2 die. Even fancier
dice = as.vector(outer(1:6,1:6,paste))
sample(dice,5,replace=TRUE) 		# replace when rolling dice

data(faithful) 				# part of R's base
names(faithful) 			# find the names for faithful
eruptions = faithful[['eruptions']] 	# or attach and detach faithful
sample(eruptions,10,replace=TRUE)
hist(eruptions,breaks=25) 		# the dataset
## the bootstrap sample
hist(sample(eruptions,100,replace=TRUE),breaks=25)

pnorm(.7) 				# standard normal
pnorm(.7,1,1) 				# normal mean 1, std 1

pnorm(.7,lower.tail=F)

qnorm(.75)

x = rnorm(5,100,16)
x
z = (x-100)/16
z

pnorm(z)
pnorm(x,100,16) 			# enter in parameters

#< Section 7: Simulations >

n=10; p=.25; S= rbinom(1,n,p)
(S - n*p)/sqrt(n*p*(1-p))

n=10; p=.25; S=rbinom(100,n,p)
X = (S - n*p)/sqrt(n*p*(1-p)) 		# has 100 random numbers

hist(X,prob=T)

results =numeric(0) 			# a place to store the results
for (i in 1:100) { 			# the for loop
  S = rbinom(1,n,p) 			# just 1 this time
  results[i]=(S- n*p)/sqrt(n*p*(1-p)) 	# store the answer
}

primes=c(2,3,5,7,11);			## loop over indices of primes with this
for(i in 1:5) print(primes[i])		## or better, loop directly
for(i in primes) print(i)

results = c();
mu = 0; sigma = 1
for(i in 1:200) {
  X = rnorm(100,mu,sigma) 		# generate random data
  results[i] = (mean(X) - mu)/(sigma/sqrt(100))
}
hist(results,prob=T)

x = rnorm(100,0,1);qqnorm(x,main='normal(0,1)'); qqline(x)
x = rnorm(100,10,15);qqnorm(x,main='normal(10,15)'); qqline(x)
x = rexp(100,1/10);qqnorm(x,main='exponential mu=10'); qqline(x)
x = runif(100,0,1);qqnorm(x,main='unif(0,1)'); qqline(x)

f = function () {
  S = rbinom(1,n,p)
  (S- n*p)/sqrt(n*p*(1-p))
}

x=simple.sim(100,f)
hist(x)

f = function(n=100,p=.5) {
  S = rbinom(1,n,p)
  (S- n*p)/sqrt(n*p*(1-p))
}

simple.sim(1000,f,100,.5)

the.range = function (x) max(x) - min(x)

find.IQR = function(x) {
  five.num = fivenum(x) 		# for Tukey's summary
  five.num[4] - five.num[2]
}

x = rnorm(100) 				# some sample data
find.IQR 				# oops! no argument. Prints definition.
find.IQR(x) 				# this is better

f = function(n=100,mu=0,sigma=1) {
  nos = rnorm(n,mu,sigma)
  (mean(nos)-mu)/(sigma/sqrt(n))
}
simulations = simple.sim(100,f,100,5,5)
hist(simulations,breaks=10,prob=TRUE)

f = function(n=100,mu=10) (mean(rexp(n,1/mu))-mu)/(mu/sqrt(n))

xvals = seq(-3,3,.01) 				# for the density plot
hist(simple.sim(100,f,1,10),probability=TRUE,main="n=1",col=gray(.95))
points(xvals,dnorm(xvals,0,1),type="l") 	# plot normal curve

#< Section 8: Exploratory Data Analysis >

data(homedata) 				# from simple package
attach(homedata)
hist(y1970); hist(y2000) 		# make two histograms
detach(homedata) 			# clean up

attach(homedata)
simple.eda(y1970); simple.eda(y2000)
detach(homedata) 			# clean up

data(exec.pay) 				# or read in from file
simple.eda(exec.pay)

log.exec.pay = log(exec.pay[exec.pay >0])/log(10) # 0 is a problem
simple.eda(log.exec.pay)

data(ewr)
names(ewr) 				# only 3-10 are raw data
airnames = names(ewr) 			# store them for later
ewr.actual = ewr[,3:10] 		# get the important columns
boxplot(ewr.actual)

par(mfrow=c(2,4)) 			# 2 rows 4 columns
attach(ewr)
for(i in 3:10) boxplot(ewr[,i] ~ as.factor(inorout),main=airnames[i])
detach(ewr)
par(mfrow=c(1,1)) 			# return graphics as is (or close window)

## symmetric: short, regular then long
X=runif(100);boxplot(X,horizontal=T,bty=n)
X=rnorm(100);boxplot(X,horizontal=T,bty=n)
X=rt(100,2);boxplot(X,horizontal=T,bty=n)
## skewed: short, regular then long
# triangle distribution
X=sample(1:6,100,p=7-(1:6),replace=T);boxplot(X,horizontal=T,bty=n)
X=abs(rnorm(200));boxplot(X,horizontal=T,bty=n)
X=rexp(200);boxplot(X,horizontal=T,bty=n)

#< Section 9: Condence Interval Estimation >

alpha = c(0.2,0.1,0.05,0.001)
zstar = qnorm(1 - alpha/2)
zstar

2*(1-pnorm(zstar))

m = 50; n=20; p = .5; 			# toss 20 coins 50 times
phat = rbinom(m,n,p)/n 			# divide by n for proportions
SE = sqrt(phat*(1-phat)/n) 		# compute SE
alpha = 0.10;zstar = qnorm(1-alpha/2)
matplot(rbind(phat - zstar*SE, phat + zstar*SE), rbind(1:m,1:m),type="l",lty=1)
abline(v=p) 				# draw line for p=0.5

prop.test(42,100)

prop.test(42,100,conf.level=0.90)

## define a function
simple.z.test = function(x,sigma,conf.level=0.95) {
  n = length(x);xbar=mean(x)
  alpha = 1 - conf.level
  zstar = qnorm(1-alpha/2)
  SE = sigma/sqrt(n)
  xbar + c(-zstar*SE,zstar*SE)
}
## now try it
simple.z.test(x,1.5)

t.test(x)

x=rnorm(100);y=rt(100,9)
boxplot(x,y)
qqnorm(x);qqline(x)
qqnorm(y);qqline(y)

xvals=seq(-4,4,.01)
plot(xvals,dnorm(xvals),type="l")
for(i in c(2,5,10,20,50)) points(xvals,dt(xvals,df=i),type="l",lty=i)

x = c(110, 12, 2.5, 98, 1017, 540, 54, 4.3, 150, 432)
wilcox.test(x,conf.int=TRUE)

#< Section 10: Hypothesis Testing >

prop.test(42,100,p=.5)

prop.test(420,1000,p=.5)

## Compute the t statistic. Note we assume mu=25 under H_0
xbar=22;s=1.5;n=10
t = (xbar-25)/(s/sqrt(n))
t
## use pt to get the distribution function of t
pt(t,df=n-1)

x = c(12.8,3.5,2.9,9.4,8.7,.7,.2,2.8,1.9,2.8,3.1,15.8)
stem(x)

wilcox.test(x,mu=5,alt="greater")

x = c(12.8,3.5,2.9,9.4,8.7,.7,.2,2.8,1.9,2.8,3.1,15.8)
simple.median.test(x,median=5) 		# accept
simple.median.test(x,median=10) 	# reject

#< Section 11: Two-sample tests >

prop.test(c(45,56),c(45+35,56+47))

x = c(15, 10, 13, 7, 9, 8, 21, 9, 14, 8)
y = c(15, 14, 12, 8, 14, 7, 16, 10, 15, 12)
t.test(x,y,alt="less",var.equal=TRUE)

t.test(x,y,alt="less")

x = c(3, 0, 5, 2, 5, 5, 5, 4, 4, 5)
y = c(2, 1, 4, 1, 4, 3, 3, 2, 3, 5)
t.test(x,y,paired=TRUE)

t.test(x,y)

data(ewr) 			# read in data set
attach(ewr) 			# unattach later
tmp=subset(ewr, inorout == "out",select=c("AA","NW"))
x=tmp[['AA']] 			# alternately AA[inorout=='out']
y=tmp[['NW']]
boxplot(x,y) 			# not shown

wilcox.test(x,y)

#< Section 12: Chi Square Tests >

x = rchisq(100,5); y=rchisq(100,50)
simple.eda(x); simple.eda(y)

freq = c(22,21,22,27,22,36)	# specify probabilities, (uniform, like this, is default though)
probs = c(1,1,1,1,1,1)/6 # or use rep(1/6,6)
chisq.test(freq,p=probs)

x = c(100,110,80,55,14)
probs = c(29, 21, 17, 17, 16)/100
chisq.test(x,p=probs)

yesbelt = c(12813,647,359,42)
nobelt = c(65963,4000,2642,303)
chisq.test(data.frame(yesbelt,nobelt))

die.fair = sample(1:6,200,p=c(1,1,1,1,1,1)/6,replace=T)
die.bias = sample(1:6,100,p=c(.5,.5,1,1,1,2)/6,replace=T)
res.fair = table(die.fair);res.bias = table(die.bias)
rbind(res.fair,res.bias)

chisq.test(rbind(res.fair,res.bias))

chisq.test(rbind(res.fair,res.bias))[['exp']]

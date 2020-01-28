#데사 과제 7

#Linear Regression

#1. Packages
library(MASS)
library(plyr)
library(ggplot2)
library(knitr)
library(GGally)



#2. Linear regression
# Import data set
crime <- read.table("http://www.andrew.cmu.edu/user/achoulde/94842/data/crime_simple.txt", sep = "\t",
header = TRUE)
# Assign more meaningful variable names
colnames(crime) <- c("crime.per.million", "young.males", "is.south", "average.ed",
 "exp.per.cap.1960", "exp.per.cap.1959", "labour.part",
 "male.per.fem", "population", "nonwhite",
 "unemp.youth", "unemp.adult", "median.assets", "num.low.salary")
# Convert is.south to a factor
# Divide average.ed by 10 so that the variable is actually average education
# Convert median assets to 1000's of dollars instead of 10's
crime <- transform(crime, is.south = as.factor(is.south),
 average.ed = average.ed / 10,
 median.assets = median.assets / 100)
# print summary of the data
summary(crime)

# Scatter plot of outcome (crime.per.million) against average.ed
qplot(average.ed, crime.per.million, data = crime)
# correlation between education and crime
with(crime, cor(average.ed, crime.per.million))
# Scatter plot of outcome (crime.per.million) against median.assets
qplot(median.assets, crime.per.million, data = crime)
# correlation between education and crime
with(crime, cor(median.assets, crime.per.million))

# Boxplots showing crime rate broken down by southern vs non-southern state
qplot(is.south, crime.per.million, geom = "boxplot", data = crime)

crime.lm <- lm(crime.per.million ~ ., data = crime)
# Summary of the linear regression model
crime.lm
summary(crime.lm)

options(scipen=4) # Set scipen = 0 to get back to default
summary(crime.lm)

# List all attributes of the linear model
attributes(crime.lm)
crime.lm$coef

# Pull coefficients element from summary(lm) object
round(summary(crime.lm)$coef, 3)

# Pull the coefficients table from summary(lm)
crime.lm.coef <- round(summary(crime.lm)$coef, 3)
# See what this gives
class(crime.lm.coef)
attributes(crime.lm.coef)
crime.lm.coef["average.ed", "Pr(>|t|)"]

plot(crime.lm)

diamonds.lm <- lm(price ~ carat + cut + clarity + color, data = diamonds)
plot(diamonds.lm)

diamonds.lm2 <- lm(log(price) ~ I(log(carat)) + cut + clarity + color, data = diamonds)
plot(diamonds.lm2)

economic.var.names <- c("exp.per.cap.1959", "exp.per.cap.1960", "unemp.adult",
"unemp.youth", "labour.part", "median.assets")
pairs(crime[,economic.var.names])

round(cor(crime[,economic.var.names]), 3)

# Function taken from ?pairs Example section.
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
 usr <- par("usr"); on.exit(par(usr))
 par(usr = c(0, 1, 0, 1))
 r <- abs(cor(x, y))
 txt <- format(c(r, 0.123456789), digits = digits)[1]
 txt <- paste0(prefix, txt)
 if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
 text(0.5, 0.5, txt, cex = pmax(1, cex.cor * r))
}
# Use panel.cor to display correlations in lower panel.
pairs(crime[,economic.var.names], lower.panel = panel.cor)

# ggpairs from GGally library
# Unlike pairs(), ggpairs() works with non-numeric
# predictors in addition to numeric ones.
# Consider ggpairs() for your final project
ggpairs(crime[,c(economic.var.names, "is.south")], axisLabels = "internal")

crime.lm.2 <- update(crime.lm, . ~ . - exp.per.cap.1959 - unemp.youth)
summary(crime.lm.2)
crime.lm.summary.2 <- summary(crime.lm.2)

kable(crime.lm.summary.2$coef, digits = c(3, 3, 3, 4), format = 'markdown')



#3. Thinking more critically about linear regression
crime.lm <- lm(crime.per.million ~ ., data = crime)
crime.lm2 <- update(crime.lm, . ~ . - exp.per.cap.1959 - unemp.youth)

kable(summary(crime.lm)$coef, digits = c(3, 3, 3, 4), format = 'markdown')

crime.lm.summary2 <- summary(crime.lm2)
kable(crime.lm.summary2$coef, digits = c(3, 3, 3, 4), format = 'markdown')

# all 95% confidence intervals
confint(crime.lm2)
# Just for education
confint(crime.lm2, parm = "average.ed")
# 75% confidence interval
confint(crime.lm2, parm = "average.ed", level = 0.75)
# How does 2 SE rule compare to confint output?
# lower endpoint
coef(crime.lm2)["average.ed"] - 2* summary(crime.lm2)$coef["average.ed", "Std. Error"]
# upper endpoint
coef(crime.lm2)["average.ed"] + 2* summary(crime.lm2)$coef["average.ed", "Std. Error"]

my.data <- data.frame(y = c(12, 13, 10, 5, 7, 12, 15),
 x1 = c(6, 6.5, 5, 2.5, 3.5, 6, 7.5),
 x2 = c(6, 6.5, 5, 2.5, 3.5, 6, 7.5))
my.data

crime.lm.summary2$coef["exp.per.cap.1960",]

crime.lm.summary2$coef["average.ed",]



#4. Factors in linear regression
#추가
colnames(birthwt) <- c("birthwt.below.2500", "mother.age", "mother.weight", 
    "race", "mother.smokes", "previous.prem.labor", "hypertension", "uterine.irr", 
    "physician.visits", "birthwt.grams")
birthwt <- transform(birthwt, 
            race = as.factor(mapvalues(race, c(1, 2, 3), 
                              c("white","black", "other"))),
            mother.smokes = as.factor(mapvalues(mother.smokes, 
                              c(0,1), c("no", "yes"))),
            hypertension = as.factor(mapvalues(hypertension, 
                              c(0,1), c("no", "yes"))),
            uterine.irr = as.factor(mapvalues(uterine.irr, 
                              c(0,1), c("no", "yes")))
            )

# Fit regression model
birthwt.lm <- lm(birthwt.grams ~ race + mother.age, data = birthwt)
# Regression model summary
summary(birthwt.lm)

# Calculate race-specific intercepts
intercepts <- c(coef(birthwt.lm)["(Intercept)"],
 coef(birthwt.lm)["(Intercept)"] + coef(birthwt.lm)["raceother"],
 coef(birthwt.lm)["(Intercept)"] + coef(birthwt.lm)["racewhite"])
lines.df <- data.frame(intercepts = intercepts,
 slopes = rep(coef(birthwt.lm)["mother.age"], 3),
 race = levels(birthwt$race))
qplot(x = mother.age, y = birthwt.grams, color = race, data = birthwt) +
geom_abline(aes(intercept = intercepts, slope = slopes, color = race),
data = lines.df)

head(model.matrix(birthwt.lm), 20)

qplot(x = mother.age, y = birthwt.grams, color = race, data = birthwt) +
geom_abline(aes(intercept = intercepts, slope = slopes, color = race),
data = lines.df)

qplot(x = mother.age, y = birthwt.grams, color = race, data = birthwt) +
stat_smooth(method = "lm", se = FALSE, fullrange = TRUE)

birthwt.lm.interact <- lm(birthwt.grams ~ race * mother.age, data = birthwt)
summary(birthwt.lm.interact)


data(gala,package = "faraway") 
head(gala);

### 1. Exploratory data analysis 
## A. data summary statistics 
summary(gala);

## B. plot for raw data (cehck possible collinearity?)
library(lattice)
splom(gala[,1:7], pscales = 0)

## C. Correlation table (the last column is Y)
cor(gala[,1:7]); 

## Might to be difficult to read so many digits
## Only keep 2 digits in the correlation matrix 
round(cor(gala[,1:7]),2)

## correlation among the X variabales
##  when the response variable Y = Species
round(cor(gala[,2:7]),2)

### 2. Linear Regression

lmod <- lm(Species ~ Area + Elevation + Nearest + Scruz + Adjacent, gala)

## We can extract regression quantities 
names(lmod)

## some demonstrations 
lmod$coefficients 
lmod$coef
coef(lmod)

lmod$residuals 
lmod$res


### summary of this linear regression model
summary(lmod)

## we might want to extract information
lmodsum <- summary(lmod);
names(lmodsum)

## Some demo
lmodsum$call
lmodsum$coefficients
lmodsum$r.squared 

### As an illustration, we have two ways to find the estimate of $\sigma$

### (a) extract  $\sigma$ directly from the "summary" object 
lmodsum$sigma  ## [1] 60.97519

### (b) estimate $\sigma$ on our own by using the formula 
## \sqrt(RSS / (n-p)), where there are two ways to compute RSS.

## (b)(i) A comparison of two ways to compute RSS
deviance(lmod)
sum(lmod$res^2)

## (b))(ii) manual estimate of $\sigma$ 
sqrt(deviance(lmod)/df.residual(lmod) ) ### [1] 60.97519

##As shown in the above analysis, the results are consistent with each other.

### 3. Assessment 

### 3(A) Goodness of Fit
lmodsum$r.squared       # [1] 0.7658469
lmodsum$adj.r.squared   # [1] 0.7170651

## It can be a mistake to "only" rely on R squares to evaluate the model 

## 3(B) F-statistics and ANOVA 
## In the Summary of Linear Regression, the last line is
## F-statistic:  15.7 on 5 and 24 DF,  p-value: 6.838e-07
## we can also compute them by hand.

## (i) Use the ANOVA
nullmod <- lm(Species ~ 1, gala) 
anova(nullmod, lmod)
# we reject the null hypothesis of \beta_Area= \beta_Elevation=...=\beta_Ajacent = 0.

## (ii) Compute on our own by using F-testing formula 
rss0 <- deviance(nullmod)
rss <- deviance(lmod)
df0 <- df.residual(nullmod)
df <- df.residual(lmod)
fstat <- ((rss0-rss)/(df0-df))/(rss/df)
1-pf(fstat, df0-df, df)  ##[1] 6.837893e-07
## The p-value is the same as the previous analysis. 
## We still reject the null hypothesis of \beta_Area= \beta_Elevation=...=\beta_Ajacent = 0.


### 3(C) Testing whether one explanatory variable can be dropped from the model
### Suppose we want to test whether $\beta_Area = 0$ or not
### There are two ways
### C(i) t-statistics
summary(lmod) 
## p-value for \beta_Area = 0.296318, 
## and thus we cannot reject the null hypothesis of H0: \beta_Area =0. 

### C(ii) F-test 
## a new smaller model to drop "Area" 
lmods <- lm(Species ~ Elevation + Nearest + Scruz + Adjacent, gala)
anova(lmods, lmod)
##Again, p-value= 0.2963 and we cannot reject H0: \beta_Area =0.

### Comments: it is important to specify which other explanatory variables 
###  should be included in the models, and specify both null and alternative models
### \beta_Area is 0 or not, depending on other variables
summary(lm(Species ~ Area, gala))
summary(lm(Species ~ Area + Nearest, gala))
summary(lm(Species ~ Area + Elevation, gala))


### 3(D) Testing whether two or more explanatory variables can be dropped from the model
### Suppose we want to test $H0: \beta_Area = \beta_Adjacent = 0$ or not
## We fit a smaller model without these two terms and conduct the F-test
lmods2 <- lm(Species ~ Elevation + Nearest + Scruz, gala)
anova(lmods2, lmod)
## since p-value is small, we reject the null hypothesis  \beta_Area = \beta_Adjacent = 0
## This means that removing these two explanatory variables from the model is not justifiable. 

### 3(E) Testing whether two coefficient are the same or not.
### Suppose we want to test $H0: \beta_Area = \beta_Adjacent$ against H1: they are different
### The key idea is to create a new variable that is the sum of Area and Adjacent
lmods3 <- lm(Species ~ I(Area + Adjacent) + Elevation + Nearest + Scruz, gala)
anova(lmods3, lmod)
## with the pvalue = 0.02793 small, we can reject H0: \beta_Area = \beta_Adjacent
## and declare that it is not justifiable to treat \beta_Area = \beta_Adjacent. 

### 3(F) Confidence Interval and  Confidence Region
## F(i) a Single parameter
confint(lmod)   # default is 95% CI
confint(lmod, level = 0.9)

## F(ii) two-dimensional case 
### you might need to run the code "
# install.packages("ellipse")
### in the first-time to run this code 
##
require(ellipse)
plot(ellipse(lmod,c(2,6)),type="l",ylim=c(-0.13,0))
points(coef(lmod)[2], coef(lmod)[6], pch=19)
abline(v=confint(lmod)[2,],lty=2)
abline(h=confint(lmod)[6,],lty=2)

## Conclusions
## (1) the joint hypothesis H0:\beta_Area = \beta_Adjacent=0 is rejected because the orgin does not lie inside the ellipse
## (ii) the null hypothesis H0:\beta_Area = 0 is not rejected because zero lies within the vertical ashed lines
## (iii) the null hypothesis H0:\beta_Adjacent = 0 is  rejected because zero lies outside the horizontal ashed lines


## Model comparison 
lmod <- lm(Species ~ Area + Elevation + Nearest + Scruz + Adjacent, gala)
lmods <- lm(Species ~ Elevation + Nearest + Scruz + Adjacent, gala)
## Standard statistical comparison 
c(summary(lmod)$r.squared, summary(lmods)$r.squared) 
c(summary(lmod)$adj.r.squared, summary(lmods)$adj.r.squared) 
anova(lmods, lmod)

## Cross Validation for model comparison 
## Training Error and Testing Error
n = dim(gala)[1];      ### total number of observations
n1 = round(0.80*n);  ### 80% of observations randomly selected for training sample
n2 = n - n1;         ### sample size for testing sample
set.seed(1234);   ### you can change this initial seed for randomization
flag = sort(sample(1:n, n1));
gala1train = gala[flag,]; ###  80% data as the training subset
gala1test  = gala[-flag,]; ### remaining 20% data as the testing subset

## use the training subset to fit 2 regression models
##  note the use on the "training" sample as the data set  
lmod.CV <- lm(Species ~ Area + Elevation + Nearest + Scruz + Adjacent, gala1train)
lmods.CV <- lm(Species ~ Elevation + Nearest + Scruz + Adjacent, gala1train)

## training errors of these two models 
TrainErr0 <- c(mean(lmod.CV$residuals^2), mean(lmods.CV$residuals^2));
TrainErr0
## in general, the training errors of bigger model should be smaller!

## (f)(2) Testing errors on the testing subset 
##  To avoid confusions, the "newdata" does not include the true Y values
Pred.CV <- predict(lmod.CV, newdata= gala1test[,2:7] ); 
Preds.CV <- predict(lmods.CV, newdata= gala1test[,2:7] ); 
TestErr0 <- c(mean(Pred.CV - gala1test[,1])^2, mean(Preds.CV - gala1test[,1])^2 );
TestErr0
rbind(TrainErr0, TestErr0)
## Often a bigger model does not necessarily has a smaller testing error!!!
## Indeed, models with larger adj.Rsquared would often have smaller testing error!  
##
## In HW#4, we provide the R code to repeat this process for B= 100 times
### END

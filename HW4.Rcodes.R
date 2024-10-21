### Read the data 
library(faraway)
head(fat); 

### Another alternative way to load the data
# if you save this file "fat.csv" in your laptop, say, 
#   in the folder "C://temp", you can read it in R as
# fat <- read.table(file = "C://temp/fat.csv", sep=",", header=TRUE);

## (a) Exploratory Data Analysis 
dim(fat)
summary(fat)
## histogram 
hist(fat$brozek)

## plot
library(lattice)
splom(fat)
## correlation matrix 
round(cor(fat),2)
## Any pairs with absolute value of "cor" >= 0.90? 

## (b) Model #1:  Fit a full linear regression model
fit1 <- lm(brozek ~ ., data = fat);
fit1; 
summary(fit1)

## (c) Model #2: fit linear regression model with $15$ predictors:
fit2 <- lm(brozek ~ .-siri-density, data = fat);
fit2;
summary(fit2)

## (d) Model #3: fit linear regression model with $10$ predictors:
## the simplest code would be
##         fit3 <- step(fit2);
## Here we spell out all 10 predictors
fit3 <- lm(brozek ~ weight+ adipos+ free+ chest+ abdom+ thigh+ 
             ankle+ biceps+ forearm+ wrist, data = fat);
fit3;
summary(fit3)

### (e) model comparison
c(summary(fit2)$r.squared, summary(fit3)$r.squared) 
c(summary(fit2)$adj.r.squared, summary(fit3)$adj.r.squared) 
anova(fit2, fit3)
### When p-value of ANOVA > 5%, we accept the smaller model
### when p-value of ANOVA <= 5%, we accept the larger model
###   as its improvement over smaller model is significant

## (f) Training Error and Testing Error
n = dim(fat)[1];      ### total number of observations
n1 = round(0.80*n);  ### 80% of observations randomly selected for training sample
n2 = n - n1;         ### sample size for testing sample
set.seed(2353);   ### you can change this initial seed for randomization
flag = sort(sample(1:n, n1));
fat1train = fat[flag,]; ###  80% data as the training subset
fat1test  = fat[-flag,]; ### remaining 20% data as the testing subset

## (f)(1) use the training subset to fit 2 regression models
##  note the difference on the data set as compared to (c) and (d)
fit2f <- lm(brozek ~ .-siri-density, data = fat1train );
fit3f <- lm(brozek ~ weight+ adipos+ free+ chest+ abdom+ thigh+ 
             ankle+ biceps+ forearm+ wrist, data = fat1train);
## training errors of these two models 
TrainErr0 <- c(mean(fit2f$residuals^2), mean(fit3f$residuals^2));
TrainErr0
## in general, the training errors of bigger model should be smaller!

## (f)(2) Testing errors on the testing subset "fat1test"
##  To avoid confusions, the "newdata" does not include the true Y values
Pred2f <- predict(fit2f, newdata= fat1test[,2:18] ); 
Pred3f <- predict(fit3f, newdata= fat1test[,2:18] ); 
TestErr0 <- c(mean(Pred2f - fat1test[,1])^2, mean(Pred3f - fat1test[,1])^2 );
TestErr0
## Often a bigger model does not necessarily has a smaller testing error!!!
## Indeed, models with larger adj.Rsquared would often have smaller testing error!  

### (g) cross validation on the performance robustness of these two models 
set.seed(2353);
B = 100;
TrainErr = NULL;  ## record all training errors 
TestErr  = NULL;  ##record all testing errors 
for (b in 1:B){
  ## with each loop, create training and testing subsets
  flagtmp = sort(sample(1:n, n1));
  fat1traintmp = fat[flagtmp,];   ###"temporal" training subset
  fat1testtmp = fat[-flagtmp, ];  ###"temporal" testing subset
  
  ###### Fit these two (2) models to the "temporal" training subset
  fit2ftmp <- lm(brozek ~ .-siri-density, data = fat1traintmp );
  fit3ftmp <- lm(brozek ~ weight+ adipos+ free+ chest+ abdom+ thigh+ 
                ankle+ biceps+ forearm+ wrist, data = fat1traintmp);
  
  ## Save training errors of these two models for the "temporal" training subset
  TrainErr <- rbind(TrainErr, c(mean(fit2ftmp$residuals^2), mean(fit3ftmp$residuals^2)));
  
  ###### Prediction of these two (2) models to the "temporal" testing subset
  Pred2ftmp <- predict(fit2ftmp, newdata= fat1testtmp[,2:18] ); 
  Pred3ftmp <- predict(fit3ftmp, newdata= fat1testtmp[,2:18] ); 
  TestErr <- rbind(TestErr, c(mean(Pred2ftmp - fat1testtmp[,1])^2, mean(Pred3ftmp - fat1testtmp[,1])^2 ));
}

### Training Error Comaprison of two models 
colnames(TrainErr) <- c("Mod2", "Mod3")
dim(TrainErr);
round(apply(TrainErr, 2, mean),4);
round(apply(TrainErr, 2, var),4);

### Testing  Error Comparisons of two models 
colnames(TestErr) <- c("Mod2", "Mod3")
dim(TestErr);
round(apply(TestErr, 2, mean),4);
round(apply(TestErr, 2, var),4);

## Statistical difference testing to compare two models 
##  We can adopt the paired two-sample test 
##   since the same training/test subsets are used for fitting two models 
## (a) First, use two-sample t-test for training and testing errors
t.test(TrainErr[,1],TrainErr[,2],paired=T)
t.test(TestErr[,1],TestErr[,2],paired=T)
## which model has smaller training error? which has smaller testing error?
## Hints: you can check the values 
round(apply(TrainErr, 2, mean),4);
round(apply(TestErr, 2, mean),4);

## (b) Second, use nonparametric Wilcox test for training and testing errors
wilcox.test(TrainErr[,1],TrainErr[,2],paired=T)
wilcox.test(TestErr[,1],TestErr[,2],paired=T)
## which model has smaller training error? which has smaller testing error?
## You can use similar ideas from t.test in (a)
##########END ##############
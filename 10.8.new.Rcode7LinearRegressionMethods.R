####
#### R code for Week #10: Linear Regression 
## 
## First, save the dataset "prostate.csv" in your laptop, say, 
##         in the local folder "C:/temp". 
##
## Data Set
prostate <- read.table("prostate.csv", header= TRUE, sep = ",")

##This dataset is from the book ESL, where the authors 
##  have split the data into the training and testing subsets
##  here we use their split to produce similar results 

training <- subset( prostate, train == TRUE)[,1:9];
test    <- subset( prostate, train == FALSE)[,1:9];
## The true Y response for the testing subset
ytrue    <- test$lpsa; 



### Below we consider seven (7) linear regression related models
### (1) Full model;  
### (2) The best subset model; 
### (3) Stepwise variable selection with AIC
### (4) Ridge Regression; 
### (5) LASSO; 
### (6) Principal Component Regression, and 
### (7) Parital Least Squares (PLS) Regression 
##
###  For each of these 7 models or methods, we fit to the training subset, 
###  and then compute its training and testing errors. 
##
##   Let us prepare to save all training and testing errors
MSEtrain <- NULL;
MSEtest  <- NULL; 

###
### (1) Linear regression with all predictors (Full Model)
###     This fits a full linear regression model on the training data
model1 <- lm( lpsa ~ ., data = training); 

## Model 1: Training error
MSEmod1train <-   mean( (resid(model1) )^2);
MSEtrain <- c(MSEtrain, MSEmod1train);
# Model 1: testing error 
pred1a <- predict(model1, test[,1:8]);
MSEmod1test <-   mean((pred1a - ytrue)^2);
MSEmod1test;
MSEtest <- c(MSEtest, MSEmod1test); 
#[1] 0.521274

### (2) Linear regression with the best subset model 
###  YOu need to first install the package "leaps"
library(leaps);
prostate.leaps <- regsubsets(lpsa ~ ., data= training, nbest= 100, really.big= TRUE); 

## Record useful information from the output
prostate.models <- summary(prostate.leaps)$which;
prostate.models.size <- as.numeric(attr(prostate.models, "dimnames")[[1]]);
prostate.models.rss <- summary(prostate.leaps)$rss;

## 2A:  The following are to show the plots of all subset models 
##   and the best subset model for each subset size k 
plot(prostate.models.size, prostate.models.rss); 
## find the smallest RSS values for each subset size 
prostate.models.best.rss <- tapply(prostate.models.rss, prostate.models.size, min); 
## Also add the results for the only intercept model
prostate.model0 <- lm( lpsa ~ 1, data = training); 
prostate.models.best.rss <- c( sum(resid(prostate.model0)^2), prostate.models.best.rss); 
## plot all RSS for all subset models and highlight the smallest values 
plot( 0:8, prostate.models.best.rss, type = "b", col= "red", xlab="Subset Size k", ylab="Residual Sum-of-Square")
points(prostate.models.size, prostate.models.rss)

# 2B: What is the best subset with k=3
op2 <- which(prostate.models.size == 3); 
flag2 <- op2[which.min(prostate.models.rss[op2])]; 

## There are two ways to fit this best subset model with k=3. 

## 2B(i) First, we can manual look at the selected model and fit it.
##      It will not be able to be implemented in cross-validation 
prostate.models[flag2,]
model2a <- lm( lpsa ~ lcavol + lweight + svi, data = training);
summary(model2a);

## 2B(ii) Second, we can auto-find the best subset with k=3
##   this way will be useful when doing cross-validation 
mod2selectedmodel <- prostate.models[flag2,]; 
mod2Xname <- paste(names(mod2selectedmodel)[mod2selectedmodel][-1], collapse="+"); 
mod2form <- paste ("lpsa ~", mod2Xname);
## To auto-fit the best subset model with k=3 to the data
model2 <- lm( as.formula(mod2form), data= training); 
# Model 2: training error 
MSEmod2train <- mean(resid(model2)^2);
## save this training error to the overall training error vector 
MSEtrain <- c(MSEtrain, MSEmod2train);
MSEtrain;
## Model 2:  testing error 
pred2 <- predict(model2, test[,1:8]);
MSEmod2test <-   mean((pred2 - ytrue)^2);
MSEtest <- c(MSEtest, MSEmod2test);
MSEtest;
## Check the answer
##[1] 0.5212740 0.4005308

## As compared to the full model #1, the best subset model with K=3
##   has a larger training eror (0.521 vs 0.439),
##   but has a smaller testing error (0.400 vs 0.521). 


### (3) Linear regression with the stepwise variable selection 
###     that minimizes the AIC criterion 
##    This can done by using the "step()" function in R, 
##       but we need to build the full model first

model1 <- lm( lpsa ~ ., data = training); 
model3  <- step(model1); 

## If you want, you can see the coefficents of model3
round(coef(model3),3)
summary(model3)

## Model 3: training  and  testing errors 
MSEmod3train <- mean(resid(model3)^2);
pred3 <- predict(model3, test[,1:8]);
MSEmod3test <-   mean((pred3 - ytrue)^2);
MSEtrain <- c(MSEtrain, MSEmod3train);
MSEtrain; 
## [1] 0.4391998 0.5210112 0.4393627
MSEtest <- c(MSEtest, MSEmod3test);
## Check your answer 
MSEtest;
## [1] 0.5212740 0.4005308 0.5165135


### (4) Ridge regression
### You might need to install "glmnet" beforehand 
library(glmnet);
set.seed(1234);
cv_ridge <- cv.glmnet(as.matrix(training[,-9]), as.matrix(training[,9]), alpha = 0, nfolds = 10) # Ridge: alpha = 0
ridge_lambda <- cv_ridge$lambda.min  # Best lambda from CV
ridge_lambda
model4 <- glmnet(as.matrix(training[,-9]), as.matrix(training[,9]), alpha = 0, lambda = ridge_lambda)

## Model 4 (Ridge): training errors 
yhat4train <- predict(model4, s = ridge_lambda, newx = as.matrix(training[,-9]));
MSEmod4train <- mean((yhat4train - training$lpsa)^2); 
MSEtrain <- c(MSEtrain, MSEmod4train); 
MSEtrain
## [1]  0.4391998 0.5210112 0.4393627 0.4473617
## Model 4 (Ridge):  testing errors in the subset "test" 
pred4test <- predict(model4, s = ridge_lambda, newx = as.matrix(test[,-9]));
MSEmod4test <-  mean((pred4test - ytrue)^2); 
MSEtest <- c(MSEtest, MSEmod4test);
MSEtest;
## [1] 0.5212740 0.4005308 0.5165135 0.4943531


## Model (5): LASSO 
set.seed(1234);
cv_lasso <- cv.glmnet(as.matrix(training[,-9]), as.matrix(training[,9]), alpha = 1, nfolds = 10) # LASSO: alpha = 1
lasso_lambda <- cv_ridge$lambda.min  # Best lambda from CV
lasso_lambda
model5 <- glmnet(as.matrix(training[,-9]), as.matrix(training[,9]), alpha = 1, lambda = lasso_lambda)

## Model 5:  training error for lasso
## 
yhat5train <- predict(model5, s = lasso_lambda, newx = as.matrix(training[,-9]));
MSEmod5train <- mean((yhat5train - training$lpsa)^2); 
MSEtrain <- c(MSEtrain, MSEmod5train); 
MSEtrain
# [1] 0.4391998 0.5210112 0.4393627 0.4473617 0.4998753
##
## Model 5:  training error for lasso  
yhat5test <- predict(model5, s = lasso_lambda, newx = as.matrix(test[,-9]));
MSEmod5test <- mean( (yhat5test - test$lpsa)^2); 
MSEtest <- c(MSEtest, MSEmod5test); 
MSEtest;
## Check your answer:
## [1] 0.5212740 0.4005308 0.5165135 0.4943531 0.4537739


#### Model 6: Principal Component Regression (PCR) 
## You need to first install the R package "pls" below
##
library(pls)

## 6B(i): call the pcr function to run the linear regression 
##        on all possible # of PCs.
##
prostate.pca <- pcr(lpsa~., data=training, validation="CV");  
## 
### 6(ii) auto-select the optimal # of components of PCs 
ncompopt  <- which.min(prostate.pca$validation$adj);
ncompopt 
## 
## 6B(iii) Training Error with the chosen choice "ncompopt" of PCs
ypred6train <- predict(prostate.pca, ncomp = ncompopt, newdata = training[1:8]); 
MSEmod6train <- mean( (ypred6train - training$lpsa)^2); 
MSEtrain <- c(MSEtrain, MSEmod6train); 
MSEtrain;
## Check your answer:
## [1] 0.4391998 0.5210112 0.4393627 0.4473265 0.4998753 0.4391998
##
## 6B(iv) Testing Error with the optimal choice of PCs
ypred6test <- predict(prostate.pca, ncomp = ncompopt, newdata = test[1:8]); 
MSEmod6test <- mean( (ypred6test - test$lpsa)^2); 
MSEtest <- c(MSEtest, MSEmod6test); 
MSEtest;
## Check your answer:
## [1] 0.5212740 0.4005308 0.5165135 0.4943531 0.4537739 0.5212740
##
## Fo this specific example, the optimal # of PC
##         ncompopt = 8, which is the full dimension of the original data
##   and thus the PCR reduces to the full model!!!


### Model 7. Partial Least Squares (PLS) Regression 
###
###  The idea is the same as the PCR and can be done by "pls" package
###  You need to call the fuction "plsr"  if you the code standalone 
#  library(pls)
prostate.pls <- plsr(lpsa ~ ., data = training, validation="CV");

### 7(i) auto-select the optimal # of components of PLS 
## choose the optimal # of components  
mod7ncompopt <- which.min(prostate.pls$validation$adj);
## The opt # of components, it turns out to be 8 for this dataset,
##       and thus PLS also reduces to the full model!!!    
 
# 7(ii) Training Error with the optimal choice of "mod7ncompopt" 
# note that the prediction is from "prostate.pls" with "mod7ncompopt" 
ypred7train <- predict(prostate.pls, ncomp = mod7ncompopt, newdata = training[1:8]); 
MSEmod7train <- mean( (ypred7train - training$lpsa)^2); 
MSEtrain <- c(MSEtrain, MSEmod7train); 
## 7(iii) Testing Error with the optimal choice of "mod7ncompopt" 
ypred7test <- predict(prostate.pls, ncomp = mod7ncompopt, newdata = test[1:8]); 
MSEmod7test <- mean( (ypred7test - test$lpsa)^2); 
MSEtest <- c(MSEtest, MSEmod7test); 

## Check your answers
MSEtrain 
## Training errors of these 7 models/methods
#[1] 0.4391998 0.5210112 0.4393627 0.4473265 0.4998753 0.4391998 0.4391998
MSEtest
## Testing errors of these 7 models/methods
#[1] 0.5212740 0.4005308 0.5165135 0.4943855 0.4537739 0.5212740 0.5212740
##
## For this specific dataset, PCR and PLS reduce to the full model!!!


## Bonus Materials 
##
## For purpose of learning, let us first conduct the manual run of PCR
## with the forst 4 PCs
##  6A: Manual PCR: 
##  6A (i) some fun plots for PCA of training data
trainpca <- prcomp(training[,1:8]);  
##
## 6A(ii)  Examine the square root of eigenvalues
## Most variation in the predictors can be explained 
## in the first a few dimensions
trainpca$sdev
round(trainpca$sdev,2)
### 6A (iii) Eigenvectors are in oj$rotation
### the dim of vectors is 8
###
matplot(1:8, trainpca$rot[,1:3], type ="l", xlab="", ylab="")
matplot(1:8, trainpca$rot[,1:5], type ="l", xlab="", ylab="")
##
## 6A (iv) Choose a number beyond which all e. values are relatively small 
plot(trainpca$sdev,type="l", ylab="SD of PC", xlab="PC number")
##
## 6A (v) An an example, suppose we want to do Regression on the first 4 PCs
## Get Pcs from obj$x
modelpca <- lm(lpsa ~ trainpca$x[,1:4], data = training)
##
## 6A (vi) note that this is on the PC space (denote by Z), with model Y= Z\gamma + epsilon
## Since the PCs Z= X U for the original data, this yields to 
## Y= X (U\gamma) + epsilon,
## which is the form Y=X\beta + epsilon in the original data space 
##  with \beta = U \gamma. 
beta.pca <- trainpca$rot[,1:4] %*% modelpca$coef[-1]; 
##
## 6A (vii) as a comparion of \beta for PCA, and OLS
##   without intercepts, all on the original data scale
cbind(beta.pca, coef(model1)[-1])
##
### 6A(viii) Prediciton for PCA
### To do so, we need to first standardize the training or testing data, 
### For any new data X, we need to impose the center as in the training data
###  This requires us to subtract the column mean of training from the test data
xmean <- apply(training[,1:8], 2, mean); 
xtesttransform <- as.matrix(sweep(test[,1:8], 2, xmean)); 
##
## 6A (iX) New testing data X on the four PCs
xtestPC <-  xtesttransform %*% trainpca$rot[,1:4]; 
##
## 6A (X) the Predicted Y
ypred6a <- cbind(1, xtestPC) %*% modelpca$coef;  
mean( (ypred6a - test$lpsa)^2)
## 
## In practice, one must choose the number of PC carefully.
##   Use validation dataset to choose it. Or Use cross-Validation 
##  This can be done use the R package, say "pls"
##  in the "pls", use the K-fold CV -- default; divide the data into K=10 parts 
##
## 6B: use the "pls" for first 4 Pc
##
## You need to first install the R package "pls" below
##
library(pls)
prostate.pca <- pcr(lpsa~., data=training, validation="CV");  

## comparison the prediction of first 4 PCs with our manual computation  
ypred6b <- predict(prostate.pca, ncomp = 4, newdata = test[1:8]); 
c(mean( (ypred6a - test$lpsa)^2), mean( (ypred6b - test$lpsa)^2))

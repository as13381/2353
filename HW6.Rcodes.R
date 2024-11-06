library(faraway)
head(fat); 

#######Part (a) ##########
#### association between brozek and chest 
## (a)(1) simple linear regression
lma <- lm(brozek ~ chest, data=fat);
summary(lma)
## Extract those information for the slope 
summary(lma)$coefficients[2,] 
##  or the t-statistics and p-value for the slope
##  t-stat=1.562426e+01= 15.62426, p-value= 7.372549e-39 
summary(lma)$coefficients[2,3:4] 

## (a)(2) Pearson's correlation 
# (i) Pearson's correlation
r1 = cor(fat$brozek, fat$chest); 
r1
# (ii) hypothesis testing via Pearson's correlation 
n= dim(fat)[1];
t.obs1 = r1* sqrt((n-2)/ (1-r1^2) ); 
t.obs1  ### compare with (i)
# p-value
pvalue1 = 2*(1-pt( abs(t.obs1), df= n-2 ));
pvalue1
# (iii) 95% CI on Pearson's correlation
alpha = 0.05;
cutoffvalue = qnorm(1- alpha/2);
Zr1 = 0.5*log((1+r1)/(1-r1));
ZCI = Zr1 + c(-1, 1)* cutoffvalue /sqrt(n-3); 
rho1.CI = (exp(2*ZCI) -1) / (exp(2*ZCI) +1);
rho1.CI

### (a)(3) Spearman's Correlation 
## (i) point estimate 
rs1= cor(fat$brozek, fat$chest, method= "spearman");
rs1

## (ii) hypothesis testing 
n= dim(fat)[1];
t.obs2 = rs1* sqrt((n-2)/ (1-rs1^2) ); 
t.obs2   
# p-value based on Spearman's correlation
pvalue2 = 2*(1-pt( abs(t.obs2), df= n-2 ));
pvalue2

#######Part (b) ##########
## please change code in (a) for part (b)
#################################

#######Part (c) ##########
## Make your own decision after comparing (a) and (b)
#################################

#######Part (d) ##########
###  Lily's Model 
modLily <- lm(brozek ~ age + weight+ height+ neck+ chest + abdom+ hip+
                thigh+ knee+ ankle+ biceps+ forearm+ wrist, data=fat); 
summary(modLily)

### model diagnostic
### (i) check equal variance assumption 
library("lmtest")
bptest(modLily)

### If you want to go above and beyond,
##   you can check the p-value of F-test when regressing 
##   the absolute value of residuals on all X variables again
## If p-value >= 5%, then it is okay to accept the equal variance assumption
## If p-value < 5%, then we can run the weighted least square regression 
lm.weight <- lm( abs(residuals(modLily))  ~ age + weight+ height+ neck+ chest + abdom+ hip+
           thigh+ knee+ ankle+ biceps+ forearm+ wrist, data=fat);  
summary(lm.weight)

### (ii) Check the normality assumption
qqnorm(residuals(modLily),ylab="Residuals",main="")
qqline(residuals(modLily), lwd=3,col="red")
shapiro.test(residuals(modLily))

#######Part (e): Collinearity ##########
### (e)(1) Pairwise correlation 
## any pairs that have large correlation? 
##   Say >= 0.90? >= 0.80? >= 0.70?

### There are two ways to extract X matrix
### The first one is to use "model.matrix" function in the regression model 
##  here we assume to exclude the intercept for collinearity analysis
Lily.X <- model.matrix(modLily)[,-1];
round(cor(Lily.X),2)

### The second one is to create the X matrix by ourselves from the raw data 
###     based on those 13 predictor variables
head(fat);
Lily.X2 <- fat[,c(4:6,9:18)]
round(cor(Lily.X2),2)
## these two methods should yield to same answers! 


### (e)(2) Condition Numbers  
Lily.X <- model.matrix(modLily)[,-1]
Lily.e <- eigen(t(Lily.X) %*% Lily.X)
Lily.e$val
sqrt(Lily.e$val[1]/Lily.e$val)

### (e)(3) VIF values 
###  There are two ways. The first one is to use the function "vif" in the package "faraway"
require(faraway)
vif(Lily.X)
max(vif(Lily.X))
mean(vif(Lily.X))

### The second one is to compute VIF on your own 
p = dim(Lily.X)[2]; 
VIF1 <- NULL;
for (i in 1:p){
 Rsqure.tmp <- summary(lm(Lily.X[,i] ~ Lily.X[,-i]))$r.squared;
 VIF1 <- cbind(VIF1, 1/(1-Rsqure.tmp));
}
VIF1 
colnames(VIF1) <- colnames(Lily.X);
VIF1
## Check whether this is the same as the "vif" function
vif(Lily.X)

#######Part (f): Transforming the response ##########
require(MASS)

### 1. summary statistics for the response variable
summary(fat$brozek) 
c(mean(fat$brozek), sd(fat$brozek) ) 

### 2. plots
hist(fat$brozek)
qqnorm(fat$brozek)
qqline(fat$brozek, lwd=3, col="blue")

### 3. Test Normality assumption 
shapiro.test(fat$brozek)

### 4. Box-Cox transformation 
library(MASS)
boxcox(modLily, plotit = T); 

### 5. Making responses to be positive before using boxcox 
modLily1 <- lm( I(0.1+brozek) ~ age + weight+ height+ neck+ chest + abdom+ hip+
                thigh+ knee+ ankle+ biceps+ forearm+ wrist, data=fat);
boxcox(modLily1, plotit = T); 
boxcox(modLily1, lambda= seq(0.5, 1.5, ,by=0.001)); 

#######Part (g): Transforming the predictor ##########
### 1. add quadatic function of "abdom"
modLily2 <- lm( brozek ~ age + weight+ height+ neck+ chest + abdom+ I(abdom^2) +
                  hip+thigh+ knee+ ankle+ biceps+ forearm+ wrist, data=fat);
summary(modLily2)

### 2. add both quadatic and cubic function of "abdom"
modLily3 <- lm( brozek ~ age + weight+ height+ neck+ chest + abdom+ I(abdom^2) +
        I(abdom^3) + hip+thigh+ knee+ ankle+ biceps+ forearm+ wrist, data=fat);
summary(modLily3)

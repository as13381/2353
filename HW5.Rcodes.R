### Read the data 
library(faraway)
head(teengamb); 

## (a) Please modify the R codes in HW#4 for  Exploratory data analysis
dim(happy)
summary(happy)
## histogram 
hist(happy$happy)

## plot
library(lattice)
splom(happy)
## correlation matrix 
round(cor(happy),2)

### (b) Jack's model
Jackmod <- lm(happy ~ money + sex + love + work, data = happy)
summary(Jackmod)
plot(Jackmod)

### (c) Model diagnostic for Jack's model
## 1. Check the constant variance assumption for the errors
plot(fitted(Jackmod),sqrt(abs(residuals(Jackmod))), xlab="Fitted",
     ylab=expression(sqrt(hat(epsilon))))
library("lmtest")
bptest(Jackmod)

## 2.  Check the normality assumption
qqnorm(residuals(Jackmod),ylab="Residuals",main="")
qqline(residuals(Jackmod))

hist(residuals(Jackmod),xlab="Residuals",main="")

shapiro.test(residuals(Jackmod))
dwtest(Jackmod)

## 3. Check for large leverage points
hatv <- hatvalues(Jackmod)
head(hatv)
n <- nrow(happy)
p <- sum(hatv)  ## there are p=5 variables including intercept  
as.numeric(which(hatv>2*p/n)) 

## 4. Check for outliers
# largest studentized residuals in the absolute value 
stud <- rstudent(Jackmod)
stud[which.max(abs(stud))]

# Compute Bonferroni critical value, qt(1-alpha/(2n), n-p)
p <- 5 # there are p=5 beta values including intercept  
qt(1- 0.05/(n*2),n-p)

# Get all outliers (if it is empty, then no outliers)
which(abs(stud)>qt(1-0.05/(n*2),n-p))

## 5. Check for influential points (if it is empty, then no influential points)
cook <- cooks.distance(Jackmod)
which(cook > qf(0.5, p, n-p))

### (d) Please use weighted least square or Boxcox transformation as in the lecture

### (e) Kathy's model
# it can also be derived as follows
# Kathymod.full <- lm(happy ~ money*sex*love*work, data = happy)
# Kathymod <- step(Kathymod.full);
Kathymod <- lm(happy ~ money + sex + love + work + I(money*sex) + 
                 I(money*love) + I(money*work) + I(sex*love) + I(sex*work) + I(love*work) 
               + I(money*sex*love) + I(sex*love*work), data = happy)
summary(Kathymod);
plot(Kathymod) 
### Please write your own to do model diagnostic for Kathy's model by yourself 

## (g) Comparison of these two models
##  (i) run ANOVA F-test 
anova(Jackmod, Kathymod) 

##  (ii) please modify the cross-validation training/testing error codes in HW#4
##   to compare these two models, Jackmod and Kathymod, on your own 

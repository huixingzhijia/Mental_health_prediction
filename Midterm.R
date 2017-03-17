# Author: Wenhui Zeng
# Title: Mid-Term Exam Project


#########################################
#  Data management part
#############################################


#predictor variables are:

#The SAS and R has different variables
#_STATE (region)
#MSCODE (Urbanity)
#X_RACE(Race group)
#X_AGE_G (age)
#SEX (gender)
#INCOME2 (income)
#X_EDUCAG (education)
#EMPLOY1 (employment status)
#HLTHPLN1 (health insurance)
#AVEDRNK2(about how many drinks did you drink on the average?)

#outcome variables are:

#numeric
# poor mental health days: MENTHLTH (1-30, 88 = None, 77 = don't know, 99 = refused)

#The binary health outcome variable 
#HAVARTH3 (Ever told) you have some form of arthritis, rheumatoid arthritis, gout, lupus, 
#or fibromyalgia? (Arthritis diagnoses include: rheumatism, polymyalgia rheumatica; 
#osteoarthritis (not osteporosis); tendonitis, bursitis, bunion, tennis elbow; 
#carpal tunnel syndrome, tarsal tunnel syndrome; joint infection, etc.)(1=Yes,2=No, 7=Don't know / Not sure,9=Refused)

#Run all the library
library(leaps)#best subset and backward
library(glmnet)
library(pls)
library(boot)#cv.glm
library(foreign)#read.xport
library(MASS)#lda
library(class)#knn

#Subset and clearn the data
LLCP2014 <- read.xport(file = "D:/courses/BSTcourse/machine learning and predictive modeling/midterm/LLCP2014XPT/LLCP2014.XPT")[ ,c("X_STATE","MSCODE",
                             "X_RACE","X_AGE_G","SEX","INCOME2","EMPLOY1","HLTHPLN1","AVEDRNK2","X_EDUCAG","HAVARTH3","MENTHLTH")]

LLCP2015 <- read.xport(file = "D:/courses/BSTcourse/machine learning and predictive modeling/midterm/LLCP2015XPT/LLCP2015.XPT")[ ,c("X_STATE","MSCODE",
         "X_RACE","X_AGE_G","SEX","INCOME2","EMPLOY1","HLTHPLN1","AVEDRNK2","X_EDUCAG","HAVARTH3","MENTHLTH")]


LLCP2014$Region[LLCP2014$X_STATE %in% c(1, 5, 10, 11, 12, 13, 21, 22, 24, 28, 37, 40, 45, 47, 48, 51, 54, 66, 72)] <- "South"
LLCP2014$Region[LLCP2014$X_STATE %in% c(2, 4, 6, 8, 15, 16, 30, 32, 35, 41, 49, 53, 56)] <- "West"
LLCP2014$Region[LLCP2014$X_STATE %in% c(9, 23, 25, 33, 34, 36, 42, 44, 50)] <- "Northeast"
LLCP2014$Region[LLCP2014$X_STATE %in% c(17, 18, 19, 20, 26, 27, 29, 31, 38, 39, 46, 55)] <- "Midwest"
LLCP2014$Region <- as.factor(LLCP2014$Region)

#MSCODE (Urbanity)
#Have lots of missing values

LLCP2014$Urbanity[LLCP2014$MSCODE == 1] <- "Center of metropolitan statistical area"
LLCP2014$Urbanity[LLCP2014$MSCODE == 2] <- "Outside metropolitan statistical area"
LLCP2014$Urbanity[LLCP2014$MSCODE == 3] <- "Suburb of metropolitan statistical area"
LLCP2014$Urbanity[LLCP2014$MSCODE == 5] <- "Non-metropolitan statistical area"
LLCP2014$Urbanity<- as.factor(LLCP2014$Urbanity)

#X_RACE(Race group)

LLCP2014$Race[LLCP2014$X_RACE==1] <-"Non-Hispanic White"
LLCP2014$Race[LLCP2014$X_RACE == 2] <- "Non-Hispanic Black"
LLCP2014$Race[LLCP2014$X_RACE %in% c(3,4,5,6,7)] <- "Non-Hispanic Others"
LLCP2014$Race[LLCP2014$X_RACE == 8] <- "Hispanic"
LLCP2014$Race[LLCP2014$X_RACE == 9] <- NA
LLCP2014$Race<- as.factor(LLCP2014$Race)


#X_AGE_G (Age)

LLCP2014$Age[LLCP2014$X_AGE_G==1] <-"18-24"
LLCP2014$Age[LLCP2014$X_AGE_G == 2] <- "25-34"
LLCP2014$Age[LLCP2014$X_AGE_G ==3] <- "35-44"
LLCP2014$Age[LLCP2014$X_AGE_G == 4] <- "45-54"
LLCP2014$Age[LLCP2014$X_AGE_G == 5] <- "55-64"
LLCP2014$Age[LLCP2014$X_AGE_G == 6] <- "65+"
LLCP2014$Age<- as.factor(LLCP2014$Age)


#SEX (gender)
LLCP2014$gender[LLCP2014$SEX==1] <-"Male"
LLCP2014$gender[LLCP2014$SEX==2] <-"Female"
LLCP2014$gender<- as.factor(LLCP2014$gender)

#INCOME2 (income)

LLCP2014$income[LLCP2014$INCOME2 %in% c(1,2)] <-"<=$15,000"
LLCP2014$income[LLCP2014$INCOME2 %in% c(3,4)] <-"$15,000-$25,000"
LLCP2014$income[LLCP2014$INCOME2 ==5] <-"$25,000-$35,000"
LLCP2014$income[LLCP2014$INCOME2 ==6] <-"$35,000-$50,000"
LLCP2014$income[LLCP2014$INCOME2 ==7] <-"$50,000-$75,000"
LLCP2014$income[LLCP2014$INCOME2 ==8] <-"$75,000+"
LLCP2014$income<- as.factor(LLCP2014$income)

#EMPLOY1 (employment status)

LLCP2014$employ[LLCP2014$EMPLOY1 %in% c(1,2)] <-"employed"
LLCP2014$employ[LLCP2014$EMPLOY1 %in% c(3,4,5,6,7,8)] <-"unemployed"
LLCP2014$employ[LLCP2014$EMPLOY1 ==9] <-NA
LLCP2014$employ<- as.factor(LLCP2014$employ)

#HLTHPLN1 (health insurance)
LLCP2014$Hinsurance[LLCP2014$HLTHPLN1 ==1] <-"Yes"
LLCP2014$Hinsurance[LLCP2014$HLTHPLN1 ==2] <-"No"
LLCP2014$Hinsurance[LLCP2014$HLTHPLN1 %in% c(7,9)] <-NA
LLCP2014$Hinsurance<- as.factor(LLCP2014$Hinsurance)

#AVEDRNK2(about how many drinks did you drink on the average?)
LLCP2014$AVEDRNK2[LLCP2014$AVEDRNK2 %in% c(77,99)] <-NA
LLCP2014$AVEDRNK2<-as.numeric(LLCP2014$AVEDRNK2)


#X_EDUCAG (education)

LLCP2014$Education[LLCP2014$X_EDUCAG ==1] <-"Did not graduate High School"
LLCP2014$Education[LLCP2014$X_EDUCAG ==2] <-"Graduated from High School"
LLCP2014$Education[LLCP2014$X_EDUCAG ==3] <-"Attended College/Technical School"
LLCP2014$Education[LLCP2014$X_EDUCAG ==4] <-"Graduated from College/Technical School"
LLCP2014$Education[LLCP2014$X_EDUCAG ==9] <-NA
LLCP2014$Education<- as.factor(LLCP2014$Education)

#Outcome numeric variable
# poor mental health days: MENTHLTH (1-30, 88 = None, 77 = don't know, 99 = refused)

LLCP2014$MENTHLTH[LLCP2014$MENTHLTH %in% c(88,77,99)] <-NA


#The binary health outcome variable 
#HAVARTH3 (If it have skin cancer)(1=Yes,2=No, 7=Don't know / Not sure,9=Refused)

LLCP2014$arthritis[LLCP2014$HAVARTH3 ==1] <-"Yes"
LLCP2014$arthritis[LLCP2014$HAVARTH3 ==2] <-"No"
LLCP2014$arthritis[LLCP2014$HAVARTH3 %in% c(7,9)] <-NA
LLCP2014$arthritis<- as.factor(LLCP2014$arthritis)

LLCP2014<-na.omit(LLCP2014)

#X_STATE (Region)
LLCP2015$Region[LLCP2015$X_STATE %in% c(1, 5, 10, 11, 12, 13, 21, 22, 24, 28, 37, 40, 45, 47, 48, 51, 54, 66, 72)] <- "South"
LLCP2015$Region[LLCP2015$X_STATE %in% c(2, 4, 6, 8, 15, 16, 30, 32, 35, 41, 49, 53, 56)] <- "West"
LLCP2015$Region[LLCP2015$X_STATE %in% c(9, 23, 25, 33, 34, 36, 42, 44, 50)] <- "Northeast"
LLCP2015$Region[LLCP2015$X_STATE %in% c(17, 18, 19, 20, 26, 27, 29, 31, 38, 39, 46, 55)] <- "Midwest"
LLCP2015$Region <- as.factor(LLCP2015$Region)

#MSCODE (Urbanity)
#Have lots of missing values

LLCP2015$Urbanity[LLCP2015$MSCODE == 1] <- "Center of metropolitan statistical area"
LLCP2015$Urbanity[LLCP2015$MSCODE == 2] <- "Outside metropolitan statistical area"
LLCP2015$Urbanity[LLCP2015$MSCODE == 3] <- "Suburb of metropolitan statistical area"
LLCP2015$Urbanity[LLCP2015$MSCODE == 5] <- "Non-metropolitan statistical area"
LLCP2015$Urbanity<- as.factor(LLCP2015$Urbanity)

#X_RACE(Race group)

LLCP2015$Race[LLCP2015$X_RACE==1] <-"Non-Hispanic White"
LLCP2015$Race[LLCP2015$X_RACE == 2] <- "Non-Hispanic Black"
LLCP2015$Race[LLCP2015$X_RACE %in% c(3,4,5,6,7)] <- "Non-Hispanic Others"
LLCP2015$Race[LLCP2015$X_RACE == 8] <- "Hispanic"
LLCP2015$Race[LLCP2015$X_RACE == 9] <- NA
LLCP2015$Race<- as.factor(LLCP2015$Race)


#X_AGE_G (Age)

LLCP2015$Age[LLCP2015$X_AGE_G==1] <-"18-24"
LLCP2015$Age[LLCP2015$X_AGE_G == 2] <- "25-34"
LLCP2015$Age[LLCP2015$X_AGE_G ==3] <- "35-44"
LLCP2015$Age[LLCP2015$X_AGE_G == 4] <- "45-54"
LLCP2015$Age[LLCP2015$X_AGE_G == 5] <- "55-64"
LLCP2015$Age[LLCP2015$X_AGE_G == 6] <- "65+"
LLCP2015$Age<- as.factor(LLCP2015$Age)


#SEX (gender)
LLCP2015$gender[LLCP2015$SEX==1] <-"Male"
LLCP2015$gender[LLCP2015$SEX==2] <-"Female"
LLCP2015$gender<- as.factor(LLCP2015$gender)

#INCOME2 (income)

LLCP2015$income[LLCP2015$INCOME2 %in% c(1,2)] <-"<=$15,000"
LLCP2015$income[LLCP2015$INCOME2 %in% c(3,4)] <-"$15,000-$25,000"
LLCP2015$income[LLCP2015$INCOME2 ==5] <-"$25,000-$35,000"
LLCP2015$income[LLCP2015$INCOME2==6] <-"$35,000-$50,000"
LLCP2015$income[LLCP2015$INCOME2 ==7] <-"$50,000-$75,000"
LLCP2015$income[LLCP2015$INCOME2 ==8] <-"$75,000+"
LLCP2015$income<- as.factor(LLCP2015$income)

#EMPLOY1 (employment status)

LLCP2015$employ[LLCP2015$EMPLOY1 %in% c(1,2)] <-"employed"
LLCP2015$employ[LLCP2015$EMPLOY1 %in% c(3,4,5,6,7,8)] <-"unemployed"
LLCP2015$employ[LLCP2015$EMPLOY1 ==9] <-NA
LLCP2015$employ<- as.factor(LLCP2015$employ)

#HLTHPLN1 (health insurance)
LLCP2015$Hinsurance[LLCP2015$HLTHPLN1 ==1] <-"Yes"
LLCP2015$Hinsurance[LLCP2015$HLTHPLN1 ==2] <-"No"
LLCP2015$Hinsurance[LLCP2015$HLTHPLN1 %in% c(7,9)] <-NA
LLCP2015$Hinsurance<- as.factor(LLCP2015$Hinsurance)

#AVEDRNK2(about how many drinks did you drink on the average?)
LLCP2015$AVEDRNK2[LLCP2015$AVEDRNK2 %in% c(77,99)] <-NA
LLCP2015$AVEDRNK2<-as.numeric(LLCP2015$AVEDRNK2)

#X_EDUCAG (education)

LLCP2015$Education[LLCP2015$X_EDUCAG ==1] <-"Did not graduate High School"
LLCP2015$Education[LLCP2015$X_EDUCAG ==2] <-"Graduated from High School"
LLCP2015$Education[LLCP2015$X_EDUCAG ==3] <-"Attended College/Technical School"
LLCP2015$Education[LLCP2015$X_EDUCAG ==4] <-"Graduated from College/Technical School"
LLCP2015$Education[LLCP2015$X_EDUCAG ==9] <-NA
LLCP2015$Education<- as.factor(LLCP2015$Education)

#Outcome numeric variable
# poor mental health days: MENTHLTH (1-30, 88 = None, 77 = don't know, 99 = refused)

LLCP2015$MENTHLTH[LLCP2015$MENTHLTH %in% c(88,77,99)] <-NA
LLCP2015$MENTHLTH<-as.numeric(LLCP2015$MENTHLTH)

#The binary health outcome variable 
#HAVARTH3 (Ever told) you have some form of arthritis, rheumatoid arthritis, gout,
#lupus, or fibromyalgia? (Arthritis diagnoses include: rheumatism, polymyalgia rheumatica; 
#osteoarthritis (not osteporosis); tendonitis, bursitis, bunion, tennis elbow; carpal tunnel 
#syndrome, tarsal tunnel syndrome; joint infection, etc.)(1=Yes,2=No, 7=Don't know / Not sure,9=Refused)

LLCP2015$arthritis[LLCP2015$HAVARTH3 ==1] <-"Yes"
LLCP2015$arthritis[LLCP2015$HAVARTH3 ==2] <-"No"
LLCP2015$arthritis[LLCP2015$HAVARTH3 %in% c(7,9)] <-NA
LLCP2015$arthritis<- as.factor(LLCP2015$arthritis)

#get rid of missing values
LLCP2015<-na.omit(LLCP2015)

brfss<-rbind(LLCP2014,LLCP2015)

BRFSS<-brfss[,c("Hinsurance","employ","gender","income","Age","Race","Urbanity","Region",
                                      "AVEDRNK2","Education","MENTHLTH","arthritis")]

sum(is.na(BRFSS))#there is no missing value

# Recode variables as indicated by the codebook

###########
#CREATE TRAIN and TEST SET
##########

#

dim(LLCP2014)[1]
dim(LLCP2015)[1]
dim(BRFSS)[1]

brfss.train<-BRFSS[1:dim(LLCP2014)[1],]
brfss.test<-BRFSS[(dim(LLCP2014)[1]+1):dim(BRFSS)[1],]
m.train<-BRFSS$MENTHLTH[1:dim(LLCP2014)[1]]
m.test<-BRFSS$MENTHLTH[(dim(LLCP2014)[1]+1):dim(BRFSS)[1]]
sc.train<-BRFSS$arthritis[1:dim(LLCP2014)[1]]
sc.test<-BRFSS$arthritis[(dim(LLCP2014)[1]+1):dim(BRFSS)[1]]


#Analysis and Prediction
################################

# Classification Model Fitting  and Prediction using 2015

################################

# Using any/all methods from the first half of the semester, create a model that
# predicts your binary outcome variable as accurately as possible from your
# explanitory variables. Describe the methods you used, and why you chose those
# methods. Report any relevant training and testing errors, along with any final
# model coefficients and how those coefficients should be interpreted.


#in this Section, we use the k-fold cross-validation method to estimate the test error!, and 
#Use the data 2015 to calculate the real test error!!!

#logistic model
lg.fit.0<-glm(arthritis~.-MENTHLTH,family = binomial,data=brfss.train)
summary(lg.fit.0)
#The model with all the ten variables was signficant. but the variable AVEDRNK2 was not significant
#different, so after we remove it from the model the test error is about 31%, is decrease from
#the model with all variables, so for logistic regression, the model with the other 9 variables
#gives good prediction.Since ohter categorical variable has different subset groups, some sub groups
#has significant effect, so we keep the main groups.

lg.fit<-glm(arthritis~.-MENTHLTH-AVEDRNK2,family = binomial,data=brfss.train)
summary(lg.fit)

# Now create a custom regression + odds ratio table
lg.fit.od <- cbind(
  Coef = coef(lg.fit),
  OR = exp(coef(lg.fit)),
  exp(confint(lg.fit)))
library(xtable)
print(xtable(lg.fit.od , caption = "Regression coefficients and odds ratios"), comment = F, caption.placement = "top")

#Using cross-validation to estimate the test error is
cv.error<- cv.glm(brfss.train, lg.fit, K=10)$delta[1]
#The cross-validation for test error is approximately 0.1975314.

# Get fitted probabilities from test set:
lg.pred <- predict(lg.fit,newdata=brfss.test, type = "response")
glm.pred <- rep("No", length(lg.pred))
glm.pred[lg.pred>0.5] <- "Yes"

# get vector of predicted classifications:
(class.table<-table(glm.pred,sc.test))
# and our crosstabulation of the predicted vs the actual classification

# Finally, our TEST ERROR RATE
a<-1-sum(diag(class.table))/sum(class.table)
#The final test errror is 0.3367116

boot.fn <- function(data, index){
  return(coef(glm(arthritis~.-MENTHLTH,family = binomial,data=brfss.train, subset = index)))
}

boot.fn(brfss.train, 1:dim(brfss.train)[1])
set.seed(1)
boot.fn(brfss.train, sample(dim(brfss.train)[1], dim(brfss.train)[1], replace = T))
coef(lg.fit)
# now let's see two steps of a bootstrap using sample()
# ... x1000 times... or use boot() to estimate the standard error

boot(brfss.train, boot.fn, R=100)



#Lindear Discriminant Analysis
lda.fit<-lda(arthritis~.-MENTHLTH,data=brfss.train)
summary(lda.fit)
lda.pred <- predict(lda.fit, brfss.test)
table(lda.pred$class, sc.test)
mean(lda.pred$class != sc.test)
#Test errror is 0.3367727

#QDA
qda.fit <- qda(arthritis~.-MENTHLTH,data=brfss.train)
summary(qda.fit)
qda.pred <- predict(qda.fit, brfss.test)
table(qda.pred$class, sc.test)
mean(qda.pred$class != sc.test)
#Test error is  0.39514

### K-Nearest Neighbors
names(brfss.train)
# Create training matrix
####Is there any easy way to create a matrix for KNN?

x.train<-cbind(brfss.train$Hinsurance,brfss.train$employ,brfss.train$gender,brfss.train$income,brfss.train$Age,
              brfss.train$Race,brfss.train$Urbanity,brfss.train$Region,brfss.train$AVEDRNK2,
              brfss.train$Education)

x.test<-cbind(brfss.test$Hinsurance,brfss.test$employ,brfss.test$gender,brfss.test$income,brfss.test$Age,
              brfss.test$Race,brfss.test$Urbanity,brfss.test$Region,brfss.test$AVEDRNK2,
              brfss.test$Education)

# Create testing matrix

# Get training classes:

sc.train<-BRFSS$arthritis[1:dim(LLCP2014)[1]]

# Now run knn()

set.seed(1) # set's the random seed number so that results can be reproduced, tell the computer 
#where should we start pulling the random number 
# run knn with k = 1

#k=3
knn.pred.3 <- knn(x.train,x.test,sc.train, k = 3)

table(knn.pred.3, sc.test)
mean(knn.pred.3 != sc.test)
#Test error is 0.3412098

#k=5
knn.pred.5 <- knn(x.train,x.test,sc.train, k = 5)
table(knn.pred.5, sc.test)
mean(knn.pred.5 != sc.test)
#Test error is 0.3313704

#k=10
knn.pred.10 <- knn(x.train,x.test,sc.train, k = 10)
table(knn.pred.10, sc.test)
mean(knn.pred.10 != sc.test)
#Test error is  0.3216801

#K=100

knn.pred.100 <- knn(x.train,x.test,sc.train, k = 100)
table(knn.pred.100, sc.test)
mean(knn.pred.100 != sc.test)
#The test error is 0.3157



### 6.5.3 Model selection by Validation Set and Cross-Validation approaches




############################
# Regression Model Fitting #
############################

# Using any/all methods from the first half of the semester, create a model that
# predicts your numeric outcome variable as accurately as possible from your
# explanitory variables. Describe the methods you used, and why you chose those
# methods. Report any relevant training and testing errors, along with any final
# model coefficients and how those coefficients should be interpreted.

#####linear regression

lm.fit<-glm(MENTHLTH~.-arthritis,data=brfss.train)
summary(lm.fit)

lm.pred<-predict(lm.fit,brfss.test)
#the mean square error of the test data was
lm.MSE<-mean((m.test-lm.pred)^2)

lm.fit<-glm(MENTHLTH~.-arthritis-gender-Urbanity,data=brfss.train)
summary(lm.fit)

lm.pred<-predict(lm.fit,brfss.test)
#the mean square error of the test data was not change, but we tend to select the simple model without the gender and Urbanity
#we can see gender, urbanity are both not significant different!
lm.MSE<-mean((m.test-lm.pred)^2)



#### 6.5.1 Best Subset selection

#Create a plot function

plot.regsummary <- function(reg.summary) {
  par(mfrow = c(2, 2), mar = c(5, 5, 1, 1))
  plot(reg.summary$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
  plot(reg.summary$adjr2, xlab = "Number of Variables", 
       ylab = expression(paste("Adjusted ", R^2)), type = "l")
  points(which.max(reg.summary$adjr2), 
         reg.summary$adjr2[which.max(reg.summary$adjr2)], 
         col = "red", cex = 2, pch = 20)
  plot(reg.summary$cp, xlab = "Number of Variables", ylab = expression(C[p]), 
       type = "l")
  points(which.min(reg.summary$cp), 
         reg.summary$cp[which.min(reg.summary$cp)], 
         col = "red", cex = 2, pch = 20)
  plot(reg.summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
  points(which.min(reg.summary$bic), 
         reg.summary$bic[which.min(reg.summary$bic)], 
         col = "red", cex = 2, pch = 20)
}


############3Best Subset selection

regfit.best <- regsubsets(MENTHLTH~.-arthritis, data = brfss.train, nvmax = 10)
reg.summary <- summary(regfit.best)

plot.regsummary(reg.summary)
#According to the plot, it seems like the RSS, CP and BIC was decreased as the number 
#of variables increase. We can't make a good decision. We try the validation approach to estimate the 
#test error to determine the best model


test.mat <- model.matrix(MENTHLTH~.-arthritis, data = brfss.test)
val.errors <- rep(NA, 10)
for (i in 1:10){
  coefi <- coef(regfit.best, id = i)
  pred <- test.mat[ , names(coefi)] %*% coefi
  val.errors[i] <- mean((m.test-pred)^2)
}
which.min(val.errors)
#In this setting, we can select a model using the
#one-standard-error rule. We first calculate the standard
#error of the estimated test MSE for each model size, and
#then select the smallest model for which the estimated test
#error is within one standard error of the lowest point on
#the curve

sd_error<-sd(val.errors)
a<-min(val.errors)+sd_error
b<-min(val.errors)-sd_error
plot(1:10,val.errors,xlab = "Number of Variables",ylab = "Estimate Test Error",ylim = c(80,90))
abline(h=a,lty=2)
abline(h=b,lty=2)
#According to the rule, 5 variables gives the simplest model. It was within the one standard from the minumum value

coef(regfit.best, 5)


### 6.5.2 Forward and Backward Stepwise Selection
# regsubsets() works here too; just specify method = "forward" or "backward"
# method = "forward":
regfit.fwd <- regsubsets(Salary ~ ., data = Hitters, nvmax=19, 
                         method ="forward")
(fwd.summary <- summary(regfit.fwd))
#it determines the model with one variable, that CRBI has the smallest RSS of 19 possible variables. 
#then we add another one,


# But doing all those plots again and again would be a pain, so let's make a 
# function:
plot.regsummary <- function(reg.summary) {
  par(mfrow = c(2, 2), mar = c(5, 5, 1, 1))
  plot(reg.summary$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
  plot(reg.summary$adjr2, xlab = "Number of Variables", 
       ylab = expression(paste("Adjusted ", R^2)), type = "l")
  points(which.max(reg.summary$adjr2), 
         reg.summary$adjr2[which.max(reg.summary$adjr2)], 
         col = "red", cex = 2, pch = 20)
  plot(reg.summary$cp, xlab = "Number of Variables", ylab = expression(C[p]), 
       type = "l")
  points(which.min(reg.summary$cp), 
         reg.summary$cp[which.min(reg.summary$cp)], 
         col = "red", cex = 2, pch = 20)
  plot(reg.summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
  points(which.min(reg.summary$bic), 
         reg.summary$bic[which.min(reg.summary$bic)], 
         col = "red", cex = 2, pch = 20)
}
plot.regsummary(fwd.summary)
# method = "backward":
regfit.bwd = regsubsets(Salary ~ ., data = Hitters, nvmax=19, 
                        method = "backward")
(bwd.summary <- summary(regfit.bwd))
# Now use our new plot function:
plot.regsummary(bwd.summary)

# Best models by Best Subset, Forward and Backward are all the same for 1:6, 
# However, model M7 is the simplest model with different variables:
coef(regfit.full, 7)
coef(regfit.fwd, 7)
coef(regfit.bwd, 7)


### 6.5.3 Model selection by Validation Set and Cross-Validation approaches

### Validation Set Approach
# Generate boolean vectors for training/testing subsetting
set.seed (1)
train <- sample(c(TRUE, FALSE), nrow(Hitters), rep = TRUE)
test <- (!train)

# perform best subsets selection on training subset
regfit.best <- regsubsets(Salary ~ ., data = Hitters[train, ], nvmax = 19)
# now create model matrix (matrix of x variables) from test data
test.mat <- model.matrix(Salary ~.,data = Hitters[test, ])
# create empty vector to hold Test MSE for each model
val.errors <- rep(NA, 19)
# loop through each model (1:19), calculating validation error for each:
for (i in 1:19){
  coefi <- coef(regfit.best, id = i)
  pred <- test.mat[ , names(coefi)] %*% coefi
  val.errors[i] <- mean((Hitters$Salary[test]-pred)^2)
}
# Which of those has smallest testing error?
which.min(val.errors)
coef(regfit.best, which.min(val.errors))
# The 10 variable model

# Sidebar: That was a PITA because ther is no predict() function for 
# regsubsets() objects, so let's make a function for future use: ... allow us 
#to pass some function we don't specific, suppose you need calcualte mean, all other argument get default value, otherwise I specific it
predict.regsubsets <- function(object, newdata, id, ...){
  form <- as.formula(object$call[[2]])#tell us to pull the second one the formula, when you working with list, you use two []
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id = id)
  xvars <- names(coefi)
  mat[ , xvars] %*% coefi
}
# Now we could do this more easily in the future

# Finally, since we now know that the "best" model will contain 10 variables,
# we need to perform Best Subset selection on the full data (training & testing)
# in order to get the correct variables and coefficient estimates.
regfit.best <- regsubsets(Salary ~ ., data = Hitters, nvmax=19)
coef(regfit.best, 10)
# Indeed, the full data is better predicted by a slightly different 10 variables
# we know the best model should have 10 varibles, but which ten will subject the train and test data

### Cross-Validation Approach

# Set number of folds
k <- 10
set.seed(1)
# Now subset the data into those folds
folds <- sample(1:k, nrow(Hitters), replace = TRUE)
# Create an empty matrix to hold the CV errors (results)
cv.errors <- matrix(NA, k, 19, dimnames = list(NULL, paste(1:19)))
# Loop through the 19 variables for each k=10 folds using our predict.regsubsets
for(j in 1:k){
  best.fit <- regsubsets(Salary ~ ., data = Hitters[folds != j, ], nvmax = 19)
  for(i in 1:19){
    pred <- predict(best.fit, Hitters[folds == j, ], id = i)
    cv.errors[j, i] = mean((Hitters$Salary[folds == j] - pred)^2)
  }
}
# Each element corresponds to the Test MSE for the i-th cross-validation fold 
# for the best j-variable model
# Average over the columns to get the average MSE for each of the 19 j-variable
# models over k=10 folds
#a vector giving the subscripts which the function will be applied over. E.g., for a matrix 1 indicates rows, 2 indicates columns
(mean.cv.errors <- apply(cv.errors, 2, mean))
# How many variables is best?
par(mfrow = c(1, 1))
plot(mean.cv.errors, type = "b")
# or...
which.min(mean.cv.errors)
# This Salary is best predicted by 11 variables, so now use Best Subset to
# find the best 11 variables on the whole, un-subsetted, data
reg.best <- regsubsets(Salary ~ ., data = Hitters, nvmax = 19)
coef(reg.best, 11)

#### 6.5.2 Forward and Backward Stepwise Selection




# Lab 6.6.1 Ridge Regression

### Lab 6.6.2 The Lasso

### 6.7.1 Principal Components Regression

### 6.7.2 Partial Least Squares




##############
# Discussion #
##############
# Answer each of the following questions in a few paragraphs or less:

# How does this approach differ from what you have done in previous coursework,
# i.e. theory-based model building, null-hypothesis testing, and/or Bayesian
# methods?

# How does your interpretation of these models differ from the interpretation
# that you might have made using more traditional methods, (i.e. rigid NHST or
# Bayesian for those familiar with Bayes) methods?














































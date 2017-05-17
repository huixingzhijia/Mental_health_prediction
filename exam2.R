library(foreign)
llcp2014 <- read.xport(file = "LLCP2014.XPT")
nrow(llcp2014)


#Let's return to the BRFSS data. This time, pick 1 binary disease outcome to model,
#the demographic variables `MARITAL`, `SEX`, `WTKG3`, `HTIN4`, and at least 6 other 
#explanitory variables that you think will help to predict your outcome, but keep no
#more than 15 total explanitory variables. You may reuse variables from the midterm, 
#but use what you learned in the midterm to inform your choices (so if one variable was 
#obviously useless, then don't choose it this time).

#Binary variable:(Ever told) you that you have a depressive disorder,
#including depression, major depression, dysthymia, or minor depression?

LLCP2014<-llcp2014[ ,c("MARITAL","SEX","WTKG3","HTIN4","ADDEPEV2",
                       "X_AGE_G","MSCODE","CHILDREN","SLEPTIM1","X_EDUCAG","X_INCOMG")]


#X_AGE_G (Age)

LLCP2014$Age[LLCP2014$X_AGE_G==1] <-"18-24"
LLCP2014$Age[LLCP2014$X_AGE_G == 2] <- "25-34"
LLCP2014$Age[LLCP2014$X_AGE_G ==3] <- "35-44"
LLCP2014$Age[LLCP2014$X_AGE_G == 4] <- "45-54"
LLCP2014$Age[LLCP2014$X_AGE_G == 5] <- "55-64"
LLCP2014$Age[LLCP2014$X_AGE_G == 6] <- "65+"
LLCP2014$Age<- as.factor(LLCP2014$Age)

#MSCODE (Urbanity)

LLCP2014$Urbanity[LLCP2014$MSCODE == 1] <- "Center of metropolitan statistical area"
LLCP2014$Urbanity[LLCP2014$MSCODE == 2] <- "Outside metropolitan statistical area"
LLCP2014$Urbanity[LLCP2014$MSCODE == 3] <- "Suburb of metropolitan statistical area"
LLCP2014$Urbanity[LLCP2014$MSCODE == 5] <- "Non-metropolitan statistical area"
LLCP2014$Urbanity<- as.factor(LLCP2014$Urbanity)


#CHILDREN
LLCP2014$numchildren<-as.numeric(LLCP2014$CHILDREN)
LLCP2014$numchildren[LLCP2014$CHILDREN %in% c(88,99)] <-NA


#INCOME2 (income)

LLCP2014$income[LLCP2014$X_INCOMG==1] <-"<=$15,000"
LLCP2014$income[LLCP2014$X_INCOMG ==2] <-"$15,000-$25,000"
LLCP2014$income[LLCP2014$X_INCOMG ==3] <-"$25,000-$35,000"
LLCP2014$income[LLCP2014$X_INCOMG  ==4] <-"$35,000-$50,000"
LLCP2014$income[LLCP2014$X_INCOMG  ==5] <-"$5000 or more"
LLCP2014$income<- as.factor(LLCP2014$income)


#X_EDUCAG (education)

LLCP2014$Education[LLCP2014$X_EDUCAG %in% c(1,2)] <-"Less than College education"
LLCP2014$Education[LLCP2014$X_EDUCAG  %in% c(3,4)] <-"Have some college education or above"
LLCP2014$Education[LLCP2014$X_EDUCAG ==9] <-NA
LLCP2014$Education<- as.factor(LLCP2014$Education)

#"SLEPTIM1"
LLCP2014$sleepdep[LLCP2014$SLEPTIM1 %in% c(77,99)] <-NA
LLCP2014$sleepdep<-ifelse(LLCP2014$SLEPTIM1<7,"Yes","No")
LLCP2014$sleepdep<-as.factor(LLCP2014$sleepdep)
#SEX (gender)
LLCP2014$gender[LLCP2014$SEX==1] <-"Male"
LLCP2014$gender[LLCP2014$SEX==2] <-"Female"
LLCP2014$gender<- as.factor(LLCP2014$gender)

#"MARITAL"
LLCP2014$maritalstatus[LLCP2014$MARITAL ==1] <-"Married"
LLCP2014$maritalstatus[LLCP2014$MARITAL %in% c(2,3,4,6)] <-"Widowed/Divorced/Separated/Living with partner"
LLCP2014$maritalstatus[LLCP2014$MARITAL ==5] <-"Never Married"
LLCP2014$maritalstatus[LLCP2014$MARITAL ==9] <-NA
LLCP2014$maritalstatus<- as.factor(LLCP2014$maritalstatus)

#"WTKG3"
LLCP2014$weight<-as.numeric(LLCP2014$WTKG3)
LLCP2014$weight[LLCP2014$WTKG3==99999]<-NA

#"HTIN4"
LLCP2014$height<-as.numeric(LLCP2014$HTIN4)

#"ADDEPEV2",depressive disorder

LLCP2014$depression[LLCP2014$ADDEPEV2=="1"]<-"Yes"
LLCP2014$depression[LLCP2014$ADDEPEV2==2]<-"No"
LLCP2014$depression[LLCP2014$ADDEPEV2 %in% c(7,9)]<-NA
LLCP2014$depression<- as.factor(LLCP2014$depression)

LLCP2014<-na.omit(LLCP2014)
sum(is.na(LLCP2014))#t
names(LLCP2014)
LLCP2014<-LLCP2014[,c("sleepdep","numchildren","gender","income","Age","Urbanity","maritalstatus",
                      "weight","Education","depression","height")]
summary(LLCP2014)

#The depression rate is 9687/41256 is 0.23. If we predict they all don't have depression, the test
#error is 0.23
#training and testing
set.seed(1)
train<-sample(nrow(LLCP2014),0.7*nrow(LLCP2014))
train.x<-LLCP2014[train,]
test.x<-LLCP2014[-train,]
train.y<-LLCP2014$depression[train]
test.y<-LLCP2014$depression[-train]


#
## Nonlinear models (4 points)
#*Using the training data only, create a good explanitory GAM of your outcome variable 
#from your explanitory variables. You may want to further split the training data into 
#another training/testing subset, do cross-validation using the traing subset, 
#or use somethign like AIC for model selection. Describe your methods, and interpret 
#the results of the best model (using figures if necessary).*

 

library(splines2)
library(gam)
library(akima)
library(boot)
library(randomForest)
library(gbm)
library(e1071)
library(tree)
library(ROCR)
#Test each variable to outcome so we can see which variable is linear or polynominal relations

#height
#There are two ways to select the best model 
#Method 1
set.seed(1)
glm.fit.0<-glm(depression~height,data=train.x,family = binomial)
glm.fit.1<-glm(depression~poly(height,2),data=train.x,family = binomial)
glm.fit.2<-glm(depression~poly(height,3),data=train.x,family = binomial)
glm.fit.3<-glm(depression~poly(height,4),data=train.x,family = binomial)
t<-anova(glm.fit.0,glm.fit.1,glm.fit.2,glm.fit.3,test = "Chisq")
names<-c("pvalue1","pvalue2","pvalue3")
a<-round(t$`Pr(>Chi)`[2:4],5)
table<-rbind(names,a)



#Method 2
poly.aic <- NA
for(i in 1:5){
  poly.aic[i] <- AIC(glm(depression ~ poly(height, i), data = train.x,family = binomial))
}
poly.aic
which(poly.aic == min(poly.aic))
(upper<-min(poly.aic)+sd(poly.aic))
(lower<-min(poly.aic)-sd(poly.aic))

#method 3
cross.error=rep(0,5)
for (i in 1:5) {
  glm.poly<- glm(depression ~ poly(height, i), data = train.x,family = binomial)
  cross.error[i]<-cv.glm(train.x,glm.poly,K=5)$delta[1]
}

which.min(cross.error)

#the minimum is degree of 5, but we can see the test error didn't change much  from degree 3 to 5. So I will choose degree of 3 as the optimal one

plot(1:5,cross.error,type = "l",ylab = "cross validation error",xlab = "degree of freedom")
abline(h=min(cross.error)+sd(cross.error))
abline(h=min(cross.error)-sd(cross.error))
#In here we can see that thrid degree will give a good decrease of the AIC value. So third degree is good enough.
#There is no necessary to go up to fourth degreee

#weight

poly.aic <- NA
for(i in 1:5){
  poly.aic[i] <- AIC(glm(depression ~ poly(weight, i), data = train.x,family = binomial))
}
poly.aic
which(poly.aic == min(poly.aic))
(upper<-min(poly.aic)+sd(poly.aic))
(lower<-min(poly.aic)-sd(poly.aic))
poly.aic<-round(poly.aic,5)
#According to the one starndar rule, it seems like third degree will give good prediction

#income
set.seed(1)
for(i in 1:5){
  poly.aic[i] <- AIC(glm(depression ~ poly(numchildren, i), data = train.x.1,family = binomial))
}
poly.aic
which(poly.aic == min(poly.aic))
(upper<-min(poly.aic)+sd(poly.aic))
(lower<-min(poly.aic)-sd(poly.aic))

#There is no difference using splines or losses for number of children, we already test that there
#is no nessary to fit the 


#Although adding the third degree of number of children can decrease the AIC, but
#Three degree will tend to overfit the model.
#In here, linear numchildren is enough to fit the model

#Other variables were all categorical variable. It is hard to have non-linear realtionship set up
#we we will buil the model

gam.fit.0<-gam(depression~poly(height,3)+sleepdep+gender+income+Age+Urbanity+
                 maritalstatus+Education+weight+numchildren,data = train.x,family = binomial(link = "logit"))
AIC<-summary(gam.fit.0)$aic#22703.38 

#Urbanity and education, and number of children has no significant effect in the depression prediction

#rebuild the model

#get rid of the education
gam.fit.0.0<-gam(depression~poly(height,2)+sleepdep+gender+income+Age+Urbanity+
                 maritalstatus+weight+numchildren,data = train.x.1,family = binomial(link = "logit"))
summary(gam.fit.0.0)#22702.12 

anova(gam.fit.0.0,gam.fit.0,test = "Chisq")
#There is no significant adding the education variable, also we can see that after we get rid of the 
#education variable, our number of children variable has turned to be significant differenct
#Also the AIC was decreasing

#get rid of urbanity
gam.fit.0.1<-gam(depression~poly(height,2)+sleepdep+gender+income+Age+
                 maritalstatus+Education+weight+numchildren,data = train.x.1,family = binomial(link = "logit"))
summary(gam.fit.0.1)
anova(gam.fit.0.1,gam.fit.0,test = "Chisq")
#There is no significant difference in adding the urbanity.The AIC is 22700.18, which is also decreasing 


#get rid of number of childern
gam.fit.0.2<-gam(depression~poly(height,2)+sleepdep+gender+income+Age+Urbanity+
                 maritalstatus+Education+weight,data = train.x.1,family = binomial(link = "logit"))
summary(gam.fit.0.2)
anova(gam.fit.0.2,gam.fit.0,test = "Chisq")
#Remove the number of children variable increase the AIC, also adding that variable gives 
#signifcant result#22712.56 

#As a conclusion, Urbanity and education can be removed. We keep the number of children variable
#Also remove the education and urbanity variables, the AIC was decreasing. But removing the
#number of children variable, AIC was increasing. So the final model should only keep eight 
#except education and Urbanity variables.


gam.fit.1<-gam(depression~poly(height,2)+sleepdep+gender+income+Age+
                 maritalstatus+weight+numchildren,data = train.x.1,family = binomial(link = "logit"))
a<-summary(gam.fit.1)
pvalue<-a$parametric.anova[5]
t<-data.frame(names=c("poly(height)","sleepdep","gender","income","Age","marital status",
                      "weight","number of children","residual"),pvalue=pvalue[1:9,])
print(xtable(t),type = "html")



## Trees
#Because we want to compare which tree method is better like basic tree, random forest, bagging,
#so we further split the data because we will use estimate test error to give a good estimation

set.seed(1)
train.tree<-sample(nrow(train.x),0.7*nrow(train.x))
train.tree.x<-train.x[train.tree,]
tree.test.x<-train.x[-train.tree,]
tree.train.y<-train.x$depression[train.tree]
tree.test.y<-train.x$depression[-train.tree]

set.seed(1)
#The outcome variable is already a 
tree.brfss2014 <- tree(depression~height+sleepdep+gender+income+Age+
                         maritalstatus+weight+numchildren+Education+Urbanity,data= train.tree.x)
summary(tree.brfss2014)

# Show a plot of the tree with text() node labels
plot.tree(tree.brfss2014)
text(tree.brfss2014, pretty = 0)

tree.pred <- predict(tree.brfss2014,tree.test.x, type = "class")
(brfss.table <- table(tree.pred, tree.test.y))
sum(diag(brfss.table ))/sum(brfss.table)

#The testing error is 0.19. Which is fine. But it is improving from we predict them all have depression
#

set.seed(3)
cv.brfss2014<- cv.tree(tree.brfss2014, FUN = prune.misclass )
cv.brfss2014

par(mfrow = c(1, 2))
plot(cv.brfss2014$size, cv.brfss2014$dev, type = "b")
plot(cv.brfss2014$k, cv.brfss2014$dev, type = "b")

#In here I try to prune the tree, But it shows it is not legitimate tree.It may because the tree
#is simple enough. There is no necessary to prune it.

#Also, I tried different method using "gini". It shows that there is no change of the deviance when
#the size or node changes.

#The test error is 0.19 for basic tree, gender and income are the important variables. 

#Bagging
set.seed(1)
bag.brfss2014 <- randomForest(depression ~ ., data = train.tree.x, mtry = 10, importance = TRUE)
bag.brfss2014 


predict.bag <- predict(bag.brfss2014 , newdata = tree.test.x,type = "response")
(brfss.table <- table(predict.bag, tree.test.y))
sum(diag(brfss.table ))/sum(brfss.table)
importance(bag.brfss2014)
varImpPlot(bag.brfss2014)

#The test error is about 21%, it seems like weight, gender,height and income are the the imporatnt variables that
#can decrease the accurancy. 

#Random forest


set.seed(1)
rf.brfss2014 <- randomForest(depression ~ ., data = train.tree.x, mtry = 3, importance = TRUE)
rf.brfss2014 


predict.bag <- predict(bag.brfss2014 , newdata = tree.test.x,type = "response")
(brfss.table <- table(predict.bag, tree.test.y))
sum(diag(brfss.table ))/sum(brfss.table)
importance(bag.brfss2014)
varImpPlot(bag.brfss2014)

#the test error is 1-0.7834175, it is about 21%


#boosting
set.seed(1)
train.tree.x$depression<-as.numeric(train.tree.x$depression)
train.tree.x$depression<-train.tree.x$depression-1
boost.brfss2014 <- gbm(depression ~ ., data = train.tree.x, distribution = "bernoulli", n.trees = 5000,
                       verbose = F)
summary(boost.brfss2014)
par(mfrow = c(1, 2)) 
plot(boost.brfss2014, i = "income") 
plot(boost.brfss2014, i = "gender")

tree.test.x$depression<-as.numeric(tree.test.x$depression)
tree.test.x$depression<-tree.test.x$depression-1
#it will return probability for bernuolli 
pred.boost.pred <- predict(boost.brfss2014, newdata = tree.test.x, n.trees = 5000,type="response")
pred <- rep("Yes", length(pred.boost.pred ))
pred[pred.boost.pred  < 0.5] <- "No"

(brfss.table <- table(pred, tree.test.y))
sum(diag(brfss.table ))/sum(brfss.table)

#It seems like income and gender are important variables.and the test error is 0.19 


#Support Vector Machine

# SVM (4 points)

##linear separate


set.seed(1)
#Because it takes a long time to run a svm using different cost number. It takes forever in my
#computer to run tune function with a range of cost, so I just run them differently.!!

#cost=0.01
brfss2014.svm <- svm(depression ~ ., data = train.tree.x, kernel="linear",
                 cost=0.01)
summary(brfss2014.svm)

ypred <- predict(brfss2014.svm, tree.test.x)
svm.table<-table(predict = ypred, truth = tree.test.y)
sum(diag(svm.table))/sum(svm.table)#0.8067863


#cost=0.1
brfss2014.svm <- svm(depression ~ ., data = train.tree.x, kernel="linear",
                     cost=0.1,scale = FALSE)
summary(brfss2014.svm)

ypred <- predict(brfss2014.svm, tree.test.x)
svm.table<-table(predict = ypred, truth = tree.test.y)
sum(diag(svm.table))/sum(svm.table)#0.8067863


#The test error seems no change using cost 0.01,0.1 and 1. But it takes forever to run. It takes more
#time use tune the svm.So in here, I only show two cost. I will use 0.1 in the following kernel.

##kernel

brfss2014.svm <- svm(depression ~ ., data = train.tree.x, kernel="radial",
                     cost=0.1,gamma=1,scale = FALSE)
summary(brfss2014.svm)

ypred <- predict(brfss2014.svm, tree.test.x)
svm.table<-table(predict = ypred, truth = tree.test.y)
sum(diag(svm.table))/sum(svm.table)#0.8067863

#The two methods show same results. There is no significant change in the testing error. In here we use
#the linear method with cost=0.1, because it faster!!!

# Pick the overall best model (4 points)
# Pick the overall best model (4 points)
#Now show your 3 best models the test data that you've held back since step 1.
#Construct a confusion matrix for each, and 
#show an ROC curve for models that also produce probabilities.
#Explain which of these three models did best, and why. Show and 
#interpret statistics for accuracy, sensitivity and specificificity,
#as well as pseudo R^2 if possible.* 


#nonlinear method:
gam.fit<-gam(depression~poly(height,2)+sleepdep+gender+income+Age+
             maritalstatus+weight+numchildren,data=test.x,family = binomial(link = "logit"))

prob<-predict(gam.fit,test.x,type = "response")
preds<-rep("No",nrow(test.x))
preds[prob>0.5]<-"Yes"
(gam.table <- table(preds, test.y))
te<-1-sum(diag(gam.table ))/sum(gam.table)
#The specificity =true negative(12223)/(true negative 12223+false positive(133))
12223/(12223+133)
#0.98923

#The sensitivity=true positive/total positive=0.0930
277/(277+2700)

#accuracy
(12223+227)/(2700+133+12223+227)#0.8146306

#ROC curve:
predob <- prediction (prob, test.y)
perf <- performance(predob , "tpr", "fpr")
plot(perf,main="ROC cruve")
#It seems like only continue variable was support 



  
#tree:basic tree
tree.brfss2014 <- tree(depression~height+sleepdep+gender+income+Age+
                         maritalstatus+weight+numchildren+Education+Urbanity,data= test.x)
summary(tree.brfss2014)

# Show a plot of the tree with text() node labels
plot(tree.brfss2014)
text(tree.brfss2014, pretty = 0)

#It seems like the tree predict all the outcome as No

tree.pred <- predict(tree.brfss2014,test.x, type = "class")
(brfss.table <- table(tree.pred,test.y))
sum(diag(brfss.table ))/sum(brfss.table)
#0.80848


#The specificity =true negative(12356)/(true negative 12356+false positive 0)
#1

#The sensitivity=true positive/total positive=0
0

#accuracy
(12356)/(12356+2927)#0.80848

#I can't make ROC of this


#support vector machine
svmfit.opt <- svm(depression ~ ., data = test.x, kernel = "linear", 
                  cost =0.1, decision.values = T)

fitted <- attributes(predict(svmfit.opt, test.x, 
                             decision.values = TRUE))$decision.values
rocplot <- function(pred, truth, ...){
  predob <- prediction (pred, truth)
  perf <- performance(predob , "tpr", "fpr")
  plot(perf ,...)
}

rocplot(fitted, test.y, main = "testing Data")

ypred <- predict(svmfit.opt, test.x)
svm.table<-table(predict = ypred, truth =test.y)
sum(diag(svm.table))/sum(svm.table)#0.80848

#It seems like the SVM method also predict all the outcome as No. It is interesting that the results was same as tree
#The specificity =true negative(12356)/(true negative 12356+false positive 0)
#1

#The sensitivity=true positive/total positive=0
0

#accuracy
(12356)/(12356+2927)#0.80848

#Roc curve area is below 0.5, it is worse than tree. 

#Above all, the non-linear model gives us the best estimate. 

# Unsupervised learning (4 points)

#Use a heirarchical clustering algorithm to find two clusters in the data. 
#How well do those two clusters conform to your binary outcome variable? 
#What might this mean for the potential relationship between our variables and the outcome?*


set.seed(1) 
c.train<-sample(nrow(LLCP2014),10000)
c.train.x<-LLCP2014[c.train,]
c.train.y<-LLCP2014$depression[c.train]
x<-as.matrix(c.train.x)#we can't scale it because we have two categorical variables.
hc.complete <- hclust(dist(x), method = "complete")
hc.average <- hclust(dist(x), method = "average")
hc.single <- hclust(dist(x), method = "single")


par(mfrow = c(1, 3))
plot(hc.complete, main = "Complete Linkage", xlab = "", 
     sub = "", cex = 0.9)
plot(hc.average, main = "Average Linkage", xlab = "", 
     sub = "", cex = 0.9)
plot(hc.single, main = "Single Linkage", xlab = "", 
     sub = "", cex = 0.9)


hc.cl.clusters<-cutree(hc.complete, 2)
c.table<-table(hc.cl.clusters, c.train.y)
sum(diag(c.table))/sum(c.table)#The accuracy is only 0.48

hc.av.clusters<-cutree(hc.average, 2)
av.table<-table(hc.av.clusters, c.train.y)
sum(diag(av.table))/sum(av.table)
#0.76

hc.sg.clusters<-cutree(hc.single, 2)
sl.table<-table(hc.sg.clusters, c.train.y)
sum(diag(sl.table))/sum(sl.table)#0.8







##The stock market Data
library(ISLR)
names(Smarket)
dim(Smarket)
pairs(Smarket)
cor(Smarket[,-9])
attach(Smarket)
plot(Volume)

#Logistic Regression
glm.fit=glm(Direction~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume , data=Smarket, family=binomial)
summary(glm.fit)
coef(glm.fit)
##Prediction
glm.probs=predict(glm.fit,type="response")
glm.probs[1:10]
contrasts(Direction)
glm.pred=rep("Down", 1250)
glm.pred[glm.probs>.5]="Up"
table(glm.pred, Direction)
mean(glm.pred==Direction)

##Landslide Data
Data=read.csv("All Data.csv")
names(Data)
dim(Data)
pairs(Data)
cor(Data[,-1])
attach(Data)
plot(Volume)
#Logistic Regression
Landslide.fit=glm(�..Y~.  , data=Data, family=binomial)
summary(Landslide.fit)
coef(Landslide.fit)
#Logistic Regression Modified 1
Landslide.fit1=glm(�..Y~.-Change  , data=Data, family=binomial)
summary(Landslide.fit1)
coef(Landslide.fit1)
#Logistic Regression Modified 2
Landslide.fit2=glm(�..Y~.-Change -SPI -TWI  , data=Data, family=binomial)
summary(Landslide.fit2)
coef(Landslide.fit2)
##Prediction for Baseline Model
glm.probs=predict(Landslide.fit,type="response")
glm.probs[1:10]

glm.pred=rep("0", 390)
glm.pred[glm.probs>.5]="1"
table(glm.pred, �..Y)
mean(glm.pred==�..Y)
##Prediction for Model Without Landuse Change
glm.probs=predict(Landslide.fit1,type="response")
glm.probs[1:10]

glm.pred=rep("0", 390)
glm.pred[glm.probs>.5]="1"
table(glm.pred, �..Y)
mean(glm.pred==�..Y)
##Prediction for Model Without Landuse Change, TWI and SPI
glm.probs=predict(Landslide.fit2,type="response")
glm.probs[1:10]

glm.pred=rep("0", 390)
glm.pred[glm.probs>.5]="1"
table(glm.pred, �..Y)
mean(glm.pred==�..Y)


##Logistic regression with Slope Considered
Data=read.csv("Dis 2.csv")
names(Data)
dim(Data)
pairs(Data)
cor(Data[,-1])
attach(Data)
plot(Volume)
#Logistic Regression
Landslide.fit_slope1=glm(Y~.  , data=Data, family=binomial)
summary(Landslide.fit_slope1)
coef(Landslide.fit_slope1)
#Logistic Regression Modified 1
Landslide.fit_slope2=glm(Y~.-Change  , data=Data, family=binomial)
summary(Landslide.fit_slope2)
coef(Landslide.fit_slope2)
#Logistic Regression Modified 2
Landslide.fit_slope3=glm(Y~.-Change -SPI -TWI  , data=Data, family=binomial)
summary(Landslide.fit_slope3)
coef(Landslide.fit_slope3)
##Prediction for Baseline Model
glm.probs=predict(Landslide.fit_slope1,type="response")
glm.probs[1:10]

glm.pred=rep("0", 392)
glm.pred[glm.probs>.5]="1"
table(glm.pred, Y)
mean(glm.pred==Y)
##Prediction for Model Without Landuse Change
glm.probs=predict(Landslide.fit_slope2,type="response")
glm.probs[1:10]

glm.pred=rep("0", 392)
glm.pred[glm.probs>.5]="1"
table(glm.pred, Y)
mean(glm.pred==Y)
##Prediction for Model Without Landuse Change, TWI and SPI
glm.probs=predict(Landslide.fit_slope3,type="response")
glm.probs[1:10]

glm.pred=rep("0", 392)
glm.pred[glm.probs>.5]="1"
table(glm.pred, Y)
mean(glm.pred==Y)



##Linear discriminant Analysis
library(MASS)
lda.fit=lda(Direction~Lag1+Lag2, data=Smarket)
lda.fit
plot(lda.fit)
lda.pred=predict(lda.fit, Smarket)
names(lda.pred)
lda.class=lda.pred$class
table(lda.class, Direction)
mean(lda.class==Direction)
##90% Threshold
lda.pred$posterior[1:20,1]
lda.class[1:20]
sum(lda.pred$posterior[,1]>.9)


##Landslides

Data=read.csv("Dis 2.csv")
library(MASS)

lda.fit=lda(Y~., data=Data)
lda.fit
plot(lda.fit)
lda.pred=predict(lda.fit, Data)
names(lda.pred)
lda.class=lda.pred$class
table(lda.class, Y)
mean(lda.class==Y)
##90% Threshold
lda.pred$posterior[1:20,1]
lda.class[1:20]
sum(lda.pred$posterior[,1]>.5)


##Landslides with out Change

Data=read.csv("Dis 2.csv")
library(MASS)

lda.fit1=lda(Y~. -Change, data=Data)
lda.fit1
plot(lda.fit1)
lda.pred=predict(lda.fit1, Data)
names(lda.pred)
lda.class=lda.pred$class
table(lda.class, Y)
mean(lda.class==Y)
##90% Threshold
lda.pred$posterior[1:20,1]
lda.class[1:20]
sum(lda.pred$posterior[,1]>.5)
names(Data)

##Landslides with out Change, TWI, SPI

Data=read.csv("Dis 2.csv")
library(MASS)

lda.fit2=lda(Y~. -Change -TWI -SPI, data=Data)
lda.fit2
plot(lda.fit2)
lda.pred=predict(lda.fit2, Data)
names(lda.pred)
lda.class=lda.pred$class
table(lda.class, Y)
mean(lda.class==Y)
##90% Threshold
lda.pred$posterior[1:20,1]
lda.class[1:20]
sum(lda.pred$posterior[,1]>.5)


##Quardratic Discriminant Analysis
library(MASS)
qda.fit=qda(Direction~Lag1+Lag2,data=Smarket)
qda.fit
qda.class=predict(qda.fit,Smarket)$class
table(qda.class,Direction)

##Quardratic Discriminant Analysis
library(MASS)
Data=read.csv("Dis 2.csv")
qda.fit1=qda(Y~.,data=Data)
qda.fit1
qda.class=predict(qda.fit1,Data)$class
table(qda.class,Y)


qda.pred=predict(qda.fit1, Data)
names(qda.pred)
qda.class=qda.pred$class
table(qda.class, Y)
mean(qda.class==Y)

qda.m1= qda(Y ~ ., data = Data, importance=TRUE)
qda.m1
varImp(qda.m1)
predict(qda.m1, Data)
test.predicted.qda=predict(qda.m1, newdata = Data)
qda.cm <- table(Data$Y, test.predicted.qda$class)
qda.cm





###Classification Methods Essentials
library(tidyverse)
library(caret)
Data=read.csv("Dis 2.csv")
##Linear Discriminant Analysis
library(MASS)
# Fit the model
model= lda(Y~., data = Data)
summary(model)
model
# Make predictions
predictions=modelpredict(Data)
# Model accuracy
mean(predictions$class==Data$Y)
plot(model)
predictions=predict(model)
names(predictions)
# Predicted classes
head(predictions$class, 6)
# Predicted probabilities of class memebership.
head(predictions$posterior, 6) 
mean(predictions$class==Data$Y)

##quardatic Discriminant Analysis
library(MASS)
# Fit the model
model1=qda(Y~., data = Data)
model1
# Make predictions
predictions= predict(model1)
names(predictions)
# Model accuracy
mean(predictions$class == Data$Y)
# Predicted classes
##Mixture Discriminant Analysis
install.packages("mda")
library(mda)
# Fit the model
model2 <- mda(Y~., data = Data)
model2
# Make predictions
predicted.classes <- model$predict(Data)
# Model accuracy
mean(predicted.classes == test.transformed$Species)

predictions= predict(model2)
names(predictions)
mean(predictions$class == Data$Y)


##Regularized Discriminant Analysis
install.packages("klaR")
library(klaR)
# Fit the model
model3 <- rda(Y~., data = Data)
model3
predictions= predict(model3)
names(predictions)
mean(predictions$class == Data$Y)



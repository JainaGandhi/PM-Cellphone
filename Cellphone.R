
library("readxl")
setwd("E:/GitHub_Repositories/Cellphone")
data = read_xlsx("Cellphone.xlsx",sheet = 2)


### Data understanding
dim(data)

str(data)

summary(data)

View(data)

# Checking null data
sapply(data,function(x) sum(is.na(x)))

# Checking # of unique values in each column
sapply(data,function(x) length(unique(x)))

# Target variable
length(which(data$Churn=="1"))*100/nrow(data)

# Correlation check
library(corrplot)
correlations = cor(data) 
corrplot(correlations, type="lower", diag = FALSE)

# Binary variables needs to be converted into factor variables
data$Churn = as.factor(data$Churn)
data$DataPlan = as.factor(data$DataPlan)
data$ContractRenewal = as.factor(data$ContractRenewal)

### Exploratory Data Analysis
attach(data)
library(ggplot2)

# For continuous variables

boxplot(AccountWeeks~Churn)

ggplot(data, aes(x = AccountWeeks)) + 
  geom_density(aes(fill = Churn), alpha = 0.3) + 
  scale_color_manual(values = c("#868686FF", "#EFC000FF")) + 
  scale_fill_manual(values = c("darkturquoise", "lightcoral")) + xlim(-30,250)

mean(AccountWeeks)
median(AccountWeeks)
range(AccountWeeks)
sqrt(sd(AccountWeeks))


boxplot(DataUsage~Churn)

ggplot(data, aes(x = DataUsage)) + 
  geom_density(aes(fill = Churn), alpha = 0.3) + 
  scale_color_manual(values = c("#868686FF", "#EFC000FF")) + 
  scale_fill_manual(values = c("darkturquoise", "lightcoral")) + xlim(-1,5)


boxplot(DayMins~Churn)

ggplot(data, aes(x = DayMins)) + 
  geom_density(aes(fill = Churn), alpha = 0.3) + 
  scale_color_manual(values = c("#868686FF", "#EFC000FF")) + 
  scale_fill_manual(values = c("darkturquoise", "lightcoral")) + xlim(-10,400)

boxplot(DayCalls~Churn)

ggplot(data, aes(x = DayCalls)) + 
  geom_density(aes(fill = Churn), alpha = 0.3) + 
  scale_color_manual(values = c("#868686FF", "#EFC000FF")) + 
  scale_fill_manual(values = c("darkturquoise", "lightcoral")) + xlim(-10,200)

boxplot(MonthlyCharge~Churn)

ggplot(data, aes(x = MonthlyCharge)) + 
  geom_density(aes(fill = Churn), alpha = 0.3) + 
  scale_color_manual(values = c("#868686FF", "#EFC000FF")) + 
  scale_fill_manual(values = c("darkturquoise", "lightcoral")) + xlim(-10,125)

boxplot(OverageFee~Churn)

ggplot(data, aes(x = OverageFee)) + 
  geom_density(aes(fill = Churn), alpha = 0.3) + 
  scale_color_manual(values = c("#868686FF", "#EFC000FF")) + 
  scale_fill_manual(values = c("darkturquoise", "lightcoral")) + xlim(-2,20)

boxplot(RoamMins~Churn)

ggplot(data, aes(x = RoamMins)) + 
  geom_density(aes(fill = Churn), alpha = 0.3) + 
  scale_color_manual(values = c("#868686FF", "#EFC000FF")) + 
  scale_fill_manual(values = c("darkturquoise", "lightcoral")) + xlim(-2,25)

boxplot(CustServCalls~Churn)

ggplot(data, aes(x = CustServCalls)) + 
  geom_density(aes(fill = Churn), alpha = 0.3) + 
  scale_color_manual(values = c("#868686FF", "#EFC000FF")) + 
  scale_fill_manual(values = c("darkturquoise", "lightcoral")) + xlim(-2,10)


# For categorical features

ggplot(data, aes(x = CustServCalls, fill = Churn)) + 
  geom_bar(width = 0.25, alpha=0.5) + 
  scale_fill_manual(values = c('darkturquoise', 'lightcoral'))

prop.table(table(CustServCalls,Churn),1)*100

ggplot(data, aes(x = ContractRenewal, fill = Churn)) + 
  geom_bar(width = 0.25, alpha=0.5) + 
  scale_fill_manual(values = c('darkturquoise', 'lightcoral'))

prop.table(table(ContractRenewal,Churn),1)*100


ggplot(data, aes(x = DataPlan, fill = Churn)) + 
  geom_bar(width = 0.25, alpha=0.5) + 
  scale_fill_manual(values = c('darkturquoise', 'lightcoral'))

prop.table(table(DataPlan,Churn),1)*100

### Data Split 

library(caret)
set.seed(1234)
trainIndex = createDataPartition(Churn, p=0.7, list = FALSE, times = 1)
train.data = data[trainIndex, ]
test.data  = data[-trainIndex,]

dim(train.data)
dim(test.data)

prop.table(table(data$Churn))
prop.table(table(train.data$Churn))
prop.table(table(test.data$Churn))

### Model Building - Logistic regression

logit_model1 = glm(Churn ~ ., data = train.data, 
                   family = binomial(link="logit"))

summary(logit_model1)

# Check for multicollinearity
library(car)
vif(logit_model1)


### Model refining - Logistic Regression
logit_model2  =  glm(Churn ~ . -DataUsage -MonthlyCharge, 
                     data = train.data, 
                     family = binomial(link="logit"))

summary(logit_model2)

vif(logit_model2)

# Likelihood ratio test
library(lmtest)
lrtest(logit_model2)

# Pseudo R-square
library(pscl)
pR2(logit_model2)

# Odds Ratio
exp(coef(logit_model2))

# Probability
exp(coef(logit_model2))/(1+exp(coef(logit_model2)))

# Accuracy | Base Line Model
nrow(train.data[train.data$Churn == 0,])/nrow(train.data)

# Performance metrics (in-sample)
pred = predict(logit_model2, data=train.data, type="response")
y_pred_num = ifelse(pred>0.5,1,0)
y_pred = factor(y_pred_num, levels=c(0,1))
y_actual = train.data$Churn
confusionMatrix(y_pred,y_actual,positive="1")

#                 Actual
#Prediction    0       1
#    0       1951     284
#    1         44      55
# Accuracy : 0.859 
# Sensitivity : 0.162
# Specificity : 0.978

# ROC plot
library(ROCR)
train.roc <- prediction(pred, train.data$Churn)
plot(performance(train.roc, "tpr", "fpr"), 
     col = "red", main = "ROC Curve for train data")
abline(0, 1, lty = 8, col = "blue")

# AUC
train.auc = performance(train.roc, "auc")
train.area = as.numeric(slot(train.auc, "y.values"))
train.area

# KS
ks.train <- performance(train.roc, "tpr", "fpr")
train.ks <- max(attr(ks.train, "y.values")[[1]] - (attr(ks.train, "x.values")[[1]]))
train.ks

# Gini
train.gini = (2 * train.area) - 1
train.gini

# Calibrating thresold levels to increase sensitivity
pred = predict(logit_model2, data=train.data, type="response")
y_pred_num = ifelse(pred>0.35,1,0)
y_pred = factor(y_pred_num, levels=c(0,1))
y_actual = train.data$Churn
confusionMatrix(y_pred,y_actual,positive="1")

# Performance metrics (out-of-the-sample)
pred = predict(logit_model2, newdata=test.data, type="response")
y_pred_num = ifelse(pred>0.35,1,0)
y_pred = factor(y_pred_num, levels=c(0,1))
y_actual = test.data$Churn
confusionMatrix(y_pred,y_actual,positive="1")

#                 Actual
#Prediction    0      1
#    0       811     94
#    1        44     50
# Accuracy : 0.862
# Sensitivity : 0.347
# Specificity : 0.949


### Model Building - KNN

# Normalize variables
scale = preProcess(train.data, method = "range")

train.norm.data = predict(scale, train.data)
test.norm.data = predict(scale, test.data)

knn_fit = train(Churn ~., data = train.norm.data, method = "knn",
                 trControl = trainControl(method = "cv", number = 3),
                 tuneLength = 10)

knn_fit$bestTune$k

# Performance metrics (in-sample)
pred = predict(knn_fit, data = train.norm.data[-1], type = "raw")
confusionMatrix(pred,train.norm.data$Churn,positive="1")

#                 Actual
#Prediction    0       1
#    0       1986     181
#    1          9     158
# Accuracy : 0.919 
# Sensitivity : 0.466
# Specificity : 0.996

# Performance metrics (out-of-the-sample)
pred = predict(knn_fit, newdata = test.norm.data[-1], type = "raw")
confusionMatrix(pred,test.norm.data$Churn,positive="1")

#                 Actual
#Prediction    0       1
#    0        841      91
#    1         14      53
# Accuracy : 0.895
# Sensitivity : 0.368
# Specificity : 0.984


### Model Building - NB

library(e1071)
NB = naiveBayes(x=train.norm.data[-c(1,5,9)], y=train.norm.data$Churn)


# Performance metrics (out-of-the-sample)
pred = predict(NB, newdata = train.norm.data[-1])
confusionMatrix(pred, train.norm.data$Churn,positive="1")

#                 Actual
#Prediction    0       1
#    0        1940    250
#    1          55     89
# Accuracy : 0.869
# Sensitivity : 0.263
# Specificity : 0.972

# Performance metrics (in-sample)
pred = predict(NB, newdata = test.norm.data[-1])
confusionMatrix(pred,test.norm.data$Churn,positive="1")

#                 Actual
#Prediction    0       1
#    0        840      109
#    1         15       35
# Accuracy : 0.876
# Sensitivity : 0.243
# Specificity : 0.982




```{r cars}
knitr::opts_chunk$set(echo = TRUE)
# title: "FINAL PROJECT"
# author: "Smrithi Ajit", "Richa Joshi", "Cameron Peltz"
# date: "12/13/2017"
# output: html_document


#INTRODUCTION 

#The dataset used for this project is an IBM Human Resources Analytics Employee Attrition & Performance dataset obtained from Kaggle.The situation requires the HR department to come up with a solution to the employee attrition problem after taking into consideration all the factors involved.The dataset contains 35 variables that are more or less self explanatory and suggestive of the reasons for an employee leaving the company.In order to develop an answer to the question 'Attrition' was taken as the dependant variable and a logistic regression model was developed in order to predict Attrition in terms of other significant independant variables.The choice of independant variables was made through trial and error.

#SMART QUESTION

#The 'SMART' question that was developed was: What are the specific factors that help best predict whether an employee will leave the company in the case study? It was specific in the sense that it addressed the issue at hand; which is to find reasons for employee attrition.Attrition and its predictability are measurable with the help of the models developed in this project that have selected variables that best contribute to the employee attrition.The question is answerable through the output of the model coupled with an intuitive undertanding of what the output suggests.The question is relevant to the problem at hand and can be addressed within a well defined timeframe.

#PROCEDURE FOLLOWED

#1.The dataset was collected and cleaned and the Exploratory data analysis was done.
#2.Attrition was chosen as the variable that indicates whether an employee would leave the company or not.Since Attrition is a binary variable with either 0 or 1, Logistic Regression was deployed. The independant variables included both continuous as well as factor variables.
#3.The dataset was divided into test and train data in a 70/30 split as per industry norms and the datset were trained and tested
#4. ROC curve was used to check the efficiency of the model
#5. McFaddens R2 was also obtained for the model but however it showed relatively low prediction efficiency quite contrary to the ROC results.
#6. The test model was giving output probabilities ranging from 0 to 1 which was meaningless since the attrition had to be 0 or 1. In order to classify the output as 1 or 0 the probability limit was set at a particular value in the test model by trial and error so that maximum model efficiency was achieved.

#RESULTS:

#10 Variables were selected after repeated trial and error and with the help of functions like bestglm namely: Business Travel, Distance From Home, Job Involvement, Job Satisfaction, Number of Companies Worked, Overtime, Relationship Satisfaction, Work Life Balance, Years Since Last Promotion and Environment Satisfaction.The model AIC values obtained have been listed below.A decrease in AIC value indicates improvement in model efficiency.
#Model1  AIC: 1115.2
#AUC (Area Under Curve): 71%
#We found the AIC (Akaike Information Criterion) value for the model to be 1115.2 and the prediction efficiency suggested by the AUC (Area Under Curve) for the test to be 71%.
#Since we felt we were over fitting the model we developed a second model with the most significant variables resulting from model 1.Though there was a deprovement in efficiency the subsequent models with new variables Job Role and Job Satisfaction showed improved efficiency. Variables like Job Role and Job Satisfaction we felt were intuitively significant. Model 4 was chosen to be the best among all the models. The details of the models 2 to 4 are summarized below:

#Model2 AIC:1164
#AUC (Area Under Curve):65%

#Model3:1078.6
#AUC (Area Under Curve):73.29%

#Model4: 1059
#AUC (Area Under Curve):75.23%

library(readr)
install.packages('caret')
install.packages('lmtest')
install.packages('ResourceSelection')
install.packages('pROC')
install.packages('VIF')
library(ResourceSelection)
library(caret)
library(lmtest)
library(pROC)
library(ROCR)
library(grid)
library(broom)
library(caret)
library(tidyr)
library(dplyr)
library(scales)
library(ggplot2)
library(data.table)
library(rpart)
library(ISLR)

#The required packages need to be installed and the relevant library needs to be called.

ibm<-read_csv("ibm.csv")
str(ibm)#the structure of the ibm dataset.
#The misclassified data was classified correctly

ibm$Attrition<-as.factor(ibm$Attrition)
levels(ibm$Attrition)[1]<-"0"
levels(ibm$Attrition)[2]<-"1"
ibm$Department<-as.factor(ibm$Department)
ibm$BusinessTravel<-as.factor(ibm$BusinessTravel)
ibm$Education <- as.factor(ibm$Education)
ibm$EducationField <- as.factor(ibm$EducationField)
ibm$EnvironmentSatisfaction <- as.factor(ibm$EnvironmentSatisfaction)
ibm$JobRole<-as.factor(ibm$JobRole)
ibm$JobInvolvement <- as.factor(ibm$JobInvolvement)
ibm$JobSatisfaction <- as.factor(ibm$JobSatisfaction)
ibm$JobLevel <- as.factor(ibm$JobLevel)
ibm$PerformanceRating <- as.factor(ibm$PerformanceRating)
ibm$RelationshipSatisfaction <- as.factor(ibm$RelationshipSatisfaction)
ibm$StockOptionLevel <- as.factor(ibm$StockOptionLevel)
ibm$WorkLifeBalance <- as.factor(ibm$WorkLifeBalance)
ibm$Gender<-as.factor(ibm$Gender)
ibm$OverTime<-as.factor(ibm$OverTime)
ibm$PerformanceRating<-as.factor(ibm$PerformanceRating)
ibm$RelationshipSatisfaction<-as.factor(ibm$RelationshipSatisfaction)
ibm$StockOptionLevel<-as.factor(ibm$StockOptionLevel)
ibm$WorkLifeBalance<-as.factor(ibm$WorkLifeBalance)

# models 1 to 5 were built
# AIC was used to check if the intitial model or the reduced model is better 
# model 1 has 10 significant variables 

model1<-glm(ibm$Attrition~ibm$BusinessTravel+ibm$DistanceFromHome+ibm$JobInvolvement+ibm$JobSatisfaction+ibm$NumCompaniesWorked+ibm$OverTime+ibm$RelationshipSatisfaction+ibm$WorkLifeBalance+ibm$YearsSinceLastPromotion+ibm$EnvironmentSatisfaction,family = binomial(link = "logit"), ibm)

summary(model1)

# AIC is 1115.2 Reducing the variables to 5 significant ones obtained from the summary of model 1

model2 <-glm(ibm$Attrition~ibm$BusinessTravel+ibm$DistanceFromHome+ibm$OverTime +ibm$WorkLifeBalance+ibm$EnvironmentSatisfaction,family = binomial(link = "logit"), ibm)
summary(model2)
# AIC is  1164. Model has slightly deproved but we are cutting down on the variables from 10 to 5 so the slight difference can be ignored
  
model3<-glm(ibm$Attrition~ibm$BusinessTravel+ibm$DistanceFromHome+ibm$OverTime +ibm$WorkLifeBalance+ibm$EnvironmentSatisfaction+ibm$JobRole,family = binomial(link = "logit"), ibm)
summary(model3)

# AIC is 1078.6.AIC has decreased showing model improvement

model4<-glm(ibm$Attrition~ibm$BusinessTravel+ibm$DistanceFromHome+ibm$OverTime +ibm$WorkLifeBalance+ibm$EnvironmentSatisfaction+ibm$JobRole+ibm$JobSatisfaction,family = binomial(link = "logit"), ibm)
summary(model4)
varImp(model4) # This function helps us find the relative importance of the variables.We clearly see that overtime is a very important factor and stands out amongst all the factors listed by the best model we have chosen
# AIC is 1059.AIC has decreased showing model improvement

#McFAdden's R2
library(pROC)
library(pscl)

pR2(model4)
# Value is quite low even for model 4 but its predictive ability cannot be concluded from Mc FAddens R2 alone. The efficiency is 22%.


# Data was split in 70/30
ibm_train<- ibm[1:1029, ]
ibm_test <- ibm[1030:1470, ]
#Alternatively 80/20 split can be used
#ibm_train<- ibm[1:1176, ]
#ibm_test <- ibm[1177:1470, ]

#Explanation:
#ROC curve was plotted in order to understand how well the model is performing in terms of predicting the outcome.Area under the curve has reduced from 0.894 to 0.8786. The decrease in efficiency of the model is not substantial which means that we could opt the reduced model with these nine key factors.

# model1 has 10 factors
# MODEL 1 training set

train.model1 <- glm(ibm_train$Attrition~ibm_train$BusinessTravel+ibm_train$DistanceFromHome+ibm_train$JobInvolvement+ibm_train$JobSatisfaction+ibm_train$NumCompaniesWorked+ibm_train$OverTime +ibm_train$RelationshipSatisfaction+ibm_train$WorkLifeBalance+ibm_train$YearsSinceLastPromotion+ibm_train$EnvironmentSatisfaction,family = binomial(link = "logit"), ibm_train)
summary(train.model1)

predict_train1=predict(train.model1, type = c("response"))
predict_train1

#Testing the model1
test.model1 <- glm(ibm_test$Attrition~ibm_test$BusinessTravel+ibm_test$DistanceFromHome+ibm_test$JobInvolvement+ibm_test$JobSatisfaction+ibm_test$NumCompaniesWorked+ibm_test$OverTime +ibm_test$RelationshipSatisfaction+ibm_test$WorkLifeBalance+ibm_test$YearsSinceLastPromotion+ibm_test$EnvironmentSatisfaction,family = binomial(link = "logit"), ibm_test)
summary(test.model1)
#Summary and prediction
summary(test.model1)
predict_test1=predict(test.model1, type = c("response"))
predict_test1

library(pROC)
h =roc(ibm_test$Attrition,predict_test1, data=ibm_test)
h
plot(h)

predict_test1new<-ifelse(predict_test1 > 0.2,1,0)
predict_test1new

library(pROC)
h <- roc(Attrition~predict_test1new, data=ibm_test)
h
plot(h)

#Efficiency is 71%

#Training model 2 considering only 5 main factors.Model 2 BusinessTravel,DistancefromHome,Overtime,Worklifebalance and EnvironmentSatisfaction

train.model2 <- glm(ibm_train$Attrition~ibm_train$BusinessTravel+ibm_train$DistanceFromHome+ibm_train$OverTime +ibm_train$WorkLifeBalance+ibm_train$EnvironmentSatisfaction,family = binomial(link = "logit"), ibm_train)
summary(train.model2)

#Testing Model 2

test.model2 <- glm(ibm_test$Attrition~ibm_test$BusinessTravel+ibm_test$DistanceFromHome+ibm_test$OverTime+ibm_test$WorkLifeBalance+ibm_test$EnvironmentSatisfaction,family = binomial(link = "logit"), ibm_test)
summary(test.model2)
library(pROC)
predict_test2=predict(test.model2, type = c("response"))
predict_test2
h =roc(ibm_test$Attrition,predict_test2, data=ibm_test)
h
plot(h)

predict_test2new<-ifelse(predict_test2 > 0.1,1,0)
predict_test2new

library(pROC)
h <- roc(Attrition~predict_test2new, data=ibm_test)
h
plot(h)

#Efficiency is 65%

#Training Model3.(Reduced model for training model 2 considering only 5 main factors BusinessTravel,DistancefromHome,Overtime,Worklifebalance and EnvironmentSatisfaction. To this if we add JobRole)

train.model3 <- glm(ibm_train$Attrition~ibm_train$BusinessTravel+ibm_train$DistanceFromHome+ibm_train$OverTime +ibm_train$WorkLifeBalance+ibm_train$EnvironmentSatisfaction+ibm_train$JobRole,family = binomial(link = "logit"), ibm_train)
summary(train.model2)

#Testing model 3
test.model3 <- glm(ibm_test$Attrition~ibm_test$BusinessTravel+ibm_test$DistanceFromHome+ibm_test$OverTime +ibm_test$WorkLifeBalance+ibm_test$EnvironmentSatisfaction+ibm_test$JobRole,family = binomial(link = "logit"), ibm_test)
summary(test.model3)

predict_test3=predict(test.model3, type = c("response"))
predict_test3

library(pROC)
h =roc(ibm_test$Attrition,predict_test3, data=ibm_test)
h
plot(h)

predict_test3new<-ifelse(predict_test3 > 0.1,1,0)
predict_test3new

library(pROC)
h <- roc(Attrition~predict_test3new, data=ibm_test)
h
plot(h)

#Efficiency is 73.29%

#Training Model4

train.model4 <- glm(ibm_train$Attrition~ibm_train$BusinessTravel+ibm_train$DistanceFromHome+ibm_train$OverTime +ibm_train$WorkLifeBalance+ibm_train$EnvironmentSatisfaction+ibm_train$JobRole+ibm_train$JobSatisfaction,family = binomial(link = "logit"), ibm_train)
summary(train.model4)

#Testing Model4

test.model4 <- glm(ibm_test$Attrition~ibm_test$BusinessTravel+ibm_test$DistanceFromHome+ibm_test$OverTime +ibm_test$WorkLifeBalance+ibm_test$EnvironmentSatisfaction+ibm_test$JobRole+ibm_test$JobSatisfaction,family = binomial(link = "logit"), ibm_test)


summary(test.model4)

predict_test4=predict(test.model4, type = c("response"))
predict_test4

library(pROC)
h =roc(ibm_test$Attrition,predict_test4, data=ibm_test)
h
plot(h)

predict_test4new<-ifelse(predict_test4 > 0.1,1,0)
predict_test4new

library(pROC)
h <- roc(Attrition~predict_test4new, data=ibm_test)
h
plot(h)

#Efficiency is 75.23%

#Confusion matrix to see how many cases of correct and incorrect classification is happening
conf_matrix<-table(predict_test4new,ibm_test$Attrition)
conf_matrix

# Output of confusion matrix corresponding to model 4
# From the confusion matrix we can see that 245 cases are correctly classified indicating efficiency of model 4 66.44%.

#                  0    1
               #0 236   8
               #1 140   57


# To find the job roles corresponding to the people who left and had done an overtime
left<-ibm[ibm$Attrition=='1',]
ot<-left[left$OverTime=='Yes',]
table(ot$JobRole)

#26%,24.4%,12.6%,24.41% are the percentages of people belonging to Job categories Research Scientist,Sales Executive, Sales Representative,Laboratory Technician 

# Visualizations
plot(ibm$Attrition~ibm$JobSatisfaction)
plot(ibm$OverTime~ibm$Attrition)
plot(ibm$OverTime[ibm$Attrition]~ibm$JobSatisfaction)

#Conclusion

#1.	Different models were built and tested to see how well the prediction works
#2.	Since the data set was small and may not help predict perfectly, we have opted the model that     gives around 75% efficiency
#3.	We feel that the factors declared significant by the model are initially relevant and related
#4.	The job roles that were found vulnerable to the attrition scenario were found to be that of      the research scientist, sales executive, sales representative,lab technician.
#5.	84% of the total employees who left belonged to these categories 
#6.	Among those who left nearly 87.4% of them did overtime

# We found that certain levels of Job satisfaction, Environment satisfaction,frequent business travel,work life balance and overtime were significant.These factors, we felt strongly connected to the job roles of the research scientist, sales executive, sales representative and lab technician.The results of the prediction model built along with intuitive understanding of the interaction of factors can be best utlised to help the HR department identify the people likely to leave the company and what specific factors to address.


```


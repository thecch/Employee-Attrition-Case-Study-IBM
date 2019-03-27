library(caTools)
library(DMwR)
library(caret)

#set working directory
setwd("C:\\Users\\Nitro5\\Desktop\\R Team Project")

ibm.dt <- data.table(read.csv("IBM HR Data (Cleaned).csv "))
##names(ibm.dt)

ibm.dt <- ibm.dt[,Application.ID:=NULL]
ibm.dt <- na.omit(ibm.dt)

# Factoring
ibm.dt$Attrition <- factor(ibm.dt$Attrition)
ibm.dt$BusinessTravel <- factor(ibm.dt$BusinessTravel)
ibm.dt$Education <- factor(ibm.dt$Education)
ibm.dt$EnvironmentSatisfaction <- factor(ibm.dt$EnvironmentSatisfaction)
ibm.dt$Gender <- factor(ibm.dt$Gender)
ibm.dt$JobInvolvement <- factor(ibm.dt$JobInvolvement)
ibm.dt$JobLevel <- factor(ibm.dt$JobLevel)
ibm.dt$JobSatisfaction <- factor(ibm.dt$JobSatisfaction)
ibm.dt$MaritalStatus <- factor(ibm.dt$MaritalStatus)
ibm.dt$PerformanceRating <- factor(ibm.dt$PerformanceRating)
ibm.dt$RelationshipSatisfaction <- factor(ibm.dt$RelationshipSatisfaction)
ibm.dt$StockOptionLevel <- factor(ibm.dt$StockOptionLevel)
ibm.dt$WorkLifeBalance <- factor(ibm.dt$WorkLifeBalance)

#################################################
# PCA
#################################################

# PCA Analysis with only continous variables
type.col <- sapply(ibm.dt,class)
cont.col <- names(type.col[type.col=="integer" | type.col=="numeric"])
PCA.dt <- ibm.dt[, cont.col, with = FALSE]

# PCA
PCA1 <- prcomp(~ ., data=PCA.dt, na.action=na.omit, scale=TRUE, center = T)

# Plot principal components
# Biplot
PCA1
biplot(PCA1)

# Scree Plot
plot(PCA1$sdev/sum(PCA1$sdev^2), type = "b")

#Remove unwanted PCA components which explain least variance
PC <- subset(PCA1$x, select=-c(PC10,PC11))

# Get factor columns
fact.col <- names(type.col[type.col == "factor"])
factor.dt <- ibm.dt[, fact.col, with = FALSE]

#Final data with PCA 
PCA.dt <- cbind(data.table(PC), factor.dt)

#################################################
# Splitting Dataset
#################################################

set.seed(2004)
sum(is.na(ibm.dt))
sum(is.na(PCA.dt))

# Train Set & Test Set split
train1 <- sample.split(Y = ibm.dt$Attrition, SplitRatio = 0.75)
train <- sample.split(Y = PCA.dt$Attrition, SplitRatio = 0.75)
trainset1 <- subset(ibm.dt, train == T)
testset1 <- subset(ibm.dt, train == F)
trainset <- subset(PCA.dt, train == T)
testset <- subset(PCA.dt, train == F)

# Check data balance
table(trainset1$Attrition)
prop.table(table(trainset1$Attrition))
table(trainset$Attrition)
prop.table(table(trainset$Attrition))

# Balance dataset
trainset1 <- SMOTE(Attrition ~., data=trainset1, perc.over = 500, perc.under = 120)
trainset <- SMOTE(Attrition ~., data=trainset, perc.over = 500, perc.under = 120)
table(trainset1$Attrition)
table(trainset$Attrition)

# Remove useless variale to save on memeory
rm(PCA.dt, PC, factor.dt, ibm.dt, PCA1)
rm(cont.col, fact.col, train, train1, type.col)

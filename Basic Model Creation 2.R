


# Without PCA
# Logistic Regression Model
#################################################
m.log0 <- glm(Attrition ~. , family = binomial(link = 'logit'), data = trainset1)
vif(m.log0)

m.log1 <- glm(Attrition ~. -YearsAtCompany - MonthlyIncome , family = binomial(link = 'logit'), data = trainset1)
vif(m.log1)
summary(m.log1)

m.log2 <- glm(Attrition ~. -YearsAtCompany - MonthlyIncome -NumCompaniesWorked - PercentSalaryHike - YearsWithCurrManager
              , family = binomial(link = 'logit'), data = trainset1)
summary(m.log2)

# Predict on test set
r.log <- predict(m.log2, type = 'response', newdata = testset1)

# ROC Curve
r.log.roc <- roc.curve(testset1$Attrition, r.log, plotit = T, n.thresholds = 1000)
print(r.log.roc$auc) # AUC:  0.7409741



# Precision & Recall
log.cutoff <- data.table(Threshold = r.log.roc$thresholds, TPR = r.log.roc$true.positive.rate, FPR = r.log.roc$false.positive.rate)
log.cutoff[, Youden := TPR - FPR]
accuracy.meas(testset1$Attrition, r.log, log.cutoff[which.max(Youden)]$Threshold) 
# precision = 0.307, recall = 0.673

# Create Confusion Matrix (for demonstration)
table(testset1$Attrition, r.log > log.cutoff[which.max(Youden)]$Threshold)

# With PCA
# Logistic Regression Model
#################################################
m.log.pca1 <- glm(Attrition ~., family = binomial(link = 'logit'), data = trainset)
vif(m.log.pca1)
summary(m.log.pca1)

m.log.pca2 <- glm(Attrition ~. -PC5, family = binomial(link = 'logit'), data = trainset)
summary(m.log.pca2)

# Predict on test set
r.log.pca <- predict(m.log.pca2, type = 'response', newdata = testset)

# ROC Curve
r.log.pca.roc <- roc.curve(testset$Attrition, r.log.pca, plotit = T, add.roc= T, col = "red", n.thresholds = 1000)
print(r.log.roc$auc) # AUC: 0.7409741

# Precision & Recall
log.pca.cutoff <- data.table(Threshold = r.log.roc$thresholds, TPR = r.log.roc$true.positive.rate, FPR = r.log.roc$false.positive.rate)
log.pca.cutoff[, Youden := TPR - FPR]
accuracy.meas(testset$Attrition, r.log.pca, log.pca.cutoff[which.max(Youden)]$Threshold) 
# precision = 0.304, recall = 0.655

# Without PCA
# Step-wise Logistic Regression Model
#################################################
m.slog <- step(glm(Attrition ~ ., data = trainset1, family = binomial("logit")), direction = "both")
vif(m.slog)

# Predict on training data
r1 <- predict(m.slog, type = 'response', newdata = testset1)

# ROC Curve
r.slog <- roc.curve(testset1$Attrition, r1, plotit = T, add.roc= T, col = "blue", n.thresholds = 1000)
print(r.slog$auc) # AUC: 0.741932

# Precision & Recall
slog.cutoff <- data.table(Threshold = r.slog$thresholds, TPR = r.slog$true.positive.rate, FPR = r.slog$false.positive.rate)
slog.cutoff[, Youden := TPR - FPR]
accuracy.meas(testset1$Attrition, r1, slog.cutoff[which.max(Youden)]$Threshold) 
# precision = 0.312, recall = 0.667


# With PCA
# Step-wise Logistic Regression Model
#################################################
m.slog.pca <- step(glm(Attrition ~ ., data = trainset, family = binomial("logit")), direction = "both")
vif(m.slog.pca)

# Predict on test set
r2 <- predict(m.slog.pca, type = 'response', newdata = testset)

# ROC Curve
r.slog.pca <- roc.curve(testset$Attrition, r2, plotit = T, add.roc= T, col = "green", n.thresholds = 1000)
print(r.slog.pca$auc) # AUC: 0.7392669 choosing the best cost for classification tree

# Precision & Recall
slog.pca.cutoff <- data.table(Threshold = r.slog.pca$thresholds, TPR = r.slog.pca$true.positive.rate, FPR = r.slog.pca$false.positive.rate)
slog.pca.cutoff[, Youden := TPR - FPR]
accuracy.meas(testset$Attrition, r2, slog.pca.cutoff[which.max(Youden)]$Threshold) 
# precision = 0.291, recall = 0.729

table(testset)


# Without PCA
# CRT
#################################################
m.crt0 <- rpart(Attrition ~ ., data = trainset1, control = rpart.control(minsplit = 2, cp = 0))

crt.cutoff <- data.table(m.crt0$cptable)
m.crt1 <- prune(m.crt0, cp = crt.cutoff[xerror < crt.cutoff[which.min(xerror), xerror+xstd], first(CP)])

# Predict on test set
r.crt <- predict(m.crt1, newdata = testset1)

#Check the accuracy
accuracy.meas(testset1$Attrition, ifelse(r.crt[,2] > r.crt[, 1], 1, 0)) 
# Precision = 0.554, recall = 0.804

# ROC Curve
r.crt1.roc <- roc.curve(testset1$Attrition, ifelse(r.crt[,2] > r.crt[, 1], 1, 0), add.roc = T, col='yellow')
print(r.crt1.roc$auc)
# AUC: 0.8384095

table(testset$Attrition, predict(m.crt1, type = 'class', newdata = testset1))


# With PCA
# CRT
#################################################
m.crt.pca0 <- rpart(Attrition ~ ., data = trainset, method = 'class', control = rpart.control(minsplit = 2, cp = 0))
plotcp(m.crt.pca0)

crt.pca.cutoff <- data.table(m.crt.pca0$cptable)
m.crt.pca1 <- prune(m.crt.pca0, cp = crt.pca.cutoff[xerror < crt.pca.cutoff[which.min(xerror), xerror+xstd], first(CP)])

# Predict on test set
r.crt.pca <- predict(m.crt.pca1, newdata = testset)

#Check the accuracy
accuracy.meas(testset$Attrition, ifelse(r.crt.pca[,2] > r.crt.pca[, 1], 1, 0)) # Precision = 0.374, recall = 0.786

# ROC Curve
r.crt.pca.roc <- roc.curve(testset$Attrition, ifelse(r.crt.pca[,2] > r.crt.pca[, 1], 1, 0), add.roc = T, col='green')
print(r.crt.pca.roc$auc)
# AUC: 0.7640604




# With PCA
# Random Forest
#################################################
library(randomForest)

# Create Models
for (i in 1:50) {
  print(i)
  set.seed(i)
  
  m.rf1 <- randomForest(Attrition ~., data = trainset, seed = i, ntree = 501)
  
  # Save model to file
  save(m.rf1, file = paste("R Models\\rf1.", toString(i), ".rda", sep = ""))
}

r.rf1.ls <- c() #AUC
#r.rf1.ls2 <- c() #Classification Rate
r.rf1.ls3 <- c() #Precision 
r.rf1.ls4 <-c() #Recall

# Load models and predict
for (i in 1:50) {
  print(i)
  
  # Load model from file
  load(paste("R Models\\rf1.", toString(i), ".rda", sep = ""))
  
  # Predict on test set
  r.rf1 <- predict(m.rf1,type = "response", testset)
  r.rf1
  
  #temp2 <- mean(testset$Attrition == r.rf1) #to drop
  #r.rf1.ls2 <- c(r.rf1.ls2, temp2)
  
  # ROC Curve
  temp <- roc.curve(testset$Attrition, r.rf1, add.roc = F)
  
  r.rf1.ls <- c(r.rf1.ls, temp$auc)
  
  #Precision
  temp3 <-  accuracy.meas(testset$Attrition, ifelse(r.rf1 =="Voluntary Resignation", 1, 0))
  r.rf1.ls3 <- c(r.rf1.ls3, temp3$precision)
  r.rf1.ls4 <- c(r.rf1.ls4, temp3$recall)
}

mean(r.rf1.ls)
#mean(r.rf1.ls2)
mean(r.rf1.ls3)
mean(r.rf1.ls4)

# Average AUC: 0.8597881
# Average Accuracy: 0.8454883
# Average Precision: 0.5171035
# Average Recall: 0.8810714

# Without PCA
# Random Forest
#################################################

# Create Models 
for (i in 1:50) {
  print(i)
  set.seed(i)
  
  m.rf0 <- randomForest(Attrition ~., data = trainset1, seed = i, ntree = 501)
  
  # Save model to file
  save(m.rf0, file = paste("R Models\\rf0.", toString(i), ".rda", sep = ""))
}

r.rf0.ls <- c()
#r.rf0.ls2 <-c()
r.rf0.ls3 <-c()
r.rf0.ls4 <-c()

# Load models and predict
for (i in 1:50) {
  print(i)
  
  # Load model from file
  load(paste("R Models\\rf0.", toString(i), ".rda", sep = ""))
  
  # Predict on test set
  r.rf0 <- predict(m.rf0, testset1)
  
  #temp3 <- mean(testset$Attrition == r.rf1)
  #r.rf0.ls2 <- c(r.rf0.ls2, temp3)
  
  # ROC Curve
  temp <- roc.curve(testset1$Attrition, r.rf0, add.roc = F)
  
  r.rf0.ls <- c(r.rf0.ls, temp$auc)
  
  #Precision
  temp4 <-  accuracy.meas(testset1$Attrition, ifelse(r.rf0 =="Voluntary Resignation", 1, 0))
  r.rf0.ls3 <- c(r.rf0.ls3, temp4$precision)
  r.rf0.ls4 <- c(r.rf0.ls4, temp4$recall)
  
}
mean(r.rf0.ls)
#mean(r.rf0.ls2)
mean(r.rf0.ls3)
mean(r.rf0.ls4)
# Average AUC: 0.9063462
# Average Accuracy: 0.8413086
# Average Precision: 0.7844582
# Average Recall: 0.8590476

#Finding the most important variables for RF w/o PCA
Random_Forest_Without_PCA <- randomForest(Attrition ~., data = trainset1, seed = 2, ntree = 501)
m.rf5
importance(Random_Forest_Without_PCA)
varImpPlot(Random_Forest_Without_PCA)

# Comparing Random Forest Model #Proving 
#################################################

# F test to compare two variances
var.test(r.rf0.ls, r.rf1.ls)
# F = 0.65559, num df = 49, denom df = 49, p-value = 0.143
# Reject null hypothesis. True ratio of variances is not equal to 1

# 2 Sample t-test
# H0: r.rf0.ls <= r.rf1.ls
# h1: r.rf0.ls > r.rf1.ls
t.test(r.rf0.ls, r.rf1.ls, var.equal = FALSE, paired = FALSE, alternative = "greater")
# t = 77.538, df = 93.935, p-value < 2.2e-16
# Random Forest without PCA is the better model at 95% confidence


# Comparing Random Forest Model and Regression Model
#################################################

# 1 Sample t-test
# Ho: r.rf0.ls <= 0.7429046 #highest AUC among LRM and CRT
# H1: r.rf0.ls > 0.7429046
t.test(r.rf0.ls, mu = 0.7429046, alternative = "greater")
# t = 432.55, df = 49, p-value < 2.2e-16
# Random forest is the better model at 95% confidence

# Remove models to save memory
rm(m.log0, m.log1)
rm(m.step0, m.step1)
rm(m.crt0, m.crt1)
rm(m.rf0, m.rf1)
rm(testset, trainset)


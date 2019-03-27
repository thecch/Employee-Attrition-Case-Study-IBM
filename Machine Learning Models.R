library(h2o)
library(tidyquant)

# Initialize Machine Learning Object
# Change port number if it doesnt init
h2o.init(nthreads = -1, max_mem_size = '8g', port = 54321)
h2o.no_progress()

# Initialize trainset and testset
train.h2o <- as.h2o(trainset1)
test.h2o <- as.h2o(testset1)

# Set independent and dependent variables
y <- "Attrition"
x <- setdiff(names(train.h2o), y)


# Distributed Random Forest Model
#################################################

# Create Models
for (i in 1:30) {
  print(i)
  set.seed(i)
  
  m.drf <- h2o.randomForest(y = y, x = x, training_frame = train.h2o, ntrees = 1001, seed = i)
  
  # Save model to file
  h2o.saveModel(object = m.drf, path = paste(getwd(), "\\R Models", sep = ""), force = TRUE)
  name <- file.path(paste(getwd(), "\\R Models", sep = ""), paste("drf", toString(i), sep = ""))
  file.rename(file.path(paste(getwd(), "\\R Models", sep = ""), m.drf@model_id), name)
}

r.drf.ls <- c() #AUC
r.drf.ls2 <- c()#Precision
r.drf.ls3 <- c()#Recall

# Load models and predict
for (i in 1:30) {
  print(i)
  
  # Load model from file
  m.drf <- h2o.loadModel(paste(getwd(), "\\R Models\\drf", toString(i), sep = ""))
  
  # Predict on test set
  r.drf <- h2o.predict(m.drf, newdata = test.h2o)
  r.drf1 <- ifelse(r.drf[2] < r.drf[3], 1, 0)
  
  # ROC Curve
  temp <- roc.curve(as.vector(test.h2o["Attrition"]), as.vector(r.drf1), add.roc = F)
  r.drf.ls <- c(r.drf.ls, temp$auc)
  
  #Precision
  temp5 <- accuracy.meas(as.vector(test.h2o["Attrition"]), as.vector(r.drf1))
  r.drf.ls2 <- c(r.drf.ls2, temp5$precision)
  r.drf.ls3 <- c(r.drf.ls3, temp5$recall)
  
}
mean(r.drf.ls)
mean(r.drf.ls2)
mean(r.drf.ls3)

# Average AUC: 0.8989143
# Average Precision: 0.6940468
# Average Recall: 0.8734127


# Gradient Boosting Model
#################################################

# Create Models
for (i in 1:30) {
  print(i)
  set.seed(i)
  
  m.gbm <- h2o.gbm(y = y, x = x, training_frame = train.h2o, ntrees = 1001, seed = i)
  
  # Save model to file
  h2o.saveModel(object = m.gbm, path = paste(getwd(), "\\R Models", sep = ""), force = TRUE)
  name <- file.path(paste(getwd(), "\\R Models", sep = ""), paste("gbm", toString(i), sep = ""))
  file.rename(file.path(paste(getwd(), "\\R Models", sep = ""), m.gbm@model_id), name)
}

r.gbm.ls <- c() #AUC
r.gbm.ls2 <- c()#Precision
r.gbm.ls3 <- c()#Recall

# Load models and predict
for (i in 1:30) {
  print(i)
  
  # Load model from file  
  m.gbm <- h2o.loadModel(paste(getwd(), "\\R Models\\gbm", toString(i), sep = ""))
  
  # Predict on test set
  r.gbm <- h2o.predict(m.gbm, newdata = test.h2o)
  r.gbm1 <- ifelse(r.gbm[2] < r.gbm[3], 1, 0)
  
  # ROC Curve
  temp <- roc.curve(as.vector(test.h2o["Attrition"]), as.vector(r.gbm1), add.roc = F)
  r.gbm.ls <- c(r.gbm.ls, temp$auc)
  
  #Precision
  temp6 <- accuracy.meas(as.vector(test.h2o["Attrition"]), as.vector(r.gbm1))
  r.gbm.ls2 <- c(r.gbm.ls2, temp6$precision)
  r.gbm.ls3 <- c(r.gbm.ls3, temp6$recall)
  
}
mean(r.gbm.ls)
mean(r.gbm.ls2)
mean(r.gbm.ls3)
# Average AUC: 0.9174232
# Average Precision: 0.9167174
# Average Recall: 0.8497024


# Comparing Random Forest Model and Distributed Random Forest Model
#################################################

# F test to compare two variances
var.test(r.rf0.ls, r.drf.ls)
# F = 1.0242, num df = 49, denom df = 29, p-value = 0.9656
# Reject null hypothesis. True ratio of variances is not equal to 1

# 2 Sample t-test to compare mean
t.test(r.rf0.ls, r.drf.ls, var.equal = FALSE, paired = FALSE, alternative = "greater")
# t = 12.135, df = 61.784, p-value < 2.2e-16
# Random Forest is the better model at 95% confidence


# Comparing Random Forest Model and Gradient Boosted Model
#################################################

# F test to compare two variances
var.test(r.rf0.ls, r.gbm.ls)
# F = Inf, num df = 49, denom df = 29, p-value < 2.2e-16
# Reject null hypothesis. True ratio of variances is not equal to 1

# 2 Sample t-test to compare mean
t.test(r.gbm.ls, r.rf0.ls, var.equal = FALSE, paired = FALSE, alternative = "greater")
# t = 47.373, df = 49, p-value < 2.2e-16
# Gradient Boosted Model is the better model at 95% confidence




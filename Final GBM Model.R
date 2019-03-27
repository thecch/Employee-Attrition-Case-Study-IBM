
# Restart h2o
h2o.shutdown(prompt = F)
h2o.init(nthreads = -1, max_mem_size = '8g', port = 50003)
h2o.no_progress()

# Initialize trainset and testset
test.h2o <- as.h2o(testset1)
train.h2o <- as.h2o(trainset1)
split.h2o <- h2o.splitFrame(data = train.h2o, ratios = 0.8)
newtrain.h2o <- split.h2o[[1]]
valid.h2o <- split.h2o[[2]]

# Set independent and dependent variables
y <- "Attrition"
x <- setdiff(names(train.h2o), y)

minDepth <- 17
maxDepth <- 27

# Parameters for final GBM Machine learning model
# http://docs.h2o.ai/h2o/latest-stable/h2o-docs/data-science/gbm.html
hyper_params <- list(
  max_depth = seq(minDepth,maxDepth,1),
  sample_rate = seq(0.2,1,0.01),
  col_sample_rate = seq(0.8,1,0.01),
  col_sample_rate_per_tree = seq(0.8,1,0.01),
  col_sample_rate_change_per_level = seq(0.9,1.1,0.01),
  min_rows = 2^seq(0,log2(nrow(newtrain.h2o))-1,1),
  nbins = 2^seq(4,10,1),
  nbins_cats = 2^seq(4,12,1),
  min_split_improvement = c(0,1e-8,1e-6,1e-4),
  histogram_type = c("UniformAdaptive","QuantilesGlobal","RoundRobin")
)

# Parameters for final GBM Machine learning grid
# http://docs.h2o.ai/h2o/latest-stable/h2o-docs/grid-search.html
search_criteria = list(
  strategy = "RandomDiscrete",
  max_models = 30,
  seed = 2004,
  stopping_rounds = 5,
  stopping_metric = "AUC",
  stopping_tolerance = 1e-3
)

# Only ran the loop till 547
# Created a total of 547 * 30 models from this machine learning model
# Top 5 model came from seeds = 159, 463, 326, 373, 340
# Machine learning algo
#################################################
for (n in 1:1000) {
  
  # Create Model
  grid <- h2o.grid(
    hyper_params = hyper_params,
    search_criteria = search_criteria,
    algorithm = "gbm",
    grid_id = "final",
    x = x,
    y = y,
    training_frame = train.h2o,
    validation_frame = valid.h2o,
    ntrees = 10000,
    learn_rate = 0.01,
    stopping_rounds = 20, stopping_tolerance = 1e-4, stopping_metric = "AUC",
    score_tree_interval = 5,
    seed = n
  )
  
  # Sort models according to best AUC
  sortedGrid <- h2o.getGrid("final", sort_by = "auc", decreasing = TRUE)
  
  # Score Model and delete models that isnt in the top 5
  # This is done due to memory constraints
  for (i in 1:length(sortedGrid@model_ids)) {
    temp <- h2o.getModel(sortedGrid@model_ids[[i]])
    
    # Save top 5 models to file
    if (i <= 5) {
      p <- h2o.saveModel(object = temp, path = paste(getwd(), "\\R Models", sep = ""), force = TRUE)
      name <- file.path(paste(getwd(), "\\R Models", sep = ""), paste("final", toString(i), sep = ""))
      file.rename(file.path(paste(getwd(), "\\R Models", sep = ""), temp@model_id), name)
    }
    # Delete all other models
    else {
      h2o.rm(temp)
    }
  }
}


# Compare top 5 model on test set
#################################################
final1 <- h2o.loadModel(paste(getwd(), "\\R Models\\final1", sep = ""))
h2o.auc(h2o.performance(final1, newdata = test.h2o))
max(h2o.accuracy(h2o.performance(final1, newdata = test.h2o))$accuracy)
max(h2o.accuracy(h2o.performance(final1, newdata = test.h2o))$accuracy)
# AUC: 0.9533666
# Accuracy: 0.9467773

final2 <- h2o.loadModel(paste(getwd(), "\\R Models\\final2", sep = ""))
h2o.auc(h2o.performance(final2, newdata = test.h2o))
max(h2o.accuracy(h2o.performance(final2, newdata = test.h2o))$accuracy)
# AUC: 0.9494378
# Accuracy: 0.9501953

final3 <- h2o.loadModel(paste(getwd(), "\\R Models\\final3", sep = ""))
h2o.auc(h2o.performance(final3, newdata = test.h2o))
max(h2o.accuracy(h2o.performance(final3, newdata = test.h2o))$accuracy)
# AUC:0.9494126
# Accuracy: 0.9492188

final4 <- h2o.loadModel(paste(getwd(), "\\R Models\\final4", sep = ""))
h2o.auc(h2o.performance(final4, newdata = test.h2o))
max(h2o.accuracy(h2o.performance(final4, newdata = test.h2o))$accuracy)
# AUC: 0.9501905
# Accuracy: 0.9506836

final5 <- h2o.loadModel(paste(getwd(), "\\R Models\\final5", sep = ""))
h2o.auc(h2o.performance(final5, newdata = test.h2o))
max(h2o.accuracy(h2o.performance(final5, newdata = test.h2o))$accuracy)
# AUC: 0.9501879
# Accuracy: 0.9472656


# Final1 seems to be the best model similar to how our leaderboard deemed them to be
final1@parameters

# Examine variance in AUC
#################################################

# Porting full dataset into h2o frame
ibm.dt <- data.table(read.csv("IBM HR Data (Cleaned).csv "))
ibm.dt <- ibm.dt[,Application.ID:=NULL]
ibm.dt <- na.omit(ibm.dt)
ibm.h2o <- as.h2o(ibm.dt)

# Create GBM with nfolds cross validation
model <- do.call(h2o.gbm,
                 {
                   p <- final1@parameters
                   p$model_id = NULL
                   p$training_frame = ibm.h2o
                   p$validation_frame = NULL
                   p$nfolds = 5
                   p
                 }
)

model@model$cross_validation_metrics_summary[5,]
# Variance seems to be really high
# Possibility that ensemble techniques will improve model even more


# Get blended prediction with top 5 models
#################################################
prob = NULL
for (i in 1:5) {
  m <- h2o.loadModel(paste(getwd(), "\\R Models\\final", toString(i), sep = ""))
  if (is.null(prob)) prob = h2o.predict(m, test.h2o)["Voluntary Resignation"]
  else prob = prob + h2o.predict(m, test.h2o)["Voluntary Resignation"]
}
prob <- prob/5

# Get ROC curve
roc.curve(as.vector(as.numeric(test.h2o[[y]])), as.vector(prob))$auc
# AUC: 0.9511223
# Ensemble techniques doesnt seem to work
# Sticking to final1 instead


# Completed Final GBM
#################################################
# No tweaks done to it after machine learning algo
finalmodel <- h2o.loadModel(paste(getwd(), "\\R Models\\final1", sep = ""))

# Cutoff for tweaking model to product maximum performance according to requirements
h2o.performance(finalmodel, newdata = test.h2o)@metrics$max_criteria_and_metric_scores

# Important variables in predicting
h2o.varimp(finalmodel)









# Restart h2o
h2o.shutdown(prompt = F)
h2o.init(nthreads = -1, max_mem_size = '8g', port = 50002)
h2o.no_progress()

# Initialize trainset and testset
test.h2o <- as.h2o(testset1)
train.h2o <- as.h2o(trainset1)
split.h2o <- h2o.splitFrame(data = train.h2o, ratios = 0.8)
train.h2o <- split.h2o[[1]]
valid.h2o <- split.h2o[[2]]

# Set independent and dependent variables
y <- "Attrition"
x <- setdiff(names(train.h2o), y)

# Baseline GBM model
#################################################
for (i in 1:30){
  print(i)
  
  # Create Model
  ml1 <- h2o.gbm(x = x, y = y, training_frame = train.h2o, seed = 2004)
  
  # Save model to file
  h2o.saveModel(object = ml1, path = paste(getwd(), "\\R Models", sep = ""), force = TRUE)
  name <- file.path(paste(getwd(), "\\R Models", sep = ""), paste("ml_1_", toString(i), sep = ""))
  file.rename(file.path(paste(getwd(), "\\R Models", sep = ""), ml1@model_id), name)
}

r.ml1.ls <- c()

# Load models and predict
for (i in 1:30) {
  print(i)
  
  # Load model from file
  ml1 <- h2o.loadModel(paste(getwd(), "\\R Models\\ml_1_", toString(i), sep = ""))
  
  # AUC
  r.ml1.ls <- c(r.ml1.ls, h2o.auc(h2o.performance(ml1, newdata = valid.h2o)))
}

mean(r.ml1.ls)
# Average AUC: 0.9805354

# New model to tune no. of trees 
#################################################
ml2 <- h2o.gbm(
  x = x,
  y = y,
  training_frame = train.h2o,
  validation_frame = valid.h2o,
  sample_rate = 0.8,
  col_sample_rate = 0.8,
  seed = 2004,
  learn_rate = 0.01,
  # Score the model for every 5 new trees
  score_tree_interval = 5,
  # More than enough trees to find the most optimal number
  ntrees = 10000,
  # Stop adding trees if AUC doesn't improve by at least 0.01% for 20 consecutive scoring events
  stopping_rounds = 20, stopping_tolerance = 1e-4, stopping_metric = "AUC"
)

# Get AUC with validation set
h2o.auc(h2o.performance(ml2, newdata = valid.h2o))
# AUC: 0.9916917

# Get optimal number of trees
ml2.p <- ml2@model
ml2.p$model_summary[1]$number_of_trees
# Optimal no. of tree = 3380


# New model to tune dept of trees
#################################################
hyper_params <- list(max_depth = seq(1, 65, 8)) 

# Huge increment to find range where it's the most suitable
grid1 <- h2o.grid(
  hyper_params = hyper_params,
  search_criteria = list(strategy = "Cartesian"),
  algorithm="gbm",
  grid_id="depth_grid",
  x = x,
  y = y,
  training_frame = train.h2o,
  validation_frame = valid.h2o,
  ntrees = 10000,
  learn_rate = 0.01,
  sample_rate = 0.8,
  col_sample_rate = 0.8,
  seed = 2004,
  stopping_rounds = 20,
  stopping_tolerance = 1e-4,
  stopping_metric = "AUC",
  score_tree_interval = 5
)

# Find range of top 5 maxDepths
sortedGrid <- h2o.getGrid("depth_grid", sort_by="auc", decreasing = TRUE)
topDepths <- sortedGrid@summary_table$max_depth[1:5]
minDepth <- min(as.numeric(topDepths))
maxDepth <- max(as.numeric(topDepths))

# Repeat with closer intervals (4)
#################################################
hyper_params <- list(max_depth = seq(minDepth, maxDepth, 4))

# Create Model
grid2 <- h2o.grid(
  hyper_params = hyper_params,
  search_criteria = list(strategy = "Cartesian"),
  algorithm="gbm",
  grid_id="depth_grid2",
  x = x,
  y = y,
  training_frame = train.h2o,
  validation_frame = valid.h2o,
  ntrees = 10000,
  learn_rate = 0.01,
  sample_rate = 0.8,
  col_sample_rate = 0.8,
  seed = 2004,
  stopping_rounds = 20,
  stopping_tolerance = 1e-4,
  stopping_metric = "AUC",
  score_tree_interval = 5
)

# Find range of top 5 maxDepths
sortedGrid <- h2o.getGrid("depth_grid2", sort_by="auc", decreasing = TRUE)
topDepths <- sortedGrid@summary_table$max_depth[1:5]
minDepth <- min(as.numeric(topDepths))
maxDepth <- max(as.numeric(topDepths))

# Repeat with closer intervals (2)
#################################################
hyper_params <- list(max_depth = seq(minDepth, maxDepth, 2))

# Create Model
grid3 <- h2o.grid(
  hyper_params = hyper_params,
  search_criteria = list(strategy = "Cartesian"),
  algorithm="gbm",
  grid_id="depth_grid3",
  x = x,
  y = y,
  training_frame = train.h2o,
  validation_frame = valid.h2o,
  ntrees = 10000,
  learn_rate = 0.01,
  sample_rate = 0.8,
  col_sample_rate = 0.8,
  seed = 2004,
  stopping_rounds = 20,
  stopping_tolerance = 1e-4,
  stopping_metric = "AUC",
  score_tree_interval = 5
)

# Find range of top 5 maxDepths
sortedGrid <- h2o.getGrid("depth_grid3", sort_by="auc", decreasing = TRUE)
topDepths <- sortedGrid@summary_table$max_depth[1:5]
minDepth <- min(as.numeric(topDepths))
maxDepth <- max(as.numeric(topDepths))

# 3380
# Max Depth: 27
# Min Depth: 17


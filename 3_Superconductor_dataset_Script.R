# The goal is to predict the "critical_temp" 
# of a superconducting material based on the features. 
    
# About Superconductor Dataset:
#   -The outcome is "critical_temp", the continuous variable in 82nd column
#    ranging from 3.25×10^−4 to 185
#   -The features are numeric variables, in columns 1:81, with different scales.
#    The column 1 is an integer, columns 2:82 are continuous variables, not normally
#    distributed and the relations outcome-predictors are not linear.

# Packages:  
if(!require(readxl)) install.packages("readxl")
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(tidyr)) install.packages("tidyr")
if(!require(dplyr)) install.packages("dplyr")
if(!require(caret)) install.packages("caret")
if(!require(randomForest)) install.packages("randomForest")

# The original file **train.csv** was re-saved as 4_Superconductors.xlsx file.
# Reading  Superconductor Dataset xlsx file:
fileName <- "4_Superconductors.xlsx"
superconductors <- read_excel(fileName) 

#  **superconductors** data  will be split the following way:
# -validation set (20%);
# -training set (48%);
# -test set  (32%),
# where training set and test set are all together 80% of the original data
# are in sc set.

# Dividing **superconductors** data into validation set (20%) and sc set(80%):
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
index_val <- createDataPartition(y = superconductors$critical_temp,
                                 times = 1, p = 0.2, list = FALSE)
validation <- superconductors[index_val, ]
sc <- superconductors[-index_val, ]

# Dividing sc set into train and test sets.
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
index <- createDataPartition(y = sc$critical_temp, times = 1, p = 0.4, list = FALSE)
test_set <- sc[index, ]
train_set <- sc[-index, ]

# Root-mean-squared-error (RMSE) function is used to evaluate the algorithm:  
RMSE <- function(true_temp, predicted_temp){
  sqrt(mean((true_temp - predicted_temp)^2))
}

# Among the algorithms tested, Random Forest showed the best result, therefore
# it is used for the Script.
# randomForest() function performed better with  default settings.
# The **importance** matrix (the output of randomForest() function)
# is used to filter predictors. 
# In the **Superconductor** dataset analysis it was shown that using 
# IncNodePurity > 110000 as cut off to find best predictors result in 
# selecting 17 predictors, train set was used to fit the data.
# 'IncNodePurity' depends on the size of the set that is used to fit the data,
# therefore for the final script, instead of using 'cut off', the number of 
# predictors will simply be rounded to 20.

# First, using train and test sets, all predictors and default parameters,
# and setting "importance = TRUE":
set.seed(1, sample.kind="Rounding")
fit_rf <- randomForest(critical_temp ~ ., importance = TRUE, data = train_set)
pred_rf <- predict(fit_rf, newdata = test_set)
rmse_rf_def <- RMSE(test_set$critical_temp, pred_rf)

# Second, finding top 20 predictors from **importance** matrix:
importance_matrix <- fit_rf$importance %>% as.data.frame() %>%
  rownames_to_column() %>% arrange(desc(IncNodePurity)) %>% slice_head(n=20)

# indices for these 20 columns:
index_imp <- which(colnames(sc) %in% importance_matrix$rowname)
# Adding critical_temp column:
index_imp <- c(index_imp, 82) %>% unique() # making sure indices are not doubled

#  Running a Random Forest algorithm using sc set (80% of Superconductor Dataset)
# and top 20 predictors from the **importance** matrix to fit the data:
set.seed(1, sample.kind="Rounding")
fit <- randomForest(critical_temp ~ ., data = sc[index_imp]) 

# making prediction for the critical temperature of superconductor
# using validation set (the other 20% of Superconductor Dataset)
pred <- predict(fit, newdata = validation[index_imp])
# and Root-mean-squared-error 
rmse_val <- RMSE(validation$critical_temp, pred) 
# calculated for the **validation** set is:  
rmse_val




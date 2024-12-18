library(xgboost)
# Matrix for xgb: dtrain and dtest, "label" is the dependent variable
dtrain <- xgb.DMatrix(X_train, label = Y_train)
dtest <- xgb.DMatrix(X_test, label = Y_test)

best_param <- list()
best_seednumber <- 1234
best_rmse <- Inf
best_rmse_index <- 0

set.seed(123)

param <- list(objective = "multi:softprob",
              eval_metric = "mlogloss",
              num_class = 12,
              max_depth = 8,
              eta = 0.05,
              gamma = 0.01, 
              subsample = 0.9,
              colsample_bytree = 0.8, 
              min_child_weight = 4,
              max_delta_step = 1
)
cv.nround = 1000
cv.nfold = 5
mdcv <- xgb.cv(data=dtrain, params = param, nthread=6, 
               nfold=cv.nfold, nrounds=cv.nround,
               verbose = T)

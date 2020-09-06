# Boosting with smote 1.1
# Gradient Boosting
fitControl <- trainControl(
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10, sampling = 'smote')
fitControl$sampling<- 'smote'
set.seed(825)
gbmFit1 <- train(Transport ~ ., data = cartrain, 
                 method = "gbm", 
                 trControl = fitControl,
                 verbose = FALSE)
gbmFit1
pred<- gbmFit1 %>% predict(cartest[,-9])
confusionMatrix(pred, cartest[,9])    # Accurcy 81.08 still good amount  of misclassification in 2 wheeler


# Gradient Boosting  with tuning  1.2

gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 9), 
                        n.trees = (1:30)*50, 
                        shrinkage = 0.1,
                        n.minobsinnode = 20)

nrow(gbmGrid)

set.seed(825)
gbmFit2 <- train(Transport ~ ., data = cartrain, 
                 method = "gbm", 
                 trControl = fitControl, 
                 verbose = FALSE, 
                 tuneGrid = gbmGrid)
gbmFit2
pred<- gbmFit2 %>% predict(cartest[,-9])
confusionMatrix(pred, cartest[,9])    # Accurcy 82.88 still good amount  of misclassification in 2 wheeler

# Xtreme Gradient Boostingtree  1.1
fitControl <- trainControl(
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10, sampling = 'smote')
fitControl$sampling<- 'smote'

model <- train( Transport ~ ., data = cartrain, method = "xgbTree", 
                trControl = fitControl)
pred<- model %>% predict(cartest[,-9])
confusionMatrix(pred, cartest[,9])    # Accurcy 80.18 still good amount  of misclassification in 2 wheeler


# XGBtree with hyper parameter tuning
xgb.grid = expand.grid(
  nrounds = 2, 
  max_depth = c(5, 10, 15), 
  eta = c(0.01, 0.001, 0.0001), 
  gamma = c(1, 2, 3), 
  colsample_bytree = c(0.4, 0.7, 1.0), 
  min_child_weight = c(0.5, 1, 1.5)
)

control = trainControl(method = "cv", 
                       summaryFunction = mnLogLoss,
                       classProbs = TRUE,
                       #allowParallel=T, 
                       number = 10,
                       ## repeated ten times
                       sampling = 'smote',
                       summaryFunction = multiClassSummary,
                       allowParallel    = TRUE)

modeltune = train(Transport ~ ., data = cartrain, method="xgbTree", trControl=control, 
             tuneGrid = xgb.grid, verbose=TRUE, metric="logLoss", nthread=3)

# XGB hyper parameter tuning
xgb_grid_1 = expand.grid(
  eta = c(0.01, 0.001, 0.0001),
  max_depth = c(2, 4, 6, 8, 10, 15),
  gamma = 1,
  nrounds = 1200,
  colsample_bytree = 1,  
  min_child_weight = 1, 
  subsample = 1
)


xgb_trcontrol_1 <-
  trainControl(
    verbose          = TRUE,
    method           = "cv",
    # K-fold cross validation
    number           = 3,
    summaryFunction  = defaultSummary,
    allowParallel    = TRUE,
    sampling = 'smote'
  )
xgb_trcontrol_1$sampling<- 'smote'
modFit = train(Transport ~ ., 
               data=cartrain,
               trControl = xgb_trcontrol_1,
               tuneGrid = xgb_grid_1,
               method = "xgbTree",
               #metric    = 'RMSE',
               maximize  = FALSE,
               preProcess = c("center", "scale"),
               nthread = 1    
)

modFit$results
pred<- modFit %>% predict(cartest[,-9])
confusionMatrix(pred, cartest[,9])   # Accuracy is 77.48



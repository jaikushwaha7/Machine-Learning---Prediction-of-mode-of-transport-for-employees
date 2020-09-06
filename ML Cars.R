
setwd("D:/Study/Great Lakes/machine learning/group assignment")
library(class)
library(dplyr)
library(mice)
library(scales)
library(superheat)
library(corrplot)
library(caret)
library(caTools)
library(usdm)
library(nnet)
library(ROSE)
library(SmartEDA)
library(tidyr)
library(plyr)


data  = read.csv(file.choose(), header = T, na.strings = NA)
head(data)
dim(data)  # 444 rows and 9 columns
# target variable -> Transport
str(data)
# GEnder and transport factor data rest 6 columns are continuous data
summary(data)
# General eda basis mean mode max min.

# NA search
anyNA(data)
# There is na in data
sum(is.na(data))
# 1 na
# MBA column has na data.

 
dat <- data %>%
  mutate(
    Engineer = factor(Engineer, levels = c(0,1), labels = c('NonEngineer','Engineer')),
    MBA = factor(MBA, levels = c(0,1), labels = c('NonMBA', 'MBA')),
    license = factor(license, levels = c(0,1), labels = c('NonDL', 'DL'))
  )

#dat <- data %>%
#  mutate(
#    Engineer = as.factor(Engineer ),
#    MBA = as.factor(MBA),
#    license = as.factor(license)
#  )


summary(dat)

levels(dat$Engineer) = c('NonEngineer','Engineer')
levels(dat$MBA)= c('NonMBA', 'MBA')
levels(dat$license) = c('NonDL', 'DL')

# NA imputation using Mice

init = mice(dat, maxit=0) 
meth = init$method
predM = init$predictorMatrix

predM
meth  # logreg
imputed = mice(dat, method=meth, predictorMatrix=predM)

imputed <- complete(imputed)

sapply(imputed, function(x) sum(is.na(x)))
# No NA data

summary(data$MBA)
summary(dat$MBA)
summary(imputed$MBA)
# NA added imputed as NonMBA

# Target value ratio
prop.table(table(imputed$Transport))
# biased towards Public Transport 67 % data 

# Corelation between variables
rm(data_numeric)
data_numeric = imputed %>%
  mutate ( Gender = as.numeric(Gender),
           Engineer = as.numeric(Engineer),
           MBA = as.numeric(MBA),
           license = as.numeric(license),
           Transport = as.numeric(Transport)
           )
summary(data_numeric)
M <- cor(data_numeric)
corrplot(M, type="upper")
# 1. Age and Workexp, salary highly corelated.
# 2. Target varaible corelated with license distance salary and gender.
# 3. Work exp and salary corelated

ExpData(data=imputed,type=1)

# Desnity plot for numerical data (univariate)
plot1 <- ExpNumViz(imputed,target=NULL,nlim=10,Page=c(2,2),sample=4)
plot1[[1]]

# Frequency chart for categorical variable (univariate)
ExpCTable(imputed,Target=NULL,margin=1,clim=10,nlim=3,round=2,bin=NULL,per=T)

# BArplot for categorical data
plot2 <- ExpCatViz(imputed,target=NULL,col ="slateblue4",clim=10,margin=2,Page = c(2,2),sample=4)
plot2[[1]]

# for numeric data
ExpNumStat(imputed,by="GA",gp="Transport",Qnt=seq(0,1,0.1),MesofShape=2,Outlier=TRUE,round=2)

# Box plot
plot4 <- ExpNumViz(imputed,target="Transport",type=1,nlim=3,fname=NULL,col=c("darkgreen","springgreen3","springgreen1","springgreen2"),Page=c(2,2),sample=4)
plot4[[1]]

# Variable importance
#varimp <- ExpCatStat(imputed,Target="Transport",result = "Stat",clim=10,nlim=5,bins=10,Pclass="Yes",plot=TRUE,top=10,Round=2)
# corrplot
imputed %>%
  #filter(Transport == "Car") %>%
  select_if(is.numeric) %>%
  cor() %>%
  corrplot::corrplot()


# Density plot for numeric data
imputed %>%
  keep(is.numeric) %>%                     # Keep only numeric columns
  gather() %>%                             # Convert to key-value pairs
  ggplot(aes(value)) +                     # Plot the values
  facet_wrap(~ key, scales = "free") +   # In separate panels
  geom_density()    

# Histogram for numeric data
imputed %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

# Categorical Univariate
# Gender
ggplot(imputed, aes(x = Gender)) + 
  geom_bar(fill = "cornflowerblue", color="black") +
  labs(x = "Gender", y = "Frequency",  title = "Gender frequency")

# Engineer
ggplot(imputed, 
       aes(x = Engineer, y = ..count.. / sum(..count..))) + 
  geom_bar() +
  labs(x = "Engineer",y = "Percent", title  = "Engineer % wise") +  scale_y_continuous(labels = scales::percent)

# MBA
# For all categorical variables
imputed %>%
  keep(is.factor) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_bar()

# For all categorical variable % wise
imputed %>%
  keep(is.factor) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_bar()

summary(imputed)

# MBA Data
plotdata<- data %>% 
  count("Gender")
plotdata
rm(plotdata)
plotdata <- imputed %>%
  count("MBA") %>%
  mutate(pct =freq / sum(freq),
         pctlabel = paste0(round(pct*100), "%"))

# plot the bars as percentages, 
# in decending order with bar labels
ggplot(plotdata, 
       aes(x = reorder(MBA, -pct),
           y = pct)) + 
  geom_bar(stat = "identity", 
           fill = "indianred3", 
           color = "black") +
  geom_text(aes(label = pctlabel), 
            vjust = -0.25) +
  scale_y_continuous(labels = percent) +
  labs(x = "MBA", 
       y = "Percent", 
       title  = "MBA %wise")
# 75 % are Non Mba employess and 25 % are mba employees

#
summary(imputed)
plotdata <- imputed %>%  count(c("license", 'Transport')) %>%  mutate(pct =freq / sum(freq),pctlabel = paste0(round(pct*100), "%"))
#plotdata
# plot the bars as percentages, 
# in decending order with bar labels
ggplot(plotdata,  aes(x = reorder(Transport, -pct),y = pct)) + 
  geom_bar(stat = "identity", fill = "indianred3", color = "black", position = 'stack') +
  geom_text(aes(label = pctlabel), vjust = -0.25) +
  scale_y_continuous(labels = percent) +labs(x = "Transport Type", y = "Percent", title  = "Transport %wise")

# Transport vs Salary
ggplot(imputed, 
       aes(x = Salary, 
           fill = Transport)) +
  geom_density(alpha = 0.4) +
  labs(title = "Salary distribution by means of Transport")

# HIgh Salried prople use casr
# 2 wheeler and public transport is mostly used by low income salary group


# Transport vs Distance
ggplot(imputed, 
       aes(x = Distance, 
           fill = Transport)) +
  geom_density(alpha = 0.4) +
  labs(title = "Distance travelled by means of Transport")
# Short distance is mostly uses public transport
# For long distnce car is used by employees

# Transport vs Age
ggplot(imputed, aes(x = Transport,y = Age)) +
  geom_boxplot(notch = TRUE, fill = "cornflowerblue", alpha = .7) +
  labs(title = "Age distribution by Transport")
# Younger age group uses 2 wheeler and employee aged above 30 mostly uses car.

superheat(data_numeric, scale = TRUE)

# BIvariate  Categorical
# Transport vs License
ggplot(imputed, 
       aes(x = Transport, 
           fill = license)) + 
  geom_bar(position = "stack")
# Non Driving license people are using the puiblic transport .
# people with license are using Car
# Most 2 wheeler user are not having Driving lIcense

#
summary(imputed)
ggplot(imputed, 
       aes(x = Transport, 
           fill = Engineer)) + 
  geom_bar(position = position_dodge(preserve = "single"))


ggplot(imputed, 
       aes(x = Transport, 
           fill = MBA)) + 
  geom_bar(position = "fill") +
  labs(y = "Proportion") +theme_minimal()

# Check for multicolinearity
# One hot encoded data
d2 <- data.frame(predict(dummyVars(~., imputed), imputed))
str(d2)
# 15 columns not one hot encoded

pairs(d2, upper.panel = NULL)
corrplot(cor(d2), type = 'lower')
pcs <- prcomp(d2, center = T, scale. = T, tol = 0.8)
print(pcs)

vif(d2)
vif(data_numeric)
# Work Exp has high multicolinearity

# Data prepration as Train and Test
split <- sample.split(imputed$Transport, SplitRatio = .75)

cartrain <-  subset( imputed, split==T)
cartest<- subset(imputed, split==F)
dim(cartrain)  # 111 rows
dim(cartest)   # 333 row
prop.table(table(cartrain$Transport))
prop.table(table(cartest$Transport))
par(mfrow = c(1, 2))
par(mfrow=c(1,2))
plotdata1 <- cartrain %>%  count("Transport") %>%  mutate(pct =freq / sum(freq),pctlabel = paste0(round(pct*100), "%"))
plotdata2 <- cartest %>%  count("Transport") %>%  mutate(pct =freq / sum(freq),pctlabel = paste0(round(pct*100), "%"))
# plot the bars as percentages, 
# in decending order with bar labels
plot1 <-ggplot(plotdata1, aes(x = reorder(Transport, -pct),y = pct)) + 
  geom_bar(stat = "identity", fill = "indianred3", color = "black") +
  geom_text(aes(label = pctlabel), vjust = -0.25) +
  scale_y_continuous(labels = percent) +labs(x = "Transport", y = "Percent", title  = "Transport distribution Train")

plot2<- ggplot(plotdata2, aes(x = reorder(Transport, -pct),y = pct)) + 
  geom_bar(stat = "identity", fill = "blue", color = "black") +
  geom_text(aes(label = pctlabel), vjust = -0.25) +
  scale_y_continuous(labels = percent) +labs(x = "Transport", y = "Percent", title  = "Transport distribution Test")
grid.arrange(plot1, plot2, ncol=2)
# distribution is same for both test and train data


# MOdelling
# KNN modelling
# A 3-nearest neighbours model with no normalization
# One hot encoded train and test data for knn
d2 <- data.frame(predict(dummyVars(~., imputed[,-9]), imputed[,-9]))
cartrainknn <-  subset( d2, split==T)
dim(cartrainknn)
cartrainknn$Transport <- subset(imputed$Transport, split==T)

cartestknn<- subset(d2, split==F)
cartestknn$Transport <- subset(imputed$Transport, split == F)
dim(cartestknn)

data_pred <- knn(train = cartrainknn[,-13], test = cartestknn[,-13], cl = cartrainknn[,13], k = 3)
count(data_pred)
# Confusion matrix
confusionMatrix(data=data_pred,  reference=cartestknn$Transport)  # accuracy 74.8 %

# Normalising the data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

cartrainknn_n <- as.data.frame(lapply(cartrainknn[,-13], normalize))
cartestknn_n<- as.data.frame(lapply(cartestknn[,-13], normalize))
cartrainknn_n$Transport<- cartrainknn$Transport
cartestknn_n$Transport <- cartestknn$Transport

# MOdelling KNN with normalized data
data_pred <- knn(train = cartrainknn_n[,-13], test = cartestknn_n[,-13], cl = cartrainknn_n[,13], k = 3)
# Confusion matrix
confusionMatrix(data=data_pred,  reference=cartestknn$Transport)  # accuracy 75.7 %

# Modelling with train control, crosvalidation and expand grid for optimum K
library(caret)
trControl <- trainControl(method  = "cv", number  = 10)
fit <- train(Transport ~ .,
             method     = "knn",
             tuneGrid   = expand.grid(k = 1:50),
             trControl  = trControl,
             metric     = "Accuracy",
             data       = cartrainknn_n)

fit  # best fit at k =15 accuracy 77.78 on train data
data_pred <- predict(fit, cartestknn_n[,-13])
# Confusion Matrix
confusionMatrix(data=data_pred,  reference=cartestknn_n$Transport)   # accuracy 76.6 on test data normalized data


fit <- train(Transport ~ .,
             method     = "knn",
             tuneGrid   = expand.grid(k = 1:50),
             trControl  = trControl,
             metric     = "Accuracy",
             data       = cartrainknn)
data_pred<- predict(fit, cartestknn)
confusionMatrix(data=data_pred,  reference=cartestknn$Transport) # test prediction 81.08 normalized data

# Naive bayes modeling
library(class)
library(e1071)
head(cartrain)
dim(cartrain)
nb <- naiveBayes(Transport ~ ., cartrain)
nb
data_pred <- predict(nb,cartest[,-9],type="class")

confusionMatrix(data_pred,cartest[,9], positive = "a", mode="everything") # accuracy %79.3 (Highes misclassification is happening in 2wheeler)

# Naive Bayes assumes that your features are conditionally independent given a class label.
# If features are correlated upon conditioning on a class label, then this assumption is 
# violated (since correlatedness implies dependence).

# With naïve Bayes, we assume that the predictor variables are conditionally independent of 
# one another given the response value. This is an extremely strong assumption. We can see quickly that our transport variable 
# data violates this as we have moderately to strongly correlated variables.
cartrain %>%
  #filter(Transport == "Yes") %>%
  select_if(is.numeric) %>%
  cor() %>%
  corrplot::corrplot()
# we can see from the corrplot that independent varaibles are highly corelated.
library(dplyr)
library(raster)
install.packages("conflicted")
library(conflicted)
cartrain %>% 
  dplyr::select('Age', 'Work.Exp', 'Salary', 'Distance') %>% 
  tidyr::gather(metric, value) %>% 
  ggplot(aes(value, fill = metric)) + 
  geom_density(show.legend = FALSE) + 
  facet_wrap(~ metric, scales = "free")


# # set up 10-fold cross validation procedure
train_control <- trainControl(
  method = "cv", 
  number = 10
)

# train model
nb.m1 <- train(
  x = cartrain[,-9],
  y = cartrain[,9],
  method = "nb",
  trControl = train_control
)

confusionMatrix(nb.m1)  # train accuracy 77.78
# Test prediction
data_pred <- predict(nb.m1,cartest[,-9],type="raw")
confusionMatrix(data_pred,cartest[,9], positive = "a", mode="everything")  #  test accuracy 81.1

# Using Hyperparameter usekernel, adjust, fl (laplace smoother)
# set up tuning grid
search_grid <- expand.grid(
  usekernel = c(TRUE, FALSE),
  fL = 0:5,
  adjust = seq(0, 5, by = 1)
)

# train model
nb.m2 <- train(
  x = cartrain[,-9],
  y =cartrain[,9],
  method = "nb",
  trControl = train_control,
  tuneGrid = search_grid,
  preProc = c("BoxCox", "center", "scale", "pca")
)

# top 5 modesl
nb.m2$results %>% 
  top_n(5, wt = Accuracy) %>%
  arrange(desc(Accuracy))


# plot search grid results
plot(nb.m2)

# results for best model
confusionMatrix(nb.m2)  # train accracy 78.98

# Prediction on test
pred <- predict(nb.m2, newdata = cartest[,-9])
confusionMatrix(pred, cartest[,9])
# Accuracy 79.3
# Still preidction of 2wheeler is mostly misclassified as public transport

# Logistic Regression
# Using multinomial linear regression 
# Removing multivcolinearity from the data
# Converting the data to one hot encoded data
cartrainglm <- data.frame(predict(dummyVars(~., cartrain[,-9]), cartrain[,-9]))
cartestglm <- data.frame(predict(dummyVars(~., cartest[,-9]), cartest[,-9]))
dim(cartrainglm)
dim(cartestglm)
cartrainglm$Transport<- cartrain$Transport
cartestglm$Transport<- cartest$Transport
str(cartrainglm)
# Removing mmulticolinearity
detach("package:VIF", unload = TRUE)
library(car)

lm1<- lm(as.numeric(as.character(Transport))~., data = cartrainglm)
vif(lm1)
lm1
lm1$coefficients
names(lm1$coefficients[!is.na(lm1$coefficients)])[c(-1)]

multinom.fit <- multinom(Transport ~ ., data = cartrainglm)
# Make predictions
data_pred <- multinom.fit %>% predict(cartestglm[,-13])
data_pred
confusionMatrix(data_pred, cartestglm[,13]) # Accuracy 74.8

# Logistic regression with normal data without one hot encoding
multinom.fit <- multinom(Transport ~ ., data = cartrain)
# Make predictions
data_pred <- multinom.fit %>% predict(cartest[,-9])
data_pred
confusionMatrix(data_pred, cartest[,9]) # Accuracy 74.8

summary(multinom.fit)
vif(multinom.fit)
# VIF of age is very high 4858.20
# Removing age column

colnames(cartrain)
multinom.fit <- multinom(Transport ~. -Age, data = cartrain)
# Make predictions
data_pred <- multinom.fit %>% predict(cartest[,-9])
data_pred
confusionMatrix(data_pred, cartest[,9]) # Accuracy 82%
vif(multinom.fit)

# Next to remove Work.Exp 33.02 score
multinom.fit <- multinom(Transport ~. -Age-Work.Exp, data = cartrain, control = list(maxit = 50))
# Make predictions
data_pred <- multinom.fit %>% predict(cartest[,-9])
data_pred
confusionMatrix(data_pred, cartest[,9]) # Accuracy 82% accuracy didnt change 2wheeler still misclassifiing
vif(multinom.fit)

# Remove Distance vif score of 20.20
multinom.fit <- multinom(Transport ~. -Age-Work.Exp-Distance, data = cartrain)
# Make predictions
data_pred <- multinom.fit %>% predict(cartest[,-9])
data_pred
confusionMatrix(data_pred, cartest[,9]) # Accuracy 80.2% accuracy decreased 2wheeler still misclassifiing
vif(multinom.fit)

# Bagging
cntrl <- trainControl(method = "cv", number = 10)
fit<- train(Transport ~ .-Age, data = cartrain, method = "treebag", trControl = cntrl)
pred<- fit %>% predict(cartest[,-9])
confusionMatrix(pred, cartest[,9])   # Accuracay 81.1

# Gradient Boosting
fitControl <- trainControl(
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)
set.seed(825)
gbmFit1 <- train(Transport ~ ., data = cartrain, 
                 method = "gbm", 
                 trControl = fitControl,
                 verbose = FALSE)
gbmFit1
pred<- gbmFit1 %>% predict(cartest[,-9])
confusionMatrix(pred, cartest[,9])    # Accurcy 78.4 still good amount  of misclassification in 2 wheeler


# Gradient Boosting  with tuning

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
confusionMatrix(pred, cartest[,9])    # Accurcy 81.1 still good amount  of misclassification in 2 wheeler

# Xtreme Gradient Boosting


model <- train( Transport ~ ., data = cartrain, method = "xgbTree", 
                trControl = trainControl("cv", number = 10))
pred<- model %>% predict(cartest[,-9])
confusionMatrix(pred, cartest[,9])    # Accurcy 79.3 still good amount  of misclassification in 2 wheeler


# XGB with hyper parameter tuning
xgb.grid = expand.grid(
  nrounds = 2, 
  max_depth = c(5, 10, 15), 
  eta = c(0.01, 0.001, 0.0001), 
  gamma = c(1, 2, 3), 
  colsample_bytree = c(0.4, 0.7, 1.0), 
  min_child_weight = c(0.5, 1, 1.5)
)

control = trainControl(method = "repeatedcv", repeats = 1, number = 3, 
                       summaryFunction = mnLogLoss,
                       classProbs = TRUE,
                       allowParallel=T)

tune = train(x=cartrain[,-9], y=cartrain[,9], method="xgbTree", trControl=control, 
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
    allowParallel    = TRUE
  )

modFit = train(Transport ~ ., 
               data=cartrain,
               trControl = xgb_trcontrol_1,
               tuneGrid = xgb_grid_1,
               method = "xgbTree",
               #metric    = 'RMSE',
               maximize  = FALSE, 
               nthread = 1    
)

modFit
pred<- modFit %>% predict(cartest[,-9])
confusionMatrix(pred, cartest[,9])    # Accurcy 76.6 still good amount  of misclassification in 2 wheeler

# Smote data for the unbalance in target variable
library(DMwR)

cartrain_balance <- SMOTE(Transport~.,
                       cartrain,
                       perc.over = 500, perc.under = 1000, k =5)
dim(cartrain_balance)
prop.table(table(cartrain_balance$Transport))
prop.table(table(cartrain$Transport))
# ROse only works for binary class
cartrain_rose <- ROSE(Transport ~ ., data = cartrain, seed = 1)$data

# Smote for multiclass imbalance
# Now, for recovering 2 imbalance class, apply SMOTE functions 2 times on data...
rm(Final_Imbalence_recover)
rm(First_Imbalence_recover)
First_Imbalence_recover <- DMwR::SMOTE(Transport ~ ., cartrain, perc.over = 2000,perc.under=100)
prop.table(table(First_Imbalence_recover$Transport))
Final_Imbalence_recover <- DMwR::SMOTE(Transport ~ ., First_Imbalence_recover, perc.over = 500,perc.under=500)
table(Final_Imbalence_recover$Transport)
prop.table(table(Final_Imbalence_recover$Transport))

# Balancing data try 2
set.seed(234)
traindown <- downSample(x = cartrain[, -9],
                     y = cartrain$Transport)
table(traindown$Transport)
summary(traindown)
# Modelling with smoted data

# KNN
trControl <- trainControl(method  = "cv", number  = 10,verboseIter = FALSE,
                          summaryFunction = multiClassSummary,
                          sampling = "SMOTE")
trControl$sampling <- "smote"
fit <- train(Transport ~ .,
             method     = "knn",
             tuneGrid   = expand.grid(k = 1:50),
             trControl  = trControl,
             metric     = "Accuracy",
             data       = cartrain)

fit  # best fit at k =1 accuracy 99.6 on train data
data_pred <- predict(fit, cartest[,-9])
# Confusion Matrix
confusionMatrix(data=data_pred,  reference=cartest$Transport) # Accuracy 73.9

# Naive Bayes  1.1
trControl <- trainControl(method  = "cv", number  = 10,verboseIter = FALSE,
                          summaryFunction = multiClassSummary,
                          sampling = "SMOTE")
trControl$sampling <- "smote"
fit <- train(x = cartrain[,-9],
             y = cartrain[,9],
             method     = "nb",
             trControl  = trControl,
             metric     = "Accuracy",
             data       = cartrain)


data_pred <- predict(fit,cartest[,-9],type="raw")
confusionMatrix(data_pred,cartest[,9], positive = "a", mode="everything")  # Accuracy 80.2

# Naive Bayes with preprocess 1.2
# train model
fit <- train(
  x = cartrain[,-9],
  y =cartrain[,9],
  method = "nb",
  trControl = trControl,
  #tuneGrid = search_grid,
  preProc = c("BoxCox", "center", "scale", "pca")
)
data_pred <- predict(fit,cartest[,-9],type="raw")
confusionMatrix(data_pred,cartest[,9], positive = "a", mode="everything")  # Accuracy 82.9
# top 5 modesl
fit$results %>% 
  top_n(5, wt = Accuracy) %>%
  arrange(desc(Accuracy))

# Naive Bayes with search grid 1.3
search_grid <- expand.grid(
  usekernel = c(TRUE, FALSE),
  fL = 0:5,
  adjust = seq(0, 5, by = 1)
)
fit <- train(
  x = cartrain[,-9],
  y =cartrain[,9],
  method = "nb",
  trControl = trControl,
  tuneGrid = search_grid,
  preProc = c("BoxCox", "center", "scale", "pca")
)
data_pred <- predict(fit,cartest[,-9],type="raw")
confusionMatrix(data_pred,cartest[,9], positive = "a", mode="everything")  # Accuracy 81.1
# top 5 modesl
fit$results %>% 
  top_n(5, wt = Accuracy) %>%
  arrange(desc(Accuracy))  # Accuracy 81.4 on train data



# Logistic Regression 1.1
trControl <- trainControl(method  = "cv", number  = 10,verboseIter = FALSE,
                          summaryFunction = multiClassSummary,
                          sampling = "SMOTE")
trControl$sampling<- 'smote'
fit_glm = train(
  Transport ~ .,
  data = cartrain,
  method = "multinom",
  trControl = trControl,
  preProcess = c("center", "scale"),
  trace = FALSE
)
summary(fit_glm)
# Make predictions
data_pred <- fit_glm %>% predict(cartest[,-9])
data_pred
confusionMatrix(data_pred, cartest[,9]) # Accuracy 83.78%
varImp(fit_glm)  #Remove Distance and engineer
car::vif(fit_glm)
# Logistic regression 1.2

fit_glm = train(
  Transport ~ .,
  data = cartrain,
  method = "multinom",
  trControl = trControl,
  preProcess = c("center", "scale"),
  trace = FALSE
)
summary(fit_glm)
# Make predictions
data_pred <- fit_glm %>% predict(cartest[,-9])
data_pred
confusionMatrix(data_pred, cartest[,9]) # Accuracy 78.38%

trControl <- trainControl(method  = "cv", number  = 10,verboseIter = FALSE,
                          summaryFunction = multiClassSummary,
                          sampling = "SMOTE")
trControl$sampling<- 'smote'

# BAgging --> TreeBag 
trControl <- trainControl(method = "repeatedcv", number = 10)
                         
trControl$sampling<- 'smote'
head(cartrain)
fit_bag1 <- train(
  Transport ~ .,
  data = cartrain,
  method = "treebag",
  trControl = trControl,
  nbagg = 200,  
  control = rpart.control(minsplit = 2, cp = 0)
)

cntrl <- trainControl(method = "cv", number = 10, sampling = 'smote')
cntrl$sampling<- 'smote'
fit<- train(Transport ~ .-Age, data = cartrain, method = "treebag",preProcess = c("center", "scale"), trControl = cntrl)
pred<- fit %>% predict(cartest[,-9])
confusionMatrix(pred, cartest[,9])   # Accuracay 78.38

# Bagging 1.2 (nbagg = 300)
fit<- train(Transport ~ .-Age, data = cartrain, method = "treebag",preProcess = c("center", "scale"), trControl = cntrl,
            nbagg = 300)
pred<- fit %>% predict(cartest[,-9])
confusionMatrix(pred, cartest[,9])   # Accuracay 81.98



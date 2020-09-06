library(ggplot2)
#install.packages('ggpubr')
library(ggpubr)
library(dplyr)
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
data$pch <- 19 + as.integer(data[, 9])
colnames(data)
# PLot for Work exp and Salary wrt to transport
par(mfrow = c(1, 2))
plot(data[, 5], data[, 6], pch = 19 + as.integer(data[, 9]),
     main = "Original Data")
plot(newData[, 1], newData[, 2], pch = 19 + as.integer(newData[, 3]),
     main = "SMOTE'd Data")

b <- ggplot(data, aes(x = Salary, y = Work.Exp))
b + geom_point(aes(color = Transport, shape = Transport))+
  geom_smooth(aes(color = Transport, fill = Transport),
              method = "lm", fullrange = TRUE) +
  #facet_wrap(~Transport) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))+
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) +
  theme_bw()

# Change the ellipse type to 'convex'
ggscatter(data, x = "Salary", y = "Work.Exp",
          color = "Transport", palette = "npg",
          shape = "Transport",
          ellipse = TRUE, ellipse.type = "convex",
          ggtheme = theme_minimal())+
  facet_wrap(~Transport)

# Bubble chart  ( Salary , Work Exp, Age and Transport)
b + geom_point(aes(color = Transport, size = Age), alpha = 0.5) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) +
  scale_size(range = c(0.5, 12))  # Adjust the range of points size

# Mutating age in data
summary(data$Age)
plot(data$Age)
data <- data %>% mutate(agegroup = case_when(Age >= 15  & Age <= 20 ~ '15-20',
                                             Age >= 21  & Age <= 25 ~ '21-25',
                                             Age >= 26  & Age <= 30 ~ '26-30',
                                             Age >= 31  & Age <= 35 ~ '31-35',
                                             Age >= 36  & Age <= 40 ~ '36-40',
                                             Age >= 41 ~ '>40'))

cartrainsmote <- cartrainsmote %>% mutate(agegroup = case_when(Age >= 15  & Age <= 20 ~ '15-20',
                                             Age >= 21  & Age <= 25 ~ '21-25',
                                             Age >= 26  & Age <= 30 ~ '26-30',
                                             Age >= 31  & Age <= 35 ~ '31-35',
                                             Age >= 36  & Age <= 40 ~ '36-40',
                                             Age >= 41 ~ '>40'))

cartest <- cartest %>% mutate(agegroup = case_when(Age >= 15  & Age <= 20 ~ '15-20',
                                                               Age >= 21  & Age <= 25 ~ '21-25',
                                                               Age >= 26  & Age <= 30 ~ '26-30',
                                                               Age >= 31  & Age <= 35 ~ '31-35',
                                                               Age >= 36  & Age <= 40 ~ '36-40',
                                                               Age >= 41 ~ '>40'))

cartrainsmote <- cartrainsmote %>% mutate(agegroup = case_when(Age >= 15  & Age <= 20 ~ '15-20',
                                                   Age >= 21  & Age <= 25 ~ '21-25',
                                                   Age >= 26  & Age <= 30 ~ '26-30',
                                                   Age >= 31  & Age <= 35 ~ '31-35',
                                                   Age >= 36  & Age <= 40 ~ '36-40'))
cartrainsmote$agegrp <- cut(cartrainsmote$Age, pretty(cartrainsmote$Age))
dim(cartrainsmote)
colnames(cartrainsmote)
cartrainsmote <- cartrainsmote[,c(10,11)]
summary(cartrainsmote$agegroup)
#summary(cartest$agegroup)
summary(cartrainsmote$Age)
data$agegroup<- factor(data$agegroup)
cartrainsmote$agegroup<- factor(cartrainsmote$agegroup)
cartest$agegroup<- factor(cartest$agegroup)
summary(data$agegroup)
summary(data$Age)
summary(data)

data$Salary[data$Work.Exp==0]<7
dat <- data %>%
  mutate(
    Engineer = factor(Engineer, levels = c(0,1), labels = c('NonEngineer','Engineer')),
    MBA = factor(MBA, levels = c(0,1), labels = c('NonMBA', 'MBA')),
    license = factor(license, levels = c(0,1), labels = c('NonDL', 'DL'))
  )

#dat <- data %>%
#  mutate(
#    Engineer = as.factor(Engineer),
#    MBA = as.factor(MBA),
#    license = as.factor(license)
#  )


summary(dat)

#levels(dat$Engineer) = c('NonEngineer','Engineer')
#levels(dat$MBA)= c('NonMBA', 'MBA')
#levels(dat$license) = c('NonDL', 'DL')

# NA imputation using Mice
library(mice)
init = mice(dat, maxit=0) 
meth = init$method
predM = init$predictorMatrix

predM
meth  # logreg
imputed = mice(dat, method=meth, predictorMatrix=predM)

imputed <- complete(imputed)

sapply(imputed, function(x) sum(is.na(x)))

imputed<- imputed[,-10]

# Outlier Handling
# Change box plot line colors by groups
p<-ggplot(imputed, aes(x=Transport, y=Age, color=Transport)) +
  geom_boxplot(notch=TRUE,outlier.colour="red", outlier.shape=8,
               outlier.size=4)
p+scale_color_brewer(palette="Dark2")+labs(title="Plot of Salary  as per Transport Usage")

library(caret)
library(dplyr)
library(tidyr)
imputed %>%
  dplyr::select("Age","Work.Exp","Salary","Distance", "Transport") %>% 
  tidyr::gather(Measure, value, -Transport) %>%                             # Convert to key-value pairs
  ggplot(aes(x = Transport, y = value, color=Transport)) +                     # Plot the values
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4)+
  facet_wrap(~ Measure, scales = "free")    # In separate panels
      



nums <- unlist(lapply(imputed, is.numeric))  
select_if(imputed, is.numeric)
names(dplyr::select_if(imputed,is.numeric))
x  = sapply(imputed, is.numeric)
x<- imputed %>%  keep(is.numeric)
head(x)

# Outlier replacement with na  1.1 
outlierreplacement <- function(dataframe){
  dataframe %>%          
    map_if(is.numeric, ~ replace(.x, .x %in% boxplot.stats(.x)$out, NA)) %>%
    bind_cols }
outlierreplacement(Clean_Data)
library(dplyr)
library(mlr)
library(outliers)
library(stringr)
# Find and store references to numeric (and integer) variables
numericCols <- imputed %>% summarizeColumns() %>% dplyr::select(type)
numericCols <- which(numericCols[[1]] %in% c("numeric", "integer"))
numericCols
# Calculate the z scores for each variable and count the number of outliers (values greater than 3)
z_score_outliers <- imputed %>% dplyr::select(numericCols) %>% mutate_all(.funs = scores, type = "z") %>%
  mutate_all(.funs = function(x) abs(x)>3) %>%
  sapply(sum) %>% as.data.frame() 

# Show the number of outliers per variable
names(z_score_outliers) = "outliers"
z_score_outliers %>% subset(outliers > 0)


# Creater a labeller function to shorten names (just remove the brackets):
shorten_names <-function(x){
  index = str_locate(x, pattern = "\\(") -2 
  out <- substr(x, 1, index)
  out[is.na(out)] <- x[is.na(out)]
  return(out)
}

shorten_names(imputed)

#Identify which variables contain outliers (according to z-score tests)
outlier.vars <- imputed %>% dplyr::select(numericCols) %>% mutate_all(.funs = scores, type = "z") %>%
  summarizeColumns() %>% subset(min < -3 | max >3) %>% dplyr::select(name) %>% unlist()
outlier.vars
library(stringr)
#Plot histograms of the data with outliers
imputed %>% dplyr::select(one_of(outlier.vars)) %>% tidyr::gather(key = variable, value = value) %>% ggplot(aes(x = value)) + 
  geom_histogram() + facet_wrap(.~variable, scales = "free", labeller = as_labeller(shorten_names))

#Perform the windsoring (function taken from Stack Overflow)
cap <- function(x){
  quantiles <- quantile(x, c(.05,0.25,0.75,.95))
  x[x < quantiles[2] - 1.5*IQR(x)] <- quantiles[1]
  x[x > quantiles[3] + 1.5*IQR(x)] <- quantiles[4]
  return(x)
}
paste(outlier.vars)

#Perform the capping  
imputed <- imputed %>% mutate_at(.vars =vars("Age", "Work.Exp"  , "Salary", "Distance" ), .funs  = cap)
imputed <- imputed %>% mutate_at(.vars =vars(paste(outlier.vars) ), .funs  = cap)

###################################################################
# For categorical variable
boxplot(Age ~ Salary, data=imputed, main="Transport across age")  # clear pattern is noticeable.
boxplot(Age ~ cut(Salary, pretty(imputed$Salary)), data=imputed, 
        main="Boxplot for Age (categorial) vs Salary", cex.axis=0.5) # this may not be significant, as day of week variable is a subset of the month var.


x <- imputed$Age
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
qnt
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
caps
H <- 1.5 * IQR(x, na.rm = T)
H
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
boxplot(x)$out
outval<- boxplot(dat$Age)$out
table(outval)

#find outlier index position in vector
which(dat$Age %in% outval)
dat$Age %in% outval

# Remove outlier from your dataset use the below code.
sales[ !(sales %in%OutVals) ]
dat1<- dat
dat2<- dat1[!(which(dat1$Age %in%outval)),]

dim(dat2)

###################################################
install.packages("rstatix")
library(rstatix)
ls()
list.files()
dat %>% identify_outliers(Age)
imputed %>% identify_outliers(Age)

boxplot(imputed$Age)

############################################################
# Method for categorising the slots for continuos variable
x<- cut(imputed$Salary, pretty(imputed$Salary))
y<- cut(imputed$Age, pretty(imputed$Age))
y
summary(imputed$agegroup)
imputed$agegroup[imputed$agegroup=='>40']= '36-40'

#############################################################
lm1 <- lm(as.numeric(Transport)~., data =imputed)

summary(lm1)
vif(lm1)

#raw data
#  Farrar - Glauber Test
colnames(imputed)
library(mctest)

#Farrar - Glauber test(F-test) for location of multicollinearity
# Individual Multicollinearity Diagnostic Measures
imcdiag_out = imcdiag(as.numeric(imputed[,-9]),as.numeric(imputed[,9]))
summary(imcdiag_out)
imcdiag_idiagval = as.data.frame(imcdiag_out$idiags)

imcdiag_idiagval$Klein
dataset<-sapply(paste0("var",1:32), function(x) assign(x,rnorm(n=48)) )
dim(dataset)
imcdiag_idiagval.Klein<-c(0 ,0 ,0, 0 ,0 ,0, 0 ,0, 0 ,0 ,0 ,0 ,0, 1, 1, 1, 1, 1, 1 ,1 ,1, 1, 1 ,1 ,0 ,0, 0, 0, 0, 0, 0, 0) 

#final data without multi-collinearity

final_dataset<-dataset[,imcdiag_idiagval.Klein==1]


#####################################################################
# One hot encoding

install.packages('mltools')
library(mltools)
library(data.table)
imputed<- data.table(imputed)
imputedoh<- one_hot(imputed[,-9])
dim(imputedoh)
imputedoh$Transport<- imputed$Transport

######################################################################
library(caret)
#
df1 = data("stack.csv")
df2 = cor(imputedoh[,-19])
df2
hc = findCorrelation(df2, cutoff=0.3) # putt any value as a "cutoff" 
hc = sort(hc)
hc
reduced_Data = imputedoh[,-c(hc)]
print (reduced_Data)
reduced_data
write.csv(reduced_Data, file = "outfile.csv", row.names = FALSE)

imputednumeric<- imputed %>% mutate_if(is.factor,as.numeric)
str(imputednumeric)

# asnother way --> data.frame(apply(d, 2, as.numeric))
tmp <- cor(imputednumeric)
tmp[upper.tri(tmp)] <- 0
tmp
diag(tmp) <- 0
tmp
x<- apply(tmp,2,function(x) any(abs(x) > 0.90))
class(x)
summary(x)
str(x)
x<- data.matrix(x)
x
rownames(x)
#a <- paste( rownames(which(x)), collapse="," )))
rr <- rownames(x)[row(x)[which(x)]]
rr  # colinear data

rr <- rownames(x)[row(x)[x==F]]
rr #non colinear data
#paste(names(a))
#colnames(x)
#data.new <- imputednumeric[,!apply(tmp,2,function(x) any(abs(x) > 0.95))]
#class(data.new)
##x <- ifelse(data.new==TRUE, paste(colnames(data.new)))
#y<- rr%>% select() %>% paste(rownames(a),sep = ',')           
#names(data.frame(data.new))
#data.new[1][1]
#x<- apply(  data.new, 1,   function(u) (paste( names(which(u)), collapse="," ) ))

##################################################################################
library(caTools)
# Data prepration as Train and Test
split <- sample.split(imputed$Transport, SplitRatio = .75)

cartrain <-  subset( imputed, split==T)
cartest<- subset(imputed, split==F)


##################################################################################
# Unbalance multiclass data handling
table(cartrain$Transport)
#cartraimsmote<- SmoteClassif(Transport~., cartrain, "balance",dist = "HVDM")
#cartraimsmote<- SmoteClassif(Transport~., cartrain, "extreme",dist = "HVDM")
C.perc = list('2Wheeler' = 3.62, Car = 4.89) 

#225/62  3.62
#225/46  4.89
library(UBL)
rm(cartrainsmote)
cartrainsmote <- SmoteClassif(Transport~., cartrain, C.perc, dist = "HEOM")
table(cartrain$Transport)
table(cartrainsmote$Transport)
dim(cartrainsmote)
str(cartrainsmote)
table(cartrainsmote$agegroup)
table(cartrainsmote$Age)
summary(cartest)
summary(cartrain)
#################################################################################
# Normalising the data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

cartrainsmotenorm <- cartrainsmote %>% keep(is.numeric) %>% gather() %>% mutate(normalize(value))

numericCols <- cartrainsmote %>% summarizeColumns() %>% dplyr::select(type)
numericCols
numericCols <- which(numericCols[[1]] %in% c("numeric", "integer"))
numericCols

factorCols <- cartrainsmote %>% summarizeColumns() %>% dplyr::select(type)
factorCols
factorCols <- which(factorCols[[1]] %in% c("factor"))
factorCols

# Calculate the z scores for each variable and count the number of outliers (values greater than 3)
cartrainsmotenorm <- imputed %>% dplyr::select(numericCols) %>%
  mutate_all(.funs = function(x) return ((x - min(x)) / (max(x) - min(x))))  %>% as.data.frame() 

# Scale Numeric Column
cartrainsmotenorm<- cartrainsmote %>%  mutate_if(is.numeric, scale)
cartrainsmotenorm<- cartrainsmote %>%  mutate_if(is.numeric, normalize)
cartest<- cartest %>%  mutate_if(is.numeric, normalize)
summary(cartrainsmotenorm)

cartrainsmotenorm[which(is.na(cartrainsmotenorm$agegroup))]
cartrainsmote[which(is.na(cartrainsmote$agegroup))]

nrow(cartrainsmote)
dim(cartrainsmotenorm)
cartrainsmotenorm<- cartrainsmotenorm[,-10]
#cartestknn_n<- as.data.frame(lapply(cartestknn[,-13], normalize))
#cartrainknn_n$Transport<- cartrainknn$Transport
#cartestknn_n$Transport <- cartestknn$Transport
class(cartrainsmotenorm)
cartrainsmotenorm<- data.frame(cartrainsmotenorm)
cartrainsmote<- data.frame(cartrainsmote)
summary(cartrainsmote)
summary(cartest)
table(cartest$Transport)
str(cartest)
class(cartest)
class(cartrain)
cartrain<- data.frame(cartrain)
cartest<- data.frame(cartest)
cartrain<- cartrain[,-1]
cartest<- cartest[,-1]
summary(cartrain)
dim(cartrain)
str(cartest)
str(cartrain)
class(imputed)
imputed<- data.frame(imputed)
cartest<- cartest %>%  mutate_if(is.numeric, normalize)
cartrain<- cartest %>% mutate_if(is.numeric, normalize)
##################################################################################

# So, multi collinearity does not affect the Naive Bayes
# MOdel 1 Naive Bayes
train_control <- trainControl(
  method = "cv", 
  number = 3   )
search_grid <- expand.grid(
  usekernel = c(TRUE, FALSE),
  fL = 0:5,
  adjust = seq(0, 5, by = 1)  )

library(caret)
library(klar)
nb.m1 <- caret::train(
  Transport ~ ., data = cartrain,
  method = "nb",
  trControl = train_control,
  tuneGrid = search_grid,
  preProc = c("BoxCox", "center", "scale", "pca")
)
warnings()
# top 5 modesl
nb.m1$results %>% 
  top_n(5, wt = Accuracy) %>%
  arrange(desc(Accuracy))

# plot search grid results
plot(nb.m1)

confusionMatrix(nb.m1)  # train accuracy 82.88
# Test prediction
data_pred <- predict(nb.m1,cartest[,-9],type="raw")
confusionMatrix(data_pred,cartest[,9], positive = "a", mode="everything")  #  test accuracy 77.48
###################################################################################################
# h20
install.packages('h2o')
library(h2o)
h2o.no_progress()
h2o.init()
#Error Output:
#  Only Java 8, 9, 10, 11, 12 and 13 are supported, system version is 14 
#Error in h2o.init() : H2O failed to start, stopping execution.
####################################################################################################
library(e1071)
#KNN
#knn.cross <- tune.knn(x = imputed[,-9], y = imputed[,9], k = 1:20,tunecontrol=tune.control(sampling = "cross"), cross=10)

cartrainX <- cartrain[,names(cartrain) != "Transport"]
preProcValues <- preProcess(x = cartrainX,method = c("center", "scale"))
preProcValues
head(preProcValues)
head(cartest)
head(cartrain)
set.seed(400)
ctrl <- trainControl(method="repeatedcv",repeats = 3) #,classProbs=TRUE,summaryFunction = twoClassSummary)
knnFit <- caret::train(Transport ~ ., data = cartrain, method = "knn", trControl = ctrl, preProcess = c("center","scale"), tuneLength = 20)
data_pred <- predict(knnFit,cartest[,-9],type="raw")
confusionMatrix(data_pred,cartest[,9], positive = "a", mode="everything")  # 78.38

ctrl <- trainControl(method="repeatedcv",repeats = 3,classProbs=FALSE,summaryFunction = multiClassSummary)
knnFit <- caret::train(Transport ~ ., data = cartrain, method = "knn", trControl = ctrl, preProcess = c("center","scale"), tuneLength = 20)
data_pred <- predict(knnFit,cartest[,-9],type="raw")
confusionMatrix(data_pred,cartest[,9], positive = "a", mode="everything")  # 81.98

# with normalized data
ctrl <- trainControl(method="repeatedcv",repeats = 3,classProbs=FALSE,summaryFunction = multiClassSummary)
knnFit <- caret::train(Transport ~ ., data = cartrain, method = "knn", trControl = ctrl, preProcess = c("center","scale"), tuneLength = 20)
data_pred <- predict(knnFit,cartest[,-9],type="raw")
confusionMatrix(data_pred,cartest[,9], positive = "a", mode="everything")  # 84.68

# knn with LOOCV -  leave one out cross validation.
ctrl <- trainControl(method="LOOCV")
knnFit <- caret::train(Transport ~ ., data = cartrain, method = "knn", trControl = ctrl, preProcess = c("center","scale"), tuneLength = 20)
data_pred <- predict(knnFit,cartest[,-9],type="raw")
confusionMatrix(data_pred,cartest[,9], positive = "a", mode="everything")  # 84.68 still same

# knn with LOOCV -  leave one out cross validation and tune grid for k
ctrl <- trainControl(method="LOOCV")
knnFit <- caret::train(Transport ~ ., data = cartrain, method = "knn", trControl = ctrl,
                       preProcess = c("center","scale"), tuneGrid   = expand.grid(k = 1:50))
data_pred <- predict(knnFit,cartest[,-9],type="raw")
confusionMatrix(data_pred,cartest[,9], positive = "a", mode="everything")  # 86.49 

knnFit$results %>%   top_n(5, wt = Accuracy) %>%  arrange(desc(Accuracy))  # K  = 3

# knn with LOOCV -  leave one out cross validation and tune grid for k, plus smote
ctrl <- trainControl(method="LOOCV", sampling = "smote")
knnFit <- caret::train(Transport ~ ., data = cartrain, method = "knn", trControl = ctrl,
                       preProcess = c("center","scale"), tuneGrid   = expand.grid(k = 1:50))
data_pred <- predict(knnFit,cartest[,-9],type="raw")
confusionMatrix(data_pred,cartest[,9], positive = "a", mode="everything")  # 81.08 accuracy decreasd

knnFit$results %>%   top_n(5, wt = Accuracy) %>%  arrange(desc(Accuracy))  # K  = 13
#plot 
cfmROC(data_pred,cartest[,9])


# Logistic Regression
# Logistic Regression 1.1 without smote
trControl <- trainControl(method  = "cv", number  = 10,verboseIter = FALSE,
                          summaryFunction = multiClassSummary)
#trControl$sampling<- 'smote'
fit_glm = caret::train(
  Transport ~ .,
  data = cartrain,
  method = "multinom",
  trControl = trControl,
  preProcess = c("center", "scale"),
  trace = FALSE
)
data_pred <- predict(fit_glm,cartest[,-9],type="raw")
confusionMatrix(data_pred,cartest[,9], positive = "a", mode="everything")  # 84.68 accuracy increased
#plot 
cfmROC(data_pred,cartest[,9])

# Logistic Regression 1.1 without smote
trControl <- trainControl(method  = "cv", number  = 10,verboseIter = FALSE,
                          summaryFunction = multiClassSummary)
trControl$sampling<- 'smote'
fit_glm = caret::train(
  Transport ~ .,
  data = cartrain,
  method = "multinom",
  trControl = trControl,
  preProcess = c("center", "scale"),
  trace = FALSE
)
fit_glm
data_pred <- predict(fit_glm,cartest[,-9],type="raw")
confusionMatrix(data_pred,cartest[,9], positive = "a", mode="everything")  # 85.59 accuracy decreasd
#plot 
cfmROC(data_pred,cartest[,9])

# Logistic Regression 1.2 without smote removing age
trControl <- trainControl(method  = "cv", number  = 10,verboseIter = FALSE,
                          summaryFunction = multiClassSummary)
trControl$sampling<- 'smote'
fit_glm = caret::train(
  Transport ~ .-Age,
  data = cartrain,
  method = "multinom",
  trControl = trControl,
  preProcess = c("center", "scale"),
  trace = FALSE
)
data_pred <- predict(fit_glm,cartest[,-9],type="raw")
confusionMatrix(data_pred,cartest[,9], positive = "a", mode="everything")  # 81.98 accuracy decreasd
#plot 
cfmROC(data_pred,cartest[,9])

# Logistic Regression 1.2 without smote removing Work.exp
trControl <- trainControl(method  = "cv", number  = 10,verboseIter = FALSE,
                          summaryFunction = multiClassSummary)
trControl$sampling<- 'smote'
fit_glm = caret::train(
  Transport ~ .-Work.Exp-Age,
  data = cartrain,
  method = "multinom",
  trControl = trControl,
  preProcess = c("center", "scale"),
  trace = FALSE
)
data_pred <- predict(fit_glm,cartest[,-9],type="raw")
confusionMatrix(data_pred,cartest[,9], positive = "a", mode="everything")  # 82.88 accuracy decreasd
#plot 
cfmROC(data_pred,cartest[,9])

# Packages
library(dplyr)       # for data wrangling
library(ggplot2)     # for awesome plotting
library(doParallel)  # for parallel backend to foreach
library(foreach)     # for parallel processing with for loops

# Modeling packages
library(caret)       # for general model fitting
library(rpart)       # for fitting decision trees
library(ipred)       # for fitting bagged decision trees

#Bagging 1.1
cntrl <- trainControl(method = "cv", number = 10)
control = rpart.control(minsplit = 2, cp = 0)
fit_bag<- caret::train(Transport ~ ., data = cartrain, method = "treebag",nbagg= 200, trControl = cntrl)
pred<- fit_bag %>% predict(cartest[,-9])
confusionMatrix(pred, cartest[,9]) # Accuracy 100% ovefitting
confusionMatrix(fit_bag)  #81.08 on trin
#plot 
cfmROC(pred,cartest[,9])

# Bagging Random Forest 1.2
# Tune using caret
# Random Search
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="random")
set.seed(7)
mtry <- sqrt(ncol(cartrain))
rf_random <- caret::train(Transport~., data=cartrain, method="rf", metric="Accuracy", tuneLength=15, trControl=control)
print(rf_random)
plot(rf_random)
pred<- rf_random %>% predict(cartest[,-9])
confusionMatrix(pred, cartest[,9]) # Accuracy 100% ovefitting
confusionMatrix(rf_random)  #81.98 on train
#plot 
cfmROC(pred,cartest[,9])

# Bagging Random Forest 1.3
# Tune using caret
# Grid Search
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
set.seed(7)
tunegrid <- expand.grid(.mtry=c(1:15))
rf_gridsearch <- caret::train(Transport~., data=cartrain, method="rf", metric="Accuracy",
                              tuneGrid=tunegrid, trControl=control)
print(rf_gridsearch)
plot(rf_gridsearch)
pred<- rf_random %>% predict(cartest[,-9])
confusionMatrix(pred, cartest[,9]) # Accuracy 100% ovefitting
confusionMatrix(rf_gridsearch)  #81.68 on train
#plot 
cfmROC(pred,cartest[,9])
local_obs <- cartest[1:2, ]
local_obs
#install.packages('lime')
library(lime)
# apply LIME   For feature explainability
explainer <- lime(cartrain, rf_gridsearch)
explanation <- explain(local_obs, explainer, n_labels = 3, n_features = 5)
plot_features(explanation)


# Packages
library(caret)
library(gbm)
install.packages('hydroGOF')
library(hydroGOF)
library(Metrics)
library(vip)

# Boosting  1.1
Control <- trainControl(  method = "repeatedcv",  number = 10, repeats = 3)
set.seed(825)
gbmFit1 <- caret::train(Transport ~ ., data = cartrain, 
                 method = "gbm", 
                 trControl = Control,
                 verbose = FALSE)
pred<- gbmFit1 %>% predict(cartest[,-9])
confusionMatrix(pred, cartest[,9]) # Accuracy 91.89% 
confusionMatrix(gbmFit1)  #81.08 on train
summary(gbmFit1)
gbmFit1$bestTune
#n.trees interaction.depth shrinkage n.minobsinnode
#4       50                2       0.1             10
#plot 
cfmROC(pred,cartest[,9])


# Boosting  1.2 Tuning
Control <- trainControl(  method ="repeatedcv",  repeats = 5, summaryFunction = multiClassSummary)
set.seed(825)
metric <- "Accuracy"
# modify hyperparameter grid
gbmGrid <-  expand.grid(interaction.depth = c(1, 3, 6, 9, 10), 
                        n.trees = (1:50)*50, 
                        shrinkage = seq(.0005, .05,.0005),
                        n.minobsinnode = 10)
# total number of combinations  
nrow(gbmGrid)  # 90 parameters
gbmFit2 <- caret::train(Transport ~ .
                   , data=cartrain
                   #, distribution="bernaulli"
                   , method="gbm"
                   , trControl=Control
                   , verbose=FALSE
                   , tuneGrid=gbmGrid
                   , metric=metric
                   , bag.fraction=0.75
)   

pred<- gbmFit2 %>% predict(cartest[,-9])
confusionMatrix(pred, cartest[,9]) # Accuracy 90.99% 
confusionMatrix(gbmFit2)  #80.18 on train
#plot 
cfmROC(pred,cartest[,9])

library(gbm)
par(mar = c(5, 8, 1, 1))
summary(  gbmFit1, cBars = 10,  method = relative.influence,   las = 2)
vip::vip(gbmFit1)

# LOcal observation
# get a few observations to perform local interpretation on
local_obs <- cartest[1:2, ]
local_obs
install.packages('lime')
library(lime)
# apply LIME   For feature explainability
explainer <- lime(cartrain, gbmFit1)
explanation <- explain(local_obs, explainer, n_labels = 3, n_features = 5)
plot_features(explanation)

# Xtreme Boosting  xgbtree 1.1
xgbtreefit <- caret::train( Transport ~ ., data = cartrain, method = "xgbTree", 
              trControl = trainControl("cv", number = 10))
pred<- xgbtreefit %>% predict(cartest[,-9])
confusionMatrix(pred, cartest[,9])   # Accuraccy 89.19
#plot 
cfmROC(pred,cartest[,9])
# apply LIME   For feature explainability
explainer <- lime(cartrain,xgbtreefit )
explanation <- explain(local_obs, explainer, n_labels = 3, n_features = 5)
plot_features(explanation)

# Xtreme booting xgbtree 1.2 tuning

# XGB with hyper parameter tuning
nrounds <- 1000
tune_grid <- expand.grid(
nrounds = seq(from = 200, to = nrounds, by = 50),
eta = c(0.025, 0.05, 0.1, 0.3),
max_depth = c(2, 3, 4, 5, 6),
gamma = 0,
colsample_bytree = 1,
min_child_weight = 1,
subsample = 1
)

tune_control <- caret::trainControl(
method = "cv", # cross-validation
number = 3, # with n folds 
#index = createFolds(tr_treated$Id_clean), # fix the folds
verboseIter = FALSE, # no training log
allowParallel = TRUE # FALSE for reproducible results 
)

xgbfit2 <- caret::train(
Transport ~ ., data = cartrain,
trControl = tune_control,
tuneGrid = tune_grid,
method = "xgbTree",
verbose = TRUE
)

pred<- xgbfit2 %>% predict(cartest[,-9])
confusionMatrix(pred, cartest[,9])    # Accuracy 94.59
#plot 
cfmROC(pred,cartest[,9])

trControl$sampling <- "smote"
trControl$sampling <- "smote"
i=1
k.optm=1
for (i in 1:28){
   knn.mod <- knn(train=cartrain[,-9], test=cartest[,-9], cl=cartrain[,9], k=i)
   k.optm[i] <- 100 * sum(cartest[,9] == knn.mod)/NROW(cartest[,9])
   k=i
   cat(k,'=',k.optm[i],'')
   }

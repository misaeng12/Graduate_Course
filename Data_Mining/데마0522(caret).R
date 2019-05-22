library(caret)
orange <- read.csv('https://raw.githubusercontent.com/selva86/datasets/master/orange_juice_withmissing.csv')
summary(orange)

set.seed(100)
trainRowNumbers <- createDataPartition(orange$Purchase, p=0.8, list=FALSE)
trainData <- orange[trainRowNumbers,]
testData <- orange[-trainRowNumbers,]
x = trainData[, 2:18]
y = trainData$Purchase
dim(trainData)
dim(testData)

# The advantage of using createDataPartition() over the traditional random sample() is,
# it preserves the proportion of the categories in Y variable, that can be disturbed if you sample randomly.
table(trainData$Purchase)/nrow(trainData)
table(testData$Purchase)/nrow(testData)

library(skimr)
skimmed <- skim_to_wide(trainData)
skimmed[, c(1:5, 9:11, 13, 15:16)]
#summary(trainData)

# Create the knn imputation model on the training data
preProcess_missingdata_model <- preProcess(trainData, method='knnImpute')
preProcess_missingdata_model

# Use the imputation model to predict the values of missing data points
library(RANN)  # required for knnInpute
trainData <- predict(preProcess_missingdata_model, newdata = trainData)
anyNA(trainData)
summary(trainData)  # NA가 없음, 평균은 다 0 (scale됨)

# One-Hot Encoding
# Creating dummy variables is converting a categorical variable to as many binary variables as here are categories.
dummies_model <- dummyVars(Purchase ~ ., data=trainData)
# Create the dummy variables using predict. The Y variable (Purchase) will not be present in trainData_mat.
trainData_mat <- predict(dummies_model, newdata = trainData)
# # Convert to dataframe
trainData <- data.frame(trainData_mat)
# # See the structure of the new dataset
str(trainData)  # 전부 다 numeric이 됨!

preProcess_range_model <- preProcess(trainData, method='range')
trainData <- predict(preProcess_range_model, newdata = trainData)
# Append the Y variable
trainData$Purchase <- y
apply(trainData[, 1:10], 2, FUN=function(x){c('min'=min(x), 'max'=max(x))})

# y의 class별 boxplot!!!
featurePlot(x = trainData[, 1:18], y = trainData$Purchase, plot = "box",
            strip = strip.custom(par.strip.text=list(cex=.7)),
            scales = list(x = list(relation="free"), y = list(relation="free")))


## feature selection using recursive feature elimination (rfe)
set.seed(100)
options(warn=-1)

subsets <- c(1:5, 10, 15, 18)  # subset size 지정
ctrl <- rfeControl(functions = rfFuncs, method = "repeatedcv", repeats=5, verbose=F)

lmProfile <- rfe(x=trainData[, 1:18], y=trainData$Purchase, sizes = subsets, rfeControl = ctrl)
lmProfile  # resampling method: Cross-Validated (10 fold, repeated 5 times)


## MARS algorithm
library(earth)
set.seed(100)
# Train the model using randomForest and predict on the training data itself.
model_mars = train(Purchase ~ ., data=trainData, method='earth')
fitted <- predict(model_mars)
fitted
model_mars
plot(model_mars, main="Model Accuracies with MARS")

# variable importance
varimp_mars <- varImp(model_mars)
plot(varimp_mars, main="Variable Importance with MARS")


## Prepare the test dataset and Predict
# Missing Value imputation –> One-Hot Encoding –> Range Normalization
# preProcess_missingdata_model –> dummies_model –> preProcess_range_model

# Step 1: Impute missing values 
testData2 <- predict(preProcess_missingdata_model, testData)  
# Step 2: Create one-hot encodings (dummy variables)
testData3 <- predict(dummies_model, testData2)
# Step 3: Transform the features to range between 0 and 1
testData4 <- predict(preProcess_range_model, testData3)
# View
head(testData4[, 1:10])

predicted <- predict(model_mars, testData4)
head(predicted)

# Compute the confusion matrix
confusionMatrix(reference = testData$Purchase, data = predicted,
                mode='everything', positive='MM')
# Accuracy: 정분류율(가장 많이 쓰이는 classification measure)
# unbalanced data -> accuracy는 바람직하지 않을 수 있음
# 밑에 Sensitivity, Specificity 등등



### hyperparameter tuning

# Define the training control
fitControl <- trainControl(
  method = 'cv',                   # k-fold cross validation
  number = 5,                      # number of folds
  savePredictions = 'final',       # saves predictions for optimal tuning parameter
  classProbs = T,                  # should class probabilities be returned
  summaryFunction=twoClassSummary  # results summary function
) 


## Hyper Parameter Tuning using tuneLength

# Step 1: Tune hyper parameters by setting tuneLength
set.seed(100)
model_mars2 = train(Purchase ~ ., data=trainData, method='earth', tuneLength = 5,
                    metric='ROC', trControl = fitControl)
model_mars2

# Step 2: Predict on testData and Compute the confusion matrix
predicted2 <- predict(model_mars2, testData4)
confusionMatrix(reference = testData$Purchase, data = predicted2,
                mode='everything', positive='MM')


## Hyper Parameter Tuning using tuneGrid 

# Step 1: Define the tuneGrid
marsGrid <-  expand.grid(nprune = c(2, 4, 6, 8, 10), degree = c(1, 2, 3))

# Step 2: Tune hyper parameters by setting tuneGrid
set.seed(100)
model_mars3 = train(Purchase ~ ., data=trainData, method='earth', metric='ROC',
                    tuneGrid = marsGrid, trControl = fitControl)
model_mars3

# Step 3: Predict on testData and Compute the confusion matrix
predicted3 <- predict(model_mars3, testData4)
confusionMatrix(reference = testData$Purchase, data = predicted3,
                mode='everything', positive='MM')



### How to evaluate performance of multiple machine learning algorithms?

## Training Adaboost
library(fastAdaboost)
model_adaboost = train(Purchase ~ ., data=trainData, method = 'adaboost',
                       tuneLength=2, trControl = fitControl)
# tuneLegnth=2; 2개 parameter에 대해서 ROC 값으로 최적의 parameter 선택
model_adaboost

## Training Random Forest
set.seed(100)
model_rf = train(Purchase ~ ., data=trainData, method = 'rf',
                 tuneLength=5, trControl = fitControl)
model_rf

## Training xgBoost Dart
set.seed(100)
# Train the model using MARS
model_xgbDART = train(Purchase ~ ., data=trainData, method = 'xgbDART',
                      tuneLength=5, trControl = fitControl, verbose=F)
model_xgbDART

## Training SVM
set.seed(100)
# Train the model using MARS
model_svmRadial = train(Purchase ~ ., data=trainData, method = 'svmRadial',
                        tuneLength=15, trControl = fitControl)
model_svmRadial


##  Run resamples() to compare the models
# Compare model performances using resample()
models_compare <- resamples(list(ADABOOST=model_adaboost, RF=model_rf, XGBDART=model_xgbDART,
                                 MARS=model_mars3, SVM=model_svmRadial))
# Summary of the models performances
summary(models_compare)
# Draw box plots to compare models
scales <- list(x=list(relation="free"), y=list(relation="free"))
bwplot(models_compare, scales=scales)
# XGB Dart가 가장 좋음, ROC -> 전부 0.85에서 0.9 사이에 있음 (나쁘지 않음)



### Ensembling the predictions (한방에 하기!)
library(caretEnsemble)

# Stacking Algorithms - Run multiple algos in one call.
trainControl <- trainControl(method = "repeatedcv", number=10, repeats=3,
                             savePredictions=TRUE, classProbs=TRUE)
algorithmList <- c('rf', 'adaboost', 'earth', 'xgbDART', 'svmRadial')

set.seed(100)
models <- caretList(Purchase ~ ., data=trainData, trControl=trainControl, methodList=algorithmList) 
results <- resamples(models)
summary(results)

# Box plots to compare models
scales <- list(x=list(relation="free"), y=list(relation="free"))
bwplot(results, scales=scales)
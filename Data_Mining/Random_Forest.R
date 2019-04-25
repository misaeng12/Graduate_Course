
## randomForest()
library(randomForest)

# mtry = p/3 or sqrt(p)
# nodesize: minimum size of terminal nodes

set.seed(1)
iris.rf <- randomForest(Species~., data=iris, importance=TRUE, proximity=TRUE)
print(iris.rf)  # Confusion matrix 포함!
                # OOB estimate of  error rate: 5.33%

#아래 결과와 같음
rf1.pred <- predict(iris.rf)
table(iris$Species, rf1.pred)
?predict.randomForest  # newdata -> OOB prediction is default

plot(iris.rf)
# trees 수가 늘어나도 training error가 계속 떨어지진 않고 stabilized됨

round(importance(iris.rf), 2)
# Mean Decrease Accuracy가 가장 큰 변수가 가장 중요한 변수
# (해당 변수를 뺐을 때 Prediction error가 가장 줄어든다)

varImpPlot(iris.rf)
partialPlot(iris.rf, pred.data=)
# Classification에서 LDA, QDA 제외하고는 전부 score 계산



### Tuning

# ntree   # ex) 500개 -> 계속 줄어든다면 1000개
# mtry => mtry가 작을수록 correlation이 작아져서 분산이 작아지지만 bias가 커짐!

# mtry의 tuning이 중요! (ntree는 안 중요)
# randomForest 패키지 안의 tuneRF() -> 느리다
# ranger() -> 약 10배 빠르다!!

install.packages("ranger")
library(ranger)

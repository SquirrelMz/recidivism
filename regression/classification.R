library(randomForest)

get_forest = function(Y, y)
{
  data3 = data1
  data3$Y = as.factor(y)
  Train <- createDataPartition(data3$Y, p=0.75, list=FALSE)
  training <- data3[Train, ]
  testing <- data3[-Train, ]
  
  forest = randomForest(Y ~ ., data = training)
  pred = predict(forest, newdata = testing, type='response')
  ROC = roc(testing$Y, as.numeric(pred))
  return(auc(ROC))
}
forest = randomForest(Recidivism_Arrest_Year2 ~ ., data = training)
varImpPlot(forest, main = "variable importance")
pred = predict(forest, newdata = testing, type='response')
roc = roc(testing$Recidivism_Arrest_Year2, as.numeric(pred))
# reportROC(testing$Recidivism_Arrest_Year2, as.numeric(pred))
# -------------------------------
library(e1071)
svmmodel = svm(Recidivism_Arrest_Year2 ~ ., data = training, kernel="radial", 
               cost=1, gamma=1/ncol(training))
pred = predict(svmmodel, newdata = testing, type='response')


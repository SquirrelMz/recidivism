library(caret)
library(pROC)
library(ROSE)
library(reportROC)
library(broom)
options (warn = -1)

data = read.csv("NIJ_s_Recidivism_Challenge_Training_Dataset.csv", 
                fileEncoding = "UTF-8-BOM")
data = na.omit(data)
colnames(data)[colnames(data) %in% c('X_v1', 'X_v2', 'X_v3', 'X_v4')] = 
  c('Prior_Arrest_Episodes_PPViolationCharges', 
    'Prior_Conviction_Episodes_PPViolationCharges', 
    'Prior_Conviction_Episodes_DVCharges', 
    'Prior_Conviction_Episodes_GunCharges')
data = data[-which(data$Supervision_Level_First==''), ]
data$Supervision_Level_First = as.factor(data$Supervision_Level_First)
data$Residence_PUMA = as.factor(data$Residence_PUMA)

data1 = data[, 2:33]  # ³öÓüÇ°

# ========================

data3 = data1
data3$Recidivism_Arrest_Year2 = as.factor(data$Recidivism_Arrest_Year2)
Train <- createDataPartition(data3$Recidivism_Arrest_Year2, p=0.75, list=FALSE)
training <- data3[Train, ]
testing <- data3[-Train, ]

# =========================
format_result = function(fit)
{
  t = tidy(fit)
  result = data.frame(term = t$term,
                      coef = t$estimate,
                      std = t$std.error,
                      p = t$p.value)
  return(result)
}


get_auc = function(Y, y)
{
  data3 = data1
  data3$Y = as.factor(y)
  Train <- createDataPartition(data3$Y, p=0.75, list=FALSE)
  training <- data3[Train, ]
  testing <- data3[-Train, ]
  logit.model = glm(Y ~ ., 
                    family=binomial, data=training)
  # logit.model = glm(Recidivism_Arrest_Year2 ~ ., family=binomial, data=training)
  pred = predict.glm(logit.model, newdata = testing, type='response')
  ROC = roc(testing$Y, pred)
  return(auc(ROC))
}


get_auc2 = function(Y, y)  # deal with data imbalance
{
  # Y="Recidivism_Arrest_Year2"
  data3 = data1
  data3$Y = as.factor(y)
  Train <- createDataPartition(data3$Y, p=0.75, list=FALSE)
  training <- data3[Train, ]
  testing <- data3[-Train, ]
  
  over = ovun.sample(Y ~ ., data = training, 
                     method = "over", seed = 1)$data
  under = ovun.sample(Y ~ ., data = training, 
                      method = "under", p = 0.5, seed = 1)$data
  
  logit.model = glm(Y ~ ., family=binomial, data=over)
  pred = predict.glm(logit.model, newdata = testing, type='response')
  ROC = roc(testing$Y, pred)
  print('over sampling')
  print(auc(ROC))
  # auc(roc(testing$Recidivism_Arrest_Year2, pred))
  
  logit.model = glm(Y ~ ., family=binomial, data=under)
  pred = predict.glm(logit.model, newdata = testing, type='response')
  ROC = roc(testing$Y, pred)
  print('under sampling')
  print(auc(ROC))
}









library(broom)
options (warn = -1)

# data3 = data[, 1:49]
data3$Employment_Exempt=data$Employment_Exempt
data3$Jobs_Per_Year = data$Jobs_Per_Year
data3$Percent_Days_Employed = data$Percent_Days_Employed
# ====================================================
data3 = data1
data3$Recidivism_Within_3years = as.factor(data$Recidivism_Within_3years)
logit.model = glm(Recidivism_Within_3years ~ ., 
                  family=binomial, data=data3)
format_result(logit.model)

data3 = data1
data3$Recidivism_Arrest_Year1 = as.factor(data$Recidivism_Arrest_Year1)
logit1.model = glm(Recidivism_Arrest_Year1 ~ ., 
                   family=binomial, data=data3)

data3 = data1
data3$Recidivism_Arrest_Year2 = as.factor(data$Recidivism_Arrest_Year2)
logit2.model = glm(Recidivism_Arrest_Year2 ~ ., 
                   family=binomial, data=data3)
pred = predict.glm(logit2.model, type='response')
auc(roc(data3$Recidivism_Arrest_Year2, pred))

data3 = data1
data3$Recidivism_Arrest_Year3 = as.factor(data$Recidivism_Arrest_Year3)
logit3.model = glm(Recidivism_Arrest_Year3 ~ ., 
                   family=binomial, data=data3)

# write.table(tidy(logit.model), file="year1.csv", sep=",")

# === AUC of the original regression ===
a = get_auc('Recidivism_Within_3years', data$Recidivism_Within_3years)
a1 = get_auc('Recidivism_Arrest_Year1', data$Recidivism_Arrest_Year1)
a2 = get_auc('Recidivism_Arrest_Year2', data$Recidivism_Arrest_Year2)
a3 = get_auc('Recidivism_Arrest_Year3', data$Recidivism_Arrest_Year3)

data.frame(h='AUC',
           Within_3years=a, 
           Arrest_Year1=a1,
           Arrest_Year2=a2,
           Arrest_Year3=a3)

# ===================================================
### deal with data imbalance ###
get_auc2("Recidivism_Arrest_Year1", data$Recidivism_Arrest_Year1)
get_auc2("Recidivism_Arrest_Year2", data$Recidivism_Arrest_Year2)
get_auc2("Recidivism_Arrest_Year3", data$Recidivism_Arrest_Year3)
# reportROC(testing$Y, pred)




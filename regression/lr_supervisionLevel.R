# Supervision_Level_First as label, multilogit
library(nnet)
data1$Supervision_Level_First = relevel(data$Supervision_Level_First, 
                                        ref="High")
multilogit = multinom(Supervision_Level_First ~ ., data=data1)
rml = summary(multilogit)
coef = rml$coefficients
z = rml$coefficients / rml$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2

# Supervision_Level_First=Standard as label, 
is.standard = c()
for (i in 1: length(data1$Supervision_Level_First)) 
{
  if (data1$Supervision_Level_First[i] == "Standard")
    {is.standard = c(is.standard, 1)}
  else
    {is.standard = c(is.standard, 0)}
}
data2 = data1
data2$isstd = is.standard
logit.model = glm(isstd ~ . - Supervision_Level_First, 
                  family=binomial, data=data2)
summary(logit.model)

# Supervision_Risk_Score_First as label, linear
linear.model = lm(Supervision_Risk_Score_First ~ ., data=data1)
summary(linear.model)

# =========================

library(broom)
result1 = rbind(coef, p)
write.table(result1, file="multilogit_SL.csv", sep=",")

result2 = tidy(logit.model)
write.table(result2, file="SL_standard.csv", sep=",")

result3 = tidy(linear.model)
write.table(result3, file="SRScore.csv", sep=",")


# =========================
# Supervision_Level_First=Specialized as label, 
is.spc = c()
for (i in 1: length(data1$Supervision_Level_First)) 
{
  if (data1$Supervision_Level_First[i] == "Specialized")
  {is.spc = c(is.spc, 1)}
  else
  {is.spc = c(is.spc, 0)}
}
data2 = data1
data2$isspc = is.spc
logit.model2 = glm(isspc ~ . - Supervision_Level_First, 
                   family=binomial, data=data2)
summary(logit.model2)

# Supervision_Level_First=High as label, 
is.spc = c()
for (i in 1: length(data1$Supervision_Level_First)) 
{
  if (data1$Supervision_Level_First[i] == "High")
  {is.spc = c(is.spc, 1)}
  else
  {is.spc = c(is.spc, 0)}
}
data2 = data1
data2$isspc = is.spc
logit.model2 = glm(isspc ~ . - Supervision_Level_First, 
                   family=binomial, data=data2)
summary(logit.model2)

# ====================
# recidivism measure ~ Supervision_Level_First
# ====================
data3 = data[, 34:53]
data3$Supervision_Level_First = data$Supervision_Level_First

data3$Recidivism_Within_3years = as.factor(data3$Recidivism_Within_3years)
data3$Recidivism_Arrest_Year1 = as.factor(data3$Recidivism_Arrest_Year1)
data3$Recidivism_Arrest_Year2 = as.factor(data3$Recidivism_Arrest_Year2)
data3$Recidivism_Arrest_Year3 = as.factor(data3$Recidivism_Arrest_Year3)

logit.model = glm(Recidivism_Within_3years ~ Supervision_Level_First, 
                  family=binomial, data=data3)
summary(logit.model)

logit1.model = glm(Recidivism_Arrest_Year1 ~ Supervision_Level_First, 
                  family=binomial, data=data3)
summary(logit1.model)

logit2.model = glm(Recidivism_Arrest_Year2 ~ Supervision_Level_First, 
                  family=binomial, data=data3)
summary(logit2.model)

logit3.model = glm(Recidivism_Arrest_Year3 ~ Supervision_Level_First, 
                  family=binomial, data=data3)
summary(logit3.model)










# This script uses Lasso regression 
# to estimate the most important variables to be used

library(glmnet)
data = read.csv("Demand_Month_Total_Unilever.csv")[-1]
View(data)
x = data.matrix(data %>% select(-c(DEMAND)))
y = data.matrix(data %>% select(DEMAND))

set.seed(1)
CV.L <- cv.glmnet(x,y,alpha=1)
summary(CV.L)
plot(CV.L)

L.min <- CV.L$lambda.min
L.min

coef(CV.L, L.min)

lm1 = lm(y~x)
lm1 = lm(y~x[,c(1,2,3,4,7)])
summary(lm1)

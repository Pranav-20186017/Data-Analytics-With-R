loans = read.csv("loans.csv")
z = table(loans$not.fully.paid)
kable(z)
z[2]/sum(z)
z = summary(loans)
kable(z)
library(mice)
set.seed(144)
vars.for.imputation = setdiff(names(loans), "not.fully.paid")
imputed = complete(mice(loans[vars.for.imputation]))
library(caTools)

set.seed(144)

spl = sample.split(loans$not.fully.paid, 0.7)

train = subset(loans, spl == TRUE)

test = subset(loans, spl == FALSE)

mod = glm(not.fully.paid~., data=train, family="binomial")
summary(mod)
-0.009317 * -10
exp(0.09317)
test$predicted.risk = predict(mod, newdata=test, type="response")

z = table(test$not.fully.paid, test$predicted.risk > 0.5)
kable(z)
sum(diag(z))/sum(z)
z = table(test$not.fully.paid)
kable(z)
z[1]/sum(z)
library(ROCR)
pred = prediction(test$predicted.risk, test$not.fully.paid)
as.numeric(performance(pred, "auc")@y.values
bivariate = glm(not.fully.paid~int.rate, data=train, family="binomial")
summary(bivariate)
pred.bivariate = predict(bivariate, newdata=test, type="response")
summary(pred.bivariate)
prediction.bivariate = prediction(pred.bivariate, test$not.fully.paid)

as.numeric(performance(prediction.bivariate, "auc")@y.values)
c = 10
r = 0.06
t = 3
c*exp(r*t)
test$profit = exp(test$int.rate*3) - 1
test$profit[test$not.fully.paid == 1] = -1
summary(test$profit)
highInterest = subset(test, int.rate >= 0.15)
mean(highInterest$profit)
z = table(highInterest$not.fully.paid)
kable(z)
z[2]/sum(z)
cutoff = sort(highInterest$predicted.risk, decreasing=FALSE)[100]
selectedLoans = subset(highInterest, predicted.risk <= cutoff) 
sum(selectedLoans$profit)
z = table(selectedLoans$not.fully.paid) 
kable(z)



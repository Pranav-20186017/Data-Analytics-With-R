machine = read.csv("machine.csv")
str(machine)


###Question 1: Compute the model R2 (the "Multiple R-squared" value)? 
model = lm(PRP ~ X + MYCT + MMIN  + MMAX + CACH + CHMIN + CHMAX,data=machine)
summary(model)
###R2 = 0.87


###Question 2: Which variables are significant in the model? 
summary(model)
#MYCT         5.064e-02  1.734e-02   2.921  0.00389 **
#MMIN         1.491e-02  1.832e-03   8.136 4.26e-14 ***
#MMAX         5.170e-03  6.517e-04   7.933 1.49e-13 ***
#CACH         8.415e-01  1.549e-01   5.433 1.60e-07 ***
#CHMAX        1.643e+00  2.251e-01   7.298 6.71e-12 ***


##Question 3: Compute the correlations between all the variables in the training set.Which of the following independent variables is MMAX highly correlated with (absolute correlation greater than 0.7)


cor(machine$MMAX, machine$X)
cor(machine$MMAX, machine$MYCT)
cor(machine$MMAX, machine$MMIN)
cor(machine$MMAX, machine$CACH)
cor(machine$MMAX, machine$CHMIN)
cor(machine$MMAX, machine$CHMAX)

#only MMIN has high correlaion with MMAX 0.7578268



#Question 4: Which of the independent variable is highly correlated with PRP

cor(machine$PRP, machine$X)
cor(machine$PRP, machine$MYCT)
cor(machine$PRP, machine$MMIN)
cor(machine$PRP, machine$MMAX)
cor(machine$PRP, machine$CHMIN)
cor(machine$PRP, machine$CHMAX)

#only MMAX has high correlation with PRP 0.8655762


#Question 5: Given that the correlations are so high, let us focus on the MMAX variable and build a model with only variables which have correlation is between -0.3 to 0.3 with MMAX. Compute the coefficient of MMAX in this reduced model
model2 = lm(PRP ~ MYCT + MMAX, data=machine)
summary(model2)

##
Residuals:
    Min      1Q  Median      3Q     Max
-230.90  -34.33    3.92   27.68  421.33

Coefficients:
              Estimate Std. Error t value Pr(>|t|)
(Intercept) -3.998e+01  1.067e+01  -3.748 0.000232 ***
MYCT         1.587e-02  2.332e-02   0.680 0.496965
MMAX         1.200e-02  5.178e-04  23.180  < 2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 80.97 on 205 degrees of freedom
Multiple R-squared:  0.7498,    Adjusted R-squared:  0.7473
F-statistic: 307.2 on 2 and 205 DF,  p-value: < 2.2e-16
##



### Question 6: Compute the above reduced model R2
#Multiple R-squared:  0.7498



##Question 7: 
ans = step(model2)
summary(ans)
##
## Residuals:
    Min      1Q  Median      3Q     Max
-230.65  -35.72    4.01   29.63  425.55

Coefficients:
              Estimate Std. Error t value Pr(>|t|)
(Intercept) -3.516e+01  7.965e+00  -4.414 1.63e-05 ***
MMAX         1.187e-02  4.784e-04  24.808  < 2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 80.87 on 206 degrees of freedom
Multiple R-squared:  0.7492,    Adjusted R-squared:  0.748
F-statistic: 615.4 on 1 and 206 DF,  p-value: < 2.2e-16
##


#Question 8:
model3 = lm(PRP ~ MYCT + MMAX,data = machine)
library(caTools)
set.seed(6)
split = sample.split(machine$PRP, SplitRatio = 0.75)
machineTrain = subset(machine, split = TRUE)
machineTest = subset(machine, split = FALSE)
pr = predict(model3, newdata = machineTest)
summary(pr)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
 -33.66   10.57   57.63  105.18  152.85  728.67
 SSE = sum(model3$residuals^2)
SST = sum(mean(model3$residuals)-mean(machineTest$PRP))
R2 = 1 - SSE/SST
R2
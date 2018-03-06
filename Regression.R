library(readxl)
library(psych)
library(pwr)
require(pwr)

pinemodel <- read_excel("C:/Murthy/Learning/Great Lakes/05 Advanced Stats/regressiondata/Regression - Volume & Diameter.xlsx", 
                    sheet = "Sheet1")
attach(pinemodel)
reg=lm(Volume~Diameter)
anova(reg)
plot(Diameter, Volume, main="Best Fit Line", col = "Green")
abline(reg, col="Blue")

summary(reg)

pineregression <- read_excel("C:/Murthy/Learning/Great Lakes/05 Advanced Stats/regressiondata/Regression - Volume & Diameter.xlsx", 
                   sheet = "Sheet1")
View(pineregression)

attach(pineregression)
model = lm (Volume~Diameter)
plot(Diameter,Volume,main="Best Fit Line", col="Green")
abline(model, col="Red")

res=residuals(model)
fit = fitted(model)
plot(fit,res,main="Residual versus x")
summary(model)$r.square



Regassumptions <- read_excel("C:/Murthy/Learning/Great Lakes/05 Advanced Stats/regressiondata/Regassumptions.xlsx", 
                             sheet = "Sheet3")
attach(Regassumptions)
model = lm (Alcohol~Tobacco)
plot(Tobacco,Alcohol,main="Best Fit Line", col="Green")
abline(model, col="Red")
summary(model)$r.square

Regassumptions1 <- read_excel("C:/Murthy/Learning/Great Lakes/05 Advanced Stats/regressiondata/Regassumptions.xlsx", 
                              sheet = "Sheet4")

attach(Regassumptions1)
Region=Regassumptions1$Region
Alcohol=Regassumptions1$Alcohol
Tobacco=Regassumptions1$Tobacco

Regassumptions1=lm(Alcohol~Tobacco)
plot(Tobacco,Alcohol,main = "Without  Northern Ireland")
abline(model, col="Red")
abline(moswl, col="Green")
summary(model)$r.square

ModelDatatransformation <- read_excel("C:/Murthy/Learning/Great Lakes/05 Advanced Stats/regressiondata/Datatransformation.xlsx", 
                                      sheet = "Sheet1")
attach(ModelDatatransformation)
model=lm(prop~time)
plot(time,prop,main="Plot of Prop Versus Time", col = "Blue")
abline(model, col = "Red")

res=residuals(model)
fit=fitted(model)
model=lm(prop~time)
plot(fit,res,main="Residual Versus Fit Plot", col = "Blue")
abline(h=0, col = "Red")

qqnorm(res)
qqline(res,col = "Red")

shapiro.test(res)

#ln(x)
Intime = log(time,base = exp(1))
newmodel = lm(prop~Intime)
plot(Intime,prop,main = "Best fit line")
abline(newmodel,col="Red")


Pg2ModelDatatransformation <- read_excel("C:/Murthy/Learning/Great Lakes/05 Advanced Stats/regressiondata/Datatransformation.xlsx", 
                                         sheet = "Sheet2")
attach(Pg2ModelDatatransformation)
model=lm(Gestation~Birthweight)
plot(Birthweight, Gestation, main = "Birth Weight and Gestation Plot", col = "Green")
abline(model, col = "blue")
# It appears that Data is linear but variance may not be equal

res = residuals(model)
fit = fitted(model)
plot(fit,res,main = "Residual Vs Fitted Plot", col = "Blue")
abline(h=0, col = "Red")
qqnorm(res)
qqline(res)
shapiro.test(res)

Ingestation=log(Gestation, base = exp(1))
newmodel=lm(Ingestation~Birthweight)
plot(Birthweight, Ingestation, main = "Gestation vs Birthweight")
abline(newmodel, col = "Blue")

# Note that, as expected the log transformation has tended to "spread out" the smaller
# gestations and tended to "bring in" the larger ones

newres = residuals(newmodel)
newfit=fitted(newmodel)
plot(newfit~newres ,main="Residual vs Fit Model", col = "Green")
abline(h=0, col = "Red")

#The new residual vs. fits plot shows a marked improvement in the spread of the residuals



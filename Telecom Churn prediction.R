if(!require(readxl))
  install.packages("readxl")
library(readxl)
require(readxl)

Cellphone <- read_excel("C:/Murthy/Learning/Great Lakes/07 Predictive Modeling/Group Assignment/Telecom Churn/Cellphone.xlsx", 
                        sheet = "Data")
View(Cellphone)
str(Cellphone)

table(Cellphone$Churn)

print('Percentage of customers getting churned'); 1- nrow(Cellphone[Cellphone$Churn == 0,])/nrow(Cellphone)

##############################################################################
# In this dataset Cellphone company churn data of which we have 483 (~14.5%) #
# customers getting churned                                                  #
##############################################################################

if(!require(caTools))
  install.packages("caTools")
library(caTools)
require(caTools)

if(!require(stats))
  install.packages("stats")
library(stats)
require(stats)

set.seed(1234)
split <- sample.split(Cellphone, SplitRatio = 0.7)
split

training <- subset(Cellphone, split == "TRUE")
testing <- subset(Cellphone, split == "FALSE")

glmmodeltrg <- glm(Churn~., family = "binomial", data = training)
summary(glmmodeltrg)

#########################################################
# we have Contract Renewal with 99.9% confidence        #
# we have Cust Service Calls with 99.9% confidence and  #
# we have Roaming Minutes with 90% confidence           #
#########################################################


############### GLM Model in training dataset ###################
#                                                               #
# Null deviance - when the Beta0 is being used 1743.3           #
# Residual deviance - including independent variables 1401.5    #
# AIC - 1423.5                                                  #
#                                                               #
#################################################################


table(ActualValue = training$Churn, PredictedValue = result>0.5)
nrow(training[training$Churn == 0,])/nrow(training)

###################  Baseline accuracy #####################
#                    of training data                      #
############################################################


glmmodeltrg <- glm(Churn~. -DayCalls -DayMins -OverageFee -AccountWeeks -DataPlan, family = "binomial", data = training)
summary(glmmodeltrg)

############### GLM Model in training dataset ###################
#       without Day calls, Day Mins, Overage Fee,               #
#               Account Weeks and Data Plan                     #
#                                                               #
# Null deviance - when the Beta0 is being used 1743.3           #
# Residual deviance - including independent variables 1405      #
# AIC - 1417                                                    #
#                                                               #
#################################################################

result <- predict(glmmodeltrg, training, type = "response")
result
result <- as.data.frame(result)

######### Predicting model accuracy in testing dataset ##########
#       without Day calls, Day Mins, Overage Fee,               #
#               Account Weeks and Data Plan                     #
#                                                               #
#                 creating confusion matrix                     #
#                                                               #
#################################################################

table(ActualValue = testing$Churn, PredictedValue = result>0.5)
print('Testing dataset model accuracy'); nrow(testing[testing$Churn == 0,])/nrow(testing)

################ Accuracy of testing dataset #####################
#                                                                #
#  Our logistic model without Day Calls, Day Mins, Overage Fee,  #
#  Account Weeks and Data Plan our prediction accuracy is 85.23% #
#                                                                #
##################################################################

if(!require(boot))
  install.packages("boot")
library(boot)
require(boot)

if(!require(ROCR))
  install.packages("ROCR")
library(ROCR)
require(ROCR)

if(!require(pROC))
  install.packages("pROC")
library(pROC)
require(pROC)

if(!require(graphics))
  install.packages("graphics")
library(graphics)
require(graphics)

if(!require(gplots))
  install.packages("gplots")
library(gplots)
require(gplots)



ROCpredicted <- predict(glmmodeltrg, newdata = training)
trgpredit = predict(ROCpredicted, training$Churn)


ROCperformance <- performance(ROCpredicted,'tpr','fpr')


plot(ROCperformance)
plot(ROCperformance, colorize = TRUE, print.cutoffs.at=seq(0.1, by=0.1))

auc <- performance(ROCpredicted, measure="auc")
auc <- auc@y.values[[1]]
auc



######### Creating decision tree model ################
#  to arrive at a business case and relevance study   #
#######################################################

if(!require(party))
  install.packages("party")
library(party)
require(party)

trgtreemodel <- ctree(Churn ~. , data = training)
trgtreemodel
summary(trgtreemodel)
plot(trgtreemodel, type = 'simple')
text(trgtreemodel, pretty = 0)

tsttreemodel <- ctree(Churn ~., data = testing)
tsttreemodel
summary(tsttreemodel)
plot(tsttreemodel, type = 'simple')
text(tsttreemodel, pretty = 0)

# treemodelpruned <- ctree(Churn ~. -DayCalls -DayMins -OverageFee -AccountWeeks -DataPlan, data = Cellphone)
# summary(treemodelpruned)
# plot(treemodelpruned, type = 'simple')
# text(treemodelpruned, pretty = 0)



###############################################################################
# The output tells that that R used 7 variables in the decision tree and that 
# we have a total of 12 terminal nodes. We also see that the residual mean 
# deviance (RMD) is 0.3372 an we have a ~5% misclassification rate. RMD is a 
# metric that indicates how well the tree fits the data. In the real world 
# however, we would be more interested in the RMD of the tree on the test 
# dataset. Generally, a lower value indicates a better fitted decision tree. 
# The classification error rate on the other hand, is an indicator of model 
# accuracy. We see that out of 3,333 observation, 
# the tree misclassified 197 (~5%) observations.
###############################################################################


###############################################################################
#
# if(!require(FactoMineR))
#   install.packages("FactoMineR")
# library(FactoMineR)
# require(FactoMineR)
# 
# trgpcamodel = PCA(training, graph = TRUE)
# trgpcamodel$eig
# trgpcamodel$var$coord
# head(pcamodel$ind$coord)
# 
# if(!require(ggplot2))
#   install.packages("ggplot2")
# library(ggplot2)
# require(ggplot2)
# 
# pcascores = as.data.frame(trgpcamodel$var)
# pcascores
# 
# ggplot(data = pcascores, aes(x = coord.Dim.2, y = coord.Dim.5, label = rownames(pcascores))) +
#   geom_hline(yintercept = 0, colour = "gray65") +
#   geom_vline(xintercept = 0, colour = "gray65") +
#   geom_text(colour = "red", alpha = 0.7, size = 4) +
#   ggtitle("PCA for Telecom Churn")
#
#################################################################################
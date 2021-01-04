setwd("C:/Users/KennyD/Documents/GitHub/ChurnAnalysisinTelecomIndustry")

churn = read.csv('churn.csv')

str(churn)

# As we see several features in character data type,
# we will convert them into factor type for better modeling.

library(dplyr)

churn = churn %>% mutate_if(is.character, as.factor)

str(churn)

# We also need to convert Senior Citizen feature to factor.

churn$SeniorCitizen = as.factor(churn$SeniorCitizen)

                        # Data Cleaning

#Let's see if there are any features that can be removed to improve accuracy.

churn = subset(churn, select = -customerID)

str(churn)

                        # Exploratory Data Analysis

# Graphs are plotted based on type of variable combinations.

# For x = continuous and y = categorical, we can use boxplot

library(ggplot2)


myboxplot = function(x)
{
    ggplot(churn, aes_string(churn[,x], y=churn$Churn, fill=churn$Churn))+
        geom_boxplot()+
        xlab(x)
        
}

myboxplot('tenure')

myboxplot('MonthlyServiceCharges')

myboxplot('TotalAmount')


# For x = categorical and y = categorical, count plots are best used.

mycountplot = function(x)
{
    ggplot(churn, aes(x=churn[,x],fill=Churn))+
        geom_bar(position='dodge')+
        geom_text(stat="count", 
                  aes(label=..count..), 
                  position = position_dodge(width=0.9),
                  vjust=-0.3)+ xlab(x)
}


mycountplot('gender')       # Distribution for both categories seem similar.

mycountplot('Partner')      # Having a partner decreases Churn rate.

mycountplot('Dependents')   # No meaningful insight from this variable.

mycountplot('CallService')  # Availability of Call service decreases churn.

mycountplot('MultipleConnections')  # Not much insight.

mycountplot('InternetConnection')   # Less churn among DSL connections.
                                    # High churn among Fiber optic connection.

mycountplot('OnlineSecurity')       # Less churn with Online security.
                                    # High churn among non-online security.
                                    # Online security should be increased.

mycountplot('OnlineBackup')         # Less churn with Online Backup.
                                    # Online Backup could be increased.

mycountplot('DeviceProtectionService')  # Device Protection could be increased.

mycountplot('TechnicalHelp')        # Technical Help service could be increased.

mycountplot('OnlineTV')     # Not much influence.

mycountplot('OnlineMovies') # Minute influence of Online Movies on churn.

mycountplot('Agreement')    # High influence.

mycountplot('BillingMethod')

mycountplot('PaymentMethod')    # Electronic check service could be reduced.


                        # Splitting the data

set.seed(1)
library(caTools)

sample = sample.split(churn, 0.8)
train = subset(churn, sample==T)
test = subset(churn, sample==F)

                        # Decision Tree Algorithm

library(rpart)
library(rpart.plot)

dtree = rpart(Churn~., train, method = 'class')
dt_preds = predict(dtree, test, type = 'class')

library(caret)
confusionMatrix(test$Churn, dt_preds)

# For set.seed(1), the Accuracy is 76%.
# We need to do better Feature Engineering to achieve better results.

x = rpart.plot(dtree)


                        # Logistic Regression

log_model = glm(Churn~.,train,family=binomial())

summary(log_model)

opt_model = step(log_model, direction = 'both')

summary(opt_model)

log_preds = predict(opt_model, test, type = "response")
log_preds = ifelse(log_preds>0.5, "Yes",'No')
log_preds = as.factor(log_preds)

confusionMatrix(test$Churn,log_preds)

cm = confusionMatrix(test$Churn,log_preds)
cm$overall[['Accuracy']]

# Accuracy by logistic Regression is 69%, which is still less.


    
                        # Naive Bayes Model
library(e1071)
NB_model = naiveBayes(Churn~.,data=train)
NB_preds = predict(NB_model, test, type = 'class')
confusionMatrix(test$Churn, NB_preds)

# Accuracy for Naive Bayes method resulted in 70% accuracy.


                        # Linear Discriminatory Analysis
library(MASS)
lda_model = lda(Churn~.,train)
summary(lda_model)
NB_preds = predict(lda_model, test)
lda_preds = preds$class
confusionMatrix(test$Churn, lda_preds)

# Naive Bayes too gives us an accuracy of 69.7%.

# From this, we can conclude that Decision Tree model is best suited
# for this dataset. 

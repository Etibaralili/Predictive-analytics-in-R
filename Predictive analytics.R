#===================================================================
# Bryan School of Buiness and Economics, UNCG
# ISM645 / IAF601 Principle of Predictive Analytics
# Assignment 4: Mock Test for Final Exam
# Due Date:     December 6 (Friday), 11:59 pm
#===================================================================



# Import the csv file (wine_quality.csv) and explore it.
#====================== Write R code HERE ==========================

data <- read.csv("wine_quality.csv")

str(data)

View(data)

library(dplyr)
library(ggplot2)
library(DescTools)
library(caTools)
library(pROC)
library(rpart)
library(rpart.plot)
library(broom)
library(randomForest)
#===================================================================




#======= Question 1 (2 Point) =======
# Q1-1. Define a binary variable of "alcohol_level" defined as "Strong" if alcohol > 10 and "Weak" otherwise.
# Q1-2. Make a bar plot for average by wine type and alcohol_level (see Canvas for an exemplar plot). (Hint: add position="dodge" option to geom_col or geom_bar function)
# Q1-3. Make a histogram of wine quality by wine types with binwidth = 1 (see Canvas for an exemplar plot). 

#====================== Write R code HERE ==========================

#Q1-1


data <- data %>%
  mutate(alcohol_level = ifelse(alcohol>10, "Strong", "Weak"))

View(data)
#Q1-2

  
av_quality <- group_by(data, type, alcohol_level) %>%
  summarise(quality = mean(quality))


ggplot(av_quality, aes(type, quality, fill = alcohol_level))+
  geom_col( position = "dodge")
#Q1-3

ggplot(data, aes(x = quality, fill = type)) + 
  geom_histogram(binwidth = 1) +
  facet_wrap(~type, nrow=1, ncol=2)

#===================================================================




#======= Question 2 (2 Point) =======
# Q2-1. Split data into 70% train and 30% test data.
# Q2-2. Based on train data, make "two" linear regression models to predict wine quality using (i) all available predictor variables and (ii) all but wine type.
# Q2-3. Based on test data, evaluate two predictive models based on RMSE (Root Mean Squared Error). Is it important to consider wine type differently in predicting quality (sensory preference)?

#====================== Write R code HERE ==========================
#Q2-1
set.seed(123)

split <- sample.split(data, SplitRatio = 0.7)

train <- subset(data, split==TRUE)
test <- subset(data, split==FALSE)


# Q2-2
#i
lm1 <- lm(quality~ type + fixed.acidity + volatile.acidity + citric.acid +
            residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + 
            density + pH + sulphates + alcohol + alcohol_level, data = train)
summary(lm1)
#ii 


lm2 <- lm(quality~ fixed.acidity + volatile.acidity + citric.acid + 
            residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + 
            density + pH + sulphates + alcohol+ factor(alcohol_level), data = train)
summary(lm2)

#Q2-3
length(coefficients(lm1))
length(coefficients(lm2))

lm1%>%
  augment(new_data=test)%>%
  mutate(residual = .fitted - quality)%>%
  mutate(sq_residual = residual^2)%>%
  summarize(mse = mean(sq_residual))%>%
  mutate(rmse = sqrt(mse))

#RMSE:0.728
 
#ii
lm2%>%
  augment(new_data = test)%>%
  mutate(residual = .fitted - quality)%>%
  mutate(sq_residual = residual^2)%>%
  summarize(mse = mean(sq_residual))%>%
  mutate(rmse = sqrt(mse))

#RMSE:0.730

#Based on RMSE we can see that difference between 2 models is negligble which amounts
#to say that it is not significantly important difference. 



#===================================================================




#======= Question 3 (2 Point) =======
# Q3-1. Based on "original" data frame, replace the target variable "quality" with a binary variable (factor type) defined as 1 if quality > 6 and 0 otherwise.
# Q3-2. Based on "original" data frame, split data again into 70% train and 30% test data.
# Q3-3. Based on train data, make a logistic regression model to predict wine quality using all available predictor variables.
# Q3-4. Based on test data, make a ROC curve and calculate AUC.

#====================== Write R code HERE ==========================

#Q3-1

data <- data %>%
  mutate(quality = ifelse(quality>6, 1, 0))
  
View(data)


#Q3-2
set.seed(123)
split <- sample.split(data, SplitRatio = 0.7)

train <- subset(data, split==TRUE)
test <- subset(data, split==FALSE)
View(test)



#Q3-3
logistic <- glm(quality~ type + fixed.acidity + volatile.acidity + citric.acid + residual.sugar +
                  chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density + pH +
                  sulphates + alcohol + alcohol_level, data = train, family="binomial")

summary(logistic)



# Q3-4

logistic <- logistic%>%
  augment(type.predictor = "response", newdata = test) %>%
  mutate(predicted = ifelse(.fitted > 0.5,1,0))

ROC <- roc(test$quality , logistic$.fitted)
plot(ROC)
auc(ROC) 

#Area under the curve: 0.8028

#===================================================================




#======= Question 4 (2 Point) =======
# Q4-1. Based on train data, build a random forest model to predict quality using all available predictor variables.
# Q4-2. Based on importance measure from the random forest, which factors do you think are important in predicting wine quality?
# Q4-3. Based on test data, make a ROC curve and calculate AUC. Which model do you think is better, logistic regression (Q3) or random forest (Q4)?

#====================== Write R code HERE ==========================

#Q4-1
random_for <- randomForest(factor(quality) ~ type + fixed.acidity + volatile.acidity + citric.acid + residual.sugar +
                                chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density + pH +
                                sulphates + alcohol, data = train, ntree = 100, importance = T)
#Q4-2
importance(random_for, type = 1)
varImpPlot(random_for)

#According to importance plot these variables play crucial role in predicting wine quality: 
# alcohol,residual.sugar, density, volatile.acidity, sulphates, citric.acid, free.sulfur.dioxide, 
#pH, chlorides, fixed.acidity, total.sulfur.dioxide


#Q4-3
prediction_new <- random_for %>%
  predict(type = "prob", newdata = test)

ROC_new <- roc(test$quality, prediction_new[, 2])
plot(ROC_new)
auc(ROC_new) 

#Area under the curve: 0.9076

#As AUC for random forest is 0.9076 we can claim that it is better than logistic regression (AUC: 0.8028). 

#===================================================================




#======= Question 5 (2 Point) =======
# Many wine experts argue that acid is a vital component of wine.
# Q5-1. Build and evaluate "two" random forest models to predict wine quality using (i) all available predictor variables and (ii) all but acidity measures (fixed.acidity, volatile.acidity, citric.acid, and pH).
# Q5-2. Based on your analysis, do you agree that acid is a significant predictor of wine quality (sensory preference)?

#====================== Write R code HERE ==========================

#Q5-1
random_for1 <- randomForest(factor(quality) ~ type + fixed.acidity + volatile.acidity + citric.acid + residual.sugar +
                                  chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density + pH +
                                  sulphates + alcohol, data = train, ntree = 100, importance = T)

prediction_1 <- random_for1 %>%
  predict(type = "prob", newdata = test)

ROC <- roc(test$quality, prediction_1[, 2])
auc(ROC)
#Area under the curve: 0.908


random_for2 <- randomForest(factor(quality) ~ type + residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density +
                                  sulphates + alcohol, data = train, ntree = 100, importance = T)

prediction_2 <- random_for2 %>%
  predict(type = "prob", newdata = test)

ROC2 <- roc(test$quality, prediction_2[, 2])
auc(ROC2)

#Area under the curve: 0.8856

#Q5-2
#After comparing 2 models we receive close AUC values which necessarily means that
#absence of acidity measures does not have significant impact on overall result. 

#===================================================================



#===================================================================
# Before submission, I recommend you restart your RStudio and press (Ctrl + Alt + R) to run all codes.
# Please ensure that there is no error message in your code.
#===================================================================

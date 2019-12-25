pack <- c("tidyverse", "ggthemes", "corrplot", "GGally", "DT", "caret", "magrittr")
install.packages(pack, dependencies = TRUE)

library(tidyverse)
library(ggthemes)
library(corrplot)
library(GGally)
library(DT)
library(caret)
library(magrittr)
library(data.table)


loan = fread("loan.csv", stringsAsFactors = TRUE)
colnames(loan)


#loan_status : Variable with multiple levels (e.g. Charged off, Current, Default, Fully Paid .)
#loan_amnt : Total amount of loan taken
#int_rate : Loan interset rate
#grade : Grade of employment
#emp_length : Duration of employment
#home_ownership : Type of ownership of house
#annual_inc : Total annual income
#term : 36-month or 60-month period

loan_select_cols = loan %>% select(loan_status , 
                                   loan_amnt , 
                                   int_rate ,
                                   grade , 
                                   emp_length ,
                                   home_ownership , 
                                   annual_inc ,
                                   term)

#check for NA across the selected data
sapply(loan_select_cols, function(x) sum(is.na(x)))


#filter the rows which do not have any value or no value
loan_select_cols = loan_select_cols %>% filter(!is.na(annual_inc),
                                               !(home_ownership %in% c("NONE", "ANY")),
                                               emp_length != "n/a")



#EDA

loan %>% 
  count(loan_status) %>%
  ggplot(aes(x = reorder(loan_status, desc(n)), y = n, fill = n)) +
  geom_col() +
  coord_flip() +
  labs(x = "Loan Status", y = "Count")

#create a new binary varibale 1 for default and 0 for non default and remove the columns for current, late payments and grace preiods. 
#convert the 10 factor variable to 2

loan_select_cols = loan_select_cols %>%
  mutate(loan_outcome = ifelse(loan_status %in% c("Charged Off", "Default"), 1,
                               ifelse(loan_status == "Fully Paid", 0, "No Info"))
  )

barplot(table(loan_select_cols$loan_outcome), col = "blue")


loan2 = loan_select_cols %>%
  select(-loan_status) %>%
  filter(loan_outcome %in% c(0,1))

#variable that may be important for modelling 
ggplot(loan2 , aes(x = grade, y = int_rate, fill = grade)) +
  geom_boxplot() +
  theme_igray() +
  labs(y = "Intrest Rate", x = "Grade")

#a table to see the count for all the loans which are paid and which defaults in each of the grade

table(loan2$grade , factor(loan2$loan_outcome, c(0,1), c("Fully Paid", "default")))
ggplot(loan2 , aes(x = grade , y = ..count.. , fill = factor(loan_outcome , c(1 , 0) , c('Default' , 'Fully Paid')))) + 
  geom_bar() + 
  theme(legend.title = element_blank())

ggplot(loan2[sample(244179 , 10000) , ] , aes(x = annual_inc , y = loan_amnt , color = int_rate)) +
  geom_point(alpha = 0.5 , size = 1.5) + 
  geom_smooth(se = F , color = 'red' , method = 'loess') +
  xlim(c(0 , 300000)) + 
  labs(x = 'Annual Income' , y = 'Loan Ammount' , color = 'Interest Rate')




#xgb model approach
library(xgboost)
library(caret)
library(rsai)

#one hot encoding for categorical varibales

#pull all the categorical variables into a data frame
loan2_categorical <- data.frame(loan2$grade, loan2$emp_length, loan2$home_ownership, loan2$term)
dmy <- dummyVars(" ~ .", data = loan2_categorical)
loan2_encoded <- data.frame(predict(dmy , newdata = loan2_categorical))

#combine the columns of the encoded data with the loan2 data
loan2 = cbind(loan2, loan2_encoded)
loan2 = as.data.frame(loan2)

#Data Modelling and divide the data into train and test
loan2$loan_outcome = as.numeric(loan2$loan_outcome)
idx = sample(dim(loan2)[1], 0.75*dim(loan2)[1], replace = F)
trainset = loan2[idx, ]
testset = loan2[-idx,]

#setting the response variable in the test data to NULL
test_outcome <- testset$loan_outcome
testset$loan_outcome <- NULL

#prepare Xgb data with test and train
xgbData <- c(
              train = xgbSetupdata2(trainset,
                        response = "loan_outcome",
                        explanatory_vars = colnames(testset)
                        ),
              test = xgbSetupdata2(testset,
                       explanatory_vars = colnames(testset)
                        )
              )

#create parameter list for xgb
param <- list(
              objective = "binary:logistic",
              max_depth = 7,           # tree-depth
              subsample = 0.6,          # randomly sample rows before fitting each tree
              colsample_bytree = 0.6,   # randomly sample columns before fitting each tree
              min.child.weight = 1,    # minimum weight per leaf
              eta = 0.001          # Learning rate
              )
  

# Build xgboost models using xgb cross-validation
set.seed(1984) # for repeatability
xgbCV <- xgb.cv(
                 params                 =  param,
                 data                   =  xgbData$train,
                 nrounds                =  1000,        # Maximum number of trees to build
                 nfold                  =  5,
                 early_stopping_rounds  =  10L,        # Stops algorithm early if performance has not improved in n rounds
                 print_every_n          =  20L,        # How often to print to console
                 prediction             =  TRUE        # Keeps the predictions
                )


# Plot evaluation log
plot(xgbCV)


# Train full model
xgbFit <- xgb.train(
                    params      =  param,
                    data        =  xgbData$train,
                    nrounds     =  xgbCV$best_iteration,
                   )

#Predict on to test data
testset<-as.data.table(testset)
testset[, loan_outcome := predict(xgbFit, xgbData$test)]


# Feature importance
featImp <- xgb.importance(xgbFit,  feature_names = colnames(xgbData$train))

featImpPlot(
             FeatImportanceSum(featImp),
             main = "Feature Importance",
             top_n = 100
            )



#accuracy matrix
k = 0
accuracy = c()
sensitivity = c()
specificity = c()


for(i in seq(from = 0.01 , to = 0.5 , by = 0.01)){
  k = k + 1
  preds_binomial = ifelse(testset$loan_outcome > i , 1 , 0)
  confmat = table(test_outcome , preds_binomial)
  accuracy[k] = sum(diag(confmat)) / sum(confmat)
  sensitivity[k] = confmat[1 , 1] / sum(confmat[ , 1])
  specificity[k] = confmat[2 , 2] / sum(confmat[ , 2])
}

threshold = seq(from = 0.01 , to = 0.5 , by = 0.01)

data = data.frame(threshold , accuracy , sensitivity , specificity)
head(data)


# Gather accuracy , sensitivity and specificity in one column
ggplot(gather(data , key = 'Metric' , value = 'Value' , 2:4) , 
       aes(x = threshold , y = Value , color = Metric)) + 
  geom_line(size = 1.5)


preds.for.30 = ifelse(preds > 0.3 , 1 , 0)
confusion_matrix_30 = table(Predicted = preds.for.30 , Actual = testset$loan_outcome)
confusion_matrix_30

acc = sum(diag(confusion_matrix_30))/sum(confusion_matrix_30)





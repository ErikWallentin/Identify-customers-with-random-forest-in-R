#####################################################################################################
# A given company needs help identifying which customers they should reach out to regarding an offer to 
# buy a software they have created. The company wish that a random forest-model, that in a 
# good way predicts potential buyers is created to help them minimize time and money on the wrong 
# customers. The company don't reach out to customers which are predicted as non-buyers, and they 
# reach out to customers who are predicted as buyers.

# To be able to create a random forest model, we have access to 14 variables which are explained in 
# more detail below.

#   Variable		      Explanation

#   LineDiscount 		  Customer Discount Agreement
#   CustomerUnit		  Customer Size Segmentation
#   Municipality		  Municipality
#   County		        County
#   Country		        Country
#   NoOfEmployees	    Number of Employees
#   Potential		      Customer IT Spend Potential
#   AccessoryRatio	  Share of Accessories Purchased
#   NSB		            Net Sales (SEK)
#   CPLow		          Customer Profit (%)
#   Days		          Days since last purchase
#   TargetDays		    Days since software purchase
#   NrOfOrder		      Number of Orders
#   Software		      Customer bought software (Yes/No)
#####################################################################################################

# Import the data.
Data <- read.csv(file.choose(), header=TRUE, sep=",")

# Remove rows which contains non-complete data.
nonComplete <- c("N/A", "NULL", "Not Defined")
Data <- Data[!Data$County %in% nonComplete,]
Data <- Data[complete.cases(Data),]

# Some columns are better suited as "numeric" types. Convert these factor-variables to numeric.
Data$CPLow <- as.numeric(Data$CPLow)
Data$NSB <- as.numeric(Data$NSB)
Data$AccessoryRatio <- as.numeric(Data$AccessoryRatio)
Data$Potential <- as.numeric(Data$Potential)
Data$NoOfEmployees <- as.numeric(Data$NoOfEmployees)

# Examine whether we can directly detect outliers in the data using the summary function.
summary(Data)

# The maximum value of the variable "NrOfOrder" looks suspiciously large. Examine the variable further. 
table(Data$NrOfOrder)
plot(Data$NrOfOrder)

# Remove the outlier from our dataframe.
Data <- Data[-which(Data$NrOfOrder==max(Data$NrOfOrder)),]

# Create a data frame containing information about how many observations are from a given county, 
# sorted from most observations to least. The 49 counties with the most observations remains the same, 
# while the others are merged into one county under the name "Others". 
# This process is done because the random forest method in R can only include a variable containing 
# no more than 53 levels.
County_Vektor <- as.data.frame(table(Data$County))  
County_Vektor <- County_Vektor[order(County_Vektor[,2], decreasing = T),]
Övriga_County <- County_Vektor[c(50:nrow(County_Vektor)),1]
Övriga_County <- as.character(Övriga_County)
Data$County <- as.character(Data$County)
Data$County[Data$County %in% Övriga_County] <- "Others"
Data$County <- as.factor(Data$County)

# Create a training dataset and a test dataset. 90% of the observations will end up in the 
# training dataset and the remaining 10% in the test dataset.
attach(Data)
set.seed(2)
train <- sample(1:nrow(Data), round(nrow(Data)*0.90, 0))

# Create a large random forest-model that estimates if a person will buy a software or not.
install.packages("randomForest")
library(randomForest)
rf <- randomForest(Software ~ LineDiscount +  CustomerUnit + County
                   + Country + NoOfEmployees + Potential + AccessoryRatio + NSB
                   + CPLow + Days + TargetDays + NrOfOrder, 
                   data = Data, subset = train,
                   mtry = 3, importance = T, ntree = 1000)

# Create the test dataset which includes the variables included in the randomforest model.
install.packages("dplyr")
library(dplyr)
test_Dataset <- select(Data[-train,], LineDiscount, CustomerUnit, County,
                       Country, NoOfEmployees, Potential, AccessoryRatio, NSB,
                       CPLow, Days, TargetDays, NrOfOrder)

# Produce estimated values of y, if a person buys a software or not.
rf.Yhatt <- predict(rf, newdata = test_Dataset)

# Produce the true classifications, which are later compared to the ones that are estimated.
Software.test <- Software[-train]

# Create a "confusion matrix" and calculate the proportion of correct / incorrect classifications.
table(Skattat = rf.Yhatt, Sant = Software.test)
mean(rf.Yhatt == Software.test)
mean(rf.Yhatt != Software.test)

# Fine-tune the "mtry" parameter by testing a value between 1-12. A plot reports
# which value of "mtry" that gives the lowest proportion of test misclassification.
rf.MissClass <- rep(0,12)
for(d in 1:12){
  rf.Check <- randomForest(Software ~ LineDiscount +  CustomerUnit + County
                           + Country + NoOfEmployees + Potential + AccessoryRatio + NSB
                           + CPLow + Days + TargetDays + NrOfOrder, 
                           data = Data, subset = train,
                           mtry = d, importance = T, ntree = 1000)
  rf.Yhatt <- predict(rf.Check, newdata = test_Dataset)
  rf.MissClass[d] <- mean(rf.Yhatt != Software.test)
}

MTRY <- (1:12)
plot(MTRY, rf.MissClass, type = "b")

# Find out which explanatory variables that are most important to our model.
# (When dealing with classification, the "MeanDecreaseGini"-plot is used)
varImpPlot(rf)

# Show that all observations that assume a value over 1100 in the "TargetDays" column are not buyers.
# This is therefore a variable that easily can classify if a person buys the software or not.
DataTargetDaysBig <- subset(Data, TargetDays > 1100)
summary(DataTargetDaysBig$Software)

# Create a smaller random forest model that doesn't include the most important explanatory variable
# "TargetDays". The proportion of correct / incorrect classifications is then calculated.
test_Dataset <- select(Data[-train,], NrOfOrder, Days, County, CPLow)

rf.MissClass <- rep(0,4)
for(d in 1:4){
  rf.Check <- randomForest(Software ~ NrOfOrder + Days + County + CPLow, 
                           data = Data, subset = train,
                           mtry = d, importance = T, ntree = 1000)
  rf.Yhatt <- predict(rf.Check, newdata = test_Dataset)
  rf.MissClass[d] <- mean(rf.Yhatt != Software.test)
}

MTRY <- (1:4)
plot(MTRY, rf.MissClass, type = "b")

rf.Small <- randomForest(Software ~ NrOfOrder + Days + County + CPLow, 
                         data = Data, subset = train,
                         mtry = 1, importance = T, ntree = 1000)

rf.Small.Yhatt <- predict(rf.Small, newdata = test_Dataset)

table(Skattat = rf.Small.Yhatt, Sant = Software.test)
mean(rf.Small.Yhatt == Software.test)
mean(rf.Small.Yhatt != Software.test)

#####################################################################################################
# The model above classifies a potential customer correctly approximately 83% of the cases of our test
# dataset. (Note: This value may vary slightly because random forest creates 1000 random trees).
# However, the majority of those who are correctly classified are people who will not buy the 
# software, while people who actually bought the software are classified as non-buyers about 72% 
# of the cases.

# A term that is used to explain the proportion of persons correctly classified as software purchasers
# is called specificity, and the proportion of persons correctly classified as non-buyers is called 
# sensitivity. In this case we are more interested in the model's specificity, which in the model 
# above assumes a value of approximately 100 - 72 = 28%. We wish to increase this proportion, and do 
# that by adjusting the value of how an observation is classified as "No" or "Yes".

# A marked increase in specificity tend to lead to a decrease in sensitivity, which in turn can lead 
# to a reduction in the proportion of people who are generally classified correctly by our model.
# In our case though, it's more important with a high specificity, than a high proportion of people 
# who are generally classified correctly. 
# The trade-off between specificity and sensitivity can  be visualized by a ROC curve.
#####################################################################################################

# Create a ROC-curve
install.packages("pROC")
library(pROC)

rf.roc<-roc(Software[train],rf.Small$votes[,2])
par(pty = "s")
plot(rf.roc, main = "ROC-curve")

# Below, a customer is classified as "No" if the probability, according to the model, that the person 
# does not buy the software exceeds 95%, and as "Yes" otherwise.
# In the two previous models, the limit > 50% is used for "No" and < 50% for "Yes". 
# If a higher specificity is desired, just enter a value even closer to 1 in the argument "cutoff".
rf.Small.Spec <- randomForest(Software ~ NrOfOrder + Days + County + CPLow, 
                              data = Data, subset = train, cutoff = c(0.95, 1 - 0.95),
                              mtry = 1, importance = T, ntree = 1000)

rf.Small.Spec.Yhatt <- predict(rf.Small.Spec, newdata = test_Dataset)

# Create a "confusion matrix" and calculate the proportion of correct / incorrect classifications.
table <- table(Skattat = rf.Small.Spec.Yhatt, Sant = Software.test)
table
mean(rf.Small.Spec.Yhatt == Software.test)
mean(rf.Small.Spec.Yhatt != Software.test)

# The ROC-curve previously created is updated with values for the specificity and sensitivity. 
# The area under the ROC-curve is also reported. The greater the area under the curve, the better.
# (The area under the curve can be adjusted by including or excluding variables in our model).
plot(rf.roc, main = "ROC-curve",
     xlab = paste("Specificity = ", round(sum(table[2,2]) / sum(table[,2]),3)),
     ylab = paste("Sensitivity = ", round(sum(table[1,1]) / sum(table[,1]),3))) 
text(0.40, 0.15, paste("The area under the curve is", round(auc(rf.roc),3)))

#####################################################################################################
# In the model above, where the value of how an observation is classified as "No" or "Yes"
# adjusted to increase specificity, we reach out to approximately 85% of all buyers in the 
# test dataset. The total number of people we reach out to has increased by approximately 
# 1000 persons, but in contrast, the sales of the software have also increased about three times 
# as much compared to the model that is not adjusted for how an observation is classified.
#####################################################################################################
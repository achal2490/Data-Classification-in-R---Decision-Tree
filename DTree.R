#installing packages
install.packages("ggplot2")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("caret")
install.packages("rattle")
install.packages("corrplot")
#loading library
library(ggplot2)
library(rpart)
library(rpart.plot)
library(rattle)
library(caret)
library(corrplot)


#assigning csv file into Bank named variable
Bank<- read.csv2("Data/bank-additional-full.csv")
#to view first n lines of Bank Variable
head(Bank)
#to produce summary of the Bank data set
summary(Bank)
#checking null values in Bank data set
sum(is.na(Bank))
#to display the internal structure of Bank
str(Bank)

########################## Data Cleaning ############################

#converting continous data into approprate data type
Bank$emp.var.rate <- as.numeric(Bank$emp.var.rate)
Bank$cons.price.idx <- as.numeric(Bank$cons.price.idx)
Bank$cons.conf.idx <- as.numeric(Bank$cons.conf.idx)
Bank$nr.employed <- as.numeric(Bank$nr.employed)
Bank$euribor3m <- as.numeric(Bank$euribor3m)

########################### End ####################################

#to produce summary of the Bank data set
summary(Bank)
#the changes happend above can be seen
str(Bank)

##################### Correlation Matrix ################################
#to show how continous data are dependend on each others
#creating a data frame or all continous data in Bank dataset
#Changing y to 0, 1
Bank$y <- as.numeric(Bank$y)
ContinousData<- data.frame(Bank$age, Bank$duration,Bank$campaign,Bank$pdays
               ,Bank$previous,Bank$emp.var.rate,Bank$cons.price.idx
               ,Bank$cons.conf.idx,Bank$euribor3m, Bank$y)
Dependency <- cor(ContinousData) # get correlations
corrplot(Dependency, method = "color") 
#Re-factoring y
Bank$y <- factor(Bank$y, labels=c("no", "yes"))
#Checking the levels
levels(Bank$y)
str(Bank)
################################# END ###################################

#assigning background color and border line
fill <- "gold1"
line <- "goldenrod2"

######################## AGE DISTRIBUTION ###################################

#calling ggplot functions to create a histogram
ggplot(Bank, aes(x=age)) + 
  geom_histogram(fill = fill, colour = line, binwidth = 1) +
  ggtitle("Histogram of Age Distribution") +
  xlab("Age") +
  ylab("Count")

################################ END ######################################


############# NO. of Clients working in Different Department##############

#calling ggplot function to create a bar graph
ggplot(data=Bank, aes(x=job)) +
  geom_bar() +
  geom_bar(fill = fill, colour = line) +
  geom_text(stat='count', aes(label=..count..), vjust=-1) +
  ggtitle("Client Employment Category")+
  xlab("Employment") +
  ylab("Count")

################################ END ######################################

######################### Marital status of clients #######################

#calling ggplot function to create a bar graph
ggplot(data=Bank, aes(x=marital)) +
  geom_bar() +
  geom_bar(fill = fill, colour = line) +
  geom_text(stat='count', aes(label=..count..), vjust=-1) +
  ggtitle("Client Marital Status") +
  xlab("Marital Status") +
  ylab("Count")

################################ END ######################################


####################### Education Distribution of clients ################

#calling ggplot function to create a bar graph
ggplot(data=Bank, aes(x=education)) +
  geom_bar() +
  geom_bar(fill = fill, colour = line) +
  geom_text(stat='count', aes(label=..count..), vjust=-1) +
  ggtitle("Client Educational Background")+
  xlab("Educational Category") +
  ylab("Count")

################################ END ######################################

############################## Ways to Contact Client #####################

#calling barplot function to create a bar graph
ggplot(data=Bank, aes(x=contact)) +
  geom_bar() +
  geom_bar(fill = fill, colour = line) +
  geom_text(stat='count', aes(label=..count..), vjust=-1) +
  ggtitle("Client Contact Category")+
  xlab("Contact Category") +
  ylab("Count")

################################ END ######################################

############################# Months Count ################################
#calling ggplot function to create a bar graph
ggplot(data=Bank, aes(x=month)) +
  geom_bar() +
  geom_bar(fill = fill, colour = line) +
  geom_text(stat='count', aes(label=..count..), vjust=-1) +
  ggtitle("Month Last Contacted")+
  xlab("Months") +
  ylab("Count")

################################ END ######################################

###to see people of different education level has loans or dont have loans##
ggplot(data=Bank, aes(x=education,fill = loan)) +
  geom_bar(position=position_dodge()) +
  scale_fill_brewer(palette="Paired") + 
  ggtitle("Education Background Vs Loan") +
  theme_minimal() +
  xlab("Education") +
  ylab("Count")

################################ END ######################################

################# Impact of previous campain on term deposit ##############
ggplot(data=Bank, aes(x=poutcome,fill = y)) +
  geom_bar(position=position_dodge()) +
  scale_fill_brewer(palette="Paired") + 
  ggtitle("Education Background Vs Loan") +
  theme_minimal() +
  xlab("Campain Results") +
  ylab("Count")

################################ END ######################################

########################## Decision Tree Model ############################

#set random seed for the results to be reproducible
set.seed(3)
#dividing data into train and test part
#diving data into 70% and 30%
id <- sample(2,nrow(Bank), prob <- c(0.7,0.3) ,replace = TRUE)
#assigning 70% to training data
TrainBank <- Bank[id==1,]
#assigning 30% to test data
TestBank <- Bank[id ==2,]

#checking no. of rows in test and train dataset
nrow(TrainBank)
nrow(TestBank)
#summary for test and train data
summary(TrainBank)
summary(TestBank)
#to display the internal structure of training and test dataset
str(TrainBank)
str(TestBank)
#getting column names using colnames function
colnames(Bank)
#making our model using train data
Bank_model<- rpart(y~., data = TrainBank)
Bank_model
#plot decision tree
fancyRpartPlot(Bank_model, palettes=c("Greys", "Oranges"))

#Prediction of test data set
Bank_prediction <- predict(Bank_model,TestBank, type = "class")
Bank_prediction

#now we need to compare it with actual value
Prediction <- table(Bank_prediction, TestBank$y)
#calling confusion matrix
confusionMatrix(Prediction)
#as our model doesn't need puring as it id=s not overfitting
# so our model is 90.98% accurate.

#This function provides the optimal prunings based on the cp value.
printcp(Bank_model)
#the below mentioned list of cp values, we can select the one having the least cross-validated error and use it to prune the tree.
#The value of cp should be least, so that the cross-validated error rate is minimum.
#Plotcp() provides a graphical representation to the cross validated error summary. The cp values are plotted against the geometric mean to depict the deviation until the minimum value is reached.
plotcp(Bank_model)

#Prune the tree to create an optimal decision tree :
ptree<- prune(Bank_model,
               cp= Bank_model$cptable[which.min(Bank_model$cptable[,"xerror"]),"CP"])
fancyRpartPlot(ptree, uniform=TRUE,
              main="Pruned Tree", palettes=c("Greys", "Oranges"))
#Even after pruning there is no such change is the decision tree


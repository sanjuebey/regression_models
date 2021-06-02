#To check the directory and to the read file
getwd()
setwd("C:/sje_ds/Personal/Regression Model")
vgame <- read.csv("videogame.csv")

str(vgame)
summary(vgame)
class(vgame)

#Let's change the names of the columns
colnames(vgame)[7] <- "NA_Sales"
colnames(vgame)[8] <- "EU_Sales"
colnames(vgame)[9] <- "JP_Sales"
colnames(vgame)[10] <- "Other_Sales"
colnames(vgame)[11] <- "Global_Sales"

ngame <- vgame[,c(7:11)]

#Let's create training and test data
set.seed(100)  
trainingRowIndex <- sample(1:nrow(ngame), 0.7*nrow(ngame)) 
trainingData <- ngame[trainingRowIndex, ]  
testData  <- ngame[-trainingRowIndex, ]

#Let's build the model on the training data
lmMod <- lm(trainingData$Other_Sales~., data=trainingData) 
summary(lmMod)

#Let's build the model on the test data
modl <- predict(lmMod, testData)
modl

#Let's plot the values predicted by our model and the actual values to check the deviation
newgame <- testData[1:100,4]
newgame
plot(newgame, type="l") #Actual values
plot(modl[1:100], type='l', col="blue") #Predicted values

#Comparison of the actual and the predicted values
newdata <- cbind(newgame,modl[1:100])
newdata
#plot(newgame, modl)

#Logistic Regression#
#To check the directory and to the read the file
setwd("C:/sje_ds/Personal/Regression Model")
employee <- read.csv("Employee_Data.csv")

#Let's convert the values to type factor
table(employee$Emp_Sal)
employee$Emp_Sal <- ifelse(employee$Emp_Sal==">50K", "High", "Low")
str(employee)
employee$Emp_Sal <- as.factor(employee$Emp_Sal)
employee
summary(employee)

#Let's clean the dataset
str(employee$Emp_Stat_type)
employee$Emp_Stat_type <- as.character(employee$Emp_Stat_type)
employee$Emp_Stat_type[which(employee$Emp_Stat_type=='?')] <-'NA'
#employee$Emp_Stat_type <- as.factor(employee$Emp_Stat_type)
summary(employee)

employee$country_of_res <- as.character(employee$country_of_res)
employee$country_of_res[which(employee$country_of_res=='?')] <-'NA'
#employee$country_of_res <- as.factor(employee$country_of_res)
summary(employee)

employee1 <- employee[which(employee$Emp_Stat_type != "NA"), ]
employee1 <- employee[which(employee$country_of_res != "NA"), ]
summary(employee1)

str(employee1)
employee1$Emp_Stat_type <- as.factor(employee1$Emp_Stat_type) 
employee1$country_of_res <- as.factor(employee1$country_of_res) 

#Let's create training and test data
set.seed(100)  
trainingRowIndex <- sample(1:nrow(employee1), 0.7*nrow(employee1)) 
trainingData <- employee1[trainingRowIndex, ]  
testData  <- employee1[-trainingRowIndex, ]

#Let's build the model on the training data
model<-glm(trainingData$Emp_Sal~. ,data = trainingData,family = "binomial")
summary(model)

#Let's build the model on the test data
pred <- predict(model, testData, type='response')
pred

#Let's create a confusion matrix with cut off value as 0.4
table(pred>0.4,testData$Emp_Sal)

#Accuracy of the logistic regression model
(1151+7033)/(1151+7033+284+1126)

#####
(exp(0.07269)-1)*100 
employee2 <- employee1
employee2$country_of_res <- as.character(employee2$country_of_res)
employee2$Emp_Sal <- as.character(employee2$Emp_Sal)
employee3 <- employee2[,c(14,15)]
employee3
employee4 <- table(employee3)
str(employee3)
#employee3$perc <- employee3$

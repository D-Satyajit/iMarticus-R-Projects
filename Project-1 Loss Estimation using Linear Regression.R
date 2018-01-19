#' ---
#' title: "Project-I : Loss Case Study - CNB Bank"
#' author: "Satyajit Dhawale"
#' ---


#Import a Dataset into R
library(readr)
loss <- read_csv("F:/Lectures/iMarticus/R/Study Material/Loss.csv")
View(loss)

loss$'Observation no' <- c(1:15290) #Add observation no. variable
dim(loss) #Shows no. of observations and variables of dataset
str(loss) #To view the structure of dataset



#Converting string into numbers : M = 1 and F = 2
loss$Gender <- ifelse(loss$Gender == "M", 1 ,2)

#Converting string into numbers : Single = 1 and Married = 2
loss$Married <- ifelse(loss$Married == "Single", 1 ,2)


table(loss$Gender) #To count number of males and females
table(loss$Married) #To count number of married and single people

#To convert interger to factor

summary(loss) #Gives min median max 1st Qu and 3rd Qu of each variable

head(loss) #Gives first 6 observation of dataset
tail(loss) #Gives last 6 observation of dataset


chooseCRANmirror(graphics=FALSE, ind=1) #This command needed for smooth Knit Creation
install.packages('psych')
library(psych)

describe(loss) #descriptive statistics

#Histograms to view data
hist(loss$Age, col = "gold", main = "Histogram for Age", xlab = "Age")
hist(loss$`Years of Experience`, col = "cornflowerblue", main = "Histogram for Years of Exp.", xlab = "Years of Exp.")
hist(loss$`Number of Vehicles`, col = "darkorchid1", main = "Histogram for Number of Vehicles", xlab = "Number of Vehicles")
hist(loss$`Losses in Thousands`, col = "red3", main = "Histogram for Losses in Thousands", xlab = "Losses in Thousands")


#Plot Boxplot for outliers
boxplot(loss$`Losses in Thousands`, col = "red3", main = "Boxplot for Losses in Thousands",xlab = "Losses in Thousands", horizontal = T)


#Plot Stem and Leaf Diagram
stem(loss$`Losses in Thousands`)


cor(loss) #To view all the coefficient of corelation of all variable with eachother


#Building a Multiple Linear Regression Model
#Another Way of doing : lm(mtcars$mpg ~ mtcars$disp+mtcars$hp+mtcars$dart+mtcars$wt)
mod1 <- lm(`Losses in Thousands` ~ Age + `Years of Experience`+ `Number of Vehicles`+Gender+ Married, data = loss)
mod1
summary(mod1) #Rsq = 0.302 | Best = Age, Married, Gender

mod2 <- lm(`Losses in Thousands` ~ Age + Gender + Married, data = loss)
mod2
summary(mod2) #Rsq = 0.3017 | Best = Age, Married, Gender


mod3 <- lm(`Losses in Thousands` ~ `Years of Experience`+ `Number of Vehicles`, data = loss)
mod3
summary(mod3) #Rsq = 0.1955 | Best = Years of Experience

mod4 <- lm(`Losses in Thousands` ~ `Years of Experience`, data = loss)
mod4
summary(mod4) #Rsq = 0.1955 


mod5 <- lm(`Losses in Thousands` ~ `Number of Vehicles`, data = loss)
mod5
summary(mod5) #Rsq = 0.0001335 

mod6 <- lm(`Losses in Thousands` ~ Age, data = loss)
mod6
summary(mod6) #Rsq = 0.1962 


mod7 <- lm(`Losses in Thousands` ~ Married, data = loss)
mod7
summary(mod7) #Rsq = 0.0704

mod8 <- lm(`Losses in Thousands` ~ Gender, data = loss)
mod8
summary(mod8) #Rsq = 0.03398


mod9 <- lm(`Losses in Thousands` ~ Age + `Years of Experience`, data = loss)
mod9
summary(mod9) #Rsq = 0.1962


mod10 <- lm(`Losses in Thousands` ~ `Years of Experience`+ Gender + Married, data = loss)
mod10
summary(mod10) # Rsq = 0.3015 | Best = All



#=================================================================================

#Age + Gender + Married
#`Years of Experience`+ Gender + Married

#Predicted Values (y^) [Values on Best Fit Line] - Age + Gender + Married 
Pred <- predict(lm(`Losses in Thousands` ~ Age + Gender + Married, data = loss))
loss$'yHat Predicted with Age+Gender+Married' <- Pred
#Error Values [Residuals] - For all variables 
error <- residuals(lm(`Losses in Thousands` ~ Age + `Years of Experience`, data = loss))
summary(error)
loss$`ResidualError with Age+Gender+Married` <- error

#Predicted Values (y^) [Values on Best Fit Line] - For `Years of Experience`, Gender and Married
Pred <- predict(lm(`Losses in Thousands` ~ `Years of Experience`+ Gender + Married, data = loss))
loss$'yHat Predicted with YoE+Gender+Married' <- Pred
#Error Values [Residuals] - For all variables 
error <- residuals(lm(`Losses in Thousands` ~ `Years of Experience`+ Gender + Married, data = loss))
summary(error)
loss$`ResidualError with YoE+Gender+Married` <- error



 
#=============================================================
#Assumptions with Age + Gender + Married Variables

#Assumption-I : Normality of Error
hist(loss$`ResidualError with Age+Gender+Married`, col = "red4", main = "Histogram for Normality of Error with Age+Gender+Married", xlab = "Residual Error for Age+Gender+Married")
boxplot(loss$`ResidualError with Age+Gender+Married`, col = "red4", horizontal = T, main = "Boxplot for Normality of Error with Age+Gender+Married", xlab = "Residual Error for Age+Gender+Married")

#Assumption-II : Linearity
plot(loss$Age , loss$`ResidualError with Age+Gender+Married`, main = "Linearity Graph with Age", col = "violetred", xlab = "Age", ylab = "Residual Error")
plot(loss$Gender, loss$`ResidualError with Age+Gender+Married`, main = "Linearity Graph with Gender", col = "violetred", xlab = "Years of Experience", ylab = "Residual Error")
plot(loss$Married, loss$`ResidualError with Age+Gender+Married`, main = "Linearity Graph with Married", col = "violetred", xlab = "Years of Experience", ylab = "Residual Error")


#Assumption-III : Independence of Error
plot(loss$`Observation no`, loss$`ResidualError with Age+Gender+Married`, main = "Independence of Error with Age+Gender+Married", col="lightblue4", xlab = "Observation Numbers", ylab = "Residual Error")

#Assumption-IV : Constant Error Variance
plot(loss$'yHat Predicted with Age+Gender+Married', loss$`ResidualError with Age+Gender+Married`, main = "Constant Error with Age+Gender+Married", col="lightblue3", xlab = "y^ Predicted with Age+Gender+Married", ylab = "Residual Error")




#=============================================================
#Assumptions with YoE + Gender + Married Variables

#Assumption-I : Normality of Error
hist(loss$`ResidualError with YoE+Gender+Married`, col = "red", main = "Histogram for Normality of Error with YoE+Gender+Married", xlab = "Residual Error for YoE+Gender+Married")
boxplot(loss$`ResidualError with YoE+Gender+Married`, col = "red", horizontal = T, main = "Boxplot for Normality of Error with YoE+Gender+Married", xlab = "Residual Error for YoE+Gender+Married")

#Assumption-II : Linearity
plot(loss$`Years of Experience` , loss$`ResidualError with YoE+Gender+Married`, main = "Linearity Graph with Years of Experience", col = "darkorange", xlab = "Years of Experience", ylab = "Residual Error")
plot(loss$Gender, loss$`ResidualError with YoE+Gender+Married`, main = "Linearity Graph with Gender", col = "darkorange", xlab = "Gender", ylab = "Residual Error")
plot(loss$Married, loss$`ResidualError with YoE+Gender+Married`, main = "Linearity Graph with Married", col = "darkorange", xlab = "Married", ylab = "Residual Error")


#Assumption-III : Independence of Error
plot(loss$`Observation no`, loss$`ResidualError with YoE+Gender+Married`, main = "Independence of Error with YoE+Gender+Married", col="chocolate4", xlab = "Observation Numbers", ylab = "Residual Error")

#Assumption-IV : Constant Error Variance
plot(loss$'yHat Predicted with YoE+Gender+Married' , loss$`ResidualError with YoE+Gender+Married`, main = "Constant Error with YoE+Gender+Married", col="chocolate4", xlab = "y^ Predicted with YoE+Gender+Married", ylab = "Residual Error")


#To save your updated dataset
write.csv(loss, "F:/Lectures/iMarticus/R/Study Material/My Scripts/loss.csv")


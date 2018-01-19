#' ---
#' title: "Project-II : Default Modelling using Logistic Regression in R"
#' author: "Satyajit Dhawale"
#' ---

#

chooseCRANmirror(graphics=FALSE, ind=1)
install.packages('readr')
install.packages('psych')

library(readr)
TrainData <- read_csv("F:/Lectures/Data Science/iMarticus/R/Study Material/My Scripts/Project-2/R_Module_Day_7.2_Credit_Risk_Train_data (1).csv")
TestData <- read_csv("F:/Lectures/Data Science/iMarticus/R/Study Material/My Scripts/Project-2/R_Module_Day_8.1_Credit_Risk_Test_data (1).csv")
ValiData <- read_csv("F:/Lectures/Data Science/iMarticus/R/Study Material/My Scripts/Project-2/R_Module_Day_8.2_Credit_Risk_Validate_data (1).csv")
View(TrainData)
View(TestData)
View(ValiData)

# Key

# Gender		  	  : Male=1 | Female=0
# Married 	    	: Yes=1	| No=0
# Dependents      : 0,1,2,3
# Education	  	  : Graduate=1 | Not Graduate=0
# Self_Employed 	: Yes=1 | No=0
# Credit_History 	: 0 and 1
# Property_Area 	: Urban=1 | Rural=2 | SemiUrban=3
# Loan_Status		  : Yes=1	| No=0

############################ Converting Variables into 0's and 1's ############################

# For Trainnig Dataset

table(TrainData$Gender)
TrainData$Gender <- ifelse(TrainData$Gender == "Male", 1,0)
table(TrainData$Married)
TrainData$Married <- ifelse(TrainData$Married == "Yes", 1,0)
table(TrainData$Dependents)
TrainData$Dependents[TrainData$Dependents == "3+"] <- "3"
TrainData$Dependents <- as.integer(TrainData$Dependents)
table(TrainData$Education)
TrainData$Education <- ifelse(TrainData$Education == "Graduate", 1,0)
table(TrainData$Self_Employed)
TrainData$Self_Employed <- ifelse(TrainData$Self_Employed == "Yes", 1,0)
table(TrainData$Loan_Amount_Term)
table(TrainData$LoanAmount)
table(TrainData$Credit_History)
table(TrainData$Property_Area)
TrainData$Property_Area <- ifelse(TrainData$Property_Area == "Urban",1,ifelse(TrainData$Property_Area == "Rural",2,3))
table(TrainData$Loan_Status)
TrainData$Loan_Status <- ifelse(TrainData$Loan_Status == "Y", 1,0)

# For Validate Dataset
table(ValiData$Gender)
ValiData$Gender <- ifelse(ValiData$Gender == "Male", 1,0)
table(ValiData$Married)
ValiData$Married <- ifelse(ValiData$Married == "Yes", 1,0)
table(ValiData$Dependents)
ValiData$Dependents[ValiData$Dependents == "3+"] <- "3"
ValiData$Dependents <- as.integer(ValiData$Dependents)
table(ValiData$Education)
ValiData$Education <- ifelse(ValiData$Education == "Graduate", 1,0)
table(ValiData$Self_Employed)
ValiData$Self_Employed <- ifelse(ValiData$Self_Employed == "Yes", 1,0)
table(ValiData$Loan_Amount_Term)
table(ValiData$LoanAmount)
table(ValiData$Credit_History)
table(ValiData$Property_Area)
ValiData$Property_Area <- ifelse(ValiData$Property_Area == "Urban",1,ifelse(ValiData$Property_Area == "Rural",2,3))
table(ValiData$outcome)
ValiData$outcome <- ifelse(ValiData$outcome == "Y", 1,0)


# For Test Dataset
table(TestData$Gender)
TestData$Gender <- ifelse(TestData$Gender == "Male", 1,0)
table(TestData$Married)
TestData$Married <- ifelse(TestData$Married == "Yes", 1,0)
table(TestData$Dependents)
TestData$Dependents[TestData$Dependents == "3+"] <- "3"
TestData$Dependents <- as.integer(TestData$Dependents)
table(TestData$Education)
TestData$Education <- ifelse(TestData$Education == "Graduate", 1,0)
table(TestData$Self_Employed)
TestData$Self_Employed <- ifelse(TestData$Self_Employed == "Yes", 1,0)
table(TestData$Loan_Amount_Term)
table(TestData$LoanAmount)
table(TestData$Credit_History)
table(TestData$Property_Area)
TestData$Property_Area <- ifelse(TestData$Property_Area == "Urban",1,ifelse(TestData$Property_Area == "Rural",2,3))












############################ NA Values Treatment ############################

sum(is.na(TrainData)) #149
sum(is.na(ValiData)) #84
sum(is.na(TestData)) #84

# Trainnig Dataset
sum(is.na(TrainData$Gender)) #13
sum(is.na(TrainData$Married)) #3
sum(is.na(TrainData$Dependents)) #15
sum(is.na(TrainData$Education)) #0
sum(is.na(TrainData$Self_Employed)) #32
sum(is.na(TrainData$ApplicantIncome)) #0
sum(is.na(TrainData$CoapplicantIncome)) #0
sum(is.na(TrainData$Loan_Amount_Term)) #14
sum(is.na(TrainData$LoanAmount)) #22
sum(is.na(TrainData$Credit_History)) #50
sum(is.na(TrainData$Property_Area)) #0
sum(is.na(TrainData$Loan_Status)) #0

TrainData$Gender[is.na(TrainData$Gender)] <- ifelse(mean(TrainData$Gender, na.rm=TRUE) > 0.5, 1,0)
TrainData$Married[is.na(TrainData$Married)] <- ifelse(mean(TrainData$Married, na.rm=TRUE) > 0.5, 1,0)
TrainData$Dependents[is.na(TrainData$Dependents)] <- ifelse(mean(TrainData$Dependents, na.rm=TRUE) > 0.5, 1,0)
TrainData$Self_Employed[is.na(TrainData$Self_Employed)] <- ifelse(mean(TrainData$Self_Employed, na.rm=TRUE) > 0.5, 1,0)
TrainData$Loan_Amount_Term[is.na(TrainData$Loan_Amount_Term)] <- ifelse(mean(TrainData$Loan_Amount_Term, na.rm=TRUE) > 315, 360, 300)
TrainData$LoanAmount[is.na(TrainData$LoanAmount)] <- round(mean(TrainData$LoanAmount, na.rm=TRUE), digits = 0)
TrainData$Credit_History[is.na(TrainData$Credit_History)] <- ifelse(mean(TrainData$Credit_History, na.rm=TRUE) > 0.5, 1, 0)



# Validate Dataset
sum(is.na(ValiData$Gender)) #11
sum(is.na(ValiData$Married)) #0
sum(is.na(ValiData$Dependents)) #10
sum(is.na(ValiData$Education)) #0
sum(is.na(ValiData$Self_Employed)) #23
sum(is.na(ValiData$ApplicantIncome)) #0
sum(is.na(ValiData$CoapplicantIncome)) #0
sum(is.na(ValiData$Loan_Amount_Term)) #6
sum(is.na(ValiData$LoanAmount)) #5
sum(is.na(ValiData$Credit_History)) #29
sum(is.na(ValiData$Property_Area)) #0

ValiData$Gender[is.na(ValiData$Gender)] <- ifelse(mean(ValiData$Gender, na.rm=TRUE) > 0.5, 1,0)
ValiData$Dependents[is.na(ValiData$Dependents)] <- ifelse(mean(ValiData$Dependents, na.rm=TRUE) > 0.5, 1,0)
ValiData$Self_Employed[is.na(ValiData$Self_Employed)] <- ifelse(mean(ValiData$Self_Employed, na.rm=TRUE) > 0.5, 1,0)
ValiData$Loan_Amount_Term[is.na(ValiData$Loan_Amount_Term)] <- ifelse(mean(ValiData$Loan_Amount_Term, na.rm=TRUE) > 315, 360, 300)
ValiData$LoanAmount[is.na(ValiData$LoanAmount)] <- round(mean(ValiData$LoanAmount, na.rm=TRUE), digits = 0)
ValiData$Credit_History[is.na(ValiData$Credit_History)] <- ifelse(mean(ValiData$Credit_History, na.rm=TRUE) > 0.5, 1, 0)



# Test Dataset
sum(is.na(TestData$Gender)) #11
sum(is.na(TestData$Married)) #0
sum(is.na(TestData$Dependents)) #10
sum(is.na(TestData$Education)) #0
sum(is.na(TestData$Self_Employed)) #23
sum(is.na(TestData$ApplicantIncome)) #0
sum(is.na(TestData$CoapplicantIncome)) #0
sum(is.na(TestData$Loan_Amount_Term)) #6
sum(is.na(TestData$LoanAmount)) #5
sum(is.na(TestData$Credit_History)) #29
sum(is.na(TestData$Property_Area)) #0

TestData$Gender[is.na(TestData$Gender)] <- ifelse(mean(TestData$Gender, na.rm=TRUE) > 0.5, 1,0)
TestData$Dependents[is.na(TestData$Dependents)] <- ifelse(mean(TestData$Dependents, na.rm=TRUE) > 0.5, 1,0)
TestData$Self_Employed[is.na(TestData$Self_Employed)] <- ifelse(mean(TestData$Self_Employed, na.rm=TRUE) > 0.5, 1,0)
TestData$Loan_Amount_Term[is.na(TestData$Loan_Amount_Term)] <- ifelse(mean(TestData$Loan_Amount_Term, na.rm=TRUE) > 315, 360, 300)
TestData$LoanAmount[is.na(TestData$LoanAmount)] <- round(mean(TestData$LoanAmount, na.rm=TRUE), digits = 0)
TestData$Credit_History[is.na(TestData$Credit_History)] <- ifelse(mean(TestData$Credit_History, na.rm=TRUE) > 0.5, 1, 0)












############################ Descriptive Statistics ############################

library(psych)

dim(TrainData) 
str(TrainData)
describe(TrainData) 
summary(TrainData)

dim(TestData) 
str(TestData) 
describe(TestData)
summary(TestData)

dim(ValiData) 
str(ValiData) 
describe(ValiData)
summary(ValiData)
warnings(describe(ValiData))




############################ Histograms and Boxplots ############################

boxplot(TrainData$ApplicantIncome)
boxplot(TrainData$CoapplicantIncome)
boxplot(TrainData$LoanAmount)

hist(TrainData$ApplicantIncome, col = "gold", main = "Histogram for Applicant Income", xlab = "Applicant Income")
hist(TrainData$CoapplicantIncome, col = "cornflowerblue", main = "Histogram for Co-Applicant Income", xlab = "Co-Applicant Income")
hist(TrainData$LoanAmount, col = "darkorchid1", main = "Histogram for Loan Ammount", xlab = "Loan Ammount (in Thousands)")




############################ Logistic Regression ############################


cor(TrainData[,2:13])
pairs(TrainData[,2:13])

# Model ran on Train Dataset
model<- glm(Loan_Status ~ Gender + Married + Dependents + Education + Self_Employed + ApplicantIncome + CoapplicantIncome + LoanAmount + Loan_Amount_Term + Credit_History + Property_Area, data = TrainData, family = binomial)
model
summary(model) # AIC: 590.6 | Not a good model

model2<- glm(Loan_Status ~ Married + Education + LoanAmount + Credit_History + Property_Area, data = TrainData, family = binomial)
model2
summary(model2) #AIC: 582.3 |  Not a good model

model3<- glm(Loan_Status ~ Married + Credit_History + Property_Area, data = TrainData, family = binomial)
model3
summary(model3) #AIC: 583.2 |  "Accuracy Value =94.55% | "Cutoff Value =" "0.66" | AUC =89.7%  | Good model


install.packages('ROCR')
install.packages('e1071')
install.packages('caret')
library(ROCR)
library(caret)
library(e1071)
# Model Predicted on Test Data and its confusion matrix
Predicted_Prob <- predict(model3,TestData, type = "response") # Predicted Probabalities
TestData$Predicted_Probabalities <- Predicted_Prob # Added New Column
TestData$ROUNDED_Predicted_LoanStatus <- round(Predicted_Prob >= 0.5, digits = 0) # Cutoff Threshold




############################ Model Validation ############################

Predicted_Prob_VD <- predict(model3, ValiData, type = "response") # Predicted Probabalities
ValiData$Predicted_Probabalities <- Predicted_Prob_VD  # Added New Column
ValiData$ROUNDED_Predicted_LoanStatus <- round(Predicted_Prob_VD  >= 0.5, digits = 0) # Cutoff Threshold


tab <- confusionMatrix(ValiData$ROUNDED_Predicted_LoanStatus, ValiData$outcome)
tab
table(ValiData$ROUNDED_Predicted_LoanStatus) # Sum of "Predicted" 0's and 1's
table(ValiData$outcome) # Sum of "Actuals" 0's and 1's


# Checking which prediction is Correct or Not Correct
ValiData$Result <- ifelse(ValiData$outcome == ValiData$ROUNDED_Predicted_LoanStatus, "Correct", "Not Correct")

table(ValiData$Result) # 20 Cases are identified incorrectly or missclassified 347/367 = 94.55% Accurate Prediction




############################ Model Performance Evaluation ############################



# Model Accuracy or The correct classification of model in %
tab # Confusion Matrix
macc <- (58+289)/(58+1+19+289) 
macc * 100 # 94.55% # Model Accuracy

# Miss classification of model in %
(1-macc)*100

# Benchmark of Model 
table(TrainData$Loan_Status) # Taking the higest number
422/614 # 614 is the number of variable in Train Data Set
# Benchmark = 68.72% which is less than model accuracy %, now we can use this model.


hist(Predicted_Prob_VD)
# From this histogram we can see these probablities varies between 0 to 1.
# And most of the points are above 0.6
# So, if we use cutoff as 0.6 then the classification might change


pred <- prediction(Predicted_Prob_VD, ValiData$outcome)
eval <- performance(pred, "acc")
plot(eval) # it shows the accuracy (y-axis) for different cutoff values (x-axis)
# you can see that when cutoff is low the accuracy is also low.


# Identifying the best cutoff and accuracy values
eval # if you see eval you will se : Slot "x.values": [[1]]
which.max(slot(eval, "y.values")[[1]]) # This will give you index, put that values in next bracket
acc <- slot(eval, "y.values")[[1]] [7]
cut <- slot(eval, "x.values")[[1]] [7]

print(c("Accuracy Value =", acc, "Cutoff Value =", cut))
# So, when we compare the default cutoff value of 0.5, the current cut off value will give us a better accuracy.


# The ROC curve (Receiver Operator Charateristics Curve)
roc <- performance(pred, "tpr", "fpr")
plot(roc, colorize = T, main="ROC Curve", xlab="FPR / 1-Specificity", ylab="TPR / Sensitivity") # Right side shows the cutoff
mtext("Cutoffs", side = 4, line= -1)
abline(a=0, b=1) 



# Area under Curve
# If the AUC is higher than benchmark then our model performance is better
auc <- performance(pred, "auc")
auc # you will see here different slots. Slot "y.values": gives you % of auc
AUC <- unlist(slot(auc, "y.values"))
legend(0.6, 0.3, title="Area Under Curve (%)", round(AUC*100, digits = 2), cex=0.6)


############################ Converting Variables to Original Form ############################



# For Validate Dataset
ValiData$Gender <- ifelse(ValiData$Gender == "1", "Male","Female")
ValiData$Married <- ifelse(ValiData$Married == "1", "Yes","No")
ValiData$Education <- ifelse(ValiData$Education == "1", "Graduate","Not Graduate")
ValiData$Self_Employed <- ifelse(ValiData$Self_Employed == "1", "Yes","No")
ValiData$Property_Area <- ifelse(ValiData$Property_Area == "1","Urban",ifelse(ValiData$Property_Area == "2","Rural","Semi Urban"))
ValiData$outcome <- ifelse(ValiData$outcome == "1", "Y","N")
ValiData$ROUNDED_Predicted_LoanStatus <- ifelse(ValiData$ROUNDED_Predicted_LoanStatus == "1", "Y","N")



# For Test Dataset
TestData$Gender <- ifelse(TestData$Gender == "1", "Male","Female")
TestData$Married <- ifelse(TestData$Married == "1", "Yes","No")
TestData$Education <- ifelse(TestData$Education == "1", "Graduate","Not Graduate")
TestData$Self_Employed <- ifelse(TestData$Self_Employed == "1", "Yes","No")
TestData$Property_Area <- ifelse(TestData$Property_Area == "1","Urban",ifelse(TestData$Property_Area == "2","Rural","Semi Urban"))
TestData$outcome <- ifelse(TestData$outcome == "1", "Y","N")
TestData$ROUNDED_Predicted_LoanStatus <- ifelse(TestData$ROUNDED_Predicted_LoanStatus == "1", "Y","N")


write.csv(TestData, "F:/Lectures/Data Science/iMarticus/R/Study Material/My Scripts/Project-2/TestData_Output.csv")
write.csv(ValiData, "F:/Lectures/Data Science/iMarticus/R/Study Material/My Scripts/Project-2/ValidateData_Output.csv")
























#' ---
#' title: "Project-III : Default Modelling using Support Vector Machines in R"
#' author: "Satyajit Dhawale"
#' ---



chooseCRANmirror(graphics=FALSE, ind=1)
install.packages('readr')
install.packages('psych')
install.packages("e1071")
install.packages('caret')
library(e1071)
library(caTools)

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
ValiData$outcome <- as.factor(ValiData$outcome)

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













############################ Converting Variables into Factors ############################

# For Trainnig Dataset
TrainData$Gender <- as.factor(TrainData$Gender)
TrainData$Married <- as.factor(TrainData$Married)
TrainData$Dependents <- as.factor(TrainData$Dependents)
TrainData$Education <- as.factor(TrainData$Education)
TrainData$Self_Employed <- as.factor(TrainData$Self_Employed)
TrainData$Credit_History <- as.factor(TrainData$Credit_History)
TrainData$Property_Area <- as.factor(TrainData$Property_Area)
TrainData$Loan_Status <- as.factor(TrainData$Loan_Status)

# For Validate Dataset
ValiData$Gender <- as.factor(ValiData$Gender)
ValiData$Married <- as.factor(ValiData$Married)
ValiData$Dependents <- as.factor(ValiData$Dependents)
ValiData$Education <- as.factor(ValiData$Education)
ValiData$Self_Employed <- as.factor(ValiData$Self_Employed)
ValiData$Credit_History <- as.factor(ValiData$Credit_History)
ValiData$Property_Area <- as.factor(ValiData$Property_Area)
ValiData$outcome <- as.factor(ValiData$outcome)

# For Test Dataset
TestData$Gender <- as.factor(TestData$Gender)
TestData$Married <- as.factor(TestData$Married)
TestData$Dependents <- as.factor(TestData$Dependents)
TestData$Education <- as.factor(TestData$Education)
TestData$Self_Employed <- as.factor(TestData$Self_Employed)
TestData$Credit_History <- as.factor(TestData$Credit_History)
TestData$Property_Area <- as.factor(TestData$Property_Area)


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





############################ Scatter Plot, Histograms and Boxplots ############################

plot(TrainData$Loan_Status)

boxplot(TrainData$ApplicantIncome)
boxplot(TrainData$CoapplicantIncome)
boxplot(TrainData$LoanAmount)

hist(TrainData$ApplicantIncome, col = "gold", main = "Histogram for Applicant Income", xlab = "Applicant Income")
hist(TrainData$CoapplicantIncome, col = "cornflowerblue", main = "Histogram for Co-Applicant Income", xlab = "Co-Applicant Income")
hist(TrainData$LoanAmount, col = "darkorchid1", main = "Histogram for Loan Ammount", xlab = "Loan Ammount (in Thousands)")






############################ SVM Modeling Before Tuning ############################

# Randomly 367 Rows selected from Training Dataset as the SVM model requires same number of observations for Traning as well as Testing.
set.seed(123)
TrainData_New <- TrainData[ sample(nrow(TrainData),367),  ]
View(TrainData_New)

cor(TrainData_New[,2:13])
pairs(TrainData_New[,2:13])

# Model Train on Training Dataset (New), Slecting Randome Parameters
# Options associated with SVM training; like changing kernel, gamma and C value.
# Kernel = radial / linear / polynomial / sigmoid
fitmodel <- svm(Loan_Status ~ Gender + Married + Dependents + Education + Self_Employed + ApplicantIncome + CoapplicantIncome + LoanAmount + Loan_Amount_Term + Credit_History + Property_Area, 
                data = TrainData_New)

summary(fitmodel)


# Model Predicted on Validate Data
predVD <- predict(fitmodel, ValiData)

# Confusion Matrix
library(caret)
tab <- confusionMatrix(predVD, ValiData$outcome)
tab


# Model Accuracy or The correct classification of model in %
modacc <- (58+286)/(58+4+19+286) # Calculated from Confusion Martix
modacc * 100

# Miss classification of model in %
(1-modacc)*100


# Kernel = radial | No. of SV = 213 | Accuracy = 93.46% | Missclassification = 6.53%
# Kernel = linear | No. of SV = 178 | Accuracy = 94.55% | Missclassification = 5.44%
# Kernel = polynomial | No. of SV = 253 | Accuracy = 79.29% | Missclassification = 20.70%
# Kernel = sigmoid | No. of SV = 192 | Accuracy = 93.46% | Missclassification = 6.53%

plot(TrainData$ApplicantIncome, TrainData$LoanAmount, col=TrainData$Loan_Status)
plot(fitmodel, data = TrainData_New, ApplicantIncome ~ LoanAmount)



############################ Model Tunining ############################
# This will help us to selcet the best model also called hyper paramerter optimization
tunemodel <- tune.svm( Loan_Status ~ Gender + Married + Dependents + Education + Self_Employed + ApplicantIncome + CoapplicantIncome + LoanAmount + Loan_Amount_Term + Credit_History + Property_Area, 
     data = TrainData_New,
     kernel="linear", # All the four kernels were tested here
     cost = 2^(2:7),
     epsilon = seq(0,1,0.1))

plot(tunemodel) # In this plot darker shade means better results are in this region. Having Lower miscalssification error
summary(tunemodel)
tunemodel$best.parameters
BestModel <- tunemodel$best.model # Shows the Best model parameters, that will give us lowest missclassification error
BestModel


# Kernel = radial | No. of SV = 197 | Cost = 4 | Gamma = 0.066
# Kernel = linear | No. of SV = 191 | Cost = 4 | Gamma = 0.066
# Kernel = polynomial | No. of SV = 220 | Cost = 16 | Gamma = 0.066
# Kernel = sigmoid | No. of SV = 155 | Cost = 4 | Gamma = 0.066


############################ Best Model ############################

fitmodel <- svm(Loan_Status ~ Gender + Married + Dependents + Education + Self_Employed + ApplicantIncome + CoapplicantIncome + LoanAmount + Loan_Amount_Term + Credit_History + Property_Area, 
                data = TrainData_New,
                kernel = "linear",
                cost = 4,
                gamma = 0.066, Type = cclassification )

summary(fitmodel) 


# Model Predicted on Validate Data
predVD <- predict(fitmodel, ValiData)
ValiData$LoanStatus_Outcome <- predVD

# Confusion Matrix
tab <- confusionMatrix(predVD, ValiData$outcome)
tab


# Model Accuracy or The correct classification of model in %
modacc <- (57+272)/(57+18+20+272) # Calculated from Confusion Martix
modacc * 100

# Miss classification of model in %
(1-modacc)*100



# Kernel = radial | No. of SV = 197 | Accuracy = 93.73% | Missclassification = 6.26%
# Kernel = linear | No. of SV = 191 | Accuracy = 94.55% | Missclassification = 5.44%
# Kernel = polynomial | No. of SV = 223 | Accuracy = 92.37% | Missclassification = 7.62%
# Kernel = sigmoid | No. of SV = 155 | Accuracy = 89.64% | Missclassification = 10.35%


# BEFORE
# Kernel = radial | No. of SV = 213 | Accuracy = 93.46% | Missclassification = 6.53%

# AFTER
# Kernel = radial | No. of SV = 197 | Accuracy = 93.73% | Missclassification = 6.26%


plot(ValiData$ApplicantIncome, ValiData$LoanAmount, col=ValiData$outcome)
plot(fitmodel, data = ValiData, ApplicantIncome ~ LoanAmount)



############################ Model Predicted on TestData ############################

# Model Predicted on Test Data
pred <- predict(fitmodel, TestData)
TestData$LoanStatus_Outcome <- pred


############################ Converting Variables to Original Form ############################



# For Validate Dataset
ValiData$Gender <- ifelse(ValiData$Gender == "1", "Male","Female")
ValiData$Married <- ifelse(ValiData$Married == "1", "Yes","No")
ValiData$Education <- ifelse(ValiData$Education == "1", "Graduate","Not Graduate")
ValiData$Self_Employed <- ifelse(ValiData$Self_Employed == "1", "Yes","No")
ValiData$Property_Area <- ifelse(ValiData$Property_Area == "1","Urban",ifelse(ValiData$Property_Area == "2","Rural","Semi Urban"))
ValiData$outcome <- ifelse(ValiData$outcome == "1", "Y","N")
ValiData$LoanStatus_Outcome <- ifelse(ValiData$LoanStatus_Outcome == "1", "Y","N")



# For Test Dataset
TestData$Gender <- ifelse(TestData$Gender == "1", "Male","Female")
TestData$Married <- ifelse(TestData$Married == "1", "Yes","No")
TestData$Education <- ifelse(TestData$Education == "1", "Graduate","Not Graduate")
TestData$Self_Employed <- ifelse(TestData$Self_Employed == "1", "Yes","No")
TestData$Property_Area <- ifelse(TestData$Property_Area == "1","Urban",ifelse(TestData$Property_Area == "2","Rural","Semi Urban"))
TestData$LoanStatus_Outcome <- ifelse(TestData$LoanStatus_Outcome == "1", "Y","N")


write.csv(TestData, "F:/Lectures/Data Science/iMarticus/R/Study Material/My Scripts/Project-3/TestData_Output.csv")
write.csv(ValiData, "F:/Lectures/Data Science/iMarticus/R/Study Material/My Scripts/Project-3/ValidateData_Output.csv")




























#Reading csv file onto R
ConcreteParent = read.csv("Concrete_Data.csv")
str(ConcreteParent)

#Editing Names for ease of access
names(ConcreteParent)   
names(ConcreteParent) = c("Cement","BF_Slag","Fly_Ash","Water","SuperPlasticizers","CA","FA","Age","Compressive_Strength","w_c")
names(ConcreteParent)

#Removing NA values from Data Set

ConcreteParent1 = ConcreteParent
ConcreteParent1 = na.omit(ConcreteParent1)
ConcreteParent = ConcreteParent1

#Concrete Without Additives Subset
ConcreteONLY = subset(ConcreteParent,BF_Slag==0 & Fly_Ash==0 & SuperPlasticizers == 0)
summary(ConcreteONLY)

#Concrete with Blast Furnace Slag, Fly Ash, SuperPlasticizer Subset
HPC = subset(ConcreteParent, BF_Slag>0&Fly_Ash>0&SuperPlasticizers>0)
summary(HPC)

#Subsets of Single additives

ConcreteSLAG = subset(ConcreteParent,BF_Slag>0 & Fly_Ash==0 & SuperPlasticizers == 0)
summary(ConcreteSLAG)
ConcreteASH = subset(ConcreteParent,BF_Slag==0 & Fly_Ash>0 & SuperPlasticizers == 0)
summary(ConcreteASH)
ConcreteSP=subset(ConcreteParent,BF_Slag==0 & Fly_Ash==0 & SuperPlasticizers>0)
summary(ConcreteSP)

#Subsets of Double additives

#Concrete MIX with Slag and Fly Ash without SuperPlasticizer is not present in Parent Data
ConcreteASH_SP = subset(ConcreteParent,BF_Slag==0 & Fly_Ash>0 & SuperPlasticizers > 0)
summary(ConcreteASH_SP)
ConcreteSLAG_SP = subset(ConcreteParent,BF_Slag>0 & Fly_Ash==0 & SuperPlasticizers > 0)
summary(ConcreteSLAG_SP)


#Co - realation between all Variables

CorelationVariables = cor(ConcreteParent)
CorelationVariables
write.csv(CorelationVariables,"Corealation BTW Variables.csv")

#Calling Required Library

library(caTools)
library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)
library(e1071)

# Spliting Training and Test Data sets


#Spliting ConcreteONLY Data set into Training and Testing sets
spl_only = sample.split(ConcreteONLY, SplitRatio = 0.7)
Train_ConcreteONLY = subset(ConcreteONLY, spl_only == TRUE) 
Test_ConcreteONLY = subset(ConcreteONLY, spl_only == FALSE)
str(Train_ConcreteONLY)
str(Test_ConcreteONLY)


#Spliting HPC Data set into Training and Testing sets
spl_HPC = sample.split(HPC, SplitRatio = 0.7)
Train_HPC = subset(HPC, spl_HPC == TRUE) 
Test_HPC = subset(HPC, spl_HPC == FALSE)
str(Train_HPC)
str(Test_HPC)


#Spliting ConcreteSLAG Data set into Training and Testing sets
spl_SLAG = sample.split(ConcreteONLY, SplitRatio = 0.7)
Train_ConcreteSLAG = subset(ConcreteSLAG, spl_SLAG == TRUE) 
Test_ConcreteSLAG = subset(ConcreteSLAG, spl_SLAG == FALSE)
str(Train_ConcreteSLAG)
str(Test_ConcreteSLAG)


#Spliting ConcreteASH Data set into Training and Testing sets
spl_ASH = sample.split(ConcreteONLY, SplitRatio = 0.7)
Train_ConcreteASH = subset(ConcreteASH, spl_ASH == TRUE) 
Test_ConcreteASH = subset(ConcreteASH, spl_ASH == FALSE)
str(Train_ConcreteASH)
str(Test_ConcreteASH)


#Spliting ConcreteSP Data set into Training and Testing sets
spl_SP = sample.split(ConcreteSP, SplitRatio = 0.7)
Train_ConcreteSP = subset(ConcreteSP, spl_SP == TRUE) 
Test_ConcreteSP = subset(ConcreteSP, spl_SP == FALSE)
str(Train_ConcreteSP)
str(Test_ConcreteSP)


#Spliting ConcreteASH_SP Data set into Training and Testing sets
spl_ASH_SP = sample.split(ConcreteASH_SP, SplitRatio = 0.7)
Train_ConcreteASH_SP= subset(ConcreteASH_SP, spl_ASH_SP == TRUE) 
Test_ConcreteASH_SP= subset(ConcreteASH_SP, spl_ASH_SP == FALSE)
str(Train_ConcreteASH_SP)
str(Test_ConcreteASH_SP)


#Spliting ConcreteSLAG_SP Data set into Training and Testing sets
spl_SLAG_SP = sample.split(ConcreteSLAG_SP, SplitRatio = 0.7)
Train_ConcreteSLAG_SP= subset(ConcreteSLAG_SP, spl_SLAG_SP == TRUE) 
Test_ConcreteSLAG_SP= subset(ConcreteSLAG_SP, spl_SLAG_SP == FALSE)
str(Train_ConcreteSLAG_SP)
str(Test_ConcreteSLAG_SP)



# Linear Regression Models

# Linear Regression Model with Water to Cement Ratio in Concrete with no Additives
lm_WC_only  = lm (Compressive_Strength ~ w_c , data = Train_ConcreteONLY)
summary(lm_WC_only)
predict_lm_WC_only = predict(lm_WC_only,newdata = Test_ConcreteONLY)
Test_ConcreteONLY$Predicted_LM = predict_lm_WC_only
write.csv(predict_lm_WC_only,"predict_lm_WC_only.csv")


# Linear Regression Model with Water to Cement Ratio in Concrete with Blast Furnance Slag, Fly Ash and Super Plasticizer
lm_WC_HPC  = lm (Compressive_Strength ~ w_c , data = Train_HPC)
summary(lm_WC_HPC)
predict_lm_WC_HPC = predict(lm_WC_only,newdata = Test_HPC)
Test_HPC$Predicted_LM = predict_lm_WC_HPC
write.csv(predict_lm_WC_HPC,"predict_lm_WC_HPC.csv")


# Linear Regression Model with Water to Cement Ratio in Concrete with only Blast Furnance Slag
lm_WC_ConcreteSLAG  = lm (Compressive_Strength ~ w_c , data = Train_ConcreteSLAG)
summary(lm_WC_ConcreteSLAG)
predict_lm_WC_ConcreteSLAG = predict(lm_WC_ConcreteSLAG,newdata = Test_ConcreteSLAG)
Test_ConcreteSLAG$Predicted_LM = predict_lm_WC_ConcreteSLAG
write.csv(predict_lm_WC_ConcreteSLAG,"predict_lm_WC_ConcreteSLAG.csv")


# Linear Regression Model with Water to Cement Ratio in Concrete with Fly Ash
# lm_WC_ConcreteASH  = lm (Compressive_Strength ~ w_c , data = Train_ConcreteASH)
# summary(lm_WC_ConcreteASH)
# predict_lm_WC_ConcreteASH = predict(lm_WC_ConcreteASH,newdata = Test_ConcreteASH)
# Test_ConcreteASH$Predicted_LM = predict_lm_WC_ConcreteASH


# Linear Regression Model with Water to Cement Ratio in Concrete with Super Plasticizer
# lm_WC_ConcreteSP  = lm (Compressive_Strength ~ w_c , data = Train_ConcreteSP)
# summary(lm_WC_ConcreteSP)
# predict_lm_WC_ConcreteSP = predict(lm_WC_ConcreteSP,newdata = Test_ConcreteSP)
# Test_ConcreteSP$Predicted_LM = predict_lm_WC_ConcreteSP


# Linear Regression Model with Water to Cement Ratio in Concrete with Fly Ash and Super Plasticizer
lm_WC_ConcreteASH_SP  = lm (Compressive_Strength ~ w_c , data = Train_ConcreteASH_SP)
summary(lm_WC_ConcreteASH_SP)
predict_lm_WC_ConcreteASH_SP = predict(lm_WC_ConcreteASH_SP,newdata = Test_ConcreteASH_SP)
Test_ConcreteASH_SP$Predicted_LM = predict_lm_WC_ConcreteASH_SP
write.csv(predict_lm_WC_ConcreteASH_SP,"predict_lm_WC_ConcreteASH_SP.csv")



# Linear Regression Model with Water to Cement Ratio in Concrete with Blast Furnance Slag and Super Plasticizer
lm_WC_ConcreteSLAG_SP  = lm (Compressive_Strength ~ w_c , data = Train_ConcreteSLAG_SP)
summary(lm_WC_ConcreteSLAG_SP)
predict_lm_WC_ConcreteSLAG_SP = predict(lm_WC_ConcreteSLAG_SP,newdata = Test_ConcreteSLAG_SP)
Test_ConcreteSLAG_SP$Predicted_LM = predict_lm_WC_ConcreteSLAG_SP
write.csv(predict_lm_WC_ConcreteSLAG_SP,"predict_lm_WC_ConcreteSLAG_SP.csv")



# Multi - Linear Regression Models



#Multi - Linear Regression Models of all additives of ConcreteONLY

mlm_ConcreteONLY = lm(Compressive_Strength ~ w_c+Cement+Water+CA+FA+Age,data = Train_ConcreteONLY)
summary(mlm_ConcreteONLY)
predict_mlm_ConcreteONLY = predict(mlm_ConcreteONLY,newdata = Test_ConcreteONLY)
Test_ConcreteONLY$Predicted_MLM = predict_mlm_ConcreteONLY
write.csv(predict_mlm_ConcreteONLY,"predict_mlm_ConcreteONLY.csv")


#Multi - Linear Regression Models of all additives of HPC

mlm_HPC = lm(Compressive_Strength ~w_c+Cement+BF_Slag+Fly_Ash+Water+SuperPlasticizers+CA+FA+Age,data = Train_HPC)
summary(mlm_HPC)
predict_mlm_HPC = predict(mlm_HPC,newdata = Test_HPC)
Test_HPC$Predicted_MLM = predict_mlm_HPC
write.csv(predict_mlm_HPC,"predict_mlm_HPC.csv")

#Multi - Linear Regression Models of all additives of ConcreteSLAG

mlm_ConcreteSLAG = lm(Compressive_Strength ~ w_c+Cement+BF_Slag+Water+CA+FA+Age,data = Train_ConcreteSLAG)
summary(mlm_ConcreteSLAG)
predict_mlm_ConcreteSLAG = predict(mlm_ConcreteSLAG,newdata = Test_ConcreteSLAG)
Test_ConcreteSLAG$Predicted_MLM = predict_mlm_ConcreteSLAG
write.csv(predict_mlm_ConcreteSLAG,"predict_mlm_ConcreteSLAG.csv")

#Multi - Linear Regression Models of all additives of ConcreteASH

# mlm_ConcreteASH = lm(Compressive_Strength ~ w_c+Cement+Fly_Ash+Water+CA+FA+Age,data = Train_ConcreteASH)
# summary(mlm_ConcreteASH)
# predict_mlm_ConcreteASH = predict(mlm_ConcreteASH,newdata = Test_ConcreteASH)
# Test_ConcreteASH$Predicted_MLM = predict_mlm_ConcreteASH


#Multi - Linear Regression Models of all additives of ConcreteSP

# mlm_ConcreteSP = lm(Compressive_Strength ~ w_c+Cement+Water+SuperPlasticizers+CA+FA+Age,data = Train_ConcreteSP)
# summary(mlm_ConcreteSP)
# predict_mlm_ConcreteSP = predict(mlm_ConcreteSP,newdata = Test_ConcreteSP)
# Test_ConcreteSP$Predicted_MLM = predict_mlm_ConcreteSP

#Multi - Linear Regression Models of all additives of ConcreteASH_SP

mlm_ConcreteASH_SP = lm(Compressive_Strength ~ w_c+Cement+Fly_Ash+Water+SuperPlasticizers+CA+FA+Age,data = Train_ConcreteASH_SP)
summary(mlm_ConcreteASH_SP)
predict_mlm_ConcreteASH_SP = predict(mlm_ConcreteASH_SP,newdata = Test_ConcreteASH_SP)
Test_ConcreteASH_SP$Predicted_MLM = predict_mlm_ConcreteASH_SP
write.csv(predict_mlm_ConcreteASH_SP,"predict_mlm_ConcreteASH_SP.csv")


#Multi - Linear Regression Models of all additives of ConcreteSLAG_SP

mlm_ConcreteSLAG_SP = lm(Compressive_Strength ~ w_c+Cement+BF_Slag+Water+SuperPlasticizers+CA+FA+Age,data = Train_ConcreteSLAG_SP)
summary(mlm_ConcreteSLAG_SP)
predict_mlm_ConcreteSLAG_SP = predict(mlm_ConcreteSLAG_SP,newdata = Test_ConcreteSLAG_SP)
Test_ConcreteSLAG_SP$Predicted_MLM = predict_mlm_ConcreteSLAG_SP
write.csv(predict_mlm_ConcreteSLAG_SP,"predict_mlm_ConcreteSLAG_SP.csv")


# CART Regeression Tree Models 


#CART Regeression Tree Model of all additives of ConcreteONLY

TREE_ConcreteONLY = rpart(Compressive_Strength ~ w_c+Cement+Water+CA+FA+Age,data = Train_ConcreteONLY)
summary(TREE_ConcreteONLY)
predict_TREE_ConcreteONLY = predict(TREE_ConcreteONLY,newdata = Test_ConcreteONLY)
Test_ConcreteONLY$Predicted_TREE = predict_TREE_ConcreteONLY
write.csv(predict_TREE_ConcreteONLY,"predict_TREE_ConcreteONLY.csv")

#CART Regeression Tree Model of all additives of HPC

TREE_HPC = rpart(Compressive_Strength ~w_c+Cement+BF_Slag+Fly_Ash+Water+SuperPlasticizers+CA+FA+Age,data = Train_HPC)
summary(TREE_HPC)
predict_TREE_HPC = predict(TREE_HPC,newdata = Test_HPC)
Test_HPC$Predicted_TREE = predict_TREE_HPC
write.csv(predict_TREE_HPC,"predict_TREE_HPC.csv")

#CART Regeression Tree Model of all additives of ConcreteSLAG

TREE_ConcreteSLAG = rpart(Compressive_Strength ~ w_c+Cement+BF_Slag+Water+CA+FA+Age,data = Train_ConcreteSLAG)
summary(TREE_ConcreteSLAG)
predict_TREE_ConcreteSLAG = predict(TREE_ConcreteSLAG,newdata = Test_ConcreteSLAG)
Test_ConcreteSLAG$Predicted_TREE = predict_TREE_ConcreteSLAG
write.csv(predict_TREE_ConcreteSLAG,"predict_TREE_ConcreteSLAG.csv")

#CART Regeression Tree Model of all additives of ConcreteASH

# TREE_ConcreteASH = rpart(Compressive_Strength ~ w_c+Cement+Fly_Ash+Water+CA+FA+Age,data = Train_ConcreteASH)
# summary(TREE_ConcreteASH)
# predict_TREE_ConcreteASH = predict(TREE_ConcreteASH,newdata = Test_ConcreteASH)
# Test_ConcreteASH$Predicted_TREE = predict_TREE_ConcreteASH


#CART Regeression Tree Model of all additives of ConcreteSP

# TREE_ConcreteSP = rpart(Compressive_Strength ~ w_c+Cement+Water+SuperPlasticizers+CA+FA+Age,data = Train_ConcreteSP)
# summary(TREE_ConcreteSP)
# predict_TREE_ConcreteSP = predict(TREE_ConcreteSP,newdata = Test_ConcreteSP)
# Test_ConcreteSP$Predicted_TREE = predict_TREE_ConcreteSP

#CART Regeression Tree Model of all additives of ConcreteASH_SP

TREE_ConcreteASH_SP = rpart(Compressive_Strength ~ w_c+Cement+Fly_Ash+Water+SuperPlasticizers+CA+FA+Age,data = Train_ConcreteASH_SP)
summary(TREE_ConcreteASH_SP)
predict_TREE_ConcreteASH_SP = predict(TREE_ConcreteASH_SP,newdata = Test_ConcreteASH_SP)
Test_ConcreteASH_SP$Predicted_TREE = predict_TREE_ConcreteASH_SP
write.csv(predict_TREE_ConcreteASH_SP,"predict_TREE_ConcreteASH_SP.csv")


#CART Regeression Tree Model of all additives of ConcreteSLAG_SP

TREE_ConcreteSLAG_SP = rpart(Compressive_Strength ~ w_c+Cement+BF_Slag+Water+SuperPlasticizers+CA+FA+Age,data = Train_ConcreteSLAG_SP)
summary(TREE_ConcreteSLAG_SP)
predict_TREE_ConcreteSLAG_SP = predict(TREE_ConcreteSLAG_SP,newdata = Test_ConcreteSLAG_SP)
Test_ConcreteSLAG_SP$Predicted_TREE = predict_TREE_ConcreteSLAG_SP
write.csv(predict_TREE_ConcreteSLAG_SP,"predict_TREE_ConcreteSLAG_SP.csv")

str(Test_ConcreteONLY)
str(Test_HPC)
str(Test_ConcreteSLAG)
str(Test_ConcreteASH)
str(Test_ConcreteSP)
str(Test_ConcreteASH_SP)
str(Test_ConcreteSLAG_SP)


# Writing Subsets into csv files on Hard Disk

write.csv(ConcreteONLY,"CONCRETE.csv")
write.csv(HPC,"HPC.csv")
write.csv(ConcreteSLAG,"SLAG.csv")
write.csv(ConcreteASH,"ASH.csv")
write.csv(ConcreteSP,"SP.csv")
write.csv(ConcreteASH_SP,"ASH_SP.csv")
write.csv(ConcreteSLAG_SP,"SLAG_SP.csv")

write.csv(Test_ConcreteONLY,"Test_CONCRETE.csv")
write.csv(Test_HPC,"Test_HPC.csv")
write.csv(Test_ConcreteSLAG,"Test_SLAG.csv")
write.csv(Test_ConcreteASH,"Test_ASH.csv")
write.csv(Test_ConcreteSP,"Test_SP.csv")
write.csv(Test_ConcreteASH_SP,"Test_ASH_SP.csv")
write.csv(Test_ConcreteSLAG_SP,"Test_SLAG_SP.csv")



# Saving Plots and Graghs


	

# Tree Plot of TREE_ConcreteONLY

jpeg("TREE_ConcreteONLY.jpeg")
prp(TREE_ConcreteONLY)
dev.off()

# Tree Plot of TREE_HPC

jpeg("TREE_HPC.jpeg")
prp(TREE_HPC)
dev.off()

# Tree Plot of TREE_ConcreteSLAG

jpeg("TREE_ConcreteSLAG.jpeg")
prp(TREE_ConcreteSLAG)
dev.off()

# Tree Plot of TREE_ConcreteASH

# jpeg("TREE_ConcreteASH.jpeg")
# prp(TREE_ConcreteASH)
# dev.off()

# Tree Plot of TREE_ConcreteSP


# jpeg("TREE_ConcreteSP.jpeg")
# prp(TREE_ConcreteSP)
# dev.off()

# Tree Plot of TREE_ConcreteASH_SP

jpeg("TREE_ConcreteASH_SP.jpeg")
prp(TREE_ConcreteASH_SP)
dev.off()

# Tree Plot of TREE_ConcreteSLAG_SP

jpeg("TREE_ConcreteSLAG_SP.jpeg")
prp(TREE_ConcreteSLAG_SP)
dev.off()



setwd("~/Documents/R/Dr Pal Project")
library(caTools)
library(rpart)
library(rpart.plot)
library(foreign)
library(nnet)
library(ggplot2)
library(reshape2)

set.seed(3000)

#loading Data
data = read.csv("EXPDATA.csv")
str(data)

#spliting data into test and train

Split = sample.split(data, SplitRatio = 0.7)
Train = subset(data,Split == TRUE)
Test = subset(data,Split ==FALSE)

#Model Multi Classification Model Damaged and Undamaged Prediction All Sensors

U_UD = multinom(Damage ~S1+S3+S4+S6+S7+S9+S10+S12+S13+S15+S16+S18, data=Train, method = "class")
U_UD_Predict = predict(U_UD,newdata=Test,type="class")
U_UD_Table = table(Test$Damage,U_UD_Predict)
U_UD_Table
write.csv(U_UD_Table,"R.All Sensors Damaged Undamged Prediction.csv")

#For Damaged and Undamaged Prediction S3-S16 Sensors

Damage_S3_S16 = multinom(Damage~S3+S16,data=Train)
Damage_S3_S16_Predict = predict(Damage_S3_S16,newdata = Test)
Damage_S3_S16_Table = table(Test$Damage,Damage_S3_S16_Predict)
Damage_S3_S16_Table
write.csv(Damage_S3_S16_Table,"R.Damaged and Undamaged S3-S16 Sensor.csv")

#For Damaged and Undamaged Prediction S4-S15 Sensors

Damage_S4_S15 = multinom(Damage~S4+S15,data=Train)
Damage_S4_S15_Predict = predict(Damage_S4_S15,newdata = Test)
Damage_S4_S15_Table = table(Test$Damage,Damage_S4_S15_Predict)
Damage_S4_S15_Table
write.csv(Damage_S4_S15_Table,"R.Damaged and Undamaged S4-S15 Sensor.csv")

#For Damaged and Undamaged Prediction S6-S13 Sensors

Damage_S6_S13 = multinom(Damage~S6+S13,data=Train)
Damage_S6_S13_Predict = predict(Damage_S6_S13,newdata = Test)
Damage_S6_S13_Table = table(Test$Damage,Damage_S6_S13_Predict)
Damage_S6_S13_Table
write.csv(Damage_S6_S13_Table,"R.Damaged and Undamaged S6-S13 Sensor.csv")

#For Damaged and Undamaged Prediction S7-S12 Sensors

Damage_S7_S12 = multinom(Damage~S7+S12,data=Train)
Damage_S7_S12_Predict = predict(Damage_S7_S12,newdata = Test)
Damage_S7_S12_Table = table(Test$Damage,Damage_S7_S12_Predict)
Damage_S7_S12_Table
write.csv(Damage_S7_S12_Table,"R.Damaged and Undamaged S7-S12 Sensor.csv")

#For Damaged and Undamaged Prediction S1-S18 Sensors

Damage_S1_S18 = multinom(Damage~S1+S18,data=Train)
Damage_S1_S18_Predict = predict(Damage_S1_S18,newdata = Test)
Damage_S1_S18_Table = table(Test$Damage,Damage_S1_S18_Predict)
Damage_S1_S18_Table
write.csv(Damage_S1_S18_Table,"R.Damaged and Undamaged S1-S18 Sensor.csv")

#For Damaged and Undamaged Prediction S9-S10 Sensors

Damage_S9_S10 = multinom(Damage~S9+S10,data=Train)
Damage_S9_S10_Predict = predict(Damage_S9_S10,newdata = Test)
Damage_S9_S10_Table = table(Test$Damage,Damage_S9_S10_Predict)
Damage_S9_S10_Table
write.csv(Damage_S9_S10_Table,"R.Damaged and Undamaged S9-S10 Sensor.csv")


#-------------------------*------------------------------


#For damage Location Prediction All Sensors Multi-Classification Model

Location = multinom(DamageLocation~S1+S3+S4+S6+S7+S9+S10+S12+S13+S15+S16+S18,data=Train)
Location_Predict = predict(Location,newdata = Test)
Location_Table = table(Test$DamageLocation,Location_Predict)
Location_Table
write.csv(Location_Table,"R.Damaged Location All Sensor.csv")

#For damage Location S3-S16 Sensors

Location_S3_S16 = multinom(DamageLocation~S3+S16,data=Train)
Location_S3_S16_Predict = predict(Location_S3_S16,newdata = Test)
Location_S3_S16_Table = table(Test$DamageLocation,Location_S3_S16_Predict)
Location_S3_S16_Table
write.csv(Location_S3_S16_Table,"R.Damaged Location S3-S16 Sensor.csv")

#For damage Location S4-S15 Sensors

Location_S4_S15 = multinom(DamageLocation~S4+S15,data=Train)
Location_S4_S15_Predict = predict(Location_S4_S15,newdata = Test)
Location_S4_S15_Table = table(Test$DamageLocation,Location_S4_S15_Predict)
Location_S4_S15_Table
write.csv(Location_S4_S15_Table,"R.Damaged Location S4-S15 Sensor.csv")

#For damage Location S6-S13 Sensors

Location_S6_S13 = multinom(DamageLocation~S6+S13,data=Train)
Location_S6_S13_Predict = predict(Location_S6_S13,newdata = Test)
Location_S6_S13_Table = table(Test$DamageLocation,Location_S6_S13_Predict)
Location_S6_S13_Table
write.csv(Location_S6_S13_Table,"R.Damaged Location S6-S13 Sensor.csv")

#For damage Location S7-S12 Sensors

Location_S7_S12 = multinom(DamageLocation~S7+S12,data=Train)
Location_S7_S12_Predict = predict(Location_S7_S12,newdata = Test)
Location_S7_S12_Table = table(Test$DamageLocation,Location_S7_S12_Predict)
Location_S7_S12_Table
write.csv(Location_S7_S12_Table,"R.Damaged Location S7-S12 Sensor.csv")

#For damage Location S1-S18 Sensors

Location_S1_S18 = multinom(DamageLocation~S1+S18,data=Train)
Location_S1_S18_Predict = predict(Location_S1_S18,newdata = Test)
Location_S1_S18_Table = table(Test$DamageLocation,Location_S1_S18_Predict)
Location_S1_S18_Table
write.csv(Location_S1_S18_Table,"R.Damaged Location S1-S18 Sensor.csv")

#For damage Location S9-S10 Sensors

Location_S9_S10 = multinom(DamageLocation~S9+S10,data=Train)
Location_S9_S10_Predict = predict(Location_S9_S10,newdata = Test)
Location_S9_S10_Table = table(Test$DamageLocation,Location_S9_S10_Predict)
Location_S9_S10_Table
write.csv(Location_S9_S10_Table,"R.Damaged Location S9-S10 Sensor.csv")

#-----------------------*---------------------------
#-----------------------*---------------------------


#For left column Damage accuracy check in all

S3_16 = read.csv("R.EXPData S3-S16 and Undamaged.csv")
summary(S3_16)
Split2 = sample.split(S3_16,SplitRatio = 0.7)
Train_2 = subset(S3_16,Split2 == TRUE)
Test_2 = subset(S3_16,Split2 == FALSE)



#For Damaged and Undamaged Prediction S3-S16 Sensors

Damage_LC_S3_S16 = multinom(Damage~S3+S16,data=Train_2)
Damage_LC_S3_S16_Predict = predict(Damage_LC_S3_S16,newdata = Test_2)
Damage_LC_S3_S16_Table = table(Test_2$Damage,Damage_LC_S3_S16_Predict)
Damage_LC_S3_S16_Table
write.csv(Damage_LC_S3_S16_Table,"R.LC S3-S16 Sensor.csv")

#For Damaged and Undamaged Prediction S4-S15 Sensors

Damage_LC_S4_S15 = multinom(Damage~S4+S15,data=Train_2)
Damage_LC_S4_S15_Predict = predict(Damage_LC_S4_S15,newdata = Test_2)
Damage_LC_S4_S15_Table = table(Test_2$Damage,Damage_LC_S4_S15_Predict)
Damage_LC_S4_S15_Table
write.csv(Damage_LC_S4_S15_Table,"R.LC S4-S15 Sensor.csv")

#For Damaged and Undamaged Prediction S6-S13 Sensors

Damage_LC_S6_S13 = multinom(Damage~S6+S13,data=Train_2)
Damage_LC_S6_S13_Predict = predict(Damage_LC_S6_S13,newdata = Test_2)
Damage_LC_S6_S13_Table = table(Test_2$Damage,Damage_LC_S6_S13_Predict)
Damage_LC_S6_S13_Table
write.csv(Damage_LC_S6_S13_Table,"R.LC S6-S13 Sensor.csv")

#For Damaged and Undamaged Prediction S7-S12 Sensors

Damage_LC_S7_S12 = multinom(Damage~S7+S12,data=Train_2)
Damage_LC_S7_S12_Predict = predict(Damage_LC_S7_S12,newdata = Test_2)
Damage_LC_S7_S12_Table = table(Test_2$Damage,Damage_LC_S7_S12_Predict)
Damage_LC_S7_S12_Table
write.csv(Damage_LC_S7_S12_Table,"R.LC S7-S12 Sensor.csv")

#For Damaged and Undamaged Prediction S1-S18 Sensors

Damage_LC_S1_S18 = multinom(Damage~S1+S18,data=Train_2)
Damage_LC_S1_S18_Predict = predict(Damage_LC_S1_S18,newdata = Test_2)
Damage_LC_S1_S18_Table = table(Test_2$Damage,Damage_LC_S1_S18_Predict)
Damage_LC_S1_S18_Table
write.csv(Damage_LC_S1_S18_Table,"R.LC S1-S18 Sensor.csv")

#For Damaged and Undamaged Prediction S9-S10 Sensors

Damage_LC_S9_S10 = multinom(Damage~S9+S10,data=Train_2)
Damage_LC_S9_S10_Predict = predict(Damage_LC_S9_S10,newdata = Test_2)
Damage_LC_S9_S10_Table = table(Test_2$Damage,Damage_LC_S9_S10_Predict)
Damage_LC_S9_S10_Table
write.csv(Damage_LC_S9_S10_Table,"R.LC S9-S10 Sensor.csv")


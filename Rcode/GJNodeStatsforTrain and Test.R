# Getting some Group 1 from Test set
# GENERAL SETTINGS
# require(tcltk)
# # Set the folder path for where the normal and cancer txt files are.
# ReturnVal <- tkmessageBox(title = "Step 1", message = "Please select the directory for the train set", icon = "info", type = "ok")
# direct1 <- tclvalue(tkchooseDirectory(initialdir= ##'Z:/QTP/Martial/LDO2017/TestSet/Testsubset'))
#                                         'C:/Users/edong/Desktop/DysplasiaGJtxt'))
# fileList <- list.files(path = direct1, pattern=".txt")

require(tcltk)
# Set the folder path for where the normal and cancer txt files are.
ReturnVal1 <- tkmessageBox(title = "Step 1", message = "Please select the directory for the train Cancer G1 set", icon = "info", type = "ok")
direct1 <- tclvalue(tkchooseDirectory(initialdir= ##'Z:/QTP/Martial/LDO2017/TestSet/Testsubset'))
                                        'Z:/QTP/Martial/LDO2017/TrainSet/Cancertxt'))
fileList1 <- list.files(path = direct1, pattern=".txt")

require(tcltk)
# Set the folder path for where the normal and cancer txt files are.
ReturnVal2 <- tkmessageBox(title = "Step 2", message = "Please select the directory for the train Cancer G9 set", icon = "info", type = "ok")
direct2 <- tclvalue(tkchooseDirectory(initialdir= ##'Z:/QTP/Martial/LDO2017/TestSet/Testsubset'))
                                        'Z:/QTP/Martial/LDO2017/TrainSet/Cancer9txt'))
fileList2 <- list.files(path = direct2, pattern=".txt")
################################################ Ready in training data ########################################
library(caret)
library(randomForest)
# Useful features.
colnm <- c("GROUP", "area", "DNA_Amount", morph, photo, discrete, markovian, fractal, runlength)

l <- length(fileList1)
Original_NumGood = vector("numeric",l)
Original_NumJunk = vector("numeric",l)
Processed_NumGood = vector("numeric",l)
Processed_NumJunk = vector("numeric",l)
Node0TP = vector("numeric",l)
Node0FP <- vector("numeric",l)
Node0TN <- vector("numeric",l)
Node0FN <- vector("numeric",l)

Layer1_Sensitivity = vector("numeric",l)
Layer1_Specificity <- vector("numeric",l)
Layer1_Precision <- vector("numeric",l)

Node1TP = vector("numeric",l)
Node1FP <- vector("numeric",l)
Node1TN <- vector("numeric",l)
Node1FN <- vector("numeric",l)

Node2TP = vector("numeric",l)
Node2FP <- vector("numeric",l)
Node2TN <- vector("numeric",l)
Node2FN <- vector("numeric",l)

Layer2_Sensitivity = vector("numeric",l)
Layer2_Specificity <- vector("numeric",l)
Layer2_Precision <- vector("numeric",l)

Node3TP = vector("numeric",l)
Node3FP <- vector("numeric",l)
Node3TN <- vector("numeric",l)
Node3FN <- vector("numeric",l)

Node4TP = vector("numeric",l)
Node4FP <- vector("numeric",l)
Node4TN <- vector("numeric",l)
Node4FN <- vector("numeric",l)

Node5TP = vector("numeric",l)
Node5FP <- vector("numeric",l)
Node5TN <- vector("numeric",l)
Node5FN <- vector("numeric",l)

Node6TP = vector("numeric",l)
Node6FP <- vector("numeric",l)
Node6TN <- vector("numeric",l)
Node6FN <- vector("numeric",l)

Layer3_Sensitivity = vector("numeric",l)
Layer3_Specificity <- vector("numeric",l)
Layer3_Precision <- vector("numeric",l)

for(i in 1:l) {
  # setwd(direct1) 
  # filename = fileList[i] 
  # dat <- as.data.frame(read.table(filename, header=TRUE, sep = "" ))
  # if (colnames(dat)[1] == "UnitId"){
  #   dat <- subset(dat, select = -UnitId)
  # }
  # dat <- dat[, colnm]
  # as.factor(dat$GROUP);
  # good_ind <- which(dat$GROUP == "1")
  # Original_NumGood[i] <- length(good_ind)
  # dat$GROUP[good_ind] <- "good"
  # 
  # junk_ind <- which(dat$GROUP == "9")
  # Original_NumJunk[i] <- length(junk_ind)
  # dat$GROUP[junk_ind] <- "junk"
  # colnames(dat)[which(colnames(dat) == "GROUP")] <- "Ycol"
  
  setwd(direct1)
  filename1 = fileList1[i]
  dat1 <- as.data.frame(read.table(filename1, header=TRUE, sep = "" ))
  if (colnames(dat1)[1] == "UnitId"){
    dat1 <- subset(dat1, select = -UnitId)
  }
  
  
  setwd(direct2)
  filename2 = fileList2[i]
  if(filename1 != filename2) {
    print("G1/G9 text file title not matched!!! Stop NOW")
  }
  dat2 <- as.data.frame(read.table(filename2, header=TRUE, sep = "" ))
  if (colnames(dat2)[1] == "UnitId"){
    dat2 <- subset(dat2, select = -UnitId)
  }
  
  dat1 <- dat1[, colnm]; dat2 <- dat2[, colnm]
  
  Original_NumGood[i] <- nrow(dat1)
  dat1$GROUP <- "good"
  
  Original_NumJunk[i] <- nrow(dat2)
  dat2$GROUP <- "junk"
  
  dat <- rbind(dat1, dat2)
  colnames(dat)[which(colnames(dat) == "GROUP")] <- "Ycol"
  ################### Processing Test Data #########################################
  # remove objects with small fractal dimension: the criterion is fractal_dimen <= 1.5
  n1 <- nrow(dat)
  fractal_dimen <- dat$fractal_dimen
  F <- any(fractal_dimen <= 1.5)
  
  if (F==TRUE){
    print("Warning: cells with small fractal dimension detected. The rows with fractal_dimen <= 1.5 will be removed to proceed with the algorithm")
    Smallf <- which(fractal_dimen <= 1.5, arr.ind=TRUE)
    dat <- dat[-Smallf,]
    rm(fractal_dimen); rm(Smallf); rm(F)
    n2 <- nrow(dat)
    print(paste((n1-n2), "rows with small fractal_dimen <= 1.5 have been deleted."))
  }
  
  
  # remove objects with abnormally large background: the criterion is DNA_Index >=5
  n1 <- nrow(dat)
  DNA_Index <- dat$DNA_Index
  D <- any(DNA_Index>=5)
  
  if (D==TRUE){
    print("Warning: cells with large background detected. The rows with DNA_Index >= 5 will be removed to proceed with the algorithm")
    LargeD <- which(DNA_Index>=5, arr.ind=TRUE)
    dat <- dat[-LargeD,]
    rm(DNA_Index); rm(LargeD); rm(D)
    n2 <- nrow(dat)
    print(paste((n1-n2), "rows with large background (DNA_Index>=5) have been deleted."))
  }
  
  # remove some objects with very small area: the criterion is area <= 200
  n1 <- nrow(dat)
  area <- dat$area
  s <- any(area<=200)
  
  if(s==TRUE){
    print("Warning: small area cells detected. The rows with area <= 200 will be removed to proceed with the algorithm")
    small <- which(area <= 200, arr.ind = TRUE)
    dat <- dat[-small, ]
    rm(area); rm(small); rm(s)
    n2 <- nrow(dat)
    print(paste((n1-n2), "rows with samll areas (area<200) have been deleted."))
  }
  
  # remove objects with very large area: the criterion is area >= 4500
  n1 <- nrow(dat)
  area <- dat$area
  L <- any(area>=4500)
  
  if(L==TRUE){
    print("Warning: Large area cells detected. The rows with area >= 4500 will be removed to proceed with the algorithm")
    large <- which(area >= 4500, arr.ind = TRUE)
    dat <- dat[-large,]
    rm(large); rm(area); rm(L)
    n2 <- nrow(dat)
    print(paste((n1-n2), "rows with large areas (area>=4500) have been deleted."))
  }
  
  # remove objects with pale stain: DNA_Amount < 60
  n1 <- nrow(dat)
  DNA_Amount <- dat$DNA_Amount
  P <- any(DNA_Amount<60)
  
  if(P==TRUE){
    print("Warning: Pale cells detected. The rows with  DNA_Amount < 60 will be removed to proceed with the algorithm")
    pale <- which(DNA_Amount < 60, arr.ind = TRUE)
    dat <- dat[-pale,]
    rm(pale); rm(DNA_Amount); rm(P)
    n2 <- nrow(dat)
    print(paste((n1-n2), "rows with pale stained (DNA_Amount < 60) have been deleted."))
  }
  
  ########################## Different Node Classification ##########################
  npgood <- table(dat$Ycol)[1]
  Processed_NumGood[i] <- npgood
  
  npjunk <- table(dat$Ycol)[2]
  Processed_NumJunk[i] <- npjunk
  
  # Predict 
  ############## 0 -> 1,2
  predprob0 <- predict(rf_goodjunkII, dat, type = "prob")[,1]
  
  predgoodind0 <- which(predprob0>=0.5)
  predjunkind0 <- which(predprob0<0.5)
  realgoodind0 <- which(dat$Ycol == "good")
  realjunkind0 <- which(dat$Ycol == "junk")
  
  Node0TP[i] <- length(intersect(predgoodind0, realgoodind0))
  Node0FP[i] <- length(intersect(predgoodind0, realjunkind0))
  Node0TN[i] <- length(intersect(predjunkind0, realjunkind0))
  Node0FN[i] <- length(intersect(predjunkind0, realgoodind0))
  
  node1 <- dat[predgoodind0, ]
  node2 <- dat[predjunkind0, ]
  
  
  Layer1_Sensitivity[i] <- Node0TP[i] / (Node0TP[i] + Node0FN[i])
  Layer1_Specificity[i] <- Node0TN[i] / (Node0TN[i] + Node0FP[i])
  Layer1_Precision[i] <- Node0TP[i] / (Node0TP[i] + Node0FP[i])
  
  ############## 1 -> 3,4
  predprob1 <- predict(rf_goodjunkIIGood, node1, type = "prob")[,1]
  
  predgoodind1 <- which(predprob1>=0.5)
  predjunkind1 <- which(predprob1<0.5)
  realgoodind1 <- which(node1$Ycol == "good")
  realjunkind1 <- which(node1$Ycol == "junk")
  
  Node1TP[i] <- length(intersect(predgoodind1, realgoodind1))
  Node1FP[i] <- length(intersect(predgoodind1, realjunkind1))
  Node1TN[i] <- length(intersect(predjunkind1, realjunkind1))
  Node1FN[i] <- length(intersect(predjunkind1, realgoodind1))
  
  node3 <- node1[predgoodind1, ]
  node4 <- node1[predjunkind1, ]
  
  ############## 2 -> 5,6
  predprob2 <- predict(rf_goodjunkIIJunk, node2, type = "prob")[,1]
  
  predgoodind2 <- which(predprob2>=0.5)
  predjunkind2 <- which(predprob2<0.5)
  realgoodind2 <- which(node2$Ycol == "good")
  realjunkind2 <- which(node2$Ycol == "junk")
  
  Node2TP[i] <- length(intersect(predgoodind2, realgoodind2))
  Node2FP[i] <- length(intersect(predgoodind2, realjunkind2))
  Node2TN[i] <- length(intersect(predjunkind2, realjunkind2))
  Node2FN[i] <- length(intersect(predjunkind2, realgoodind2))
  
  node5 <- node2[predgoodind2, ]
  node6 <- node2[predjunkind2, ]
  
  
  Layer2_Sensitivity[i] <- (Node1TP[i] + Node2TP[i]) / (Node1TP[i] + Node1FN[i] + Node2TP[i] + Node2FN[i])
  Layer2_Specificity[i] <- (Node1TN[i] + Node2TN[i]) / (Node1TN[i] + Node1FP[i] + Node2TN[i] + Node2FP[i])
  Layer2_Precision[i]   <- (Node1TP[i] + Node2TP[i]) / (Node1TP[i] + Node1FP[i] + Node2TP[i] + Node2FP[i])
  
  ############## 3 -> 7,8
  predprob3 <- predict(rf_goodjunkIIGood_Good, node3, type = "prob")[,1]
  
  predgoodind3 <- which(predprob3>=0.5)
  predjunkind3 <- which(predprob3<0.5)
  realgoodind3 <- which(node3$Ycol == "good")
  realjunkind3 <- which(node3$Ycol == "junk")
  
  Node3TP[i] <- length(intersect(predgoodind3, realgoodind3))
  Node3FP[i] <- length(intersect(predgoodind3, realjunkind3))
  Node3TN[i] <- length(intersect(predjunkind3, realjunkind3))
  Node3FN[i] <- length(intersect(predjunkind3, realgoodind3))
  
  node7 <- node3[predgoodind3, ]
  node8 <- node3[predjunkind3, ]
  
  ############## 4 -> 9,10
  predprob4 <- predict(rf_goodjunkIIGood_Junk, node4, type = "prob")[,1]
  
  predgoodind4 <- which(predprob4>=0.5)
  predjunkind4 <- which(predprob4<0.5)
  realgoodind4 <- which(node4$Ycol == "good")
  realjunkind4 <- which(node4$Ycol == "junk")
  
  Node4TP[i] <- length(intersect(predgoodind4, realgoodind4))
  Node4FP[i] <- length(intersect(predgoodind4, realjunkind4))
  Node4TN[i] <- length(intersect(predjunkind4, realjunkind4))
  Node4FN[i] <- length(intersect(predjunkind4, realgoodind4))
  
  node9  <- node4[predgoodind4, ]
  node10 <- node4[predjunkind4, ]
  
  ############## 5 -> 11,12
  predprob5 <- predict(rf_goodjunkIIJunk_Good, node5, type = "prob")[,1]
  
  predgoodind5 <- which(predprob5>=0.5)
  predjunkind5 <- which(predprob5<0.5)
  realgoodind5 <- which(node5$Ycol == "good")
  realjunkind5 <- which(node5$Ycol == "junk")
  
  Node5TP[i] <- length(intersect(predgoodind5, realgoodind5))
  Node5FP[i] <- length(intersect(predgoodind5, realjunkind5))
  Node5TN[i] <- length(intersect(predjunkind5, realjunkind5))
  Node5FN[i] <- length(intersect(predjunkind5, realgoodind5))
  
  node11 <- node5[predgoodind5, ]
  node12 <- node5[predjunkind5, ]
  
  ############## 6 -> 13,14
  predprob6 <- predict(rf_goodjunkIIJunk_Junk, node6, type = "prob")[,1]
  
  predgoodind6 <- which(predprob6>=0.5)
  predjunkind6 <- which(predprob6<0.5)
  realgoodind6 <- which(node6$Ycol == "good")
  realjunkind6 <- which(node6$Ycol == "junk")
  
  Node6TP[i] <- length(intersect(predgoodind6, realgoodind6))
  Node6FP[i] <- length(intersect(predgoodind6, realjunkind6))
  Node6TN[i] <- length(intersect(predjunkind6, realjunkind6))
  Node6FN[i] <- length(intersect(predjunkind6, realgoodind6))
  
  node13 <- node6[predgoodind6, ]
  node14 <- node6[predjunkind6, ]
  
  
  Layer3_Sensitivity[i] <- (Node3TP[i] + Node4TP[i] + Node5TP[i] + Node6TP[i]) / (Node3TP[i] + Node3FN[i] + Node4TP[i] + Node4FN[i] + Node5TP[i] + Node5FN[i] + Node6TP[i] + Node6FN[i])
  Layer3_Specificity[i] <- (Node3TN[i] + Node4TN[i] + Node5TN[i] + Node6TN[i]) / (Node3TN[i] + Node3FP[i] + Node4TN[i] + Node4FP[i] + Node5TN[i] + Node5FP[i] + Node6TN[i] + Node6FP[i])
  Layer3_Precision[i]   <- (Node3TP[i] + Node4TP[i] + Node5TP[i] + Node6TP[i]) / (Node3TP[i] + Node3FP[i] + Node4TP[i] + Node4FP[i] + Node5TP[i] + Node5FP[i] + Node6TP[i] + Node6FP[i])
}

Name_Numbers <- cbind(fileList1, Original_NumGood, Original_NumJunk, Processed_NumGood, Processed_NumJunk)
NodeTPFPTNFN <- cbind(Node0TP, Node0FP, Node0TN, Node0FN, Layer1_Sensitivity, Layer1_Specificity, Layer1_Precision,
                      Node1TP, Node1FP, Node1TN, Node1FN, 
                      Node2TP, Node2FP, Node2TN, Node2FN, Layer2_Sensitivity, Layer2_Specificity, Layer2_Precision,
                      Node3TP, Node3FP, Node3TN, Node3FN, 
                      Node4TP, Node4FP, Node4TN, Node4FN, 
                      Node5TP, Node5FP, Node5TN, Node5FN,
                      Node6TP, Node6FP, Node6TN, Node6FN, Layer3_Sensitivity, Layer3_Specificity, Layer3_Precision)

SUM <- as.data.frame.matrix(cbind(Name_Numbers, NodeTPFPTNFN))

ReturnVal2 <- tkmessageBox(title = "Step 2", message = "Please select the directory to save the result", icon = "info", type = "ok")
fileOut <- tclvalue(tkgetSaveFile(initialfile ="NodeStats_Train_Cancer.csv", initialdir="Z:/QTP/Martial/LDO2017", filetypes = "{{csv file} {.csv}}"))
write.csv(SUM, fileOut)


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
  dat1$GROUP <- "good"
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
  
  # ########################## Predict Prob and Draw Density Plot ##########################
  table(dat$Ycol)
  # Predict
  predprob <- predict(rf_goodjunkII, dat, type = "prob")[,1]
  pred <- rep("junk", length(predprob0))
  pred[which(predprob>=0.5)] <- "good"
  confusionMatrix(pred, dat$Ycol)
  
  # Density plot of all test sets
  lower.limit <- min(predprob)
  upper.limit <- max(predprob)
  dgood <- density(predprob[which(dat$Ycol=="good", arr.ind=TRUE)], from = lower.limit, to = upper.limit, n = 2^10 )
  djunk <- density(predprob[which(dat$Ycol!="good", arr.ind=TRUE)], from = lower.limit, to = upper.limit, n = 2^10 )
  plot(djunk, ylim=range(c(dgood$y, djunk$y)), xlim=c(0:1), main= paste(c("Distribution of Probability Socres by Actual Class in",
       filename), sep = ""), col="Blue" , xlab= paste(c("0 = Junk, 1 = Good, n=", toString(nrow(dat))), sep = ""))
  lines(dgood, col="Red")
  legend("top", cex=0.75, pch=16, col=c("blue", "red"), legend=c("Junk", "Good"), ncol=2)
  
}

  
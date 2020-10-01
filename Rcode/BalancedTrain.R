# Group of features(CIS/SSC + D1/HYP + D2)
is.installed <- function(mypkg) is.element(mypkg, installed.packages()[,1])
if(!is.installed("randomForest")) {
  install.packages('randomForest', dependencies=TRUE)
}
if(!is.installed("caret")) {
  install.packages('caret', dependencies=TRUE)
}

# GENERAL SETTINGS
require(tcltk)
# Set the folder path for where the normal and cancer txt files are.
ReturnVal <- tkmessageBox(title = "Step 1", message = "Please select the directory for the train set", icon = "info", type = "ok")
direct1 <- tclvalue(tkchooseDirectory(initialdir='Z:/QTP/Martial/LDO2017/TrainSet'))
#Need function: getX() for cell threshold.-> for ration score
################################################ Ready in training data ########################################
# preprocesstrain <- function(wd){

setwd("Z:/QTP/Martial/LDO2017/scripts")
source("Features_Group_Strings.r")
# This Function is unnecessary for R 3.3.1, but I am limited to the Java version and has to use R 2.3.1
# list.dirself <- function(path=".", pattern=NULL, all.dirs=FALSE,
#                          full.names=FALSE, ignore.case=FALSE) {
#   # use full.names=TRUE to pass to file.info
#   all <- list.files(path, pattern, all.dirs,
#                     full.names=TRUE, recursive=FALSE, ignore.case)
#   dirs <- all[file.info(all)$isdir]
#   # determine whether to return full names or just dir names
#   if(isTRUE(full.names))
#     return(dirs)
#   else
#     return(basename(dirs))
# }
# folderList <- list.dirself(path = direct1, full.names = TRUE) #"./Cancer" "./Normal"

# In R 3.3.1: 
folderList <- list.dirs(path = direct1, recursive = FALSE) 
folderList <- folderList[c(3,6)] #Only take the Cancertxt and Normaltxt from [Cancer, Cancertxt, Normal, Normaltxt]
m <- length(folderList) #2
# Because the number of folders equal to the number of classes. Normal +Cancer = 2
numlist <- vector("integer", m+1)
numlist[1] = 0
wordlist <- c("cancer","normal") # Because folders are arranged according to their first letter, "c"<"n"
# Build an empty data.frame with all of the features.
colnm <- c(morph, photo, discrete, markovian, fractal, runlength)
dfacc <- data.frame(matrix(ncol = length(colnm), nrow = 0))
colnames(dfacc) <- colnm
# Build another empty data.frame with the Ycol
df <- data.frame(matrix(ncol = 1, nrow = 0))

for(t in 1:m){
  setwd(folderList[t])
  fileList <- list.files(path = folderList[t], pattern=".txt")
  l <- length(fileList)
  for(i in 1:l) {
    dat <- as.data.frame(read.table(fileList[i], header=TRUE,sep = "" ))
    dat <- dat[colnm]
    dfacc<- rbind.data.frame(dfacc,dat)
  }
  nrows <- nrow(dfacc)
  numlist[[t+1]] <- nrows-numlist[[t]]
  df0 <- data.frame(rep(wordlist[t],times = numlist[[t+1]]))
  df <- rbind(df,df0)
}

# The Ultimate Traning Set, with 6 feature groups #11573 cells * (96 features+ 1 response), no archi, red, and non-markovian.
colnames(df) <- "Ycol"
dat.tr <- cbind(dfacc,df)

print("Number of cancer and normal training cells")
numlist[-1] #5788 Cancer, 5785 Normal
# }


###################################################### Test the 80% RF Classifier #############################################
set.seed(432)
mix<-sample(nrows,nrows) # The final nrows = 11573
node0t <- dat.tr[mix[1:floor((nrows/10)*8)],] # 9258 cells
node0tst <- dat.tr[mix[(floor((nrows/10)*8)+1):nrows],] # 2315 cells

# Export dat so we don't have to do the cleaning steps again
setwd("Z:\\QTP\\Martial\\LDO2017\\scripts")
write.csv(dat.tr, file="dattr11573.csv", row.names=FALSE)
write.csv(node0t, file="train80%.csv", row.names=FALSE)
write.csv(node0tst, file="test20%.csv", row.names=FALSE)


library(randomForest)
library(caret)

# Caret random search tune
set.seed(432)
mySeeds <- sapply(simplify = FALSE, 1:101, function(u) sample(10^4, 10))
cvCtrl = caret::trainControl(method = "repeatedcv", number = 5, repeats = 3,
                             classProbs = TRUE, search= "random", seeds = mySeeds)
rfmodel <- train(Ycol~., node0t, method='rf', trControl=cvCtrl, ntree= 100, metric = "Accuracy", tuneLength = 10, replace=FALSE)
rfmodel # mtry_best = 55

# Run a RF with more trees
set.seed( unlist(tail(mySeeds,1))[1])
rf_balancetrain <- randomForest(Ycol ~ ., data=node0t, ntree=500, importance=TRUE, mtry = 55, type = "classification")
plot(rf_balancetrain)

t1<- table(node0tst$Ycol,predict(rf_balancetrain,node0tst))
sum(diag(t1))/sum(t1) # 0.9019438 = Accuracy

imp <- as.matrix(rf_balancetrain$importance)
rownames(imp)[order(as.matrix(imp[,1]), decreasing=TRUE)] #:)

setwd('Z:/QTP/Martial/LDO2017/scripts')
load("rf_balancetrain.rdata") 
b<- cbind(table(node0tst$Ycol,predict(rf_balancetrain,node0tst)),diag(prop.table(table(node0tst$Ycol,predict(rf_balancetrain,node0tst)),1)))# 90.19% = Accuracy
# Randomly split for test N and C (1177, 1138 respectively)
#          cancer normal          
# cancer   1013    164 0.8606627
# normal     63   1075 0.9437610
setwd('Z:/QTP/Martial/LDO2017/scripts')
save(rf_balancetrain, file="rf_balancetrain.RData")
tab <- table(train$Ycol,predict(rf_balancetrain,train))
bind<- cbind(tab,diag(prop.table(tab,1)))

par(mfrow=c(2,1))
# Train
votes <- predict(rf_balancetrain, node0t, type="prob")[,1]
dcanc <- density(votes[which(node0t$Ycol=="cancer", arr.ind=TRUE)])
dnorm <- density(votes[which(node0t$Ycol!="cancer", arr.ind=TRUE)])
plot(dnorm, ylim=range(c(dcanc$y, dnorm$y)), xlim=c(0:1), main="Distribution of Voting Socres by Actual Class in Train Set", col="Blue" , xlab="n=9258; 0 = Normal, 1 = Cancer")
lines(dcanc, col="Red")
legend("top", cex=0.75, pch=16, col=c("blue", "red"), legend=c("Normal", "Cancer"), ncol=2)

# Plot the voting proportions of the testing set
votes <- predict(rf_balancetrain, node0tst, type="prob")[,1]
dcanc <- density(votes[which(node0tst$Ycol=="cancer", arr.ind=TRUE)])
dnorm <- density(votes[which(node0tst$Ycol!="cancer", arr.ind=TRUE)])
plot(dnorm, ylim=range(c(dcanc$y, dnorm$y)), xlim=c(0:1), main="Distribution of Voting Socres by Actual Class in Val Set" , col="Blue", xlab="n=2315; 0 = Normal, 1 = Cancer")
lines(dcanc, col="Red")
legend("top", cex=0.75, pch=16, col=c("blue", "red"), legend=c("Normal", "Cancer"), ncol=2)

# ############################################### Build a RF with all Train ###############################################
# library(randomForest)
# library(caret)
# 
# # Caret random search tune
# set.seed(432)
# mySeeds <- sapply(simplify = FALSE, 1:101, function(u) sample(10^4, 10))
# cvCtrl = caret::trainControl(method = "repeatedcv", number = 5, repeats = 3, 
#                              classProbs = TRUE, search= "random", seeds = mySeeds)
rfall <- train(Ycol~., dat.tr, method='rf', trControl=cvCtrl, ntree= 100, metric = "Accuracy", tuneLength = 10, replace=FALSE)
rfall # mtry_best = 69
# 
# # Run a RF with more trees
set.seed( unlist(tail(mySeeds,1))[1])
rf_more <- randomForest(Ycol ~ ., data=dat.tr, ntree=500, importance=TRUE, mtry = 69, type = "classification")
plot(rf_more)
# 
imp <- as.matrix(rf_more$importance)
rownames(imp)[order(as.matrix(imp[,1]), decreasing=TRUE)] #:)
# 
setwd("Z:/QTP/Martial/LDO2017/scripts")
save(rf_more, file="rf_more.RData")
tab <- table(dat.tr$Ycol,predict(rf_more,dat.tr))
bind<- cbind(tab,diag(prop.table(tab,1)))

# Train
votes <- predict(rf_more, dat.tr, type="prob")[,1]
dcanc <- density(votes[which(dat.tr$Ycol=="cancer", arr.ind=TRUE)])
dnorm <- density(votes[which(dat.tr$Ycol!="cancer", arr.ind=TRUE)])
plot(dnorm, ylim=range(c(dcanc$y, dnorm$y)), xlim=c(0:1), main="Distribution of Voting Socres by Actual Class in
     All Train Set", col="Blue" , xlab="n=11573; 0 = Normal, 1 = Cancer")
lines(dcanc, col="Red")
legend("top", cex=0.75, pch=16, col=c("blue", "red"), legend=c("Normal", "Cancer"), ncol=2)


##################################################### Try new analysis without 2 vars ############################################
dat.noboth <- subset(dat.tr, select = -c(mean_intensity, var_intensity))
dat.nointen <- subset(dat.tr, select = -mean_intensity)
dat.novarin <- subset(dat.tr, select = -var_intensity)

set.seed(432)
mySeeds <- sapply(simplify = FALSE, 1:101, function(u) sample(10^4, 10))
cvCtrl = caret::trainControl(method = "repeatedcv", number = 5, repeats = 3,
                             classProbs = TRUE, search= "random", seeds = mySeeds)
rfmodel <- train(Ycol~., dat.novarin, method='rf', trControl=cvCtrl, ntree= 100, metric = "Accuracy", tuneLength = 10, replace=FALSE)
rfmodel # mtry_best = 29
# Run a RF with more trees
set.seed( unlist(tail(mySeeds,1))[1])
rf_novarin<- randomForest(Ycol ~ ., data=dat.novarin, ntree=500, importance=TRUE, mtry = 29, type = "classification")

# tab <- table(dat.tr$Ycol,predict(rf_novarin,dat.tr))
# bind<- cbind(tab,diag(prop.table(tab,1)))
imp <- as.matrix(rf_novarin$importance)
rownames(imp)[order(as.matrix(imp[,1]), decreasing=TRUE)] #:)

votes <- predict(rf_novarin, dat.novarin, type="prob")[,1]
dcanc <- density(votes[which(dat.novarin$Ycol=="cancer", arr.ind=TRUE)])
dnorm <- density(votes[which(dat.novarin$Ycol!="cancer", arr.ind=TRUE)])
plot(dnorm, ylim=range(c(dcanc$y, dnorm$y)), xlim=c(0:1), main="Distribution of Voting Socres by Actual Class in
     Train Set with no var_intensity feature", col="Blue" , xlab="n=11573; 0 = Normal, 1 = Cancer")
lines(dcanc, col="Red")
legend("top", cex=0.75, pch=16, col=c("blue", "red"), legend=c("Normal", "Cancer"), ncol=2)
############################################
set.seed(432)
mySeeds <- sapply(simplify = FALSE, 1:101, function(u) sample(10^4, 10))
cvCtrl = caret::trainControl(method = "repeatedcv", number = 5, repeats = 3,
                             classProbs = TRUE, search= "random", seeds = mySeeds)
rfmodel <- train(Ycol~., dat.nointen, method='rf', trControl=cvCtrl, ntree= 100, metric = "Accuracy", tuneLength = 10, replace=FALSE)
rfmodel # mtry_best = 53
# Run a RF with more trees
set.seed( unlist(tail(mySeeds,1))[1])
rf_noIntensity<- randomForest(Ycol ~ ., data=dat.nointen, ntree=500, importance=TRUE, mtry = 53, type = "classification")

# tab <- table(dat.tr$Ycol,predict(rf_noIntensity,dat.tr)) 
# bind<- cbind(tab,diag(prop.table(tab,1)))
imp <- as.matrix(rf_noIntensity$importance)
rownames(imp)[order(as.matrix(imp[,1]), decreasing=TRUE)] #:)

votes <- predict(rf_noIntensity, dat.nointen, type="prob")[,1]
dcanc <- density(votes[which(dat.nointen$Ycol=="cancer", arr.ind=TRUE)])
dnorm <- density(votes[which(dat.nointen$Ycol!="cancer", arr.ind=TRUE)])
plot(dnorm, ylim=range(c(dcanc$y, dnorm$y)), xlim=c(0:1), main="Distribution of Voting Socres by Actual Class in
   Train Set without mean_intensity feature", col="Blue" , xlab="n=11573; 0 = Normal, 1 = Cancer")
lines(dcanc, col="Red")
legend("top", cex=0.75, pch=16, col=c("blue", "red"), legend=c("Normal", "Cancer"), ncol=2)
############################################
set.seed(432)
mySeeds <- sapply(simplify = FALSE, 1:101, function(u) sample(10^4, 10))
cvCtrl = caret::trainControl(method = "repeatedcv", number = 5, repeats = 3,
                             classProbs = TRUE, search= "random", seeds = mySeeds)
rfmodel <- train(Ycol~., dat.noboth, method='rf', trControl=cvCtrl, ntree= 100, metric = "Accuracy", tuneLength = 10, replace=FALSE)
rfmodel # mtry_best = 53
# Run a RF with more trees
set.seed( unlist(tail(mySeeds,1))[1])
rf_noboth<- randomForest(Ycol ~ ., data=dat.noboth, ntree=500, importance=TRUE, mtry = 53, type = "classification")

# tab <- table(dat.tr$Ycol,predict(rf_noboth,dat.tr))
# bind<- cbind(tab,diag(prop.table(tab,1)))
imp <- as.matrix(rf_noboth$importance)
rownames(imp)[order(as.matrix(imp[,1]), decreasing=TRUE)] #:)

votes <- predict(rf_noboth, dat.noboth, type="prob")[,1]
dcanc <- density(votes[which(dat.noboth$Ycol=="cancer", arr.ind=TRUE)])
dnorm <- density(votes[which(dat.noboth$Ycol!="cancer", arr.ind=TRUE)])
plot(dnorm, ylim=range(c(dcanc$y, dnorm$y)), xlim=c(0:1), main="Distribution of Voting Socres by Actual Class in
     Train Set without both var_intensity and mean_intensity", col="Blue" , xlab="n=11573; 0 = Normal, 1 = Cancer")
lines(dcanc, col="Red")
legend("top", cex=0.75, pch=16, col=c("blue", "red"), legend=c("Normal", "Cancer"), ncol=2)

# ################################################# Totally Balancing test proportion Experiment ##########################################
# # Split data into equal sized train for N and C, equal sized test for N and C (1158, 1157 respectively)
# subcan <- as.data.frame(dat.tr[1:5788,])
# subnor <- as.data.frame(dat.tr[5789:11573, ])
# mix1 <- sample(5788, 5788)
# mix2 <- sample(5785, 5785)
# train <- rbind(subcan[mix1[1:floor((5788/10)*8)],], subnor[mix2[1:floor((5785/10)*8)],])
# test <- rbind(subcan[mix1[(floor((5788/10)*8)+1):5788],], subnor[mix2[(floor((5785/10)*8)+1):5785],])
# rf_try <- randomForest(Ycol ~ ., data=train, ntree=500, importance=TRUE, mtry = 55, type = "classification")
# tab <- table(test$Ycol,predict(rf_try,test))
# bind<- cbind(tab,diag(prop.table(tab,1)))
# # We can see not much difference with random split matrix in 80% train section.
# #         cancer normal          
# # cancer   1011    147 0.8730570
# # normal     75   1082 0.9351772

##### Random Forests #####
setwd("Z:/QTP/Martial/LDO2017/scripts/GoodJunkClassifier")
dat <- read.csv("dat.csv", header=TRUE) 
library(randomForest)

# # Randomly assign training/validation/holdout sets
# set.seed(432)
# N <- dim(dat)[1]
# mix <- sample(N,N)
# node0t <- dat[mix[1:floor((N/10)*9)],]
# node0tst <- dat[mix[(floor((N/10)*9)+1):N],]

# Showing the proportion of diff groups
table(dat$Ycol) # good: 11486; junk: 104794
# table(node0t$Ycol) # good: 10325; junk: 94627
############################################### Build a RF with All Train ###############################################
# Step 1: First Good Junk Split
# Train
set.seed(432)                                                                # Down here is the boostrap size       # Tuned mtry = 30
rf_goodjunkII <- randomForest(x=dat[ ,-1], y=dat$Ycol, xtest = dat[ ,-1], ytest = dat$Ycol, keep.forest = TRUE,
                              ntree=500, importance=TRUE, sampsize=c('good'=10000, 'junk'= 10000),  mtry = 30,
                              replace = FALSE, type = "classification") 
rf_goodjunkII                 # OOB Confusion
prob=predict(rf_goodjunkII, dat[ , -1], type="prob") # OOB prob
probs0 = prob[,1]
# set.seed(432)
# tuneRF(dat[,-1], dat$Ycol, mtryStart=30, ntreeTry=50, stepFactor=2, improve=0.05,
#         trace=F, plot=TRUE, doBest=FALSE, sampsize=c('good'=10000, 'junk'=10000), replace=F)

# see variable importance
# imp <- as.matrix(rf_goodjunkII$importance)
# rownames(imp)[order(as.matrix(imp[,1]), decreasing=TRUE)] #:)

# Save first RF
setwd("Z:/QTP/Martial/LDO2017/scripts/GoodJunkClassifier")
save(rf_goodjunkII, file="rf_goodjunkII.RData")

# rfnode0p <- predict(rf_goodjunktry, node0tst)
# confusionMatrix(rfnode0p, node0tst$Ycol)

# Create nodes 1 and 2 for training set and holdout (validation) set
node1t <- dat[which(rf_goodjunkII$pred == "good", arr.ind=TRUE),]
node2t <- dat[which(rf_goodjunkII$pred == "junk", arr.ind=TRUE),]

# node1h <- node0tst[which(rfnode0p== "good",arr.ind=TRUE),]
# node2h <- node0tst[which(rfnode0p== "junk",arr.ind=TRUE),]
write.csv(node1t, "node1t.csv")
write.csv(node2t, "node2t.csv")
# write.csv(node1h, "node1h.csv")
# write.csv(node2h, "node2h.csv")


# Plot on OOB after node 0 
# probs0 <- predict(rf_goodjunktry, node0tst, type="prob")[,1]
dgood <- density(probs0[which(dat$Ycol=="good", arr.ind=TRUE)])
djunk <- density(probs0[which(dat$Ycol!="good", arr.ind=TRUE)])
plot(djunk, ylim=range(c(dgood$y, djunk$y)), xlim=c(0:1), main="Distribution of Probability of being Good by Actual Class in 
     OOB Set", col="Blue" , xlab="n=116280, njunk = 104794, ngood= 11486; 0 = Junk, 1 = Good")
lines(dgood, col="Red")
legend("top", cex=0.75, pch=16, col=c("blue", "red"), legend=c("Junk", "Good"), ncol=2)
##################################################################
# I.2 STEP 2
# Node 1 (Good) -> 3 (Good), 4 (Junk)
# Want to extract the Good cells from the rest
table(node1t$Ycol)
#good  junk 
#10029 15753 

set.seed(432)
rf_goodjunkIIGood <- randomForest(Ycol~., data = node1t,
                                  ntree=500, importance=TRUE, sampsize=c('good'=10000, 'junk'= 10000),  mtry = 60,
                                  replace = FALSE, type = "classification") 
# set.seed(432)
# tuneRF(node1t[,-1], node1t$Ycol, mtryStart=30, ntreeTry=50, stepFactor=2, improve=0.05,
#         trace=F, plot=TRUE, doBest=FALSE, sampsize=c('good'=10000, 'junk'=10000), replace=F)

# Save first RF
setwd("Z:/QTP/Martial/LDO2017/scripts/GoodJunkClassifier")
save(rf_goodjunkIIGood, file="rf_goodjunkIIGood.RData")

rf_goodjunkIIGood                 # OOB Confusion
prob1=predict(rf_goodjunkIIGood, node1t[ , -1], type="prob") # OOB prob
probs1 = prob1[,1]

# Plot on OOB after node 0 
dgood <- density(probs1[which(node1t$Ycol=="good", arr.ind=TRUE)])
djunk <- density(probs1[which(node1t$Ycol!="good", arr.ind=TRUE)])
plot(djunk, ylim=range(c(dgood$y, djunk$y)), xlim=c(0:1), main="Distribution of Probability of being Good by Actual Class in 
     OOB Set", col="Blue" , xlab="n=25782, njunk = 15753, ngood= 10029; 0 = Junk, 1 = Good")
lines(dgood, col="Red")
legend("top", cex=0.75, pch=16, col=c("blue", "red"), legend=c("Junk", "Good"), ncol=2)

# Create nodes 3 and 4 for training set and holdout (validation) set
node3t <- node1t[which(rf_goodjunkIIGood$pred == "good", arr.ind=TRUE),]
node4t <- node1t[which(rf_goodjunkIIGood$pred == "junk", arr.ind=TRUE),]
##################################################################
# I.2 STEP 3
# Node 2 (Junk) -> 5 (Good), 6 (Junk)
# Want to extract the Good cells from the rest
table(node2t$Ycol)
#good  junk 
#1457 89041 

set.seed(432)
rf_goodjunkIIJunk <- randomForest(Ycol~., data = node2t,
                                  ntree=500, importance=TRUE, sampsize=c('good'=1400, 'junk'= 1400),  mtry = 60,
                                  replace = FALSE, type = "classification") 
# set.seed(432)
# tuneRF(node2t[,-1], node2t$Ycol, mtryStart=30, ntreeTry=50, stepFactor=2, improve=0.05,
#         trace=F, plot=TRUE, doBest=FALSE, sampsize=c('good'=1400, 'junk'= 1400), replace=F)
# Save first RF
setwd("Z:/QTP/Martial/LDO2017/scripts/GoodJunkClassifier")
save(rf_goodjunkIIJunk, file="rf_goodjunkIIJunk.RData")

# Create nodes 5 and 6 for training set and holdout (validation) set
node5t <- node2t[which(rf_goodjunkIIJunk$pred == "good", arr.ind=TRUE),]
node6t <- node2t[which(rf_goodjunkIIJunk$pred == "junk", arr.ind=TRUE),]

# rfnode2p <- predict(rf_goodjunkonJunk, node2h)
# confusionMatrix(rfnode2p, node2h$Ycol)
# node5h <- node2h[which(rfnode2p== "good",arr.ind=TRUE),]
# node6h <- node2h[which(rfnode2p== "junk",arr.ind=TRUE),]

##################################################################
# I.2 STEP 4
# Node 3 (Good) -> 7 (Good), 8 (Junk)
# Want to extract the Good cells from the rest
table(node3t$Ycol)
#good  junk 
#4763  3527

set.seed(432)
rf_goodjunkIIGood_Good <- randomForest(Ycol~., data = node3t,
                                       ntree=500, importance=TRUE, sampsize=c('good'=3500, 'junk'= 3500),  mtry = 15,
                                       replace = FALSE, type = "classification") 
# set.seed(432)
# tuneRF(node3t[,-1], node3t$Ycol, mtryStart=30, ntreeTry=50, stepFactor=2, improve=0.05,
#        trace=F, plot=TRUE, doBest=FALSE, sampsize=c('good'=3500, 'junk'=3500), replace=F)

# Save first RF
setwd("Z:/QTP/Martial/LDO2017/scripts/GoodJunkClassifier")
save(rf_goodjunkIIGood_Good, file="rf_goodjunkIIGood_Good.RData")

# Create nodes 7 and 8 for training set and holdout (validation) set
node7t <- node3t[which(rf_goodjunkIIGood_Good$pred == "good", arr.ind=TRUE),]
node8t <- node3t[which(rf_goodjunkIIGood_Good$pred == "junk", arr.ind=TRUE),]
#################################################################
# I.2 STEP 5
# Node 4 (Junk) -> 9 (Good), 10 (Junk)
# Want to extract the Good cells from the rest
table(node4t$Ycol)
#good  junk 
#2926  12226

set.seed(432)
rf_goodjunkIIGood_Junk <- randomForest(Ycol~., data = node4t,
                                       ntree=500, importance=TRUE, sampsize=c('good'=2900, 'junk'= 2900),  mtry = 60,
                                       replace = FALSE, type = "classification") 
# set.seed(432)
# tuneRF(node4t[,-1], node4t$Ycol, mtryStart=30, ntreeTry=50, stepFactor=2, improve=0.05,
#        trace=F, plot=TRUE, doBest=FALSE, sampsize=c('good'=2900, 'junk'= 2900), replace=F)

# Save first RF
setwd("Z:/QTP/Martial/LDO2017/scripts/GoodJunkClassifier")
save(rf_goodjunkIIGood_Junk, file="rf_goodjunkIIGood_Junk.RData")

# Create nodes 7 and 8 for training set and holdout (validation) set
node9t <-  node4t[which(rf_goodjunkIIGood_Junk$pred == "good", arr.ind=TRUE),]
node10t <- node4t[which(rf_goodjunkIIGood_Junk$pred == "junk", arr.ind=TRUE),]

##################################################################
# I.2 STEP 6
# Node 5 (Good) -> 11 (Good), 12 (Junk)
# Want to extract the Good cells from the rest
table(node5t$Ycol)
#good  junk 
#

set.seed(432)
rf_goodjunkIIJunk_Good <- randomForest(Ycol~., data= node5t,
                                       ntree=500, importance=TRUE, sampsize=c('good'= , 'junk'= ),  mtry = 60,
                                       replace = FALSE, type = "classification") 
set.seed(432)
tuneRF(node5t[,-1], node5t$Ycol, mtryStart=30, ntreeTry=50, stepFactor=2, improve=0.05,
       trace=F, plot=TRUE, doBest=FALSE, sampsize=c('good'= , 'junk'= ), replace=F)

# Save first RF
setwd("Z:/QTP/Martial/LDO2017/scripts/GoodJunkClassifier")
save(rf_goodjunkIIJunk_Good, file="rf_goodjunkIIJunk_Good.RData")

# Create nodes 7 and 8 for training set and holdout (validation) set
node11t <- node5t[which(rf_goodjunkIIJunk_Good$pred == "good", arr.ind=TRUE),]
node12t <- node5t[which(rf_goodjunkIIJunk_Good$pred == "junk", arr.ind=TRUE),]
#################################################################
# I.2 STEP 7
# Node 6 (Junk) -> 13 (Good), 14 (Junk)
# Want to extract the Good cells from the rest
table(node6t$Ycol)
#good  junk 
#

set.seed(432)
rf_goodjunkIIJunk_Junk <- randomForest(Ycol~., data = node6t,
                                       ntree=500, importance=TRUE, sampsize=c('good'= , 'junk'= ),  mtry = 60,
                                       replace = FALSE, type = "classification") 
set.seed(432)
tuneRF(node6t[,-1], node6t$Ycol, mtryStart=30, ntreeTry=50, stepFactor=2, improve=0.05,
       trace=F, plot=TRUE, doBest=FALSE, sampsize=c('good'= , 'junk'= ), replace=F)

# Save first RF
setwd("Z:/QTP/Martial/LDO2017/scripts/GoodJunkClassifier")
save(rf_goodjunkIIJunk_Junk, file="rf_goodjunkIIJunk_Junk.RData")

# Create nodes 13 and 14 for training set and holdout (validation) set
node13t <- node6t[which(rf_goodjunkIIJunk_Junk$pred == "good", arr.ind=TRUE),]
node14t <- node6t[which(rf_goodjunkIIJunk_Junk$pred == "junk", arr.ind=TRUE),]




########################## Alternative Hard Negative Mining ###########################
realgoodind <- which(dat$Ycol == "good")
realjunkind <- which(dat$Ycol == "junk")
predgoodind <- which(rf_goodjunkII$predicted == "good")
predjunkind <- which(rf_goodjunkII$predicted == "junk")

predgood_labelasjunk <- dat[intersect(predgoodind, realjunkind), ]
realgoodset <- dat[realgoodind, ]
hardmdata <- rbind(predgood_labelasjunk, realgoodset)

table(hardmdata$Ycol) #good: 11486; junk: 15753
set.seed(432)
rf_HardNegaMining<- randomForest(Ycol~., data = hardmdata,
                                       ntree=500, importance=TRUE, sampsize=c('good'= 11000, 'junk'= 11000),  mtry = 30,
                                       replace = FALSE, type = "classification") 
set.seed(432)
tuneRF(hardmdata[,-1], hardmdata$Ycol, mtryStart=30, ntreeTry=50, stepFactor=2, improve=0.05,
       trace=F, plot=TRUE, doBest=FALSE, sampsize=c('good'= 11000, 'junk'= 11000), replace=F)

# Save first RF
setwd("Z:/QTP/Martial/LDO2017/scripts/GoodJunkClassifier")
save(rf_HardNegaMining, file="rf_HardNegaMining.RData")

############### Hard Positive mining? ###################
predjunk_labelasgood <- dat[intersect(predjunkind, realgoodind), ]
realjunkset <- dat[realjunkind, ]
hardmposdata <- rbind(predjunk_labelasgood, realjunkset)

table(hardmposdata$Ycol) #good: 1457; junk: 104794
set.seed(432)
rf_HardPosiMining<- randomForest(Ycol~., data = hardmposdata,
                                 ntree=500, importance=TRUE, sampsize=c('good'= 1400, 'junk'= 1400),  mtry = 15,
                                 replace = FALSE, type = "classification") 
set.seed(432)
tuneRF(hardmposdata[,-1], hardmposdata$Ycol, mtryStart=30, ntreeTry=50, stepFactor=2, improve=0.05,
       trace=F, plot=TRUE, doBest=FALSE, sampsize=c('good'= 1400, 'junk'= 1400), replace=F)

# Save first RF
setwd("Z:/QTP/Martial/LDO2017/scripts/GoodJunkClassifier")
save(rf_HardPosiMining, file="rf_HardPosiMining.RData")

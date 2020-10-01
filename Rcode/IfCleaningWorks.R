#Compare If Cleaning Works
######################################################## Overall ###########################################################
setwd("C:/Users/edong/Desktop/CompleteSubset")
textfiles <- list.files(pattern=".txt") # length32
l <- length(textfiles)
colnm <- c(morph, photo, discrete, markovian, fractal, runlength)
dat.anita <- data.frame(matrix(ncol = length(colnm), nrow = 0))
colnames(dat.anita) <- colnm
for(i in 1:l) {
  dat <- as.data.frame(read.table(textfiles[i], header=TRUE, sep = "" ))
  dat <- dat[colnm]
  dat.anita<- rbind.data.frame(dat.anita, dat)
}
# dim(dat.anita) = 4620*96 for combined group 1
 

setwd("X:/Oral/Histology/Imaging/COOLS/Estella_Trial_Uncomplete_mb0/subset")
textfiles <- list.files(pattern=".txt") # length32
l <- length(textfiles)
colnm <- c(morph, photo, discrete, markovian, fractal, runlength, "GROUP")
dat.estella <- data.frame(matrix(ncol = length(colnm), nrow = 0))
colnames(dat.estella) <- colnm
for(i in 1:l) {
  dat <- as.data.frame(read.table(textfiles[i], header=TRUE, sep = "" ))
  dat <- dat[colnm]
  dat.estella <- rbind.data.frame(dat.estella, dat)
}
# dim(dat.estella) = 5356*97
dat.estella_assume <- dat.estella[,-97] # Assume that all group 1&2 data are combined as one group.
                                        # 5356*96
dat.estella1 <- dat.estella[which(dat.estella$GROUP == 1), -97] #3801*96
dat.estella2 <- dat.estella[which(dat.estella$GROUP == 2), -97] #1555*96


# Run rf_cancer_normal on anita's and estella's.
# Based on combined 32 files
library(randomForest)
predprob.anita <- predict(rf_more, dat.anita, type="prob") [,1] 
LDO.anita <- mean(predprob.anita)

predprob.estella <- predict(rf_more, dat.estella_assume, type="prob") [,1] 
LDO.estella <- mean(predprob.estella)
# LDO.estella-LDO.anita = 0.3055011-0.2936052 = 0.0118959

####################################################### Each file #########################################################
setwd("C:/Users/edong/Desktop/CompleteSubset")
textfiles <- list.files(pattern=".txt") # length32
l <- length(textfiles)
ncellsAnita <- vector("numeric",l)
colnm <- c(morph, photo, discrete, markovian, fractal, runlength)
LDOAnita <- vector("numeric",l)
for(i in 1:l) {
  dat <- as.data.frame(read.table(textfiles[i], header=TRUE, sep = "" ))
  dat <- dat[colnm]
  ncellsAnita[i] <- nrow(dat)
  predprob <- predict(rf_more, dat, type="prob") [,1] 
  LDOAnita[i] <- mean(predprob)
}

setwd("X:/Oral/Histology/Imaging/COOLS/Estella_Trial_Uncomplete_mb0/subset")
textfiles <- list.files(pattern=".txt") # length32
LDOEstella <- vector("numeric",l)
ncellsEstella <- vector("numeric",l)
for(i in 1:l) {
  dat <- as.data.frame(read.table(textfiles[i], header=TRUE, sep = "" ))
  dat <- dat[colnm]
  ncellsEstella[i] <- nrow(dat)
  predprob <- predict(rf_more, dat, type="prob") [,1] 
  LDOEstella[i] <- mean(predprob)
}

SUM <- as.data.frame.matrix(cbind(textfiles, ncellsEstella, LDOEstella, ncellsAnita, LDOAnita))
write.csv(SUM, file = paste0("C:/Users/edong/Desktop/TestFile.csv"))








# Group of features(Can <- SSC/CIS + D2 + D1/HYP -> Nor)
is.installed <- function(mypkg) is.element(mypkg, installed.packages()[,1])
if(!is.installed("randomForest")) {
  install.packages('randomForest', dependencies=TRUE)
}
if(!is.installed("caret")) {
  install.packages('caret', dependencies=TRUE)
}

allf <- c( "CellNb"        ,   "CellId"       ,    "LayNo"         ,   "VorArea"       ,  "VorPeri"       ,   "VorRF"          ,  "DelNeiNb"         ,"NeaNei" ,         
           "DelNeaNei"     ,   "Del3NeiDist"    ,  "ClusCoef"      ,   "VorArea_Mean"  ,  "VorPeri_Mean"  ,   "VorRF_Mean"   ,    "DelNeiNb_Mean"  ,  "NeaNei_Mean"      ,"DelNeaNei_Mean" , 
           "Del3NeiDist_Mean", "ClusCoef_Mean"  ,  "VorArea_Stdv"  ,   "VorPeri_Stdv"  ,  "VorRF_Stdv"   ,    "DelNeiNb_Stdv",    "NeaNei_Stdv"    ,  "DelNeaNei_Stdv"  , "Del3NeiDist_Stdv",
           "ClusCoef_Stdv" ,   "VorArea_Skew"    , "VorPeri_Skew"  ,   "VorRF_Skew"   ,   "DelNeiNb_Skew" ,   "NeaNei_Skew"   ,   "DelNeaNei_Skew" ,  "Del3NeiDist_Skew", "ClusCoef_Skew" ,
           "VorArea_Kurt"  ,   "VorPeri_Kurt"    , "VorRF_Kurt"   ,    "DelNeiNb_Kurt" ,  "NeaNei_Kurt"  ,    "DelNeaNei_Kurt" ,  "Del3NeiDist_Kurt", "ClusCoef_Kurt"  ,  "area"           , 
           "area_m"      ,     "mean_radius"     , "max_radius"    ,   "mean_radius_m" ,  "max_radius_m"  ,   "var_radius"     ,  "sphericity"   ,    "eccentricity"  ,   "cell_orient"     ,
           "inertia_shape"  ,  "compactness"    ,  "elongation"   ,    "energy"        ,  "correlation"  ,    "contrast"      ,   "homogeneity"   ,   "cl_shade"      ,   "cl_prominence"   ,
           "mean_intensity" ,  "var_intensity"   , "fractal_dimen" ,   "fractal1_area",   "fractal2_area" ,   "freq_low_fft"  ,   "freq_high_fft" ,   "harmon01_fft"  ,   "harmon02_fft"    ,
           "harmon03_fft"  ,   "harmon04_fft"    , "harmon05_fft"   ,  "harmon06_fft"  ,  "harmon07_fft"  ,   "harmon08_fft"  ,   "harmon09_fft" ,    "harmon10_fft"  ,   "harmon11_fft"    ,
           "harmon12_fft"  ,   "harmon13_fft"   ,  "harmon14_fft"   ,  "harmon15_fft"  ,  "harmon16_fft"  ,   "harmon17_fft"  ,   "harmon18_fft"  ,   "harmon19_fft"  ,   "harmon20_fft"    ,
           "harmon21_fft"  ,   "harmon22_fft"   ,  "harmon23_fft"  ,   "harmon24_fft"  ,  "harmon25_fft"  ,   "harmon26_fft"  ,   "harmon27_fft"  ,   "harmon28_fft"  ,   "harmon29_fft"    ,
           "harmon30_fft"  ,   "harmon31_fft"  ,   "harmon32_fft"  ,   "DNA_Index"     ,  "DNA_Amount"    ,   "OD_maximum"    ,   "OD_variance"   ,   "OD_skewness"   ,   "OD_kurtosis"     ,
           "lowDNAarea"  ,     "medDNAarea"    ,   "hiDNAarea"     ,   "lowDNAamnt"    ,  "medDNAamnt"    ,   "hiDNAamnt"     ,   "lowDNAcomp"    ,   "medDNAcomp"    ,  "hiDNAcomp"        ,
           "mhDNAcomp"   ,     "low_av_dst"     ,  "med_av_dst"    ,   "hi_av_dst"     ,  "mh_av_dst"     ,   "lowVSmed_DNA"  ,   "lowVShigh_DNA" ,   "lowVSmh_DNA"   ,   "low_den_obj"     ,
           "med_den_obj"  ,    "high_den_obj"   ,  "entropy"        ,  "den_lit_spot"   , "den_drk_spot"  ,   "range_extreme" ,   "range_average" ,   "center_of_grav",   "low_cntr_mass"   ,
           "med_cntr_mass",    "high_cntr_mass",   "texture_orient" ,  "size_txt_orient", "short_runs1"   ,   "short_runs2"   ,   "short_runs3"   ,   "short_runs4"   ,   "short0_runs"     ,
           "short45_runs"  ,   "short90_runs"  ,   "short135_runs"  ,  "long_runs1"   ,   "long_runs2"    ,   "long_runs3"    ,   "long_runs4"    ,  "long0_runs"      , "long45_runs" ,
           "long90_runs" ,     "long135_runs"  ,   "gray_level1"    ,  "gray_level2" ,    "gray_level3"   ,   "gray_level4"   ,   "gray0_level"   ,   "gray45_level"  ,   "gray90_level" ,   
           "gray135_level" ,   "run_length1"   ,   "run_length2"    ,  "run_length3"  ,   "run_length4"   ,   "run0_length"    ,  "run45_length"   ,  "run90_length"   ,  "run135_length" ,  
           "run_percent1"  ,   "run_percent2"  ,   "run_percent3"   ,  "run_percent4"   , "run0_percent"  ,   "run45_percent" ,   "run90_percent"  ,  "run135_percent",   "x_centroid"     , 
           "y_centroid"  ,     "vor_area"     ,    "brratio"        ,  "bgratio"        , "rgratio"        ,  "brratiov"      ,   "bgratiov"     ,    "rgratiov"      ,   "voron_x"         ,
           "voron_y"    ,      "cytoarea"      ,   "cytoratio"      ,  "nb_mindist"     , "nb_maxdist"     ,  "nb_meandist" ,     "nb_vardist"  ,     "nb_count"      ,   "nb_layer"        ,
           "nb_orient"   ,     "nb_basdist"   ,    "background"     ,  "screenx"       ,  "screeny"        ,  "diagnosis"    ,    "prob"        ,     "GROUP"         ,   "num"             ,
           "locks"        ,    "class"         ,    "stg_x"          ,  "stg_y"      ,     "stg_z" )


#Red+Non-Markovian group
redg <- c( "CellNb"      ,    "CellId"      ,    "LayNo"        ,   "ClusCoef"    ,    "ClusCoef_Mean" , 
           "ClusCoef_Stdv" ,  "ClusCoef_Skew" ,  "ClusCoef_Kurt"  , "area"        ,    "mean_radius" ,   
           "max_radius"    ,  "cell_orient"   ,  "harmon06_fft" ,   "harmon07_fft"  ,  "harmon08_fft"  , 
           "harmon09_fft" ,   "harmon10_fft" ,   "harmon11_fft"  ,  "harmon12_fft"   , "harmon13_fft"   ,
           "harmon14_fft"  ,  "harmon15_fft"  ,  "harmon16_fft" ,   "harmon17_fft"  ,  "harmon18_fft" ,  
           "harmon19_fft"  ,  "harmon20_fft"  ,  "harmon21_fft"  ,  "harmon22_fft"   , "harmon23_fft"   ,
           "harmon24_fft"  ,  "harmon25_fft"  ,  "harmon26_fft"  ,  "harmon27_fft"  ,  "harmon28_fft"  , 
           "harmon29_fft"  ,  "harmon30_fft"  ,  "harmon31_fft" ,   "harmon32_fft" ,   "DNA_Amount"   ,  
           "texture_orient" , "size_txt_orient" ,"x_centroid" ,     "y_centroid"  ,    "vor_area"  ,     
           "brratio"   ,      "bgratio"    ,     "rgratio"     ,    "brratiov"    ,    "bgratiov"   ,    
           "rgratiov"   ,     "voron_x"    ,     "voron_y"    ,     "cytoarea"  ,      "cytoratio" ,     
           "nb_mindist"    ,  "nb_maxdist"   ,   "nb_meandist" ,    "nb_vardist"  ,    "nb_count"  ,     
           "nb_layer"    ,    "nb_orient"   ,    "nb_basdist"   ,   "background"  ,    "screenx"   ,     
           "screeny"    ,     "diagnosis"  ,     "prob"       ,     "GROUP"      ,     "num"    ,        
           "locks"  ,         "class"        ,   "stg_x"       ,    "stg_y"       ,    "stg_z",  
           "den_lit_spot"   , "den_drk_spot"   , "range_extreme"  , "range_average" ,  "center_of_grav"  )

#Archi
archi <- c( "VorArea"      ,    "VorPeri"       ,   "VorRF"      ,      "DelNeiNb"   ,      "NeaNei" ,         
            "DelNeaNei"    ,    "Del3NeiDist"    ,  "VorArea_Mean"   ,  "VorPeri_Mean" ,    "VorRF_Mean",     
            "DelNeiNb_Mean" ,   "NeaNei_Mean"   ,   "DelNeaNei_Mean" ,  "Del3NeiDist_Mean", "VorArea_Stdv",    
            "VorPeri_Stdv"  ,   "VorRF_Stdv"    ,   "DelNeiNb_Stdv"  ,  "NeaNei_Stdv"  ,    "DelNeaNei_Stdv",  
            "Del3NeiDist_Stdv", "VorArea_Skew"  ,   "VorPeri_Skew"   ,  "VorRF_Skew" ,      "DelNeiNb_Skew",   
            "NeaNei_Skew"   ,   "DelNeaNei_Skew",   "Del3NeiDist_Skew", "VorArea_Kurt" ,    "VorPeri_Kurt" ,   
            "VorRF_Kurt"  ,     "DelNeiNb_Kurt",    "NeaNei_Kurt"  ,    "DelNeaNei_Kurt"  , "Del3NeiDist_Kurt")

################################################################################################################################################

#Morph 
morph <- c("area_m"    ,    "mean_radius_m" ,"max_radius_m" , "var_radius"  ,  "sphericity" ,   "eccentricity" ,"inertia_shape", "compactness",  
           "elongation" ,   "freq_low_fft" , "freq_high_fft" ,"harmon01_fft" , "harmon02_fft" , "harmon03_fft" , "harmon04_fft" , "harmon05_fft" ) #16

#Photo
photo <-  c("DNA_Index" ,"OD_maximum"  ,"OD_variance", "mean_intensity", 
            "var_intensity", "OD_skewness" , "OD_kurtosis") #7

#Discrete 
discrete <-  c("lowDNAarea",  "medDNAarea",  "hiDNAarea"  ,  "lowDNAamnt" ,  "medDNAamnt", "hiDNAamnt",    "lowDNAcomp" ,   "medDNAcomp" ,  "hiDNAcomp" , 
               "mhDNAcomp"  , "low_av_dst",  "med_av_dst" ,  "hi_av_dst" ,   "mh_av_dst" , "lowVSmed_DNA", "lowVShigh_DNA", "lowVSmh_DNA" , "low_den_obj" , 
               "med_den_obj", "high_den_obj","low_cntr_mass","med_cntr_mass","high_cntr_mass" ) #23

#Markovian
markovian <- c("entropy", "energy", "correlation" , "contrast", "homogeneity", "cl_shade" ,"cl_prominence") #70

#Fractal
fractal <- c("fractal_dimen" ,"fractal1_area", "fractal2_area" ) #3

#RunLength 
#Left side 4*4 variables are generated from the right side 4*4 variables
runlength <-  c("short_runs1",  "short_runs2",  "short_runs3",  "short_runs4",   "short0_runs",  "short45_runs",  "short90_runs",  "short135_runs",  
                "long_runs1",   "long_runs2" ,  "long_runs3",   "long_runs4" ,   "long0_runs" ,  "long45_runs",   "long90_runs",   "long135_runs" ,  
                "gray_level1",  "gray_level2",  "gray_level3",  "gray_level4",   "gray0_level",  "gray45_level",  "gray90_level",  "gray135_level" , 
                "run_length1",  "run_length2",  "run_length3",  "run_length4",   "run0_length",  "run45_length",  "run90_length",  "run135_length", 
                "run_percent1", "run_percent2", "run_percent3", "run_percent4",  "run0_percent", "run45_percent", "run90_percent", "run135_percent") #40


###################################################################################################################################################

# GENERAL SETTINGS
require(tcltk)
# Set the folder path for where the normal and cancer txt files are.
ReturnVal <- tkmessageBox(title = "Step 1", message = "Please select the directory for the train set", icon = "info", type = "ok")
direct1 <- tclvalue(tkchooseDirectory(initialdir='Z:/QTP/Martial/LDO2017/TrainSet'))
#Need function: getX() for cell threshold.-> for ration score
################################################ Ready in training data ########################################
# preprocesstrain <- function(wd){

# Good #11573

# This Function is unnecessary for < R 3.3.1, but I am limited to the Java version and has to use R 2.3.1
# In R 2.3.1: manyfolders <- list.dirs(path = direct1, recursive = FALSE) 
list.dirself <- function(path=".", pattern=NULL, all.dirs=FALSE,
                         full.names=FALSE, ignore.case=FALSE) {
  # use full.names=TRUE to pass to file.info
  all <- list.files(path, pattern, all.dirs,
                    full.names=TRUE, recursive=FALSE, ignore.case)
  dirs <- all[file.info(all)$isdir]
  # determine whether to return full names or just dir names
  if(isTRUE(full.names))
    return(dirs)
  else
    return(basename(dirs))
}

manyfolders <- list.dirself(path = direct1, full.names = TRUE) 
folderList <- manyfolders[c(3,6)]#Only take the Cancertxt and Normaltxt from [Cancer, Cancer9txt, Cancertxt, Normal, Normal9txt, Normaltxt]
wordlist <- c("good", "junk")
# Build an empty data.frame with all of the features.
colnm <- c("DNA_Amount", "area", morph, photo, discrete, markovian, fractal, runlength)
dfacc1 <- data.frame(matrix(ncol = length(colnm), nrow = 0))
colnames(dfacc1) <- colnm
# Build another empty data.frame with the Ycol
df1 <- data.frame(matrix(ncol = 1, nrow = 0))

for(t in 1:2){
  setwd(folderList[t])
  fileList <- list.files(path = folderList[t], pattern=".txt")
  l <- length(fileList)
  for(i in 1:l) {
    dat <- as.data.frame(read.table(fileList[i], header=TRUE,sep = "" ))
    if (colnames(dat)[1] == "UnitId"){
      dat <- subset(dat, select = -UnitId)
    }
    dat <- dat[, colnm]
    dfacc1<- rbind.data.frame(dfacc1,dat)
  }
}
numgood <- nrow(dfacc1)
df0 <- data.frame(rep(wordlist[1], times = numgood))
df1 <- rbind(df1,df0)
dat.tr1 <- cbind(dfacc1,df1)


# Junk; 
# Build an empty data.frame with all of the features.
colnm <- c("DNA_Amount", "area", morph, photo, discrete, markovian, fractal, runlength)
dfacc2 <- data.frame(matrix(ncol = length(colnm), nrow = 0))
colnames(dfacc2) <- colnm
# Build another empty data.frame with the Ycol
df2 <- data.frame(matrix(ncol = 1, nrow = 0))

folders <- manyfolders[c(2,5)] # Only Cancer9txt and Normal9txt from [Cancer, Cancer9txt, Cancertxt, Normal, Normal9txt, Normaltxt]
for(t in 1:2){
  setwd(folders[t])
  files <- list.files(path = folders[t], pattern=".txt")
  p <- length(files)
  for(i in 1:p) {
    dat <- as.data.frame(read.table(files[i], header=TRUE,sep = "" ))
    if (colnames(dat)[1] == "UnitId"){
      dat <- subset(dat, select = -UnitId)
    }
    dat <- dat[, colnm]
    dfacc2<- rbind.data.frame(dfacc2,dat)
  }
}
numjunk <- nrow(dfacc2)
df0 <- data.frame(rep(wordlist[2], times = numjunk))
df2 <- rbind(df2,df0)
dat.tr2 <- cbind(dfacc2,df2)

print("Number of good and junk training cells")
c(numgood, numjunk) #11573 good, 163097 junk
colnames(dat.tr1)[length(dat.tr1)] <- "Ycol"
colnames(dat.tr2)[length(dat.tr2)] <- "Ycol"

dat.tr <- rbind(dat.tr1, dat.tr2); 
numtotal <- nrow(dat.tr)          #174670
dat <- dat.tr

###################################################### Preprocess and delete bad objects ######################################
####
# remove objects with small fractal dimension: the criterion is fractal_dimen <= 1.5
n1 <- nrow(dat)
fractal_dimen <- dat$fractal_dimen
F <- any(fractal_dimen <= 1.5)

if (F==TRUE){
  print("Warning: cells with small fractal dimension detected. The rows with fractal_dimen <= 5 will be removed to proceed with the algorithm")
  Smallf <- which(fractal_dimen <= 1.5, arr.ind=TRUE)
  dat <- dat[-Smallf,]
  rm(fractal_dimen); rm(Smallf); rm(F)
  n2 <- nrow(dat)
  print(paste((n1-n2), "rows with large background (DNA_Index>=5) have been deleted."))
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

###################################################### Ultimate Train Set #####################################################
# # Subsample junk to make good & junk balance.
# set.seed(432)
# mix<-sample(numgood,numgood)
# dat.tr2sub <- dat.tr2[mix[1:numgood],]

# The Ultimate Traning Set, with 6 feature groups # 11573*2 = 23146 cells * (96 features+ 1 response), no archi, red, and non-markovian.

colnmsub <- c("Ycol", morph, photo, discrete, markovian, fractal, runlength )
dat.trprocessed <- dat[,colnmsub]
length(which(dat.trprocessed$Ycol == "good")) #11498
length(which(dat.trprocessed$Ycol == "junk")) #105127

dat.trnew <- dat.trprocessed # 116625 * (96+1)


# Export dat.trnew so we don't have to do the cleaning steps again
# setwd("E:/")
# write.csv(dat.trnew, file="dat.trnew.csv", row.names=FALSE)

# Export dat so we don't have to do the cleaning steps again
setwd("Z:/QTP/Martial/LDO2017/scripts/GoodJunkClassifier")
write.csv(dat, file="dat.csv", row.names=FALSE)

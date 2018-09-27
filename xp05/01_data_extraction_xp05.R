# Wed Aug 15 13:17:30 2018 ------------------------------
###########
#### RWA & APT
###########

# Data preparation --------------------------------------------------------

# Optional generic preliminaries:

#graphics.off() # This closes all of R's graphics windows.
#rm(list=ls())  # Careful! This clears all of R's memory!

## install, load and update necessary packages if required##

if (!require("pacman")) install.packages("pacman")
p_load(dplyr, 
       stringi,
       data.table,
       reshape2,
       rlist,
       readr,
       install = TRUE,
       update = getOption("pac_update"),
       character.only = FALSE)

# We get the current working directory and it will be used to load the data

curwd <- dirname(rstudioapi::getActiveDocumentContext()$path)

# set the working directory to the folder containing the data

setwd(paste(curwd,"/raw_data/",sep=""))

#-----------------------------------
## We did this only once for pseudonymisation
## Not necessary to run this
## uncomment if you want to do it anyway

# create a list with files names contained in the folder
#files <- list.files()

# read these files
#files_read <- lapply(files, function(i){read.csv(i, header=FALSE, stringsAsFactors=F)})

# pseudonymisation of id and ip
#pseudon = function(x) {
#  x[2,c(8,10)] <- "deleted for pseudonymisation"
#  return(x)
#}
#files_read <- lapply(files_read, pseudon)

# remove original files
#file.remove(files)

# create new ones with random name and makes id/ip
#lapply(files_read, function (x) write.table(x, file=paste(stri_rand_strings(1, 12, '[A-Z]'), "csv", sep=".") ) )
#-----------------------------------

# create a list with files names contained in the folder
files <- list.files()

create_dfs <- function(x) {
  out <- read.table(x, skip = 3, header = TRUE)
  out <- out[,-1]
  names <- gsub(".csv", "", x)
  cbind(ppt=names, out)
}

all_raw <- lapply(files, create_dfs)

dat_all <- rbindlist(all_raw)

setwd("../")
write.csv(dat_all,file="table_all_data.csv", row.names=F)

##Keep relevant columns
df <- data.table(select(dat_all, ppt, subjectGroup, condition1, stim1, stim2, response, RT,stimFormat))

## Recode families of Greebles

df$family<-df$stim1
df[family %in% c("M1","M2", "M3","M4","M5","M6","M7","M8"), family := "M"]
df[family %in% c("B1","B2", "B3","B4","B5","B6","B7","B8"), family := "B"]


## Recode US valence (first block, i.e., evaluative conditioning)

# subjectGroup 1, 2: B+ / M-
# subjectGroup 3, 4: B- / M+

df[subjectGroup %in% c("1", "2") & family %in% "M", usvalence := -0.5]
df[subjectGroup %in% c("1", "2") & family %in% "B", usvalence := 0.5]
df[subjectGroup %in% c("3", "4") & family %in% "M", usvalence := 0.5]
df[subjectGroup %in% c("3", "4") & family %in% "B", usvalence := -0.5]


## Recode experimental condition
# order of ratings type: 0.5 = explicit ratings before implicit ratings
# -0.5 = implicit ratings before explicit ratings


df[subjectGroup %in% c("1", "2"), order := 0.5]
df[subjectGroup %in% c("3", "4"), order := -0.5]

######Create RWA levels dataframe######

RWA <- data.table(filter(df, stimFormat == "word"))

##Long to wide dataframe
RWA <- dcast(RWA, ppt ~ condition1, value.var="response")

#Transform variables to numeric format
RWA <- setDT(RWA)
RWA[, (2:31) := lapply(.SD, as.numeric), .SDcols = (2:31)]

#reversed items. RWA items 2, 4, 6, 8, 10, 12, 14.
RWA<- within(RWA,{
  RWA2R<- 8-RWA2R
  RWA4R<- 8-RWA4R
  RWA6R<- 8-RWA6R
  RWA8R<-8-RWA8R
  RWA10R<- 8-RWA10R
  RWA12R<- 8-RWA12R
  RWA14R<- 8-RWA14R
})

#RWA score
RWA<- within(RWA,{
  RWAscore <- rowSums(RWA[, c("RWA1","RWA10R","RWA11","RWA12R","RWA13","RWA14R","RWA15","RWA2R","RWA3","RWA4R", "RWA5","RWA6R","RWA7","RWA8R","RWA9")], na.rm=TRUE)
})

#Check data & See data

#boxplot(RWA$RWAscore)
#hist(RWA$RWAscore)

quart1 <- quantile(RWA$RWAscore)[2]
quart3 <- quantile(RWA$RWAscore)[4]
iqr <- IQR(RWA$RWAscore)

#which(RWA$RWAscore<quart1-3*iqr)
#which(RWA$RWAscore>quart3+3*iqr)

# Remove highly extreme values of RWA
RWA$RWAscore <-  ifelse (RWA$RWAscore<quart1-3*iqr | RWA$RWAscore>quart3+3*iqr, NA, RWA$RWAscore)
RWA <- RWA[which(!is.na(RWA$RWAscore)),]

# scale RWA score
RWA$RWAscore<-scale(RWA$RWAscore, center = TRUE, scale = TRUE)

######Create APT ratings dataframe######

APT <- data.table(filter(df, condition1 == "IMPLIratings"))

##Recode APT words valence: 0.5 = positive; -0.5 = negative
APT$wordvalence<-APT$stim2
APT[wordvalence %in% c("Suicide","Pain", "Injury","Violence","Fatal","Lethal","Cruelty","Killer"), wdval := -0.5]
APT[wordvalence %in% c("Confident","Generous", "Excellent","Delight","Smiling","Cheerful","Praise","Pleasure"), wdval := 0.5]

#Code accuracy
APT$acc <- NA
APT$acc[APT$wdval == 0.5 & APT$response== "p"] <- 1
APT$acc[APT$wdval == -0.5 & APT$response == "n"] <- 1
APT$acc[APT$wdval == 0.5 & APT$response == "n"] <- 0
APT$acc[APT$wdval == -0.5 & APT$response == "p"] <- 0

# Number of errors
length(which(APT$acc == 0)) / length(which(APT$acc == 1)) * 100

##Recode congruent trials
APT$congruent<-NA
APT$congruent[APT$wdval == 0.5 & APT$usvalence == 0.5] <- 0.5
APT$congruent[APT$wdval == -0.5 & APT$usvalence == -0.5] <- 0.5
APT$congruent[APT$wdval == 0.5 & APT$usvalence == -0.5] <- -0.5
APT$congruent[APT$wdval == -0.5 & APT$usvalence == 0.5] <- -0.5

######Create direct ratings dataframe######

direct <- data.table(filter(df, condition1 == "EXPLIratings"))

######Merge RWA with  APT ratings dataframe######
apt_df<-full_join(APT, RWA)

#Check data & See data

apt_df$RT <-  ifelse (apt_df$acc == 0, NA, apt_df$RT)

#boxplot(apt_df$RT)
#hist(apt_df$RT)

fastRT <- which(apt_df$RT<200)
slowRT <- which(apt_df$RT>1500)

length(fastRT)
length(slowRT)

# Number of RTs out of reliable range
(length(fastRT) + length(slowRT)) / length(apt_df$RT) * 100

# Remove highly extreme values of apt_df
apt_df$RT <-  ifelse (apt_df$RT<200 | apt_df$RT>1500, NA, apt_df$RT)
hist(apt_df$RT)
apt_df <- apt_df[which(!is.na(apt_df$RT)),]

######Merge RWA with  direct ratings dataframe######
expli_df<-full_join(direct, RWA)
expli_df$response <- as.numeric(as.character(expli_df$response))

# for IDA

xp5_df <- expli_df[expli_df$order == 0.5,]
col2k <- c("ppt", "stim1", "RWAscore", "usvalence", "response")
xp5_df <- xp5_df[,col2k]
xp5_df$XP <- "XP05"

setwd("../ida")
write.csv(xp5_df,file="XP05.csv", row.names=F)




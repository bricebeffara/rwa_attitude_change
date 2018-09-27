###########
#### RWA & double counter-conditioning: Experiment 2 (+time)
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

# create new ones with random name and mask id/ip
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

## Keep relevant columns

df <- data.table(select(dat_all, ppt, subjectGroup, condition1, stim1, stim2, response, RT,stimFormat, trialNo))

## Recode families of Greebles

df$family<-df$stim1
df[family %in% c("M1","M2", "M3","M4","M5","M6","M7","M8"), family := "M"]
df[family %in% c("B1","B2", "B3","B4","B5","B6","B7","B8"), family := "B"]

## Recode US valence (first block, i.e., evaluative conditioning)

# subjectGroup 1, 3, 5, 7: B+ / M-
# subjectGroup 2, 4, 6, 7: B- / M+

df[subjectGroup %in% c("1", "3", "5", "7") & family %in% "M", usvalence := -0.5]
df[subjectGroup %in% c("1", "3", "5", "7") & family %in% "B", usvalence := 0.5]
df[subjectGroup %in% c("2", "4", "6", "8") & family %in% "M", usvalence := 0.5]
df[subjectGroup %in% c("2", "4", "6", "8") & family %in% "B", usvalence := -0.5]

## Recode experimental condition

# subjectGroup 1, 2: conditioning - counterconditioning - ratings                       = condition "-0.25"
# subjectGroup 3, 4: conditioning - counterconditioning - counterconditioning - ratings = condition "0.75"
# subjectGroup 5, 6: conditioning - counterconditioning - time delay          - ratings = condition "-0.25"
# subjectGroup 3, 4: conditioning - time delay          - counterconditioning - ratings = condition "-0.25"

df[subjectGroup %in% c("1", "2"), contrast1 := -0.25]
df[subjectGroup %in% c("3", "4"), contrast1 := 0.75]
df[subjectGroup %in% c("5", "6"), contrast1 := -0.25]
df[subjectGroup %in% c("7", "8"), contrast1 := -0.25]

df[subjectGroup %in% c("1", "2"), contrast2 := 0.5]
df[subjectGroup %in% c("3", "4"), contrast2 := 0]
df[subjectGroup %in% c("5", "6"), contrast2 := -0.25]
df[subjectGroup %in% c("7", "8"), contrast2 := -0.25]

df[subjectGroup %in% c("1", "2"), contrast3 := 0]
df[subjectGroup %in% c("3", "4"), contrast3 := 0]
df[subjectGroup %in% c("5", "6"), contrast3 := 0.25]
df[subjectGroup %in% c("7", "8"), contrast3 := -0.25]

df[subjectGroup %in% c("1", "2"), condition := "one"]
df[subjectGroup %in% c("3", "4"), condition := "two"]
df[subjectGroup %in% c("5", "6"), condition := "one_time"]
df[subjectGroup %in% c("7", "8"), condition := "time_one"]

## Create RWA score dataframe

RWA <- df[condition1 %in% "RWA",] # subset relevant rows
RWA <- data.table(dcast(RWA, ppt ~ trialNo, value.var="response")) # long to wide format
colnames(RWA) <- paste("RWA", colnames(RWA), sep = "_") # rename wide format columns
colnames(RWA)[1] <- "ppt" # rename wide format columns

RWA[, (2:16) := lapply(.SD, as.numeric), .SDcols = (2:16)] # set as numeric

# reversed items. RWA items 2, 4, 6, 10, 12, 14.
RWA<- within(RWA,{
  RWA2R<- 8-RWA_2
  RWA4R<- 8-RWA_4
  RWA6R<- 8-RWA_6
  RWA10R<- 8-RWA_10
  RWA8R<-8-RWA_8
  RWA12R<- 8-RWA_12
  RWA14R<- 8-RWA_14
})

# compute RWA score
RWA<- within(RWA,{
  RWAscore <- rowSums(RWA[, c("RWA_1","RWA10R","RWA_11","RWA12R","RWA_13","RWA14R","RWA_15","RWA2R","RWA_3","RWA4R", "RWA_5","RWA6R","RWA_7","RWA8R","RWA_9")], na.rm=TRUE)
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

## create evaluative ratings dataframe ##

ratings <- df[condition1 %in% c("ratings1","ratings2","ratings3","ratings4",
                                "ratings5","ratings6","ratings7","ratings8"), ]

## Merge RWA with ratings dataframe 

doubletime_df<-full_join(ratings, RWA)

double_one_df <- subset(doubletime_df, condition == "one" | condition == "two")
double_one_df$counter2 <- ifelse( double_one_df$condition == "one", -0.5, 0.5)

double_onetime_df <- subset(doubletime_df, condition == "one_time" | condition == "two")
double_onetime_df$counter2 <- ifelse( double_onetime_df$condition == "one_time", -0.5, 0.5)

double_timeone_df <- subset(doubletime_df, condition == "time_one" | condition == "two")
double_timeone_df$counter2 <- ifelse( double_timeone_df$condition == "time_one", -0.5, 0.5)

# for IDA

xp8_df <- doubletime_df[doubletime_df$condition == "one",]
col2k <- c("ppt", "stim1", "RWAscore", "usvalence", "response")
xp8_df <- xp8_df[,col2k]
xp8_df$XP <- "XP08"

setwd("../ida")
write.csv(xp8_df,file="XP08.csv", row.names=F)



# Tue Jun 26 11:06:36 2018 ------------------------------
###########
#### RWA & Warning
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
       yarrr,
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
df <- data.table(select(dat_all, ppt, subjectGroup, condition1, stim1, key, response, RT,stimFormat, trialNo))

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

# subjectGroups 1, 2, 5, 6: Warning
# subjectGroups 3, 4, 7, 8: No warning

df[subjectGroup %in% c("1", "2", "5", "6"), warn := 0.5]
df[subjectGroup %in% c("3", "4", "7", "8"), warn := -0.5]

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

# scale RWA score
RWA$RWAscore<-scale(RWA$RWAscore, center = TRUE, scale = TRUE)

## create evaluative ratings dataframe ##

ratings <- df[condition1 %in% c("ratings1","ratings2","ratings3","ratings4"), ]

## Merge RWA with ratings dataframe 

warn_df <- full_join(ratings, RWA)
warn_df <- warn_df[!is.na(warn_df$response),]
warn_df$response <- as.numeric(as.character(warn_df$response))




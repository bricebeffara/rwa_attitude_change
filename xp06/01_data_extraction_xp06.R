# Wed Aug 15 13:17:30 2018 ------------------------------
###########
#### RWA & AMP
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

# We get the current working direct_dfory and it will be used to load the data

curwd <- dirname(rstudioapi::getActiveDocumentContext()$path)

# set the working direct_dfory to the folder containing the data

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
df <- data.table(select(dat_all, ppt, subjectGroup, condition1, trialNo, stim1, response, RT, orderFile, randomBlock))

##Recode Greebles families

df$family[df$orderFile>=35 & df$orderFile<=42 & df$subjectGroup == "1"]<- "B"
df$family[df$orderFile>=43 & df$orderFile<=50 & df$subjectGroup == "1"]<- "M"
df$family[df$orderFile>=83 & df$orderFile<=90 & df$subjectGroup == "2"]<- "B"
df$family[df$orderFile>=91 & df$orderFile<=98 & df$subjectGroup == "2"]<- "M"
df$family[df$orderFile>=67 & df$orderFile<=74 & df$subjectGroup == "3"]<- "B"
df$family[df$orderFile>=75 & df$orderFile<=82 & df$subjectGroup == "3"]<- "M"
df$family[df$orderFile>=115 & df$orderFile<=122 & df$subjectGroup == "4"]<- "B"
df$family[df$orderFile>=123 & df$orderFile<=130 & df$subjectGroup == "4"]<- "M"

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


df[subjectGroup %in% c("1", "3"), order := 0.5]
df[subjectGroup %in% c("2", "4"), order := -0.5]

##Compute RWA score by participant

#Extract relevant variables and arrange the dataframe
RWA <- filter(df,condition1 =="RWA")
RWA <- data.table(dcast(RWA, ppt ~ trialNo, value.var="response"))

#Rename variables
RWA <- setnames(RWA, c("ppt", "RWA1","RWA10","RWA11","RWA12","RWA13","RWA14","RWA15","RWA2","RWA3","RWA4", "RWA5","RWA6","RWA7","RWA8","RWA9"))

#Transform variables to numeric format
RWA[, (2:16) := lapply(.SD, as.numeric), .SDcols = (2:16)]

#reversed items. RWA items 2, 4, 6, 10, 12, 14.
RWA<- within(RWA,{
  RWA2_r<- 8-RWA2
  RWA4_r<- 8-RWA4
  RWA6_r<- 8-RWA6
  RWA8_r<- 8-RWA8
  RWA10_r<- 8-RWA10
  RWA12_r<- 8-RWA12
  RWA14_r<- 8-RWA14
})

#RWA score
RWA <- within(RWA,{
  RWAscore <- rowSums(RWA[, c("RWA1","RWA10_r","RWA11","RWA12_r","RWA13","RWA14_r","RWA15","RWA2_r","RWA3","RWA4_r", "RWA5","RWA6_r","RWA7","RWA8","RWA9")], na.rm=TRUE)
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

###################indirect_dfRatings DF##########################

indir_df <- select(df, ppt, usvalence, subjectGroup, order, condition1, stim1, response)
indir_df <- data.table(subset(indir_df,   condition1=="affectivemisattributionprocedure1" |
                                          condition1=="affectivemisattributionprocedure2" |
                                          condition1=="affectivemisattributionprocedure3" |
                                          condition1=="affectivemisattributionprocedure4"))

##Final data frame. Direct ratings
indir_df <- full_join(indir_df, RWA, by ="ppt")
indir_df <- data.table(indir_df)

indir_df[response %in% "p", ampresp := 1]
indir_df[response %in% "u", ampresp := 0]

###################direct_dfRatings DF##########################

#select only relevant rows
direct_df <- select(df, ppt, subjectGroup, order, condition1, stim1, response)
direct_df <- data.table(subset(direct_df, condition1=="ratings1" | condition1=="ratings2"| condition1=="ratings3" | condition1=="ratings4"))

#recode by Greeble family
direct_df$family <- direct_df$stim1
direct_df[family %in% c("M1","M2", "M3","M4","M5","M6","M7","M8"), family := "M"]
direct_df[family %in% c("B1","B2", "B3","B4","B5","B6","B7","B8"), family := "B"]

## Recode US valence (first block, i.e., evaluative conditioning)

# subjectGroup 1, 2: B+ / M-
# subjectGroup 3, 4: B- / M+

direct_df[subjectGroup %in% c("1", "2") & family %in% "M", usvalencedir := -0.5]
direct_df[subjectGroup %in% c("1", "2") & family %in% "B", usvalencedir := 0.5]
direct_df[subjectGroup %in% c("3", "4") & family %in% "M", usvalencedir := 0.5]
direct_df[subjectGroup %in% c("3", "4") & family %in% "B", usvalencedir := -0.5]

direct_df$response <- as.numeric(as.character(direct_df$response))

##Final data frame. Direct ratings
direct_df <- full_join(direct_df, RWA, by ="ppt")

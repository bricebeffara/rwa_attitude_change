###########
#### RWA & load
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

# Select only text files corresponding to collected data
load_df <- read_delim("table_all_data.csv", 
                        ";", escape_double = FALSE, trim_ws = TRUE, 
                        skip = 1)

##CODING CONDITION +0.5 = LOAD  -0.5 = CONTROL
load_df$load <- ifelse(grepl("COND1",load_df$ExperimentName),+0.5,-0.5)

##RWA SCORE
load_df$RWARECOD<-ifelse(grepl("R",load_df$codage),8-load_df$RWAquest.RESP,load_df$RWAquest.RESP)
load_dfsum <- aggregate(RWARECOD ~ Subject,load_df,FUN = sum)
load_df <- merge(load_df,load_dfsum,by="Subject")
load_df$RWA<-load_df$RWARECOD.y

#Check data & See data

#boxplot(load_df$RWA)
#hist(load_df$RWA)

quart1 <- quantile(load_df$RWA)[2]
quart3 <- quantile(load_df$RWA)[4]
iqr <- IQR(load_df$RWA)

#which(load_df$RWAscore<quart1-3*iqr)
#which(load_df$RWAscore>quart3+3*iqr)

# Remove highly extreme values of RWA
load_df$RWA <-  ifelse (load_df$RWA<quart1-3*iqr | load_df$RWA>quart3+3*iqr, NA, load_df$RWA)
load_df <- load_df[which(!is.na(load_df$RWA)),]

# scale RWA score
load_df$RWAscore <- scale(load_df$RWA, center = TRUE, scale = TRUE)

load_df$response<-load_df$Slide1.RESP
load_df$stim1<-load_df$`CS[Trial]`
load_df$ppt <- as.character(load_df$Subject)

##RECODING OF BLOC 1 valence
load_dfassi <- aggregate(assignment ~ Subject, load_df[which(!is.na(load_df$assignment)),],FUN = unique)
load_df <- merge(load_df,load_dfassi,by="Subject")
load_df$assignment <- load_df$assignment.y
load_df$usvalence <- ifelse ((load_df$assignment == 1 & grepl("M",load_df$stim1)) | (load_df$assignment == 2 & grepl("B",load_df$stim1)), +0.5, -0.5)
load_df <- load_df[which(!is.na(load_df$Slide1.RESP)),]

col2k <- c("ppt", "usvalence", "load", "RWAscore", "response", "stim1")
load_df <- load_df[,col2k]

# for IDA

xp9_df <- load_df[load_df$load == -0.5,]
col2k <- c("ppt", "stim1", "RWAscore", "usvalence", "response")
xp9_df <- xp9_df[,col2k]
xp9_df$XP <- "XP09"

setwd("../ida")
write.csv(xp9_df,file="XP09.csv", row.names=F)

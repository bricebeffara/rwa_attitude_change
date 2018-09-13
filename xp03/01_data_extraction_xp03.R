###########
#### RWAscore & counter-conditioning + bloc 1 vs. bloc 2 : Experiment 3
###########

# Data preparation --------------------------------------------------------

# Optional generic preliminaries:

#graphics.off() # This closes all of R's graphics windows.
#rm(list=ls())  # Careful! This clears all of R's memory!

## install, load and update necessary packages if required##

if(!require("pacman")) install.packages("pacman")
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

curwd <- paste(curwd, "/raw_data" ,sep = "")

setwd (curwd)

myFiles <- list.files()

readcsv <- function(filename){
  data <- read.csv2(filename, header=TRUE,fill=TRUE,sep=",",skip=2)
  data$ppt <- filename #EDIT
  data
}

d<-lapply(myFiles, readcsv)

d <- d[sapply(d, function(x) nrow(x)) == 65] # Remove files with wrong data recording (2 data frames in one)

for (i in 1:length(d)){
  d[[i]]$RWAr<-ifelse(d[[i]]$condition1=="RWA",d[[i]]$responses,NA)
  d[[i]]$RWAr<-ifelse(d[[i]]$trialNo==2 | d[[i]]$trialNo==4 | d[[i]]$trialNo==6 | d[[i]]$trialNo==8 | d[[i]]$trialNo==10 | d[[i]]$trialNo==12 | d[[i]]$trialNo==14,10-d[[i]]$RWAr,d[[i]]$RWAr)
  d[[i]]$RWAsc<-sum(d[[i]]$RWAr[which(d[[i]]$condition1=="RWA")])
  d[[i]]$group<-d[[i]]$condition1[22]
}

mergedT<-NULL
for (i in 1:length(d)){
  mergedT<-rbind(mergedT,d[[i]])}
#View(mergedT)
mergedN<-mergedT
mergedN$cond<-ifelse(grepl("B",mergedN$stim1),"faB","faM")
mergedN$responses<-as.numeric(mergedN$responses)
mergedN$valence14<-ifelse(mergedN$cond=="faM","Pos","Neg")
mergedN$valence23<-ifelse(mergedN$cond=="faM","Neg","Pos")
mergedN$bloc<-ifelse(grepl("Grp3",mergedN$group) | grepl("Grp4",mergedN$group), "1", "2")
mergedN$valence<-ifelse(grepl("Grp1",mergedN$group) | grepl("Grp4",mergedN$group), mergedN$valence14,mergedN$valence23)
col2keep<-c("stim1","ppt","responses","RTs","RWAsc","bloc","valence","condition1")
lines2keep<-which(grepl("ratings",mergedN$condition1))
XP3_Greebles<-mergedN[col2keep]
XP3_Greebles<-XP3_Greebles[lines2keep,]

curwd <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd (curwd)

write.csv2(XP3_Greebles, "XP3_Greebles.csv")

bloc_df <- read_delim("XP3_Greebles.csv", 
                           ";", escape_double = FALSE, trim_ws = TRUE)

bloc_df$RWAscore <- round( (bloc_df$RWAsc)/9*7)

bloc_df <-
bloc_df %>%
  mutate(
    usvalence2 = case_when(
      .$bloc == 1 & .$valence == "Neg" ~ -0.5, #  last bloc = negative
      .$bloc == 1 & .$valence == "Pos" ~ 0.5, #  last bloc = positive
      .$bloc == 2 & .$valence == "Neg" ~ -0.5, #  last bloc = negative
      .$bloc == 2 & .$valence == "Pos" ~ 0.5, #  last bloc = positive
      TRUE ~ 999
    )
  )

bloc_df <-
  bloc_df %>%
  mutate(
    usvalence1 = case_when(
      .$bloc == 1 & .$valence == "Neg" ~ -0.5, #  first bloc = negative
      .$bloc == 1 & .$valence == "Pos" ~ 0.5, #  first bloc = positive
      .$bloc == 2 & .$valence == "Neg" ~ 0.5, #  first bloc = positive
      .$bloc == 2 & .$valence == "Pos" ~ -0.5, #  first bloc = negative
      TRUE ~ 999
    )
  )

bloc_df$response <- bloc_df$responses
bloc_df$bloc <- ifelse(bloc_df$bloc == 1, -0.5, +0.5)
bloc_df <- bloc_df[c("ppt","stim1","RWAscore","usvalence", "bloc", "response")]

# ? outliers ? Check data & See data

#boxplot(bloc_df$RWAscore)
#hist(bloc_df$RWAscore)
#unique(bloc_df$usvalence)
#mean(bloc_df$usvalence)

quart1 <- quantile(bloc_df$RWAscore)[2]
quart3 <- quantile(bloc_df$RWAscore)[4]
iqr <- IQR(bloc_df$RWAscore)

#which(bloc_df$RWAscore<quart1-3*iqr)
#unique(bloc_df$ppt[which(bloc_df$RWAscore>quart3+3*iqr)])
#unique(bloc_df$RWAscore[which(bloc_df$RWAscore>quart3+3*iqr)])

bloc_df_backup <- bloc_df
bloc_df$RWAscore <-  ifelse (bloc_df$RWAscore<quart1-3*iqr | bloc_df$RWAscore>quart3+3*iqr, NA, bloc_df$RWAscore)
bloc_df <- bloc_df[which(!is.na(bloc_df$RWAscore)),]
bloc_df$RWAscore <- scale (bloc_df$RWAscore, center = TRUE, scale = TRUE)

# for IDA

xp3_df <- bloc_df[bloc_df$bloc == 0.5,]
xp3_df$usvalence <- xp3_df$usvalence1
col2k <- c("ppt", "stim1", "RWAscore", "usvalence", "response")
xp3_df <- xp3_df[,col2k]
xp3_df$XP <- "XP03"

setwd("../ida")
write.csv(xp3_df,file="XP03.csv", row.names=F)

###########
#### RWAscore & counter-conditioning: Experiment 1
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

# Load data (Script for data extraction from raw data is not functional anymore,
# because of a package update. We're working on it)

base_df <- read_csv("XP1_Greebles.csv", 
                         col_types = cols(Subject = col_character()))

base_df <- base_df[-which(base_df$RWA == 0),]
base_df$ppt <- base_df$Subject
base_df$stim1 <- base_df$CS
base_df$RWAscore <- (base_df$RWA)
base_df$usvalence <-  ifelse(base_df$assignment == -1 & grepl("B",base_df$CS) | base_df$assignment == 1 & grepl("M",base_df$CS), +0.5, -0.5) # -0.5 -> bloc1 = negative, bloc2 = positive // +0.5 -> bloc1 = positive, bloc2 = negative
base_df$response <- base_df$CSratings
base_df <- base_df[c("ppt","stim1","RWAscore","usvalence","response")]

# ? outliers ? Check data & See data

#boxplot(base_df$RWAscore)
#hist(base_df$RWAscore)
#unique(base_df$usvalence)
#mean(base_df$usvalence)

quart1 <- quantile(base_df$RWAscore)[2]
quart3 <- quantile(base_df$RWAscore)[4]
iqr <- IQR(base_df$RWAscore)

#which(base_df$RWAscore<quart1-3*iqr)
#which(base_df$RWAscore>quart3+3*iqr)

base_df$RWAscore <-  ifelse (base_df$RWAscore<quart1-3*iqr | base_df$RWAscore>quart3+3*iqr, NA, base_df$RWAscore)
base_df <- base_df[which(!is.na(base_df$RWAscore)),]
base_df$RWAscore <- scale (base_df$RWAscore, center = TRUE, scale = TRUE)

# for IDA

base_df$XP <- "XP01"

setwd("../ida")
write.csv(base_df,file="XP01.csv", row.names=F)

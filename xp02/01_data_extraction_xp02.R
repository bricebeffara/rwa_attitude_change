###########
#### RWAscore & counter-conditioning + social vs. non social + consciousness: Experiment 2
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

# Load data (Script for data extraction from raw data is not functional anymore,
# because of a package update. We're working on it)

social_df <- read_delim("XP2_Greebles.csv", 
                           ";", escape_double = FALSE, trim_ws = TRUE)

social_df <- base_df[-which(base_df$RWA == 0),]
social_df$ppt <- social_df$Eprime.Basename
social_df$stim1 <- social_df$CS
social_df$RWAscore <- (social_df$RWA)
social_df$usvalence <- ifelse(social_df$interactC == "NegPos", -0.5, 0.5) # # -0.5 -> bloc1 = negative, bloc2 = positive // 0.5 -> bloc1 = positive, bloc2 = negative
social_df$response <- social_df$score

social_df$aware <- social_df$awareF
social_df$aware <- gsub ("f", "", social_df$aware)
social_df$aware <- as.numeric(social_df$aware)
social_df$aware <- ifelse(social_df$aware > 100, substr(social_df$aware, 1, nchar(social_df$aware)-1), social_df$aware)
social_df$aware <- as.numeric(social_df$aware)

social_df <- social_df[c("ppt","stim1","RWAscore","usvalence","response", "aware")]

# ? outliers ? Check data & See data

#boxplot(social_df$RWAscore)
#hist(social_df$RWAscore)
#unique(social_df$usvalence)
#mean(social_df$usvalence)

quart1 <- quantile(social_df$RWAscore)[2]
quart3 <- quantile(social_df$RWAscore)[4]
iqr <- IQR(social_df$RWAscore)

#which(social_df$RWAscore<quart1-3*iqr)
#which(social_df$RWAscore>quart3+3*iqr)

social_df$RWAscore <-  ifelse (social_df$RWAscore<quart1-3*iqr | social_df$RWAscore>quart3+3*iqr, NA, social_df$RWAscore)
social_df <- social_df[which(!is.na(social_df$RWAscore)),]
social_df$RWAscore <- scale (social_df$RWAscore, center = TRUE, scale = TRUE)

social_df$social <- ifelse (grepl("Greebles", social_df$ppt), 0.5, -0.5)

# for IDA

xp2_df <- social_df[social_df$social == 0.5,]
col2k <- c("ppt", "stim1", "RWAscore", "usvalence", "response")
xp2_df <- xp2_df[,col2k]
xp2_df$XP <- "XP02"

setwd("../ida")
write.csv(xp2_df,file="XP02.csv", row.names=F)

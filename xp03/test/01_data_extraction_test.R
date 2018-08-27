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

setwd (curwd)

bloc_df2 <- read_delim("XP3_Greebles.csv", 
                           ";", escape_double = FALSE, trim_ws = TRUE)

bloc_df2$RWAscore <- round( (bloc_df2$RWAsc)/9*7)

bloc_df2 <-
bloc_df2 %>%
  mutate(
    usvalence = case_when(
      .$bloc == 1 & .$valence == "Neg" ~ -0.5, #  bloc1 = negative
      .$bloc == 1 & .$valence == "Pos" ~ 0.5, #  bloc1 = positive
      .$bloc == 2 & .$valence == "Neg" ~ 0.5, #  bloc1 = positive
      .$bloc == 2 & .$valence == "Pos" ~ -0.5, #  bloc1 = negative
      TRUE ~ 999
    )
  )

bloc_df2$response <- bloc_df2$responses
bloc_df2$bloc <- ifelse(bloc_df2$bloc == 1, -0.5, +0.5)
bloc_df2 <- bloc_df2[c("ppt","stim1","RWAscore","usvalence", "bloc", "response")]

# ? outliers ? Check data & See data

#boxplot(bloc_df2$RWAscore)
#hist(bloc_df2$RWAscore)
#unique(bloc_df2$usvalence)
#mean(bloc_df2$usvalence)

quart1 <- quantile(bloc_df2$RWAscore)[2]
quart3 <- quantile(bloc_df2$RWAscore)[4]
iqr <- IQR(bloc_df2$RWAscore)

#which(bloc_df2$RWAscore<quart1-3*iqr)
#unique(bloc_df2$ppt[which(bloc_df2$RWAscore>quart3+3*iqr)])
#unique(bloc_df2$RWAscore[which(bloc_df2$RWAscore>quart3+3*iqr)])

bloc_df2_backup <- bloc_df2
bloc_df2$RWAscore <-  ifelse (bloc_df2$RWAscore<quart1-3*iqr | bloc_df2$RWAscore>quart3+3*iqr, NA, bloc_df2$RWAscore)
bloc_df2 <- bloc_df2[which(!is.na(bloc_df2$RWAscore)),]
bloc_df2$RWAscore <- scale (bloc_df2$RWAscore, center = TRUE, scale = TRUE)


#################################################################################################################
# Wed Mar 22 13:49:16 2017 ------------------------------
# Script data extraction and analysis for IAT/evaluative conditionning study A.G. Bret march 2017
# Scipt by B. Beffara & A.G. Bret
#################################################################################################################

#############################
# Part 1 = Get the data frame
#############################

# clean all
rm(list=ls())

# Installing (if not already) and loading needed packages with "pacman"
if (!"pacman" %in% installed.packages()[,"Package"] ) install.packages("pacman" )
pacman::p_load(data.table, IATscores, lme4, piecewiseSEM, ggplot2) 

# Set working directory here in the current project
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Opening file with all participants inside while removing problemq relative to formats and encoding
file <- "mergeIAT.txt"
tt <- tempfile()
system(paste0("tr < ", file, " -d '\\000' >", tt ) )
DFIAT <- fread(tt, skip = 3 )
# View(DFIAT) # just if you want to have a look a it

###############################################
# Part 2 = Extracting and Organizing variables
###############################################

############ Dealing with RWA scores ########################

# item number of RWA questionnaire should be numerical
DFIAT$RWA <- as.numeric(DFIAT$RWA )
# response on RWA scale should be numerical
DFIAT$RWAquest.RESP <- as.numeric(DFIAT$RWAquest.RESP )
# recoding necessary items
DFIAT$RWAr<-ifelse(DFIAT$RWAquest.RESP == 4  | DFIAT$RWAquest.RESP == 5, 8-DFIAT$RWAquest.RESP, DFIAT$RWAquest.RESP )
# I find it bette to have non numerical participants ids
DFIAT$Subject <- as.character(DFIAT$Subject)
# Computing RWA scores (sum) for each participant
RWAS <- aggregate(RWAr ~ Subject, DFIAT, sum )
# Renaming the columns of the new df with RWA sums
colnames(RWAS) <- c("Subject", "RWAS" )
# Merging the new df with RWA sums with the orginial df
DFIAT <- merge (DFIAT, RWAS, by = "Subject" )

############ Dealing with IAT scores ########################

# measures should be numerical
DFIAT$GreeblesDisplay.ACC <- ifelse (DFIAT$GreeblesDisplay.RESP == "NULL", NA, ifelse (DFIAT$GreeblesDisplay.RESP == DFIAT$CorrectAnswer,1 , 0 ) )
DFIAT$StimDisplay.ACC <- ifelse (DFIAT$StimDisplay.RESP == "NULL", NA, ifelse (DFIAT$StimDisplay.RESP == DFIAT$CorrectAnswer,1 , 0 ) )
DFIAT$GreeblesDisplay.RT <- as.numeric(DFIAT$GreeblesDisplay.RT)
DFIAT$StimDisplay.RT <- as.numeric(DFIAT$StimDisplay.RT)

# Creating the factor "EC" for all rows and all participants
for(i in unique(DFIAT$Subject)){
  DFIAT$order[which(DFIAT$Subject == i)] <- ifelse (DFIAT$`Procedure[Block]`[which(DFIAT$Subject == i)][100]=="EC1","EC1","EC2")
  #print(DFIAT$`Procedure[Block]`[which(DFIAT$Subject == i)][100])
}

# Computing order of presentation based on CS family ("B" vs. "M) and procedure (EC1 vs. EC2) for Greebles in IAT
DFIAT$orderS <- ifelse( nchar(DFIAT$Stimulus == 2 ), ifelse (grepl("B", DFIAT$Stimulus ) & DFIAT$order == "EC1" | grepl("M", DFIAT$Stimulus ) & DFIAT$order == "EC2", "NegPos", "PosNeg" ), NA )

# Computing valence (-1 = neg, 1 = pos) for words in IAT
DFIAT$val <- ifelse(nchar (DFIAT$Stimulus > 4 ), ifelse (DFIAT$Stimulus == "cauchemar" | DFIAT$Stimulus == "crainte" | DFIAT$Stimulus == "douleur" | DFIAT$Stimulus == "guerre" | DFIAT$Stimulus == "larme" | DFIAT$Stimulus == "pistolet" | DFIAT$Stimulus == "regret", -1, 1 ), NA )

# Compute mean ACC for Greebles IAT
greebACC <- aggregate (GreeblesDisplay.ACC ~ Subject*orderS, DFIAT, FUN = mean)
colnames (greebACC) <- c("Subject", "orderS", "greebACC")
# Compute mean RT for Greebles IAT
greebRT <- aggregate (GreeblesDisplay.RT ~ Subject*orderS, DFIAT, FUN = mean)
colnames (greebRT) <- c("Subject", "orderS", "greebRT")

# Merging with the big DF
DFIAT <- Reduce(function(...) merge(..., by = c("Subject","orderS") , all=T), list(DFIAT, greebACC, greebRT))

# Compute mean ACC for words IAT
stimACC <- aggregate (StimDisplay.ACC ~ Subject*val, DFIAT, FUN = mean)
colnames (stimACC) <- c("Subject", "val", "stimACC")

# Compute RT for words IAT
stimRT <- aggregate (StimDisplay.RT ~ Subject*val, DFIAT, FUN = mean)
colnames (stimRT) <- c("Subject", "val", "stimRT")

# Merging with the big DF
DFIAT <- Reduce(function(...) merge(..., by = c("Subject","val") , all=T), list(DFIAT, stimACC, stimRT))

# Keeping only explicit ratings trials (other scores are everywhere and will be saved)
DFIAT <- DFIAT [which(DFIAT$Slide1.RESP != "NULL"), ]

# Computing order of CS presentation for explicit ratings
DFIAT$order <- ifelse (grepl("B", DFIAT$`CS[Trial]` ) & DFIAT$order == "EC1" | grepl("M", DFIAT$`CS[Trial]` ) & DFIAT$order == "EC2", "NegPos", "PosNeg" )

# Renaming some columns for convenience
DFIAT$expli <- DFIAT$Slide1.RESP
DFIAT$CS <- DFIAT$`CS[Trial]`

# We keep only the columns we're interested in  
c2k <- c("Subject", "val", "orderS", "expli", "stimACC", "stimRT", "greebACC", "greebRT", "RWAS", "order", "CS")
DFIAT <- DFIAT[,..c2k]
View(DFIAT)

#############################################################################################
###### Variables
# Subject = participant id
# val = valence words IAT (-1 = neg, 1 = pos)
# orderS = order of CS presentation for IAT ratings
# expli = explicit ratings
# stimACC = words accuracy IAT (corresponding to "val")
# stimRT = words RT IAT (corresponding to "val")
# greebACC = accuracy for greebles in IAT (corresponding to "orderS")
# greebRT = RT for greebles in IAT (corresponding to "orderS")
# RWAS = RWA sum score
# order = order of CS presentation for explicit ratings (corresponding to "order")
# CS = CS id
#############################################################################################

# Wed Mar 22 22:17:33 2017 ------------------------------



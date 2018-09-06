# clean all
rm(list=ls())

# Installing (if not already) and loading needed packages
if (!"pacman" %in% installed.packages()[,"Package"] ) install.packages("pacman" )
pacman::p_load(data.table) 

# Set working directory here in the current project
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Opening file with all participants inside while removing problematic formats
# !!!!!!!! Ouvrir le fichier mergeIAT.txt avec un tableur et enregistrer sous en .csv avec encodage UTF-8
file <- "mergeIAT.csv"
DFIAT <- fread(file, skip = 3 )
View(DFIAT)

DFIAT$RWA <- as.numeric(DFIAT$RWA)
DFIAT$RWAquest.RESP <- as.numeric(DFIAT$RWAquest.RESP)
DFIAT$RWAr<-ifelse(DFIAT$RWAquest.RESP==4  | DFIAT$RWAquest.RESP==5,8-DFIAT$RWAquest.RESP,DFIAT$RWAquest.RESP)
DFIAT$Subject <- as.character(DFIAT$Subject)
RWAS <- aggregate(RWAr ~ Subject, DFIAT, sum)
colnames(RWAS) <- c("Subject", "RWAS")
DFIAT <- merge (DFIAT, RWAS, by = "Subject")

#JUSQUICI CA MARCHE
Sys.Date

k1<-which(!is.na(DFIAT$Slide1.RESP))
keep = c(k1)
DFIAT<-DFIAT[keep,]
DFIAT$codage<-NULL
DFIAT$Running<-NULL
DFIAT$RWAquest.RESP<-NULL
ratings<-data.frame(1:16)
ratings$value<-DFIAT$Slide1.RESP[1:16]
ratings$X1.16<-NULL
ratings$CS<-DFIAT$CS[1:16]
orderDf<-order(ratings$CS)
ratings<-ratings[orderDf, ]
DFIAT$CS[1:16]<-ratings$CS
DFIAT$Slide1.RESP[1:16]<-ratings$value

DFIAT$CE1<-ifelse(DFIAT$ordre=="CE1",1,-1)
DFIAT$famiC<-ifelse(DFIAT$fami==1,"A","B")

all_df <- lapply(myFiles[3:120], createDF)
DFIAT = Reduce(function(...) merge(..., all=T), all_df)
View(DFIAT)
DFIAT$interact<-DFIAT$ordR*DFIAT$fami
DFIAT$interactC<-ifelse(DFIAT$interact==1,"PosNeg","NegPos")
head(DFIAT)
View(DFIAT)
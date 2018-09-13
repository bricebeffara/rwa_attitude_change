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

XP01 <- read_csv("XP01.csv")
XP02 <- read_csv("XP02.csv")
XP03 <- read_csv("XP03.csv")
XP04 <- read_csv("XP04.csv")
XP05 <- read_csv("XP05.csv")
XP06 <- read_csv("XP06.csv")
XP07 <- read_csv("XP07.csv")
XP08 <- read_csv("XP08.csv")
XP09 <- read_csv("XP09.csv")
XP11 <- read_csv("XP11.csv")

IDA <- rbind(XP01,
             XP02,
             XP03,
             XP04,
             XP05,
             XP06,
             XP07,
             XP08,
             XP09,
             XP11)

XP01$usvalence_neg <- ifelse (XP01$usvalence == -0.5, 0, 1)
XP02$usvalence_neg <- ifelse (XP02$usvalence == -0.5, 0, 1)
XP03$usvalence_neg <- ifelse (XP03$usvalence == -0.5, 0, 1)
XP04$usvalence_neg <- ifelse (XP04$usvalence == -0.5, 0, 1)
XP05$usvalence_neg <- ifelse (XP05$usvalence == -0.5, 0, 1)
XP06$usvalence_neg <- ifelse (XP06$usvalence == -0.5, 0, 1)
XP07$usvalence_neg <- ifelse (XP07$usvalence == -0.5, 0, 1)
XP08$usvalence_neg <- ifelse (XP08$usvalence == -0.5, 0, 1)
XP09$usvalence_neg <- ifelse (XP09$usvalence == -0.5, 0, 1)
XP11$usvalence_neg <- ifelse (XP11$usvalence == -0.5, 0, 1)
IDA$usvalence_neg <- ifelse (IDA$usvalence == -0.5, 0, 1)

XP01$usvalence_pos <- ifelse (XP01$usvalence == 0.5, 0, 1)
XP02$usvalence_pos <- ifelse (XP02$usvalence == 0.5, 0, 1)
XP03$usvalence_pos <- ifelse (XP03$usvalence == 0.5, 0, 1)
XP04$usvalence_pos <- ifelse (XP04$usvalence == 0.5, 0, 1)
XP05$usvalence_pos <- ifelse (XP05$usvalence == 0.5, 0, 1)
XP06$usvalence_pos <- ifelse (XP06$usvalence == 0.5, 0, 1)
XP07$usvalence_pos <- ifelse (XP07$usvalence == 0.5, 0, 1)
XP08$usvalence_pos <- ifelse (XP08$usvalence == 0.5, 0, 1)
XP09$usvalence_pos <- ifelse (XP09$usvalence == 0.5, 0, 1)
XP11$usvalence_pos <- ifelse (XP11$usvalence == 0.5, 0, 1)
IDA$usvalence_pos <- ifelse (IDA$usvalence == 0.5, 0, 1)


# Numeber of ppt

#length(unique(IDA$ppt))

#length(unique(XP01$ppt)) +
#length(unique(XP02$ppt)) +
#length(unique(XP03$ppt)) +
#length(unique(XP04$ppt)) +
#length(unique(XP05$ppt)) +
#length(unique(XP06$ppt)) +
#length(unique(XP07$ppt)) +
#length(unique(XP08$ppt)) +
#length(unique(XP09$ppt)) +    
#length(unique(XP11$ppt))

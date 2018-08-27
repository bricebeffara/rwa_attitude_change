# Wed Nov 15 18:05:53 2017 ------------------------------
# Clean script data loading
# Bret et al. 2017
# Right-Wing Authoritarianism Predicts weakened Attitude Change in an Evaluative Counter-conditioning Paradigm
# OSF : https://osf.io/vz78f/
# Preprint : https://psyarxiv.com/3mfa8/
#################################################################################################################

# Set working directory
setwd(paste(curwd,"/all_dfs/",sep=""))

# Load data XP 1 - Basic - 
XP1_Greebles <- read_csv("XP1_Greebles.csv", 
                            col_types = cols(Subject = col_character()))
#View(XP1_Greebles)

# Load data XP 2 - Social vs. Object - 
XP2_Greebles <- read_delim("XP2_Greebles.csv", 
                              ";", escape_double = FALSE, trim_ws = TRUE)
#View(XP2_Greebles)

# Load data XP 3 - Bloc 1 vs. Bloc 2 -
XP3_Greebles <- read_delim("XP3_Greebles.csv", 
                              ";", escape_double = FALSE, trim_ws = TRUE)

#View(XP3_Greebles)

# --------------------
# Following experiments are only included in the supplementary materials
# --------------------


# Load data XP 4 - Affective Priming Task -
XP4_Greebles <- read_csv2("XP4_Greebles.csv")

#View(XP4_Greebles)

# Load data XP 5 - Affective Misattribution Paradigm -
XP5_Greebles <- read_csv2("XP5_Greebles.csv")

#View(XP5_Greebles)

# Load data XP 6 - Implicit Association Test -
XP6_Greebles <- read_csv2("XP6_Greebles.csv")

#View(XP6_Greebles)

# Load data XP 7 - Double counter conditioning -
XP7_Greebles <- read_csv2("XP7_Greebles.csv")

#View(XP7_Greebles)

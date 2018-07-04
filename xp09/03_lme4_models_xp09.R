# File name: brms_models_xp09.R
# Online archive: gitlab
# Authors: Brice Beffara & Amélie Bret 
# Wed Jul 04 14:31:56 2018 ------------------------------
# Contact: brice.beffara@slowpen.science amelie.bret@univ-grenoble-alpes.fr http://slowpen.science
#
# This R script was used to build and compute brms models
# corresponding to the 10th experiment of Amelie Bret's doctoral work
#
# This R script defines and computes brms models
# main effects, interaction effects, and simple slopes of interest
# 
# 3 posependent variables of interest :
# RWA (continuous, centered and scaled)
# usvalence : positive (0.5) vs. negative (-0.5)
# and load : no load (-0.5) vs. load (0.5)
#
# and 1 ordinal dependent variables :
# Ratings of Greebles from 1 (very negative) to 9 (very positive)
#
# This program is believed to be free of errors, but it comes with no guarantee! 
# The user bears all responsibility for interpreting the results.
#
# This preambule is largely inspired by John K. Kruschke's work at https://osf.io/wp2ry/
#
### To run this program, please do the following:
### 1. Install the general-purpose programming language R from  
###      http://www.r-project.org/
###    Install the version of R appropriate for your computer's operating
###    system (Wposows, MacOS, or Linux).   
### 2. Install the R editor, RStudio, from
###      http://rstudio.org/
###    This editor is not necessary, but highly recommended.
### 3. After the above actions are accomplished, this program should
###    run as-is in R. You may "source" it to run the whole thing at once,
###    or, preferably, run lines consecutively from the beginning.
################################################################################

# Loading packages needed (and installing if necessary) for this part

p_load(lme4, # main package for models
       htmlTable, # helps to extract results
       xtable,
       install = TRUE,
       gridExtra,
       sjstats,
       sjmisc,
       update = getOption("pac_update"),
       character.only = FALSE)

# In case we want to save summaries
col2keep <- c("Estimate", "l-95% CI", "u-95% CI")

#------------------------------------------------------------------------------------
# We run our first model for fixed main and interaction effects
#------------------------------------------------------------------------------------

# model
load_resp_lme4 <- lmer(response ~ usvalence * load * RWAscore + (1|ppt) + (1|stim1),
                   data = load_df)

# Save summary & confint
model_gen_xp09_lme4 <- round(cbind(summary(load_resp_lme4)$coefficients,
                             confint(load_resp_lme4)[c(4:11),]), 2)

# export output
png("tables/lme4/model_gen_xp09_lme4.png", height=480, width=720)
p<-tableGrob(model_gen_xp09_lme4)
grid.arrange(p)
dev.off()

#------------------------------------------------------------------------------------
# Then we run our second step models to decompose interactions effects
# We look at the interaction between RWA and usvalence at each level of load
#------------------------------------------------------------------------------------

#-------------
### RWA * valence in the !!no load!! condition
#-------------

# model
load_resp_nolo_lme4 <- lmer(response ~ usvalence * no_load  * RWAscore + (1|ppt) + (1|stim1),
                         data = load_df)

# Save summary & confint
model_nolo_xp09_lme4 <- round(cbind(summary(load_resp_nolo_lme4)$coefficients,
                                   confint(load_resp_nolo_lme4)[c(4:11),]), 2)

# export output
png("tables/lme4/model_nolo_xp09_lme42.png", height=480, width=720)
p<-tableGrob(model_nolo_xp09_lme4)
grid.arrange(p)
dev.off()

#-------------
### RWA * usvalence in the !!load!! condition
#-------------

# model
load_resp_yelo_lme4 <- lmer(response ~ usvalence * ye_load  * RWAscore + (1|ppt) + (1|stim1),
                               data = load_df)

# Save summary & confint
model_yelo_xp09_lme4 <- round(cbind(summary(load_resp_yelo_lme4)$coefficients,
                                     confint(load_resp_yelo_lme4)[c(4:11),]), 2)

# export output
png("tables/lme4/model_yelo_xp09_lme4.png", height=480, width=720)
p<-tableGrob(model_yelo_xp09_lme4)
grid.arrange(p)
dev.off()

#------------------------------------------------------------------------------------
# Then we run our third step models to decompose the interactions 
# between RWA and usvalence in the no load and load conditions
#------------------------------------------------------------------------------------

#############################
##################### No load
#############################

#-------------
### Simple slope of RWA in the !!negative!! valence & !!load!! condition
#-------------


#model
load_resp_nolo_neg_lme4 <- lmer(response ~ usvalence_neg * no_load  * RWAscore + (1|ppt) + (1|stim1),
                              data = load_df)

# Save summary & confint
model_nolo_neg_xp09_lme4 <- round(cbind(summary(load_resp_nolo_neg_lme4)$coefficients,
                                     confint(load_resp_nolo_neg_lme4)[c(4:11),]), 2)

# export output
png("tables/lme4/model_nolo_neg_xp09_lme4.png", height=480, width=720)
p<-tableGrob(model_nolo_neg_xp09_lme4)
grid.arrange(p)
dev.off()

#-------------
### Simple slope of RWA in the !!positive!! valence & !!no load!! condition
#-------------

#model
load_resp_nolo_pos_lme4 <- lmer(response ~ usvalence_pos * no_load  * RWAscore + (1|ppt) + (1|stim1),
                                   data = load_df)

# Save summary & confint
model_nolo_pos_xp09_lme4 <- round(cbind(summary(load_resp_nolo_pos_lme4)$coefficients,
                                         confint(load_resp_nolo_pos_lme4)[c(4:11),]), 2)

# export output
png("tables/lme4/model_nolo_pos_xp09_lme4.png", height=480, width=720)
p<-tableGrob(model_nolo_pos_xp09_lme4)
grid.arrange(p)
dev.off()


#############################
##################### Load
#############################

#-------------
### Simple slope of RWA in the !!negative!! valence & !!load!! condition
#-------------

#model
load_resp_yelo_neg_lme4 <- lmer(response ~ usvalence_neg * ye_load  * RWAscore + (1|ppt) + (1|stim1),
                                   data = load_df)

# Save summary & confint
model_yelo_neg_xp09_lme4 <- round(cbind(summary(load_resp_yelo_neg_lme4)$coefficients,
                                         confint(load_resp_yelo_neg_lme4)[c(4:11),]), 2)

# export output
png("tables/lme4/model_yelo_neg_xp09_lme4.png", height=480, width=720)
p<-tableGrob(model_yelo_neg_xp09_lme4)
grid.arrange(p)
dev.off()

#-------------
### Simple slope of RWA in the !!positive!! valence & !!load!! condition
#-------------

#model
load_resp_yelo_pos_lme4 <- lmer(response ~ usvalence_pos * ye_load  * RWAscore + (1|ppt) + (1|stim1),
                                   data = load_df)

# Save summary & confint
model_yelo_pos_xp09_lme4 <- round(cbind(summary(load_resp_yelo_pos_lme4)$coefficients,
                                         confint(load_resp_yelo_pos_lme4)[c(4:11),]), 2)

# export output
png("tables/lme4/model_yelo_pos_xp09_lme4.png", height=480, width=720)
p<-tableGrob(model_yelo_pos_xp09_lme4)
grid.arrange(p)
dev.off()

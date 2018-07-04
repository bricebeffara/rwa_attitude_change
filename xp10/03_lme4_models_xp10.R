# File name: brms_models_xp10.R
# Online archive: gitlab
# Authors: Brice Beffara & Amélie Bret 
# Thu Jun 07 16:08:17 2018 ------------------------------
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
# and spreading : level 1 (-0.5) vs. level 2 (0.5) conditioning
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
spread_resp_lme4 <- lmer(response ~ usvalence * spreading * RWAscore + (1|ppt) + (1|stim1),
                   data = spreading)

# Save summary & confint
model_gen_xp10_lme4 <- round(cbind(summary(spread_resp_lme4)$coefficients,
                             confint(spread_resp_lme4)[c(4:11),]), 2)

# export output
png("tables/lme4/model_gen_xp10_lme4.png", height=480, width=720)
p<-tableGrob(model_gen_xp10_lme4)
grid.arrange(p)
dev.off()

#------------------------------------------------------------------------------------
# Then we run our second step models to decompose interactions effects
# We look at the interaction between RWA and usvalence at each level of conditioning
#------------------------------------------------------------------------------------

#-------------
### RWA * valence in the !!level 1!! conditioning condition
#-------------

# model
spread_resp_lvone_lme4 <- lmer(response ~ usvalence * level_one  * RWAscore + (1|ppt) + (1|stim1),
                         data = spreading)

# Save summary & confint
model_lvone_xp10_lme4 <- round(cbind(summary(spread_resp_lvone_lme4)$coefficients,
                                   confint(spread_resp_lvone_lme4)[c(4:11),]), 2)

# export output
png("tables/lme4/model_lvone_xp10_lme42.png", height=480, width=720)
p<-tableGrob(model_lvone_xp10_lme4)
grid.arrange(p)
dev.off()

#-------------
### RWA * usvalence in the !!level 2!! conditioning condition
#-------------

# model
spread_resp_lvtwo_lme4 <- lmer(response ~ usvalence * level_two  * RWAscore + (1|ppt) + (1|stim1),
                               data = spreading)

# Save summary & confint
model_lvtwo_xp10_lme4 <- round(cbind(summary(spread_resp_lvtwo_lme4)$coefficients,
                                     confint(spread_resp_lvtwo_lme4)[c(4:11),]), 2)

# export output
png("tables/lme4/model_lvtwo_xp10_lme4.png", height=480, width=720)
p<-tableGrob(model_lvtwo_xp10_lme4)
grid.arrange(p)
dev.off()

#------------------------------------------------------------------------------------
# Then we run our third step models to decompose the interactions 
# between RWA and usvalence in the level 1 and level 2 conditioning conditions
#------------------------------------------------------------------------------------

#############################
##################### Level 1
#############################

#-------------
### Simple slope of RWA in the !!negative!! valence & !!level 1!! conditioning condition
#-------------


#model
spread_resp_lvone_neg_lme4 <- lmer(response ~ usvalence_neg * level_one  * RWAscore + (1|ppt) + (1|stim1),
                              data = spreading)

# Save summary & confint
model_lvone_neg_xp10_lme4 <- round(cbind(summary(spread_resp_lvone_neg_lme4)$coefficients,
                                     confint(spread_resp_lvone_neg_lme4)[c(4:11),]), 2)

# export output
png("tables/lme4/model_lvone_neg_xp10_lme4.png", height=480, width=720)
p<-tableGrob(model_lvone_neg_xp10_lme4)
grid.arrange(p)
dev.off()

#-------------
### Simple slope of RWA in the !!positive!! valence & !!level 1!! conditioning condition
#-------------

#model
spread_resp_lvone_pos_lme4 <- lmer(response ~ usvalence_pos * level_one  * RWAscore + (1|ppt) + (1|stim1),
                                   data = spreading)

# Save summary & confint
model_lvone_pos_xp10_lme4 <- round(cbind(summary(spread_resp_lvone_pos_lme4)$coefficients,
                                         confint(spread_resp_lvone_pos_lme4)[c(4:11),]), 2)

# export output
png("tables/lme4/model_lvone_pos_xp10_lme4.png", height=480, width=720)
p<-tableGrob(model_lvone_pos_xp10_lme4)
grid.arrange(p)
dev.off()


#############################
##################### Level 2
#############################

#-------------
### Simple slope of RWA in the !!negative!! valence & !!level 2!! conditioning condition
#-------------

#model
spread_resp_lvtwo_neg_lme4 <- lmer(response ~ usvalence_neg * level_two  * RWAscore + (1|ppt) + (1|stim1),
                                   data = spreading)

# Save summary & confint
model_lvtwo_neg_xp10_lme4 <- round(cbind(summary(spread_resp_lvtwo_neg_lme4)$coefficients,
                                         confint(spread_resp_lvtwo_neg_lme4)[c(4:11),]), 2)

# export output
png("tables/lme4/model_lvtwo_neg_xp10_lme4.png", height=480, width=720)
p<-tableGrob(model_lvtwo_neg_xp10_lme4)
grid.arrange(p)
dev.off()

#-------------
### Simple slope of RWA in the !!positive!! valence & !!level 2!! conditioning condition
#-------------

#model
spread_resp_lvtwo_pos_lme4 <- lmer(response ~ usvalence_pos * level_two  * RWAscore + (1|ppt) + (1|stim1),
                                   data = spreading)

# Save summary & confint
model_lvtwo_pos_xp10_lme4 <- round(cbind(summary(spread_resp_lvtwo_pos_lme4)$coefficients,
                                         confint(spread_resp_lvtwo_pos_lme4)[c(4:11),]), 2)

# export output
png("tables/lme4/model_lvtwo_pos_xp10_lme4.png", height=480, width=720)
p<-tableGrob(model_lvtwo_pos_xp10_lme4)
grid.arrange(p)
dev.off()

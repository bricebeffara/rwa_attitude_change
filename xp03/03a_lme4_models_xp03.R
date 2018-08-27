# File name: brms_models_xp03.R
# Online archive: gitlab
# Authors: Brice Beffara & Amélie Bret 
# Fri Aug 10 17:39:09 2018 ------------------------------
# Contact: brice.beffara@slowpen.science amelie.bret@univ-grenoble-alpes.fr http://slowpen.science
#
# This R script was used to build and compute lme4 models
# corresponding to the 3rd experiment of Amelie Bret's doctoral work
#
# This R script defines and computes brms models
# main effects, interaction effects, and simple slopes of interest
# 
# 3 independent variables of interest :
# RWA (continuous, centered and scaled)
# usvalence22 : positive (0.5) vs. negative (-0.5)
# and bloc : 1 bloc (-0.5) vs. 2 blocs (0.5)
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
bloc_resp_lme4 <- lmer(response ~ usvalence2 * bloc * RWAscore + (1|ppt) + (1|stim1),
                         data = bloc_df)

# Save summary & confint
model_gen_xp03_lme4 <- round(cbind(summary(bloc_resp_lme4)$coefficients,
                                   confint(bloc_resp_lme4)[c(4:11),]), 2)

# export output
png("tables/lme4/model_gen_xp03_lme4.png", height=480, width=720)
p<-tableGrob(model_gen_xp03_lme4)
grid.arrange(p)
dev.off()

#------------------------------------------------------------------------------------
# Then we run our second step models to decompose interactions effects
# We look at the interaction between RWA and usvalence2 at each level of conditioning
#------------------------------------------------------------------------------------

#-------------
### RWA * valence in the !!1 bloc!! condition
#-------------

# model
bloc_resp_b1_lme4 <- lmer(response ~ usvalence2 * bloc1  * RWAscore + (1|ppt) + (1|stim1),
                               data = bloc_df)

# Save summary & confint
model_b1_xp03_lme4 <- round(cbind(summary(bloc_resp_b1_lme4)$coefficients,
                                     confint(bloc_resp_b1_lme4)[c(4:11),]), 2)

# export output
png("tables/lme4/model_b1_xp03_lme4.png", height=480, width=720)
p<-tableGrob(model_b1_xp03_lme4)
grid.arrange(p)
dev.off()

#-------------
### RWA * usvalence2 in the !!2 blocs!! condition
#-------------

# model
bloc_resp_b2_lme4 <- lmer(response ~ usvalence2 * bloc2  * RWAscore + (1|ppt) + (1|stim1),
                               data = bloc_df)

# Save summary & confint
model_b2_xp03_lme4 <- round(cbind(summary(bloc_resp_b2_lme4)$coefficients,
                                     confint(bloc_resp_b2_lme4)[c(4:11),]), 2)

# export output
png("tables/lme4/model_b2_xp03_lme4.png", height=480, width=720)
p<-tableGrob(model_b2_xp03_lme4)
grid.arrange(p)
dev.off()

#------------------------------------------------------------------------------------
# Then we run our third step model to decompose the interaction 
# between RWA and usvalence2 in the !!2 blocs!! conditioning conditions
# The interaction slope between RWA and usvalence2 includes 0 in the !!1 bloc!! condition
#------------------------------------------------------------------------------------

#############################
##################### 2 blocks
#############################

#-------------
### Simple slope of RWA in the !!negative!! valence & !!2 blocks!! condition
#-------------


#model
bloc_resp_b2_neg_lme4 <- lmer(response ~ usvalence2_neg * bloc2  * RWAscore + (1|ppt) + (1|stim1),
                                   data = bloc_df)

# Save summary & confint
model_b2_neg_xp03_lme4 <- round(cbind(summary(bloc_resp_b2_neg_lme4)$coefficients,
                                         confint(bloc_resp_b2_neg_lme4)[c(4:11),]), 2)

# export output
png("tables/lme4/model_b2_neg_xp03_lme4.png", height=480, width=720)
p<-tableGrob(model_b2_neg_xp03_lme4)
grid.arrange(p)
dev.off()

#-------------
### Simple slope of RWA in the !!positive!! valence & !!2 blocks!! condition
#-------------

#model
bloc_resp_b2_pos_lme4 <- lmer(response ~ usvalence2_pos * bloc2  * RWAscore + (1|ppt) + (1|stim1),
                                   data = bloc_df)

# Save summary & confint
model_b2_pos_xp03_lme4 <- round(cbind(summary(bloc_resp_b2_pos_lme4)$coefficients,
                                         confint(bloc_resp_b2_pos_lme4)[c(4:11),]), 2)

# export output
png("tables/lme4/model_b2_pos_xp03_lme4.png", height=480, width=720)
p<-tableGrob(model_b2_pos_xp03_lme4)
grid.arrange(p)
dev.off()

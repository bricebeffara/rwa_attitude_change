# File name: brms_models_xp08.R
# Online archive: gitlab
# Authors: Brice Beffara & Amélie Bret 
# Mon Jul 09 11:12:09 2018 ------------------------------
# Contact: brice.beffara@slowpen.science amelie.bret@univ-grenoble-alpes.fr http://slowpen.science
#
# This R script was used to build and compute brms models
# corresponding to the 08th experiment of Amelie Bret's doctoral work
#
# This R script defines and computes brms models
# main effects, interaction effects, and simple slopes of interest
# 
# 3 posependent variables of interest :
# RWA (continuous, centered and scaled)
# usvalence : positive (0.5) vs. negative (-0.5)
# and number of counter conditioning : 1 (- 0.5) vs. 2 (0.5)
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


# contrast of interest ----------------------------------------------------

# model
doubletime_resp_lme4 <- lmer(response ~ usvalence * contrast1 * RWAscore + (1|ppt) + (1|stim1),
                         data = doubletime_df)

# Save summary & confint
model_gen_xp08_lme4 <- round(cbind(summary(doubletime_resp_lme4)$coefficients,
                                   confint(doubletime_resp_lme4)[c(4:11),]), 2)

# export output
png("tables/lme4/model_gen_xp08_lme4.png", height=480, width=720)
p<-tableGrob(model_gen_xp08_lme4)
grid.arrange(p)
dev.off()

# Replication conditions = one conditioning vs.two ----------------------------------------------------

# model
doubleone_resp_lme4 <- lmer(response ~ usvalence * counter2 * RWAscore + (1|ppt) + (1|stim1),
                            data = double_one_df)

# Save summary & confint
model_one_xp08_lme4 <- round(cbind(summary(doubleone_resp_lme4)$coefficients,
                                   confint(doubleone_resp_lme4)[c(4:11),]), 2)

# export output
png("tables/lme4/model_one_xp08_lme4.png", height=480, width=720)
p<-tableGrob(model_one_xp08_lme4)
grid.arrange(p)
dev.off()

# First control condition = one conditioning followed by time delay vs.two ----------------------------------------------------

# model
doubleonetime_resp_lme4 <- lmer(response ~ usvalence * counter2 * RWAscore + (1|ppt) + (1|stim1),
                                data = double_onetime_df)

# Save summary & confint
model_onetime_xp08_lme4 <- round(cbind(summary(doubleonetime_resp_lme4)$coefficients,
                                       confint(doubleonetime_resp_lme4)[c(4:11),]), 2)

# export output
png("tables/lme4/model_onetime_xp08_lme4.png", height=480, width=720)
p<-tableGrob(model_onetime_xp08_lme4)
grid.arrange(p)
dev.off()

# Second control condition = time delay followed by one conditioning vs.two ----------------------------------------------------

# model
doubletimeone_resp_lme4 <- lmer(response ~ usvalence * counter2 * RWAscore + (1|ppt) + (1|stim1),
                                data = double_timeone_df)

# Save summary & confint
model_timeone_xp08_lme4 <- round(cbind(summary(doubletimeone_resp_lme4)$coefficients,
                                       confint(doubletimeone_resp_lme4)[c(4:11),]), 2)

# export output
png("tables/lme4/model_timeone_xp08_lme4.png", height=480, width=720)
p<-tableGrob(model_timeone_xp08_lme4)
grid.arrange(p)
dev.off()

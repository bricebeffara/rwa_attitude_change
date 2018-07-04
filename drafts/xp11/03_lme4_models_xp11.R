# File name: brms_models_xp11.R
# Online archive: gitlab
# Authors: Brice Beffara & Amélie Bret 
# Tue Jun 26 14:56:10 2018 ------------------------------
# Contact: brice.beffara@slowpen.science amelie.bret@univ-grenoble-alpes.fr http://slowpen.science
#
# This R script was used to build and compute brms models
# corresponding to the 10th experiment of Amelie Bret's doctoral work
#
# This R script defines and computes brms models
# main effects, interaction effects, and simple slopes of interest
# 
# 3 independent variables of interest :
# RWA (continuous, centered and scaled)
# usvalence : positive (0.5) vs. negative (-0.5)
# and warning : direct (0.5) vs. indirect (-0.5) conditioning
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
###    system (Windows, MacOS, or Linux).   
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
warn_resp_lme4 <- lmer(response ~ usvalence * warn * RWAscore + (1|ppt) + (1|stim1),
                   data = warn_df)

# Save summary & confint
model_gen_xp11_lme4 <- round(cbind(summary(warn_resp_lme4)$coefficients,
                             confint(warn_resp_lme4)[c(4:11),]), 2)

# export output
png("tables/lme4/model_gen_xp11_lme4.png", height=480, width=720)
p<-tableGrob(model_gen_xp11_lme4)
grid.arrange(p)
dev.off()

#------------------------------------------------------------------------------------
# Then we run our second step models to decompose interactions effects
# We look at the interaction between RWA and warning at each level of usvalence
#------------------------------------------------------------------------------------

#-------------
### RWA * warning in the !!positive!! valence condition
#-------------

# model
warn_resp_uspos_lme4 <- lmer(response ~ usvalence_pos * warn * RWAscore + (1|ppt) + (1|stim1),
                         data = warn_df)

# Save summary & confint
model_uspos_xp11_lme4 <- round(cbind(summary(warn_resp_uspos_lme4)$coefficients,
                                   confint(warn_resp_uspos_lme4)[c(4:11),]), 2)

# export output
png("tables/lme4/model_uspos_xp11_lme42.png", height=480, width=720)
p<-tableGrob(model_uspos_xp11_lme4)
grid.arrange(p)
dev.off()

#-------------
### RWA * warning in the !!negative!! valence condition
#-------------

# model
warn_resp_usneg_lme4 <- lmer(response ~ usvalence_neg * warn * RWAscore + (1|ppt) + (1|stim1),
                               data = warn_df)

# Save summary & confint
model_usneg_xp11_lme4 <- round(cbind(summary(warn_resp_usneg_lme4)$coefficients,
                                     confint(warn_resp_usneg_lme4)[c(4:11),]), 2)

# export output
png("tables/lme4/model_usneg_xp11_lme4.png", height=480, width=720)
p<-tableGrob(model_usneg_xp11_lme4)
grid.arrange(p)
dev.off()

#------------------------------------------------------------------------------------
# Then we run our third step models to decompose the interaction 
# between RWA and warning in the usvalence positive condition
# (We don't decompose the interaction in the usvalence positive condition
# the parameter includes 0. See table model_usneg_xp11_lme4.png)
#------------------------------------------------------------------------------------

#-------------
### Simple slope of RWA in the !!direct!! conditioning & !!positive!! valence condition
#-------------

#model
warn_resp_uspos_dir_lme4 <- lmer(response ~ usvalence_pos * warn_dir * RWAscore + (1|ppt) + (1|stim1),
                              data = warn_df)

# Save summary & confint
model_uspos_dir_xp11_lme4 <- round(cbind(summary(warn_resp_uspos_dir_lme4)$coefficients,
                                     confint(warn_resp_uspos_dir_lme4)[c(4:11),]), 2)

# export output
png("tables/lme4/model_uspos_dir_xp11_lme4.png", height=480, width=720)
p<-tableGrob(model_uspos_dir_xp11_lme4)
grid.arrange(p)
dev.off()

#-------------
### Simple slope of RWA in the !!indirect!! conditioning & !!positive!! valence condition
#-------------

## first step = recode variable warning
## We interpret parameters for a 0 level of others

#model
warn_resp_uspos_ind_lme4 <- lmer(response ~ usvalence_pos * warn_ind * RWAscore + (1|ppt) + (1|stim1),
                                   data = warn_df)

# Save summary & confint
model_uspos_ind_xp11_lme4 <- round(cbind(summary(warn_resp_uspos_ind_lme4)$coefficients,
                                         confint(warn_resp_uspos_ind_lme4)[c(4:11),]), 2)

# export output
png("tables/lme4/model_uspos_ind_xp11_lme4.png", height=480, width=720)
p<-tableGrob(model_uspos_ind_xp11_lme4)
grid.arrange(p)
dev.off()


# other effects -----------------------------------------------------------


#-------------
### Simple slopes of valence and RWA in the !!direct! conditioning condition
#-------------

#model
warn_resp_dir_lme4 <- lmer(response ~ usvalence * warn_dir * RWAscore + (1|ppt) + (1|stim1),
                             data = warn_df)

# Save summary & confint
model_dir_xp11_lme4 <- round(cbind(summary(warn_resp_dir_lme4)$coefficients,
                                   confint(warn_resp_dir_lme4)[c(4:11),]), 2)

# export output
png("tables/lme4/model_dir_xp11_lme4.png", height=480, width=720)
p<-tableGrob(model_dir_xp11_lme4)
grid.arrange(p)
dev.off()


#-------------
### Simple slopes of valence and RWA in the !!indirect! conditioning condition
#-------------

#model
warn_resp_ind_lme4 <- lmer(response ~ usvalence * warn_ind * RWAscore + (1|ppt) + (1|stim1),
                             data = warn_df)

# Save summary & confint
model_ind_xp11_lme4 <- round(cbind(summary(warn_resp_ind_lme4)$coefficients,
                                   confint(warn_resp_ind_lme4)[c(4:11),]), 2)

# export output
png("tables/lme4/model_ind_xp11_lme4.png", height=480, width=720)
p<-tableGrob(model_ind_xp11_lme4)
grid.arrange(p)
dev.off()
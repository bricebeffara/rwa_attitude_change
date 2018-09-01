# File name: brms_models_xp06.R
# Online archive: gitlab
# Authors: Brice Beffara & Amélie Bret 
# Sat Sep  1 17:17:41 2018 ------------------------------
# Contact: brice.beffara@slowpen.science amelie.bret@univ-grenoble-alpes.fr http://slowpen.science
#
# This R script was used to build and compute brms models
# corresponding to the 06th experiment of Amelie Bret's doctoral work
#
# This R script defines and computes lme4 models
# main effects, interaction effects, and simple slopes of interest
# of RWA with grebles' ratings in all combinations of the other indepentdent variables :
# usvalencedir : positive (0.5) vs. negative (-0.5)
# and order : direct first (0.5) vs. indirect first (-0.5)
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
direct_resp_lme4 <- lmer(response ~ usvalencedir * order * RWAscore + (1|ppt) + (1|stim1),
                   data = direct_df)

# Save summary & confint
direct_gen_xp06_lme4 <- round(cbind(summary(direct_resp_lme4)$coefficients,
                             confint(direct_resp_lme4)[c(4:11),]), 2)

# export output
png("tables/lme4/direct_gen_xp06_lme4.png", height=480, width=720)
p<-tableGrob(direct_gen_xp06_lme4)
grid.arrange(p)
dev.off()

#------------------------------------------------------------------------------------
# Then we run our second step models to decompose interactions effects
# We look at the interaction between RWA and usvalencedir at each level of order
#------------------------------------------------------------------------------------

#-------------
### RWA * valence in the !!indirect ratings before direct ratings (order1)!! condition
#-------------

# model
direct_resp_ord1_lme4 <- lmer(response ~ usvalencedir * order1  * RWAscore + (1|ppt) + (1|stim1),
                         data = direct_df)

# Save summary & confint
model_ord1_xp06_lme4 <- round(cbind(summary(direct_resp_ord1_lme4)$coefficients,
                                   confint(direct_resp_ord1_lme4)[c(4:11),]), 2)

# export output
png("tables/lme4/model_ord1_xp06_lme42.png", height=480, width=720)
p<-tableGrob(model_ord1_xp06_lme4)
grid.arrange(p)
dev.off()

#-------------
### RWA * usvalencedir in the !!direct ratings before indirect ratings (order2)!! condition
#-------------

# model
direct_resp_ord2_lme4 <- lmer(response ~ usvalencedir * order2  * RWAscore + (1|ppt) + (1|stim1),
                               data = direct_df)

# Save summary & confint
model_ord2_xp06_lme4 <- round(cbind(summary(direct_resp_ord2_lme4)$coefficients,
                                     confint(direct_resp_ord2_lme4)[c(4:11),]), 2)

# export output
png("tables/lme4/model_ord2_xp06_lme4.png", height=480, width=720)
p<-tableGrob(model_ord2_xp06_lme4)
grid.arrange(p)
dev.off()

#------------------------------------------------------------------------------------
# Then we run our third step models to decompose the interactions 
# between RWA and usvalencedir in the order 2 condition
#------------------------------------------------------------------------------------

#############################
##################### Order 2
#############################

#-------------
### Simple slope of RWA in the !!negative!! valence & !!order2!! condition
#-------------

#model
direct_resp_ord2_neg_lme4 <- lmer(response ~ usvalencedir_neg * order2  * RWAscore + (1|ppt) + (1|stim1),
                                   data = direct_df)

# Save summary & confint
model_ord2_neg_xp06_lme4 <- round(cbind(summary(direct_resp_ord2_neg_lme4)$coefficients,
                                         confint(direct_resp_ord2_neg_lme4)[c(4:11),]), 2)

# export output
png("tables/lme4/model_ord2_neg_xp06_lme4.png", height=480, width=720)
p<-tableGrob(model_ord2_neg_xp06_lme4)
grid.arrange(p)
dev.off()

#-------------
### Simple slope of RWA in the !!positive!! valence & !!order2!! condition
#-------------

#model
direct_resp_ord2_pos_lme4 <- lmer(response ~ usvalencedir_pos * order2  * RWAscore + (1|ppt) + (1|stim1),
                                   data = direct_df)

# Save summary & confint
model_ord2_pos_xp06_lme4 <- round(cbind(summary(direct_resp_ord2_pos_lme4)$coefficients,
                                         confint(direct_resp_ord2_pos_lme4)[c(4:11),]), 2)

# export output
png("tables/lme4/model_ord2_pos_xp06_lme4.png", height=480, width=720)
p<-tableGrob(model_ord2_pos_xp06_lme4)
grid.arrange(p)
dev.off()

#############################
##################### w/o order
#############################

#-------------
### Simple slope of RWA in the !!negative!! valence condition
#-------------

#model
direct_resp_neg_lme4 <- lmer(response ~ usvalencedir_neg * RWAscore + (1|ppt) + (1|stim1),
                                 data = direct_df)

# Save summary & confint
model_neg_xp06_lme4 <- round(cbind(summary(direct_resp_neg_lme4)$coefficients,
                                        confint(direct_resp_neg_lme4)[c(4:7),]), 2)

# export output
png("tables/lme4/model_neg_xp06_lme4.png", height=480, width=720)
p<-tableGrob(model_neg_xp06_lme4)
grid.arrange(p)
dev.off()


#-------------
### Simple slope of RWA in the !!positive!! valence condition
#-------------

#model
direct_resp_pos_lme4 <- lmer(response ~ usvalencedir_pos  * RWAscore + (1|ppt) + (1|stim1),
                                 data = direct_df)

# Save summary & confint
model_pos_xp06_lme4 <- round(cbind(summary(direct_resp_pos_lme4)$coefficients,
                                        confint(direct_resp_pos_lme4)[c(4:7),]), 2)

# export output
png("tables/lme4/model_pos_xp06_lme4.png", height=480, width=720)
p<-tableGrob(model_pos_xp06_lme4)
grid.arrange(p)
dev.off()

# File name: 02_brms_models_xp11.R
# Online archive: gitlab
# Authors: Brice Beffara & Amélie Bret 
# Tue Jun 26 11:06:46 2018 ------------------------------
# Contact: brice.beffara@slowpen.science amelie.bret@univ-grenoble-alpes.fr http://slowpen.science
#
# This R script was used to build and compute brms models
# corresponding to the 11th experiment of Amelie Bret's doctoral work
#
# This R script defines and computes brms models
# main effects, interaction effects, and simple slopes of interest
# 
# 3 independent variables of interest :
# RWA (continuous, centered and scaled)
# usvalence : positive (0.5) vs. negative (-0.5)
# and warn : warning (0.5) vs. no warning (-0.5)
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

p_load(brms, # main package for models
       htmlTable, # helps to extract results
       xtable,
       install = TRUE,
       gridExtra,
       sjstats,
       sjmisc,
       update = getOption("pac_update"),
       character.only = FALSE)

#------------------------------------------------------------------------------------
# First we define priors for our models
#------------------------------------------------------------------------------------

priors <- c(
  prior(normal(0, 10), class = Intercept, coef = ""),
  prior(normal(0, 0.5), class = b),
  prior(exponential(2), class = sd) )

# In case we want to save summaries
# col2keep <- c("Estimate", "l-95% CI", "u-95% CI")

#------------------------------------------------------------------------------------
# Then we run our first model for fixed main and interaction effects
#------------------------------------------------------------------------------------

# model
warn_resp <- brm(response ~ usvalence * warn  * RWAscore + (1|ppt) + (1|stim1),
                   data = warn_df, 
                   family = cumulative (link = "logit", threshold = "flexible"),
                   prior = priors,
                   warmup = 1000, iter = 2000,
                   chains = 4, cores = parallel::detectCores(),
                   control = list(adapt_delta = 0.8, max_treedepth = 10),
                   sample_prior = TRUE)

# In case we want to save summary ! change the name if you export both summary and tidy_stan
# model_gen_xp11 <- summary(warn_resp)$fixed[,col2keep]
# model_gen_xp11 <- round(model_gen_xp11, 3)

# arrange output
model_gen_xp11 <- tidy_stan(warn_resp,
                            typical = "mean",
                            prob = .95)


# export output
png("tables//brms/model_gen_xp11.png", height=480, width=720)
p<-tableGrob(model_gen_xp11)
grid.arrange(p)
dev.off()

# test with rope
equi_gen_xp11 <- equi_test(warn_resp, rope = c(-0.2, 0.2))
equi_gen_xp11 <- equi_gen_xp11[c(9:15),]
equi_gen_xp11[,c(3:5)] <- round(equi_gen_xp11[,c(3:5)], 2)

# export test
png("tables/rope/equi_gen_xp11.png", height=480, width=720)
p<-tableGrob(equi_gen_xp11)
grid.arrange(p)
dev.off()

#------------------------------------------------------------------------------------
# Then we run our second step models to decompose interactions effects
# We look at the interaction between RWA and spreading at each level of usvalence
#------------------------------------------------------------------------------------

#-------------
### RWA * warn in the !!positive!! valence condition
#-------------

## first step = recode variable usvalence
## We interpret parameters for a 0 level of others

# Coding !!positive!! valence as 0
warn_df$usvalence_pos <- ifelse (warn_df$usvalence == 0.5, 0, 1)

warn_resp_uspos <- brm(response ~ usvalence_pos * warn  * RWAscore + (1|ppt) + (1|stim1), #here we change the usvalence variable with the recoded one
                   data = warn_df, 
                   family = cumulative (link = "logit", threshold = "flexible"),
                   prior = priors,
                   warmup = 1000, iter = 2000,
                   chains = 4, cores = parallel::detectCores(),
                   control = list(adapt_delta = 0.8, max_treedepth = 10),
                   sample_prior = TRUE)

# In case we want to save summary ! change the name if you export both summary and tidy_stan
# model_uspos_xp11 <- summary(warn_resp_uspos)$fixed[,col2keep]
# model_uspos_xp11 <- round(model_uspos_xp11, 3)

# arrange output
model_uspos_xp11 <- tidy_stan(warn_resp_uspos,
                              typical = "mean",
                              prob = .95)

# export output
png("tables/model_uspos_xp11.png", height=480, width=720)
p<-tableGrob(model_uspos_xp11)
grid.arrange(p)
dev.off()

# test with rope
equi_uspos_xp11 <- equi_test(warn_resp_uspos, rope = c(-0.2, 0.2))
equi_uspos_xp11 <- equi_uspos_xp11[c(9:15),]
equi_uspos_xp11[,c(3:5)] <- round(equi_uspos_xp11[,c(3:5)], 2)

# export test
png("tables/rope/equi_uspos_xp11.png", height=480, width=720)
p<-tableGrob(equi_uspos_xp11)
grid.arrange(p)
dev.off()


#-------------
### RWA * warn in the !!negative!! valence condition
#-------------

## first step = recode variable usvalence
## We interpret parameters for a 0 level of others

# Coding !!negative!! valence as 0
warn_df$usvalence_neg <- ifelse (warn_df$usvalence == -0.5, 0, 1)

warn_resp_usneg <- brm(response ~ usvalence_neg * warn  * RWAscore + (1|ppt) + (1|stim1), #here we change the usvalence variable with the recoded one
                         data = warn_df, 
                         family = cumulative (link = "logit", threshold = "flexible"),
                         prior = priors,
                         warmup = 1000, iter = 2000,
                         chains = 4, cores = parallel::detectCores(),
                         control = list(adapt_delta = 0.8, max_treedepth = 10),
                         sample_prior = TRUE)

# In case we want to save summary ! change the name if you export both summary and tidy_stan
# model_usneg_xp11 <- summary(warn_resp_usneg)$fixed[,col2keep]
# model_usneg_xp11 <- round(model_usneg_xp11, 3)

# arrange output
model_usneg_xp11 <- tidy_stan(warn_resp_usneg,
                              typical = "mean",
                              prob = .95)

# export output
png("tables/model_usneg_xp11.png", height=480, width=720)
p<-tableGrob(model_usneg_xp11)
grid.arrange(p)
dev.off()

# test with rope
equi_usneg_xp11 <- equi_test(warn_resp_usneg, rope = c(-0.2, 0.2))
equi_usneg_xp11 <- equi_usneg_xp11[c(9:15),]
equi_usneg_xp11[,c(3:5)] <- round(equi_usneg_xp11[,c(3:5)], 2)

# export test
png("tables/rope/equi_usneg_xp11.png", height=480, width=720)
p<-tableGrob(equi_usneg_xp11)
grid.arrange(p)
dev.off()

#------------------------------------------------------------------------------------
# Then we run our third step models to decompose the interaction 
# between RWA and spreading in the usvalence positive condition
# (We don't decompose the interaction in the usvalence positive condition
# the parameter includes 0. See table model_usneg_xp11.png)
#------------------------------------------------------------------------------------

#-------------
### Simple slope of RWA in the !!direct!! conditioning & !!positive!! valence condition
#-------------

## first step = recode variable spreading
## We interpret parameters for a 0 level of others

# Coding !!direct!! conditioning as 0
warn_df$spreading_dir <- ifelse (warn_df$spreading == -0.5, 0, 1)

warn_resp_uspos_dir <- brm(response ~ usvalence_pos * warn_dir  * RWAscore + (1|ppt) + (1|stim1), #here we change the spreading variable with the recoded one
                         data = warn_df, 
                         family = cumulative (link = "logit", threshold = "flexible"),
                         prior = priors,
                         warmup = 1000, iter = 2000,
                         chains = 4, cores = parallel::detectCores(),
                         control = list(adapt_delta = 0.8, max_treedepth = 10),
                         sample_prior = TRUE)

# In case we want to save summary ! change the name if you export both summary and tidy_stan
# model_uspos_dir_xp11 <- summary(warn_resp_uspos_dir)$fixed[,col2keep]
# model_uspos_dir_xp11 <- round(model_uspos_dir_xp11, 3)

# arrange output
model_uspos_dir_xp11 <- tidy_stan(warn_resp_uspos_dir,
                              typical = "mean",
                              prob = .95)

# export output
png("tables/model_uspos_dir_xp11.png", height=480, width=720)
p<-tableGrob(model_uspos_dir_xp11)
grid.arrange(p)
dev.off()

# test with rope
equi_uspos_dir_xp11 <- equi_test(warn_resp_uspos_dir, rope = c(-0.2, 0.2))
equi_uspos_dir_xp11 <- equi_uspos_dir_xp11[c(9:15),]
equi_uspos_dir_xp11[,c(3:5)] <- round(equi_uspos_dir_xp11[,c(3:5)], 2)

# export test
png("tables/rope/equi_uspos_dir_xp11.png", height=480, width=720)
p<-tableGrob(equi_uspos_dir_xp11)
grid.arrange(p)
dev.off()

#-------------
### Simple slope of RWA in the !!indirect!! conditioning & !!positive!! valence condition
#-------------

## first step = recode variable spreading
## We interpret parameters for a 0 level of others

# Coding !!indirect!! conditioning as 0
warn_df$spreading_ind <- ifelse (warn_df$spreading == 0.5, 0, 1)

warn_resp_uspos_ind <- brm(response ~ usvalence_pos * warn_ind  * RWAscore + (1|ppt) + (1|stim1), #here we change the spreading variable with the recoded one
                             data = warn_df, 
                             family = cumulative (link = "logit", threshold = "flexible"),
                             prior = priors,
                             warmup = 1000, iter = 2000,
                             chains = 4, cores = parallel::detectCores(),
                             control = list(adapt_delta = 0.8, max_treedepth = 10),
                             sample_prior = TRUE)

# In case we want to save summary ! change the name if you export both summary and tidy_stan
# model_uspos_ind_xp11 <- summary(warn_resp_uspos_ind)$fixed[,col2keep]
# model_uspos_ind_xp11 <- round(model_uspos_ind_xp11, 3)

# arrange output
model_uspos_ind_xp11 <- tidy_stan(warn_resp_uspos_ind,
                                  typical = "mean",
                                  prob = .95)

# export output
png("tables/model_uspos_ind_xp11.png", height=480, width=720)
p<-tableGrob(model_uspos_ind_xp11)
grid.arrange(p)
dev.off()

# test with rope
equi_uspos_ind_xp11 <- equi_test(warn_resp_uspos_ind, rope = c(-0.2, 0.2))
equi_uspos_ind_xp11 <- equi_uspos_ind_xp11[c(9:15),]
equi_uspos_ind_xp11[,c(3:5)] <- round(equi_uspos_ind_xp11[,c(3:5)], 2)

# export test
png("tables/rope/equi_uspos_ind_xp11.png", height=480, width=720)
p<-tableGrob(equi_uspos_ind_xp11)
grid.arrange(p)
dev.off()

# other effects -----------------------------------------------------------


#-------------
### Simple slopes of valence and RWA in the !!direct! conditioning condition
#-------------

warn_resp_dir <- brm(response ~ usvalence * warn_dir  * RWAscore + (1|ppt) + (1|stim1), #here we change the spreading variable with the recoded one
                             data = warn_df, 
                             family = cumulative (link = "logit", threshold = "flexible"),
                             prior = priors,
                             warmup = 1000, iter = 2000,
                             chains = 4, cores = parallel::detectCores(),
                             control = list(adapt_delta = 0.8, max_treedepth = 10),
                             sample_prior = TRUE)

# In case we want to save summary ! change the name if you export both summary and tidy_stan
# model_uspos_ind_xp11 <- summary(warn_resp_uspos_ind)$fixed[,col2keep]
# model_uspos_ind_xp11 <- round(model_uspos_ind_xp11, 3)

# arrange output
model_dir_xp11 <- tidy_stan(warn_resp_dir,
                                  typical = "mean",
                                  prob = .95)

# export output
png("tables/model_dir_xp11.png", height=480, width=720)
p<-tableGrob(model_dir_xp11)
grid.arrange(p)
dev.off()

# test with rope
equi_dir_xp11 <- equi_test(warn_resp_dir, rope = c(-0.2, 0.2))
equi_dir_xp11 <- equi_dir_xp11[c(9:15),]
equi_dir_xp11[,c(3:5)] <- round(equi_dir_xp11[,c(3:5)], 2)

# export test
png("tables/rope/equi_dir_xp11.png", height=480, width=720)
p<-tableGrob(equi_dir_xp11)
grid.arrange(p)
dev.off()

#-------------
### Simple slopes of valence and RWA in the !!indirect! conditioning condition
#-------------

warn_resp_ind <- brm(response ~ usvalence * warn_ind  * RWAscore + (1|ppt) + (1|stim1), #here we change the spreading variable with the recoded one
                       data = warn_df, 
                       family = cumulative (link = "logit", threshold = "flexible"),
                       prior = priors,
                       warmup = 1000, iter = 2000,
                       chains = 4, cores = parallel::detectCores(),
                       control = list(adapt_delta = 0.8, max_treedepth = 10),
                       sample_prior = TRUE)

# In case we want to save summary ! change the name if you export both summary and tidy_stan
# model_uspos_ind_xp11 <- summary(warn_resp_uspos_ind)$fixed[,col2keep]
# model_uspos_ind_xp11 <- round(model_uspos_ind_xp11, 3)

# arrange output
model_ind_xp11 <- tidy_stan(warn_resp_ind,
                            typical = "mean",
                            prob = .95)

# export output
png("tables/model_ind_xp11.png", height=480, width=720)
p<-tableGrob(model_ind_xp11)
grid.arrange(p)
dev.off()

# test with rope
equi_ind_xp11 <- equi_test(warn_resp_ind, rope = c(-0.2, 0.2))
equi_ind_xp11 <- equi_ind_xp11[c(9:15),]
equi_ind_xp11[,c(3:5)] <- round(equi_ind_xp11[,c(3:5)], 2)

# export test
png("tables/rope/equi_ind_xp11.png", height=480, width=720)
p<-tableGrob(equi_ind_xp11)
grid.arrange(p)
dev.off()

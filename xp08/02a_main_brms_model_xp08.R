# File name: brms_models_xp08.R
# Online archive: gitlab
# Authors: Brice Beffara & Amélie Bret 
# Sun Jul 08 13:00:42 2018 ------------------------------
# Contact: brice.beffara@slowpen.science amelie.bret@univ-grenoble-alpes.fr http://slowpen.science
#
# This R script was used to build and compute brms models
# corresponding to the 08th experiment of Amelie Bret's doctoral work
#
# This R script defines and computes brms models
# main effects, interaction effects, and simple slopes of interest
# 
# 3 independent variables of interest :
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

# Define ROPE for effects involving RWA
# We compute a global maximum range of RWA

rrwa <- round(max ( doubletime_df$RWAscore) - min ( doubletime_df$RWAscore))

# ROPE = 1 (minimum change on rating scale) / rrwa (global RWA range) / 2 (e.g. see Kruschke, 2018 p.6)
#(rounded in order to have integers simpler to visualise. We don't need very high precision)
# (here we devide or smallest possible effect by two in order to take variations into account,
# e.g. RWA range above limits
# and avoid to be too conservative just above/below lower/upperlimits.
# e.g. we consider that somme intermediate values could be of interest on the latent metric scale (i.e. between 0.5 and 1))

roperwaN <- c( round( -1/rrwa/2, digits = 1), round( 1/rrwa/2, digits = 1))
roperwaC <- paste( roperwaN[1], ", ", roperwaN[2], sep = "")


#------------------------------------------------------------------------------------
# Then we run our first model for fixed main and interaction effects
#------------------------------------------------------------------------------------


# Contrast of interest ----------------------------------------------------

# model
doubletime_resp <- brm(response ~ usvalence * contrast1 * RWAscore + (1|ppt) + (1|stim1),
                   data = doubletime_df, 
                   family = cumulative (link = "logit", threshold = "flexible"),
                   prior = priors,
                   warmup = 1000, iter = 2000,
                   chains = 4, cores = parallel::detectCores(),
                   control = list(adapt_delta = 0.8, max_treedepth = 10),
                   sample_prior = TRUE)

# In case we want to save summary ! change the name if you export both summary and tidy_stan
# model_gen_xp08 <- summary(doubletime_resp)$fixed[,col2keep]
# model_gen_xp08 <- round(model_gen_xp08, 3)

# arrange output
model_gen_xp08 <- tidy_stan(doubletime_resp,
                            typical = "mean",
                            prob = .95)


# export output
png("tables/brms/model_gen_xp08.png", height=480, width=720)
p<-tableGrob(model_gen_xp08)
grid.arrange(p)
dev.off()

# test with rope
equi_gen_xp08_rwa <- equi_test(doubletime_resp, rope = roperwaN)
equi_gen_xp08_rwa <- equi_gen_xp08_rwa[c(11,13:15),]
equi_gen_xp08_rwa$ROPE <- roperwaC

equi_gen_xp08_0.5 <- equi_test(doubletime_resp, rope = c(-0.5, 0.5))
equi_gen_xp08_0.5 <- equi_gen_xp08_0.5[c(9,10,12),]
equi_gen_xp08_0.5$ROPE <- "-0.5, 0.5"

equi_gen_xp08 <- rbind(equi_gen_xp08_rwa, equi_gen_xp08_0.5)
equi_gen_xp08[,c(3:5)] <- round(equi_gen_xp08[,c(3:5)], 2)
ordrow <- c("5","6","1","7","2","3","4")
equi_gen_xp08 <- equi_gen_xp08[ordrow,]

# export test
png("tables/rope/equi_gen_xp08.png", height=480, width=720)
p<-tableGrob(equi_gen_xp08)
grid.arrange(p)
dev.off()

# Replication conditions = one conditioning vs.two ----------------------------------------------------

# model
doubleone_resp <- brm(response ~ usvalence * counter2 * RWAscore + (1|ppt) + (1|stim1),
                      data = double_one_df, 
                      family = cumulative (link = "logit", threshold = "flexible"),
                      prior = priors,
                      warmup = 1000, iter = 2000,
                      chains = 4, cores = parallel::detectCores(),
                      control = list(adapt_delta = 0.8, max_treedepth = 10),
                      sample_prior = TRUE)

# In case we want to save summary ! change the name if you export both summary and tidy_stan
# model_one_xp08 <- summary(doubleone_resp)$fixed[,col2keep]
# model_one_xp08 <- round(model_one_xp08, 3)

# arrange output
model_one_xp08 <- tidy_stan(doubleone_resp,
                            typical = "mean",
                            prob = .95)


# export output
png("tables/brms/model_one_xp08.png", height=480, width=720)
p<-tableGrob(model_one_xp08)
grid.arrange(p)
dev.off()

# test with rope
equi_one_xp08_rwa <- equi_test(doubleone_resp, rope = roperwaN)
equi_one_xp08_rwa <- equi_one_xp08_rwa[c(11,13:15),]
equi_one_xp08_rwa$ROPE <- roperwaC

equi_one_xp08_0.5 <- equi_test(doubleone_resp, rope = c(-0.5, 0.5))
equi_one_xp08_0.5 <- equi_one_xp08_0.5[c(9,10,12),]
equi_one_xp08_0.5$ROPE <- "-0.5, 0.5"

equi_one_xp08 <- rbind(equi_one_xp08_rwa, equi_one_xp08_0.5)
equi_one_xp08[,c(3:5)] <- round(equi_one_xp08[,c(3:5)], 2)
ordrow <- c("5","6","1","7","2","3","4")
equi_one_xp08 <- equi_one_xp08[ordrow,]

# export test
png("tables/rope/equi_one_xp08.png", height=480, width=720)
p<-tableGrob(equi_one_xp08)
grid.arrange(p)
dev.off()

# First control condition = one conditioning followed by time delay vs.two ----------------------------------------------------

# model
doubleonetime_resp <- brm(response ~ usvalence * counter2 * RWAscore + (1|ppt) + (1|stim1),
                          data = double_onetime_df, 
                          family = cumulative (link = "logit", threshold = "flexible"),
                          prior = priors,
                          warmup = 1000, iter = 2000,
                          chains = 4, cores = parallel::detectCores(),
                          control = list(adapt_delta = 0.8, max_treedepth = 10),
                          sample_prior = TRUE)

# In case we want to save summary ! change the name if you export both summary and tidy_stan
# model_onetime_xp08 <- summary(doubleonetime_resp)$fixed[,col2keep]
# model_onetime_xp08 <- round(model_onetime_xp08, 3)

# arrange output
model_onetime_xp08 <- tidy_stan(doubleonetime_resp,
                                typical = "mean",
                                prob = .95)


# export output
png("tables/brms/model_onetime_xp08.png", height=480, width=720)
p<-tableGrob(model_onetime_xp08)
grid.arrange(p)
dev.off()

# test with rope
equi_onetime_xp08_rwa <- equi_test(doubleonetime_resp, rope = roperwaN)
equi_onetime_xp08_rwa <- equi_onetime_xp08_rwa[c(11,13:15),]
equi_onetime_xp08_rwa$ROPE <- roperwaC

equi_onetime_xp08_0.5 <- equi_test(doubleonetime_resp, rope = c(-0.5, 0.5))
equi_onetime_xp08_0.5 <- equi_onetime_xp08_0.5[c(9,10,12),]
equi_onetime_xp08_0.5$ROPE <- "-0.5, 0.5"

equi_onetime_xp08 <- rbind(equi_onetime_xp08_rwa, equi_onetime_xp08_0.5)
equi_onetime_xp08[,c(3:5)] <- round(equi_onetime_xp08[,c(3:5)], 2)
ordrow <- c("5","6","1","7","2","3","4")
equi_onetime_xp08 <- equi_onetime_xp08[ordrow,]

# export test
png("tables/rope/equi_onetime_xp08.png", height=480, width=720)
p<-tableGrob(equi_onetime_xp08)
grid.arrange(p)
dev.off()

# Second control condition = time delay followed by one conditioning vs.two ----------------------------------------------------

# model
doubletimeone_resp <- brm(response ~ usvalence * counter2 * RWAscore + (1|ppt) + (1|stim1),
                          data = double_timeone_df, 
                          family = cumulative (link = "logit", threshold = "flexible"),
                          prior = priors,
                          warmup = 1000, iter = 2000,
                          chains = 4, cores = parallel::detectCores(),
                          control = list(adapt_delta = 0.8, max_treedepth = 10),
                          sample_prior = TRUE)

# In case we want to save summary ! change the name if you export both summary and tidy_stan
# model_timeone_xp08 <- summary(doubletimeone_resp)$fixed[,col2keep]
# model_timeone_xp08 <- round(model_timeone_xp08, 3)

# arrange output
model_timeone_xp08 <- tidy_stan(doubletimeone_resp,
                                typical = "mean",
                                prob = .95)


# export output
png("tables/brms/model_timeone_xp08.png", height=480, width=720)
p<-tableGrob(model_timeone_xp08)
grid.arrange(p)
dev.off()

# test with rope
equi_timeone_xp08_rwa <- equi_test(doubletimeone_resp, rope = roperwaN)
equi_timeone_xp08_rwa <- equi_timeone_xp08_rwa[c(11,13:15),]
equi_timeone_xp08_rwa$ROPE <- roperwaC

equi_timeone_xp08_0.5 <- equi_test(doubletimeone_resp, rope = c(-0.5, 0.5))
equi_timeone_xp08_0.5 <- equi_timeone_xp08_0.5[c(9,10,12),]
equi_timeone_xp08_0.5$ROPE <- "-0.5, 0.5"

equi_timeone_xp08 <- rbind(equi_timeone_xp08_rwa, equi_timeone_xp08_0.5)
equi_timeone_xp08[,c(3:5)] <- round(equi_timeone_xp08[,c(3:5)], 2)
ordrow <- c("5","6","1","7","2","3","4")
equi_timeone_xp08 <- equi_timeone_xp08[ordrow,]

# export test
png("tables/rope/equi_timeone_xp08.png", height=480, width=720)
p<-tableGrob(equi_timeone_xp08)
grid.arrange(p)
dev.off()


# See 02b_decint2_brms_models_xp08.R for other models 
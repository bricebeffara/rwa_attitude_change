# File name: brms_models_xp03.R
# Online archive: gitlab
# Authors: Brice Beffara & Amélie Bret 
# Mon Aug 06 13:59:37 2018 ------------------------------
# Contact: brice.beffara@slowpen.science amelie.bret@univ-grenoble-alpes.fr http://slowpen.science
#
# This R script was used to build and compute brms models
# corresponding to the 3rd experiment of Amelie Bret's doctoral work
#
# This R script defines and computes brms models
# main effects, interaction effects, and simple slopes of interest
# 
# 3 independent variables of interest :
# RWA (continuous, centered and scaled)
# usvalence2 : positive (0.5) vs. negative (-0.5)
# and bloc : bloc(0.5) vs. object (-0.5)
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
### 1. Install the gen_bloc1eral-purpose programming language R from  
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

rrwa <- round(max ( bloc_df$RWAscore) - min ( bloc_df$RWAscore))

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

# model
bloc1_resp <- brm(response ~ usvalence1 * bloc  * RWAscore + (1|ppt) + (1|stim1),
                   data = bloc_df, 
                   family = cumulative (link = "logit", threshold = "flexible"),
                   prior = priors,
                   warmup = 1000, iter = 2000,
                   chains = 4, cores = parallel::detectCores(),
                   control = list(adapt_delta = 0.8, max_treedepth = 10),
                   sample_prior = TRUE)

# In case we want to save summary ! change the name if you export both summary and tidy_stan
# model_gen_bloc1_xp03 <- summary(bloc1_resp)$fixed[,col2keep]
# model_gen_bloc1_xp03 <- round(model_gen_bloc1_xp03, 3)

# arrange output
model_gen_bloc1_xp03 <- tidy_stan(bloc1_resp,
                            typical = "mean",
                            prob = .95)


# export output
png("tables/brms/model_gen_bloc1_xp03.png", height=480, width=720)
p<-tableGrob(model_gen_bloc1_xp03)
grid.arrange(p)
dev.off()

# test with rope
equi_gen_bloc1_xp03_rwa <- equi_test(bloc1_resp, rope = roperwaN)
equi_gen_bloc1_xp03_rwa <- equi_gen_bloc1_xp03_rwa[c(11,13:15),]
equi_gen_bloc1_xp03_rwa$ROPE <- roperwaC

equi_gen_bloc1_xp03_0.5 <- equi_test(bloc1_resp, rope = c(-0.5, 0.5))
equi_gen_bloc1_xp03_0.5 <- equi_gen_bloc1_xp03_0.5[c(9,10,12),]
equi_gen_bloc1_xp03_0.5$ROPE <- "-0.5, 0.5"

equi_gen_bloc1_xp03 <- rbind(equi_gen_bloc1_xp03_rwa, equi_gen_bloc1_xp03_0.5)
equi_gen_bloc1_xp03[,c(3:5)] <- round(equi_gen_bloc1_xp03[,c(3:5)], 2)
ordrow <- c(5, 6, 1, 7, 2, 3, 4)
equi_gen_bloc1_xp03 <- equi_gen_bloc1_xp03[ordrow,]

# export test
png("tables/rope/equi_gen_bloc1_xp03.png", height=480, width=720)
p<-tableGrob(equi_gen_bloc1_xp03)
grid.arrange(p)
dev.off()

# See 02e_decint2_brms_1bloc_xp03 for other models 
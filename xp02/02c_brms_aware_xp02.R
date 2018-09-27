# File name: brms_models_xp02.R
# Online archive: gitlab
# Authors: Brice Beffara & Amélie Bret 
# Mon Aug 06 13:59:37 2018 ------------------------------
# Contact: brice.beffara@slowpen.science amelie.bret@univ-grenoble-alpes.fr http://slowpen.science
#
# This R script was used to build and compute brms models
# corresponding to the 2d experiment of Amelie Bret's doctoral work
#
# This R script defines and computes brms models
# main effects, interaction effects, and simple slopes of interest
# 
# 3 independent variables of interest :
# RWA (continuous, centered and scaled)
# usvalence : positive (0.5) vs. negative (-0.5)
# and social : social(0.5) vs. object (-0.5)
#
# and 1 pseudo-continuous dependent variables :
# Awareness of pairing from 0% to 100%
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
  prior(cauchy(0, 5), class = sd) )

# In case we want to save summaries
# col2keep <- c("Estimate", "l-95% CI", "u-95% CI")

# Define ROPE for effects involving RWA
# We compute a global maximum range of RWA

rrwa <- round(max ( social_df$RWAscore) - min ( social_df$RWAscore))

# ROPE = 1 (minimum change on rating scale) / rrwa (global RWA range) / 2 (e.g. see Kruschke, 2018 p.6)
#(rounded in order to have integers simpler to visualise. We don't need very high precision)
# (here we devide or smallest possible effect by two in order to take variations into account,
# e.g. RWA range above limits
# and avoid to be too conservative just above/below lower/upperlimits.
# e.g. we consider that somme intermediate values could be of interest on the latent metric scale (i.e. between 0.5 and 1))

round2 = function(x, n) {
  posneg = sign(x)
  z = abs(x)*10^n
  z = z + 0.5
  z = trunc(z)
  z = z/10^n
  z*posneg
}

roperwaN <- c( round2( -10/rrwa/2, n = 1), round2( 10/rrwa/2, n = 1))
roperwaC <- paste( roperwaN[1], ", ", roperwaN[2], sep = "")


#------------------------------------------------------------------------------------
# Then we run our first model for fixed main and interaction effects
#------------------------------------------------------------------------------------

# model
aware_resp <- brm(aware ~ usvalence * social  * RWAscore + (1|ppt) + (1|stim1),
                   data = social_df, 
                   family = gaussian(),
                   prior = priors,
                   warmup = 1000, iter = 2000,
                   chains = 4, cores = parallel::detectCores(),
                   control = list(adapt_delta = 0.8, max_treedepth = 10),
                   sample_prior = TRUE)

# In case we want to save summary ! change the name if you export both summary and tidy_stan
# model_awa_xp02 <- summary(social_resp)$fixed[,col2keep]
# model_awa_xp02 <- round(model_awa_xp02, 3)

# arrange output
model_awa_xp02 <- tidy_stan(aware_resp,
                            typical = "mean",
                            prob = .95)


# export output
png("tables/brms/model_awa_xp02.png", height=480, width=720)
p<-tableGrob(model_awa_xp02)
grid.arrange(p)
dev.off()

# test with rope

equi_awa_xp02_int <- equi_test(aware_resp, rope = c(47.5, 52.5))
equi_awa_xp02_int <- equi_awa_xp02_int[1,]
equi_awa_xp02_int$ROPE <- "47.5, 52.5"

equi_awa_xp02_rwa <- equi_test(aware_resp, rope = roperwaN)
equi_awa_xp02_rwa <- equi_awa_xp02_rwa[c(4,6:8),]
equi_awa_xp02_rwa$ROPE <- roperwaC

equi_awa_xp02_0.5 <- equi_test(aware_resp, rope = c(-5, 5))
equi_awa_xp02_0.5 <- equi_awa_xp02_0.5[c(2:3,5),]
equi_awa_xp02_0.5$ROPE <- "-5, 5"

equi_awa_xp02 <- rbind(equi_awa_xp02_rwa, equi_awa_xp02_0.5)
equi_awa_xp02 <- rbind(equi_awa_xp02_int, equi_awa_xp02)
equi_awa_xp02[,c(3:5)] <- round(equi_awa_xp02[,c(3:5)], 2)
ordrow <- c(1, 6, 7, 2, 8, 3, 4, 5)
equi_awa_xp02 <- equi_awa_xp02[ordrow,]

# export test
png("tables/rope/equi_awa_xp02.png", height=480, width=720)
p<-tableGrob(equi_awa_xp02)
grid.arrange(p)
dev.off()

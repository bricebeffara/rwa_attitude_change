#################################################################################################################
# Thu Nov 23 15:19:02 2017 ------------------------------
# Clean script models
# Bret et al. 2017
# Right-Wing Authoritarianism Predicts weakened Attitude Change in an Evaluative Counter-conditioning Paradigm
# OSF : https://osf.io/vz78f/
# Preprint : https://psyarxiv.com/3mfa8/
#################################################################################################################

# Define priors for all models

priors <- c(
  prior(normal(0, 10), class = Intercept, coef = ""),
  prior(normal(0, 0.5), class = b),
  prior(exponential(2), class = sd) )

#---------------------- Part 1 -> XP1 XP2 XP3 --------------------------------------------

##########################################
####### MAIN & INTERACTION EFFECTs #######
##########################################

## XP1 

ec01rwa <- brm(ratings ~ rwa * usvalence + (1|ppt) + (1|stim),
             data = XP1_Greebles, 
             family = cumulative (link = "logit", threshold = "flexible"),
             prior = priors,
             warmup = 1000, iter = 2000,
             chains = 4, cores = parallel::detectCores(),
             control = list(adapt_delta = 0.8, max_treedepth = 10),
             sample_prior = TRUE)

## XP2

ec02rwa <- brm(ratings ~ rwa * usvalence + (1|ppt) + (1|stim),
               data = XP2_Greebles, 
               family = cumulative (link = "logit", threshold = "flexible"),
               prior = priors,
               warmup = 1000, iter = 2000,
               chains = 4, cores = parallel::detectCores(),
               control = list(adapt_delta = 0.8, max_treedepth = 10),
               sample_prior = TRUE)

## XP3

ec03rwa <- brm(ratings ~ rwa * usvalence * bloc + (1|ppt) + (1|stim),
              data = XP3_Greebles, 
              family = cumulative (link = "logit", threshold = "flexible"),
              prior = priors,
              warmup = 1000, iter = 2000,
              chains = 4, cores = parallel::detectCores(),
              control = list(adapt_delta = 0.8, max_treedepth = 10),
              sample_prior = TRUE)

## IDA3

ec1t3rwa <- brm(ratings ~ rwa * usvalence + (1|ppt) + (1|stim) + (1|XP),
               data = IDA3_Greebles, 
               family = cumulative (link = "logit", threshold = "flexible"),
               prior = priors,
               warmup = 1000, iter = 2000,
               chains = 4, cores = parallel::detectCores(),
               control = list(adapt_delta = 0.8, max_treedepth = 10),
               sample_prior = TRUE)


##############################
####### SIMPLE SLOPES ########
##############################

## Coding

# XP1

XP1_Greebles$simpPOS <- ifelse (XP1_Greebles$usvalence == 0.5, 0, 1)
XP1_Greebles$simpNEG <- ifelse (XP1_Greebles$usvalence == -0.5, 0, 1)

# XP2

XP2_Greebles$simpPOS <- ifelse (XP2_Greebles$usvalence == 0.5, 0, 1)
XP2_Greebles$simpNEG <- ifelse (XP2_Greebles$usvalence == -0.5, 0, 1)

## XP3

XP3_Greebles$simpPOS <- ifelse (XP3_Greebles$usvalence == 0.5, 0, 1)
XP3_Greebles$simpNEG <- ifelse (XP3_Greebles$usvalence == -0.5, 0, 1)

# IDA3

IDA3_Greebles$simpPOS <- ifelse (IDA3_Greebles$usvalence == 0.5, 0, 1)
IDA3_Greebles$simpNEG <- ifelse (IDA3_Greebles$usvalence == -0.5, 0, 1)


## Models
# ! For communication reasons !
# ! From here !
# ! negative/positive will refer to bloc 1 instead of bloc 2 usvalence !
# ! This explains the reversal between coding (in the data) and names of the models !

# XP1

# Simple slope in the (valence Bloc 1) positive condition

ec01rwaP <- brm(ratings ~ rwa * simpNEG + (1|ppt) + (1|stim),
               data = XP1_Greebles, 
               family = cumulative (link = "logit", threshold = "flexible"),
               prior = priors,
               warmup = 1000, iter = 2000,
               chains = 4, cores = parallel::detectCores(),
               control = list(adapt_delta = 0.8, max_treedepth = 10),
               sample_prior = TRUE)

# Simple slope in the (valence Bloc 1) negative condition

ec01rwaN <- brm(ratings ~ rwa * simpPOS + (1|ppt) + (1|stim),
                data = XP1_Greebles, 
                family = cumulative (link = "logit", threshold = "flexible"),
                prior = priors,
                warmup = 1000, iter = 2000,
                chains = 4, cores = parallel::detectCores(),
                control = list(adapt_delta = 0.8, max_treedepth = 10),
                sample_prior = TRUE)

# XP2

# Simple slope in the (valence Bloc 1) positive condition

ec02rwaP <- brm(ratings ~ rwa * simpNEG + (1|ppt) + (1|stim),
                data = XP2_Greebles, 
                family = cumulative (link = "logit", threshold = "flexible"),
                prior = priors,
                warmup = 1000, iter = 2000,
                chains = 4, cores = parallel::detectCores(),
                control = list(adapt_delta = 0.8, max_treedepth = 10),
                sample_prior = TRUE)


# Simple slope in the (valence Bloc 1) negative condition

ec02rwaN <- brm(ratings ~ rwa * simpPOS + (1|ppt) + (1|stim),
                data = XP2_Greebles, 
                family = cumulative (link = "logit", threshold = "flexible"),
                prior = priors,
                warmup = 1000, iter = 2000,
                chains = 4, cores = parallel::detectCores(),
                control = list(adapt_delta = 0.8, max_treedepth = 10),
                sample_prior = TRUE)

# XP3

# Simple slope in the (valence Bloc 1) positive condition

ec03rwaP <- brm(ratings ~ rwa * simpNEG + (1|ppt) + (1|stim),
                data = XP3_Greebles, 
                family = cumulative (link = "logit", threshold = "flexible"),
                prior = priors,
                warmup = 1000, iter = 2000,
                chains = 4, cores = parallel::detectCores(),
                control = list(adapt_delta = 0.8, max_treedepth = 10),
                sample_prior = TRUE)

# Simple slope in the (valence Bloc 1) negative condition

ec03rwaN <- brm(ratings ~ rwa * simpPOS + (1|ppt) + (1|stim),
                data = XP3_Greebles, 
                family = cumulative (link = "logit", threshold = "flexible"),
                prior = priors,
                warmup = 1000, iter = 2000,
                chains = 4, cores = parallel::detectCores(),
                control = list(adapt_delta = 0.8, max_treedepth = 10),
                sample_prior = TRUE)

# IDA3

# Simple slope in the (valence Bloc 1) positive condition

ec1t3rwaP <- brm(ratings ~ rwa * simpNEG + (1|ppt) + (1|stim) + (1|XP),
               data = IDA3_Greebles, 
               family = cumulative (link = "logit", threshold = "flexible"),
               prior = priors,
               warmup = 1000, iter = 2000,
               chains = 4, cores = parallel::detectCores(),
               control = list(adapt_delta = 0.8, max_treedepth = 10),
               sample_prior = TRUE)

# Simple slope in the (valence Bloc 1) negative condition

ec1t3rwaN <- brm(ratings ~ rwa * simpPOS + (1|ppt) + (1|stim) + (1|XP),
                data = IDA3_Greebles, 
                family = cumulative (link = "logit", threshold = "flexible"),
                prior = priors,
                warmup = 1000, iter = 2000,
                chains = 4, cores = parallel::detectCores(),
                control = list(adapt_delta = 0.8, max_treedepth = 10),
                sample_prior = TRUE)


#---------------------- Part 2 -> XP4 XP5 XP6 XP7 (supplementary materials) --------------------------------------------


##########################################
####### MAIN & INTERACTION EFFECTs #######
##########################################

## XP4 

ec04rwa  <- brm(ratings ~ rwa * usvalence + (1|ppt) + (1|stim),
                 data = XP4_Greebles, 
                 family = cumulative (link = "logit", threshold = "flexible"),
                 prior = priors,
                 warmup = 1000, iter = 2000,
                 chains = 4, cores = parallel::detectCores(),
                 control = list(adapt_delta = 0.8, max_treedepth = 10),
                 sample_prior = TRUE)

## XP5

ec05rwa  <- brm(ratings ~ rwa * usvalence + (1|ppt) + (1|stim),
                 data = XP5_Greebles, 
                 family = cumulative (link = "logit", threshold = "flexible"),
                 prior = priors,
                 warmup = 1000, iter = 2000,
                 chains = 4, cores = parallel::detectCores(),
                 control = list(adapt_delta = 0.8, max_treedepth = 10),
                 sample_prior = TRUE)


## XP6

ec06rwa  <- brm(ratings ~ rwa * usvalence + (1|ppt) + (1|stim),
                 data = XP6_Greebles, 
                 family = cumulative (link = "logit", threshold = "flexible"),
                 prior = priors,
                 warmup = 1000, iter = 2000,
                 chains = 4, cores = parallel::detectCores(),
                 control = list(adapt_delta = 0.8, max_treedepth = 10),
                 sample_prior = TRUE)


## XP7

ec07rwa  <- brm(ratings ~ rwa * usvalence + (1|ppt) + (1|stim),
                    data = XP7_Greebles, 
                    family = cumulative (link = "logit", threshold = "flexible"),
                    prior = priors,
                    warmup = 1000, iter = 2000,
                    chains = 4, cores = parallel::detectCores(),
                    control = list(adapt_delta = 0.8, max_treedepth = 10),
                    sample_prior = TRUE)

## IDA7

ec1t7rwa  <- brm(ratings ~ rwa * usvalence + (1|ppt) + (1|stim) + (1|XP),
                 data = IDA7_Greebles, 
                 family = cumulative (link = "logit", threshold = "flexible"),
                 prior = priors,
                 warmup = 1000, iter = 2000,
                 chains = 4, cores = parallel::detectCores(),
                 control = list(adapt_delta = 0.8, max_treedepth = 10),
                 sample_prior = TRUE)

##############################
#######SIMPLE SLOPES##########
##############################

## Coding

XP4_Greebles$simpPOS <- ifelse (XP4_Greebles$usvalence == 0.5, 0, 1)
XP4_Greebles$simpNEG <- ifelse (XP4_Greebles$usvalence == -0.5, 0, 1)

XP5_Greebles$simpPOS <- ifelse (XP5_Greebles$usvalence == 0.5, 0, 1)
XP5_Greebles$simpNEG <- ifelse (XP5_Greebles$usvalence == -0.5, 0, 1)

XP6_Greebles$simpPOS <- ifelse (XP6_Greebles$usvalence == 0.5, 0, 1)
XP6_Greebles$simpNEG <- ifelse (XP6_Greebles$usvalence == -0.5, 0, 1)

XP7_Greebles$simpPOS <- ifelse (XP7_Greebles$usvalence == 0.5, 0, 1)
XP7_Greebles$simpNEG <- ifelse (XP7_Greebles$usvalence == -0.5, 0, 1)

IDA7_Greebles$simpPOS <- ifelse (IDA7_Greebles$usvalence == 0.5, 0, 1)
IDA7_Greebles$simpNEG <- ifelse (IDA7_Greebles$usvalence == -0.5, 0, 1)

## Models
# ! For communication reasons !
# ! From here !
# ! negative/positive will refer to bloc 1 instead of bloc 2 usvalence !
# ! This explains the reversal between coding (in the data) and names of the models !


# XP4

# Simple slope in the (valence Bloc 1) positive condition

ec04rwaP  <- brm(ratings ~ rwa * simpNEG + (1|ppt) + (1|stim),
                  data = XP4_Greebles, 
                  family = cumulative (link = "logit", threshold = "flexible"),
                  prior = priors,
                  warmup = 1000, iter = 2000,
                  chains = 4, cores = parallel::detectCores(),
                  control = list(adapt_delta = 0.8, max_treedepth = 10),
                  sample_prior = TRUE)

# Simple slope in the (valence Bloc 1) negative condition

ec04rwaN  <- brm(ratings ~ rwa * simpPOS + (1|ppt) + (1|stim),
                 data = XP4_Greebles, 
                 family = cumulative (link = "logit", threshold = "flexible"),
                 prior = priors,
                 warmup = 1000, iter = 2000,
                 chains = 4, cores = parallel::detectCores(),
                 control = list(adapt_delta = 0.8, max_treedepth = 10),
                 sample_prior = TRUE)


# XP5

# Simple slope in the (valence Bloc 1) positive condition

ec05rwaP  <- brm(ratings ~ rwa * simpNEG + (1|ppt) + (1|stim),
                  data = XP5_Greebles, 
                  family = cumulative (link = "logit", threshold = "flexible"),
                  prior = priors,
                  warmup = 1000, iter = 2000,
                  chains = 4, cores = parallel::detectCores(),
                  control = list(adapt_delta = 0.8, max_treedepth = 10),
                  sample_prior = TRUE)

# Simple slope in the (valence Bloc 1) negative condition

ec05rwa  <- brm(ratings ~ rwa * simpPOS + (1|ppt) + (1|stim),
                data = XP5_Greebles, 
                family = cumulative (link = "logit", threshold = "flexible"),
                prior = priors,
                warmup = 1000, iter = 2000,
                chains = 4, cores = parallel::detectCores(),
                control = list(adapt_delta = 0.8, max_treedepth = 10),
                sample_prior = TRUE)

# XP6

# Simple slope in the (valence Bloc 1) positive condition

ec06rwaP  <- brm(ratings ~ rwa * simpNEG + (1|ppt) + (1|stim),
                  data = XP6_Greebles, 
                  family = cumulative (link = "logit", threshold = "flexible"),
                  prior = priors,
                  warmup = 1000, iter = 2000,
                  chains = 4, cores = parallel::detectCores(),
                  control = list(adapt_delta = 0.8, max_treedepth = 10),
                  sample_prior = TRUE)

# Simple slope in the (valence Bloc 1) negative condition

ec06rwaN  <- brm(ratings ~ rwa * simpPOS + (1|ppt) + (1|stim),
                 data = XP6_Greebles, 
                 family = cumulative (link = "logit", threshold = "flexible"),
                 prior = priors,
                 warmup = 1000, iter = 2000,
                 chains = 4, cores = parallel::detectCores(),
                 control = list(adapt_delta = 0.8, max_treedepth = 10),
                 sample_prior = TRUE)


# XP7

# Simple slope in the (valence Bloc 1) positive condition

ec07rwaP  <- brm(ratings ~ rwa * simpNEG + (1|ppt) + (1|stim),
                     data = XP7_Greebles, 
                     family = cumulative (link = "logit", threshold = "flexible"),
                     prior = priors,
                     warmup = 1000, iter = 2000,
                     chains = 4, cores = parallel::detectCores(),
                     control = list(adapt_delta = 0.8, max_treedepth = 10),
                     sample_prior = TRUE)

# Simple slope in the (valence Bloc 1) negative condition

ec07rwaN  <- brm(ratings ~ rwa * simpPOS + (1|ppt) + (1|stim),
                 data = XP7_Greebles, 
                 family = cumulative (link = "logit", threshold = "flexible"),
                 prior = priors,
                 warmup = 1000, iter = 2000,
                 chains = 4, cores = parallel::detectCores(),
                 control = list(adapt_delta = 0.8, max_treedepth = 10),
                 sample_prior = TRUE)

# IDA7

# Simple slope in the (valence Bloc 1) positive condition

ec1t7rwaP  <- brm(ratings ~ rwa * simpNEG + (1|ppt) + (1|stim) + (1|XP),
                  data = IDA7_Greebles, 
                  family = cumulative (link = "logit", threshold = "flexible"),
                  prior = priors,
                  warmup = 1000, iter = 2000,
                  chains = 4, cores = parallel::detectCores(),
                  control = list(adapt_delta = 0.8, max_treedepth = 10),
                  sample_prior = TRUE)

# Simple slope in the (valence Bloc 1) negative condition

ec1t7rwaN  <- brm(ratings ~ rwa * simpPOS + (1|ppt) + (1|stim) + (1|XP),
                  data = IDA7_Greebles, 
                  family = cumulative (link = "logit", threshold = "flexible"),
                  prior = priors,
                  warmup = 1000, iter = 2000,
                  chains = 4, cores = parallel::detectCores(),
                  control = list(adapt_delta = 0.8, max_treedepth = 10),
                  sample_prior = TRUE)



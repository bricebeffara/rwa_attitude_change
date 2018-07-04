#------------------------------------------------------------------------------------
# Then we run our second step models to decompose interactions effects
# We look at the interaction between RWA and usvalence at each level of conditioning
#------------------------------------------------------------------------------------

#-------------
### RWA * valence in the !!level 1!! conditioning condition
#-------------

## first step = recode variable spreading
## We interpret parameters for a 0 level of others

# Coding !!level 1!! conditioning as 0
spreading$level_one <- ifelse (spreading$spreading == -0.5, 0, 1)

spread_resp_lvone <- brm(response ~ usvalence * level_one  * RWAscore + (1|ppt) + (1|stim1), #here we change the spreading variable with the recoded one
                         data = spreading, 
                         family = cumulative (link = "logit", threshold = "flexible"),
                         prior = priors,
                         warmup = 1000, iter = 2000,
                         chains = 4, cores = parallel::detectCores(),
                         control = list(adapt_delta = 0.8, max_treedepth = 10),
                         sample_prior = TRUE)

# In case we want to save summary ! change the name if you export both summary and tidy_stan
# model_lvone_xp10 <- summary(spread_resp_lvone)$fixed[,col2keep]
# model_lvone_xp10 <- round(model_lvone_xp10, 3)

# arrange output
model_lvone_xp10 <- tidy_stan(spread_resp_lvone,
                              typical = "mean",
                              prob = .95)

# export output
png("tables/brms/model_lvone_xp10.png", height=480, width=720)
p<-tableGrob(model_lvone_xp10)
grid.arrange(p)
dev.off()

# test with rope
equi_lvone_xp10_rwa <- equi_test(spread_resp_lvone, rope = roperwaN)
equi_lvone_xp10_rwa <- equi_lvone_xp10_rwa[c(11,13:15),]
equi_lvone_xp10_rwa$ROPE <- roperwaC

equi_lvone_xp10_0.5 <- equi_test(spread_resp_lvone, rope = c(-0.5, 0.5))
equi_lvone_xp10_0.5 <- equi_lvone_xp10_0.5[c(9,10,12),]
equi_lvone_xp10_0.5$ROPE <- "-0.5, 0.5"

equi_lvone_xp10 <- rbind(equi_lvone_xp10_rwa, equi_lvone_xp10_0.5)
equi_lvone_xp10[,c(3:5)] <- round(equi_lvone_xp10[,c(3:5)], 2)
ordrow <- c("5","6","1","7","2","3","4")
equi_lvone_xp10 <- equi_lvone_xp10[ordrow,]

# export test
png("tables/rope/equi_lvone_xp10.png", height=480, width=720)
p<-tableGrob(equi_lvone_xp10)
grid.arrange(p)
dev.off()


#-------------
### RWA * usvalence in the !!level 2!! conditioning condition
#-------------

## first step = recode variable spreading
## We interpret parameters for a 0 level of others

# Coding !!level 2!! conditioning as 0
spreading$level_two <- ifelse (spreading$spreading == 0.5, 0, 1)

spread_resp_lvtwo <- brm(response ~ usvalence * level_two  * RWAscore + (1|ppt) + (1|stim1), #here we change the spreading variable with the recoded one
                         data = spreading, 
                         family = cumulative (link = "logit", threshold = "flexible"),
                         prior = priors,
                         warmup = 1000, iter = 2000,
                         chains = 4, cores = parallel::detectCores(),
                         control = list(adapt_delta = 0.8, max_treedepth = 10),
                         sample_prior = TRUE)

# In case we want to save summary ! change the name if you export both summary and tidy_stan
# model_lvtwo_xp10 <- summary(spread_resp_lvtwo)$fixed[,col2keep]
# model_lvtwo_xp10 <- round(model_lvtwo_xp10, 3)

# arrange output
model_lvtwo_xp10 <- tidy_stan(spread_resp_lvtwo,
                              typical = "mean",
                              prob = .95)

# export output
png("tables/brms/model_lvtwo_xp10.png", height=480, width=720)
p<-tableGrob(model_lvtwo_xp10)
grid.arrange(p)
dev.off()

# test with rope
equi_lvtwo_xp10_rwa <- equi_test(spread_resp_lvtwo, rope = roperwaN)
equi_lvtwo_xp10_rwa <- equi_lvtwo_xp10_rwa[c(11,13:15),]
equi_lvtwo_xp10_rwa$ROPE <- roperwaC

equi_lvtwo_xp10_0.5 <- equi_test(spread_resp_lvtwo, rope = c(-0.5, 0.5))
equi_lvtwo_xp10_0.5 <- equi_lvtwo_xp10_0.5[c(9,10,12),]
equi_lvtwo_xp10_0.5$ROPE <- "-0.5, 0.5"

equi_lvtwo_xp10 <- rbind(equi_lvtwo_xp10_rwa, equi_lvtwo_xp10_0.5)
equi_lvtwo_xp10[,c(3:5)] <- round(equi_lvtwo_xp10[,c(3:5)], 2)
ordrow <- c("5","6","1","7","2","3","4")
equi_lvtwo_xp10 <- equi_lvtwo_xp10[ordrow,]

# export test
png("tables/rope/equi_lvtwo_xp10.png", height=480, width=720)
p<-tableGrob(equi_lvtwo_xp10)
grid.arrange(p)
dev.off()

# See 02c_decint1_brms_models_xp10.R for other models 
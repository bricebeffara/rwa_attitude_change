#------------------------------------------------------------------------------------
# Then we run our second step models to decompose interactions effects
# We look at the interaction between RWA and usvalence at each level of conditioning
#------------------------------------------------------------------------------------

#-------------
### RWA * valence in the !!no warning!! condition
#-------------

## first step = recode variable warn
## We interpret parameters for a 0 level of others

# Coding !!level 1!! conditioning as 0
warn_df$no_warn <- ifelse (warn_df$warn == -0.5, 0, 1)

warn_resp_nowa <- brm(response ~ usvalence * no_warn  * RWAscore + (1|ppt) + (1|stim1), #here we change the warn variable with the recoded one
                         data = warn_df, 
                         family = cumulative (link = "logit", threshold = "flexible"),
                         prior = priors,
                         warmup = 1000, iter = 2000,
                         chains = 4, cores = parallel::detectCores(),
                         control = list(adapt_delta = 0.8, max_treedepth = 10),
                         sample_prior = TRUE)

# In case we want to save summary ! change the name if you export both summary and tidy_stan
# model_nowa_xp11 <- summary(warn_resp_nowa)$fixed[,col2keep]
# model_nowa_xp11 <- round(model_nowa_xp11, 3)

# arrange output
model_nowa_xp11 <- tidy_stan(warn_resp_nowa,
                              typical = "mean",
                              prob = .95)

# export output
png("tables/brms/model_nowa_xp11.png", height=480, width=720)
p<-tableGrob(model_nowa_xp11)
grid.arrange(p)
dev.off()

# test with rope
equi_nowa_xp11_rwa <- equi_test(warn_resp_nowa, rope = roperwaN)
equi_nowa_xp11_rwa <- equi_nowa_xp11_rwa[c(11,13:15),]
equi_nowa_xp11_rwa$ROPE <- roperwaC

equi_nowa_xp11_0.5 <- equi_test(warn_resp_nowa, rope = c(-0.5, 0.5))
equi_nowa_xp11_0.5 <- equi_nowa_xp11_0.5[c(9,10,12),]
equi_nowa_xp11_0.5$ROPE <- "-0.5, 0.5"

equi_nowa_xp11 <- rbind(equi_nowa_xp11_rwa, equi_nowa_xp11_0.5)
equi_nowa_xp11[,c(3:5)] <- round(equi_nowa_xp11[,c(3:5)], 2)
ordrow <- c(5, 6, 1, 7, 2, 3, 4)
equi_nowa_xp11 <- equi_nowa_xp11[ordrow,]

# export test
png("tables/rope/equi_nowa_xp11.png", height=480, width=720)
p<-tableGrob(equi_nowa_xp11)
grid.arrange(p)
dev.off()


#-------------
### RWA * usvalence in the !!warning!! condition
#-------------

## first step = recode variable warning
## We interpret parameters for a 0 level of others

# Coding !!level 2!! conditioning as 0
warn_df$ye_warn <- ifelse (warn_df$warn == 0.5, 0, 1)

warn_resp_yewa <- brm(response ~ usvalence * ye_warn  * RWAscore + (1|ppt) + (1|stim1), #here we change the warn variable with the recoded one
                         data = warn_df, 
                         family = cumulative (link = "logit", threshold = "flexible"),
                         prior = priors,
                         warmup = 1000, iter = 2000,
                         chains = 4, cores = parallel::detectCores(),
                         control = list(adapt_delta = 0.8, max_treedepth = 10),
                         sample_prior = TRUE)

# In case we want to save summary ! change the name if you export both summary and tidy_stan
# model_yewa_xp11 <- summary(warn_resp_yewa)$fixed[,col2keep]
# model_yewa_xp11 <- round(model_yewa_xp11, 3)

# arrange output
model_yewa_xp11 <- tidy_stan(warn_resp_yewa,
                              typical = "mean",
                              prob = .95)

# export output
png("tables/brms/model_yewa_xp11.png", height=480, width=720)
p<-tableGrob(model_yewa_xp11)
grid.arrange(p)
dev.off()

# test with rope
equi_yewa_xp11_rwa <- equi_test(warn_resp_yewa, rope = roperwaN)
equi_yewa_xp11_rwa <- equi_yewa_xp11_rwa[c(11,13:15),]
equi_yewa_xp11_rwa$ROPE <- roperwaC

equi_yewa_xp11_0.5 <- equi_test(warn_resp_yewa, rope = c(-0.5, 0.5))
equi_yewa_xp11_0.5 <- equi_yewa_xp11_0.5[c(9,10,12),]
equi_yewa_xp11_0.5$ROPE <- "-0.5, 0.5"

equi_yewa_xp11 <- rbind(equi_yewa_xp11_rwa, equi_yewa_xp11_0.5)
equi_yewa_xp11[,c(3:5)] <- round(equi_yewa_xp11[,c(3:5)], 2)
ordrow <- c(5, 6, 1, 7, 2, 3, 4)
equi_yewa_xp11 <- equi_yewa_xp11[ordrow,]

# export test
png("tables/rope/equi_yewa_xp11.png", height=480, width=720)
p<-tableGrob(equi_yewa_xp11)
grid.arrange(p)
dev.off()

# See 02c_decint1_brms_models_xp11.R for other models 
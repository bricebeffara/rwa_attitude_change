#------------------------------------------------------------------------------------
# Then we run our second step models to decompose interactions effects
# We look at the interaction between RWA and usvalence2 at each level of bloc
#------------------------------------------------------------------------------------

#-------------
### RWA * valence in the !!1 bloc!! condition
#-------------

## first step = recode variable bloc
## We interpret parameters for a 0 level of others

# Coding !!1 bloc!! conditioning as 0
bloc_df$bloc1 <- ifelse (bloc_df$bloc == -0.5, 0, 1)

bloc_resp_b1<- brm(response ~ usvalence2 * bloc1  * RWAscore + (1|ppt) + (1|stim1), #here we change the bloc variable with the recoded one
                         data = bloc_df, 
                         family = cumulative (link = "logit", threshold = "flexible"),
                         prior = priors,
                         warmup = 1000, iter = 2000,
                         chains = 4, cores = parallel::detectCores(),
                         control = list(adapt_delta = 0.8, max_treedepth = 10),
                         sample_prior = TRUE)

# In case we want to save summary ! change the name if you export both summary and tidy_stan
# model_b1_xp03 <- summary(bloc_resp_b1)$fixed[,col2keep]
# model_b1_xp03 <- round(model_b1_xp03, 3)

# arrange output
model_b1_xp03 <- tidy_stan(bloc_resp_b1,
                              typical = "mean",
                              prob = .95)

# export output
png("tables/brms/model_b1_xp03.png", height=480, width=720)
p<-tableGrob(model_b1_xp03)
grid.arrange(p)
dev.off()

# test with rope
equi_b1_xp03_rwa <- equi_test(bloc_resp_b1, rope = roperwaN)
equi_b1_xp03_rwa <- equi_b1_xp03_rwa[c(11,13:15),]
equi_b1_xp03_rwa$ROPE <- roperwaC

equi_b1_xp03_0.5 <- equi_test(bloc_resp_b1, rope = c(-0.5, 0.5))
equi_b1_xp03_0.5 <- equi_b1_xp03_0.5[c(9,10,12),]
equi_b1_xp03_0.5$ROPE <- "-0.5, 0.5"

equi_b1_xp03 <- rbind(equi_b1_xp03_rwa, equi_b1_xp03_0.5)
equi_b1_xp03[,c(3:5)] <- round(equi_b1_xp03[,c(3:5)], 2)
ordrow <- c(5, 6, 1, 7, 2, 3, 4)
equi_b1_xp03 <- equi_b1_xp03[ordrow,]

# export test
png("tables/rope/equi_b1_xp03.png", height=480, width=720)
p<-tableGrob(equi_b1_xp03)
grid.arrange(p)
dev.off()


#-------------
### RWA * usvalence2 in the !!2 blocs!! condition
#-------------

## first step = recode variable blocing
## We interpret parameters for a 0 level of others

# Coding !!2 blocs!! conditioning as 0
bloc_df$bloc2 <- ifelse (bloc_df$bloc == 0.5, 0, 1)

bloc_resp_b2 <- brm(response ~ usvalence2 * bloc2  * RWAscore + (1|ppt) + (1|stim1), #here we change the bloc variable with the recoded one
                         data = bloc_df, 
                         family = cumulative (link = "logit", threshold = "flexible"),
                         prior = priors,
                         warmup = 1000, iter = 2000,
                         chains = 4, cores = parallel::detectCores(),
                         control = list(adapt_delta = 0.8, max_treedepth = 10),
                         sample_prior = TRUE)

# In case we want to save summary ! change the name if you export both summary and tidy_stan
# model_b2_xp03 <- summary(bloc_resp_b2)$fixed[,col2keep]
# model_b2_xp03 <- round(model_b2_xp03, 3)

# arrange output
model_b2_xp03 <- tidy_stan(bloc_resp_b2,
                              typical = "mean",
                              prob = .95)

# export output
png("tables/brms/model_b2_xp03.png", height=480, width=720)
p<-tableGrob(model_b2_xp03)
grid.arrange(p)
dev.off()

# test with rope
equi_b2_xp03_rwa <- equi_test(bloc_resp_b2, rope = roperwaN)
equi_b2_xp03_rwa <- equi_b2_xp03_rwa[c(11,13:15),]
equi_b2_xp03_rwa$ROPE <- roperwaC

equi_b2_xp03_0.5 <- equi_test(bloc_resp_b2, rope = c(-0.5, 0.5))
equi_b2_xp03_0.5 <- equi_b2_xp03_0.5[c(9,10,12),]
equi_b2_xp03_0.5$ROPE <- "-0.5, 0.5"

equi_b2_xp03 <- rbind(equi_b2_xp03_rwa, equi_b2_xp03_0.5)
equi_b2_xp03[,c(3:5)] <- round(equi_b2_xp03[,c(3:5)], 2)
ordrow <- c(5, 6, 1, 7, 2, 3, 4)
equi_b2_xp03 <- equi_b2_xp03[ordrow,]

# export test
png("tables/rope/equi_b2_xp03.png", height=480, width=720)
p<-tableGrob(equi_b2_xp03)
grid.arrange(p)
dev.off()

# See 02c_decint1_brms_models_xp03.R for other models 

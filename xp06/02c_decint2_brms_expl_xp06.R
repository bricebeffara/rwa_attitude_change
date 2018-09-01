#------------------------------------------------------------------------------------
# Then we run our second step models to decompose interactions effects
# We look at the interaction between RWA and usvalencedir at each level of order
#------------------------------------------------------------------------------------

#-------------
### RWA * valence in the !!indirect ratings before direct ratings!! condition
#-------------

## first step = recode variable direct
## We interpret parameters for a 0 level of others

# Coding !!indirect ratings before direct ratings!! as 0
direct_df$order1 <- ifelse (direct_df$order == -0.5, 0, 1)

direct_resp_ord1 <- brm(response ~ usvalencedir * order1  * RWAscore + (1|ppt) + (1|stim1), #here we change the order variable with the recoded one
                         data = direct_df, 
                         family = cumulative (link = "logit", threshold = "flexible"),
                         prior = priors,
                         warmup = 1000, iter = 2000,
                         chains = 4, cores = parallel::detectCores(),
                         control = list(adapt_delta = 0.8, max_treedepth = 10),
                         sample_prior = TRUE)

# In case we want to save summary ! change the name if you export both summary and tidy_stan
# model_order1_xp06 <- summary(direct_resp_order1)$fixed[,col2keep]
# model_order1_xp06 <- round(model_order1_xp06, 3)

# arrange output
model_ord1_xp06 <- tidy_stan(direct_resp_ord1,
                              typical = "mean",
                              prob = .95)

# export output
png("tables/brms/model_ord1_xp06.png", height=480, width=720)
p<-tableGrob(model_ord1_xp06)
grid.arrange(p)
dev.off()

# test with rope
equi_ord1_xp06_rwa <- equi_test(direct_resp_ord1, rope = roperwaN)
equi_ord1_xp06_rwa <- equi_ord1_xp06_rwa[c(11,13:15),]
equi_ord1_xp06_rwa$ROPE <- roperwaC

equi_ord1_xp06_0.5 <- equi_test(direct_resp_ord1, rope = c(-0.5, 0.5))
equi_ord1_xp06_0.5 <- equi_ord1_xp06_0.5[c(9,10,12),]
equi_ord1_xp06_0.5$ROPE <- "-0.5, 0.5"

equi_ord1_xp06 <- rbind(equi_ord1_xp06_rwa, equi_ord1_xp06_0.5)
equi_ord1_xp06[,c(3:5)] <- round(equi_ord1_xp06[,c(3:5)], 2)
ordrow <- c(5,6,1,7,2,3,4)
equi_ord1_xp06 <- equi_ord1_xp06[ordrow,]

# export test
png("tables/rope/equi_ord1_xp06.png", height=480, width=720)
p<-tableGrob(equi_ord1_xp06)
grid.arrange(p)
dev.off()


#-------------
### RWA * usvalencedir in the !!direct ratings before indirect ratings!! condition
#-------------

## first step = recode variable order
## We interpret parameters for a 0 level of others

# Coding !!direct ratings before indirect ratings!! as 0
direct_df$order2 <- ifelse (direct_df$order == 0.5, 0, 1)

direct_resp_ord2 <- brm(response ~ usvalencedir * order2  * RWAscore + (1|ppt) + (1|stim1), #here we change the direct variable with the recoded one
                         data = direct_df, 
                         family = cumulative (link = "logit", threshold = "flexible"),
                         prior = priors,
                         warmup = 1000, iter = 2000,
                         chains = 4, cores = parallel::detectCores(),
                         control = list(adapt_delta = 0.8, max_treedepth = 10),
                         sample_prior = TRUE)

# In case we want to save summary ! change the name if you export both summary and tidy_stan
# model_ord2_xp06 <- summary(direct_resp_ord2)$fixed[,col2keep]
# model_ord2_xp06 <- round(model_ord2_xp06, 3)

# arrange output
model_ord2_xp06 <- tidy_stan(direct_resp_ord2,
                              typical = "mean",
                              prob = .95)

# export output
png("tables/brms/model_ord2_xp06.png", height=480, width=720)
p<-tableGrob(model_ord2_xp06)
grid.arrange(p)
dev.off()

# test with rope
equi_ord2_xp06_rwa <- equi_test(direct_resp_ord2, rope = roperwaN)
equi_ord2_xp06_rwa <- equi_ord2_xp06_rwa[c(11,13:15),]
equi_ord2_xp06_rwa$ROPE <- roperwaC

equi_ord2_xp06_0.5 <- equi_test(direct_resp_ord2, rope = c(-0.5, 0.5))
equi_ord2_xp06_0.5 <- equi_ord2_xp06_0.5[c(9,10,12),]
equi_ord2_xp06_0.5$ROPE <- "-0.5, 0.5"

equi_ord2_xp06 <- rbind(equi_ord2_xp06_rwa, equi_ord2_xp06_0.5)
equi_ord2_xp06[,c(3:5)] <- round(equi_ord2_xp06[,c(3:5)], 2)
ordrow <- c(5,6,1,7,2,3,4)
equi_ord2_xp06 <- equi_ord2_xp06[ordrow,]

# export test
png("tables/rope/equi_ord2_xp06.png", height=480, width=720)
p<-tableGrob(equi_ord2_xp06)
grid.arrange(p)
dev.off()

# See 02c_decint1_brms_models_xp06.R for other models 
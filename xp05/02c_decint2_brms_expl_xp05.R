#------------------------------------------------------------------------------------
# Then we run our second step models to decompose interactions effects
# We look at the interaction between RWA and usvalence at each level of order
#------------------------------------------------------------------------------------

#-------------
### RWA * valence in the !!implicit ratings before explicit ratings!! condition
#-------------

## first step = recode variable expli
## We interpret parameters for a 0 level of others

# Coding !!implicit ratings before explicit ratings!! as 0
expli_df$order1 <- ifelse (expli_df$order == -0.5, 0, 1)

expli_resp_ord1 <- brm(response ~ usvalence * order1  * RWAscore + (1|ppt) + (1|stim1), #here we change the order variable with the recoded one
                         data = expli_df, 
                         family = cumulative (link = "logit", threshold = "flexible"),
                         prior = priors,
                         warmup = 1000, iter = 2000,
                         chains = 4, cores = parallel::detectCores(),
                         control = list(adapt_delta = 0.8, max_treedepth = 10),
                         sample_prior = TRUE)

# In case we want to save summary ! change the name if you export both summary and tidy_stan
# model_order1_xp05 <- summary(expli_resp_order1)$fixed[,col2keep]
# model_order1_xp05 <- round(model_order1_xp05, 3)

# arrange output
model_ord1_xp05 <- tidy_stan(expli_resp_ord1,
                              typical = "mean",
                              prob = .95)

# export output
png("tables/brms/model_ord1_xp05.png", height=480, width=720)
p<-tableGrob(model_ord1_xp05)
grid.arrange(p)
dev.off()

# test with rope
equi_ord1_xp05_rwa <- equi_test(expli_resp_ord1, rope = roperwaN)
equi_ord1_xp05_rwa <- equi_ord1_xp05_rwa[c(11,13:15),]
equi_ord1_xp05_rwa$ROPE <- roperwaC

equi_ord1_xp05_0.5 <- equi_test(expli_resp_ord1, rope = c(-0.5, 0.5))
equi_ord1_xp05_0.5 <- equi_ord1_xp05_0.5[c(9,10,12),]
equi_ord1_xp05_0.5$ROPE <- "-0.5, 0.5"

equi_ord1_xp05 <- rbind(equi_ord1_xp05_rwa, equi_ord1_xp05_0.5)
equi_ord1_xp05[,c(3:5)] <- round(equi_ord1_xp05[,c(3:5)], 2)
ordrow <- c(5, 6, 1, 7, 2, 3, 4)
equi_ord1_xp05 <- equi_ord1_xp05[ordrow,]

# export test
png("tables/rope/equi_ord1_xp05.png", height=480, width=720)
p<-tableGrob(equi_ord1_xp05)
grid.arrange(p)
dev.off()


#-------------
### RWA * usvalence in the !!explicit ratings before implicit ratings!! condition
#-------------

## first step = recode variable order
## We interpret parameters for a 0 level of others

# Coding !!explicit ratings before implicit ratings!! as 0
expli_df$order2 <- ifelse (expli_df$order == 0.5, 0, 1)

expli_resp_ord2 <- brm(response ~ usvalence * order2  * RWAscore + (1|ppt) + (1|stim1), #here we change the expli variable with the recoded one
                         data = expli_df, 
                         family = cumulative (link = "logit", threshold = "flexible"),
                         prior = priors,
                         warmup = 1000, iter = 2000,
                         chains = 4, cores = parallel::detectCores(),
                         control = list(adapt_delta = 0.8, max_treedepth = 10),
                         sample_prior = TRUE)

# In case we want to save summary ! change the name if you export both summary and tidy_stan
# model_ord2_xp05 <- summary(expli_resp_ord2)$fixed[,col2keep]
# model_ord2_xp05 <- round(model_ord2_xp05, 3)

# arrange output
model_ord2_xp05 <- tidy_stan(expli_resp_ord2,
                              typical = "mean",
                              prob = .95)

# export output
png("tables/brms/model_ord2_xp05.png", height=480, width=720)
p<-tableGrob(model_ord2_xp05)
grid.arrange(p)
dev.off()

# test with rope
equi_ord2_xp05_rwa <- equi_test(expli_resp_ord2, rope = roperwaN)
equi_ord2_xp05_rwa <- equi_ord2_xp05_rwa[c(11,13:15),]
equi_ord2_xp05_rwa$ROPE <- roperwaC

equi_ord2_xp05_0.5 <- equi_test(expli_resp_ord2, rope = c(-0.5, 0.5))
equi_ord2_xp05_0.5 <- equi_ord2_xp05_0.5[c(9,10,12),]
equi_ord2_xp05_0.5$ROPE <- "-0.5, 0.5"

equi_ord2_xp05 <- rbind(equi_ord2_xp05_rwa, equi_ord2_xp05_0.5)
equi_ord2_xp05[,c(3:5)] <- round(equi_ord2_xp05[,c(3:5)], 2)
ordrow <- c(5, 6, 1, 7, 2, 3, 4)
equi_ord2_xp05 <- equi_ord2_xp05[ordrow,]

# export test
png("tables/rope/equi_ord2_xp05.png", height=480, width=720)
p<-tableGrob(equi_ord2_xp05)
grid.arrange(p)
dev.off()

# See 02c_decint1_brms_models_xp05.R for other models 
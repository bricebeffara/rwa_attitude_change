#-------------
### Valence in the !!1 bloc!! condition
#-------------

## first step = recode variable bloc
## We interpret parameters for a 0 level of others

# Coding !!1 bloc!! conditioning as 0
# bloc_df$bloc1 <- ifelse (bloc_df$bloc == -0.5, 0, 1) #already coded in 02b_decint2_brms_models_xp03

bloc1_resp_b1<- brm(response ~ usvalence1 * bloc1 + (1|ppt) + (1|stim1), #here we change the bloc variable with the recoded one
                         data = bloc_df, 
                         family = cumulative (link = "logit", threshold = "flexible"),
                         prior = priors,
                         warmup = 1000, iter = 2000,
                         chains = 4, cores = parallel::detectCores(),
                         control = list(adapt_delta = 0.8, max_treedepth = 10),
                         sample_prior = TRUE)

# In case we want to save summary ! change the name if you export both summary and tidy_stan
# model_bloc1_b1_xp03 <- summary(bloc1_resp_b1)$fixed[,col2keep]
# model_bloc1_b1_xp03 <- round(model_bloc1_b1_xp03, 3)

# arrange output
model_bloc1_b1_xp03 <- tidy_stan(bloc1_resp_b1,
                              typical = "mean",
                              prob = .95)

# export output
png("tables/brms/model_bloc1_b1_xp03.png", height=480, width=720)
p<-tableGrob(model_bloc1_b1_xp03)
grid.arrange(p)
dev.off()

# test with rope

equi_bloc1_b1_xp03_0.5 <- equi_test(bloc1_resp_b1, rope = c(-0.5, 0.5))
equi_bloc1_b1_xp03_0.5 <- equi_bloc1_b1_xp03_0.5[c(9,10,11),]
equi_bloc1_b1_xp03_0.5$ROPE <- "-0.5, 0.5"

equi_bloc1_b1_xp03 <- equi_bloc1_b1_xp03_0.5
equi_bloc1_b1_xp03[,c(3:5)] <- round(equi_bloc1_b1_xp03[,c(3:5)], 2)

# export test
png("tables/rope/equi_bloc1_b1_xp03.png", height=480, width=720)
p<-tableGrob(equi_bloc1_b1_xp03)
grid.arrange(p)
dev.off()


#-------------
### Valence in the !!2 blocs!! condition
#-------------

## first step = recode variable blocing
## We interpret parameters for a 0 level of others

# Coding !!2 blocs!! conditioning as 0
# bloc_df$bloc2 <- ifelse (bloc_df$bloc == 0.5, 0, 1) #already coded in 02b_decint2_brms_models_xp03


bloc1_resp_b2 <- brm(response ~ usvalence1 * bloc2 + (1|ppt) + (1|stim1), #here we change the bloc variable with the recoded one
                         data = bloc_df, 
                         family = cumulative (link = "logit", threshold = "flexible"),
                         prior = priors,
                         warmup = 1000, iter = 2000,
                         chains = 4, cores = parallel::detectCores(),
                         control = list(adapt_delta = 0.8, max_treedepth = 10),
                         sample_prior = TRUE)

# In case we want to save summary ! change the name if you export both summary and tidy_stan
# model_bloc1_b2_xp03 <- summary(bloc1_resp_b2)$fixed[,col2keep]
# model_bloc1_b2_xp03 <- round(model_bloc1_b2_xp03, 3)

# arrange output
model_bloc1_b2_xp03 <- tidy_stan(bloc1_resp_b2,
                              typical = "mean",
                              prob = .95)

# export output
png("tables/brms/model_bloc1_b2_xp03.png", height=480, width=720)
p<-tableGrob(model_bloc1_b2_xp03)
grid.arrange(p)
dev.off()

# test with rope

equi_bloc1_b2_xp03_0.5 <- equi_test(bloc1_resp_b2, rope = c(-0.5, 0.5))
equi_bloc1_b2_xp03_0.5 <- equi_bloc1_b2_xp03_0.5[c(9,10,11),]
equi_bloc1_b2_xp03_0.5$ROPE <- "-0.5, 0.5"

equi_bloc1_b2_xp03 <- equi_bloc1_b2_xp03_0.5
equi_bloc1_b2_xp03[,c(3:5)] <- round(equi_bloc1_b2_xp03[,c(3:5)], 2)

# export test
png("tables/rope/equi_bloc1_b2_xp03.png", height=480, width=720)
p<-tableGrob(equi_bloc1_b2_xp03)
grid.arrange(p)
dev.off()


# See 02f_decint1_brms_1bloc_xp03 for other models 
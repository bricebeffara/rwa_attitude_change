#------------------------------------------------------------------------------------
# Then we run our second step model to decompose the interaction 
# Effect of RWA in each usvalence condition
#------------------------------------------------------------------------------------

#-------------
### Simple slope of RWA in the !!negative!! valence condition
#-------------

## first step = recode variable usvalence
## We interpret parameters for a 0 level of others

# Coding !!negative!! valence as 0
social_df$usvalence_neg <- ifelse (social_df$usvalence == -0.5, 0, 1)

social_resp_neg <- brm(response ~ usvalence_neg  * RWAscore + (1|ppt) + (1|stim1), #here we change the usvalence variable with the recoded one
                             data = social_df, 
                             family = cumulative (link = "logit", threshold = "flexible"),
                             prior = priors,
                             warmup = 1000, iter = 2000,
                             chains = 4, cores = parallel::detectCores(),
                             control = list(adapt_delta = 0.8, max_treedepth = 10),
                             sample_prior = TRUE)

# In case we want to save summary ! change the name if you export both summary and tidy_stan
# model_neg_xp02 <- summary(social_resp_neg)$fixed[,col2keep]
# model_neg_xp02 <- round(model_neg_xp02, 3)

# arrange output
model_neg_xp02 <- tidy_stan(social_resp_neg,
                                  typical = "mean",
                                  prob = .95)

# export output
png("tables/brms/model_neg_xp02.png", height=480, width=720)
p<-tableGrob(model_neg_xp02)
grid.arrange(p)
dev.off()

# test with rope
equi_neg_xp02_rwa <- equi_test(social_resp_neg, rope = roperwaN)
equi_neg_xp02_rwa <- equi_neg_xp02_rwa[c(10,11),]
equi_neg_xp02_rwa$ROPE <- roperwaC

equi_neg_xp02_0.5 <- equi_test(social_resp_neg, rope = c(-0.5, 0.5))
equi_neg_xp02_0.5 <- equi_neg_xp02_0.5[9,]
equi_neg_xp02_0.5$ROPE <- "-0.5, 0.5"

equi_neg_xp02 <- rbind(equi_neg_xp02_rwa, equi_neg_xp02_0.5)
equi_neg_xp02[,c(3:5)] <- round(equi_neg_xp02[,c(3:5)], 2)
ordrow <- c("3", "1", "2")
equi_neg_xp02 <- equi_neg_xp02[ordrow,]

# export test
png("tables/rope/equi_neg_xp02.png", height=480, width=720)
p<-tableGrob(equi_neg_xp02)
grid.arrange(p)
dev.off()

#-------------
### Simple slope of RWA in the !!positive!! valence condition
#-------------

## first step = recode variable usvalence
## We interpret parameters for a 0 level of others

# Coding !!positive!! valence as 0
social_df$usvalence_pos <- ifelse (social_df$usvalence == 0.5, 0, 1)

social_resp_pos <- brm(response ~ usvalence_pos * RWAscore + (1|ppt) + (1|stim1), #here we change the usvalence variable with the recoded one
                             data = social_df, 
                             family = cumulative (link = "logit", threshold = "flexible"),
                             prior = priors,
                             warmup = 1000, iter = 2000,
                             chains = 4, cores = parallel::detectCores(),
                             control = list(adapt_delta = 0.8, max_treedepth = 10),
                             sample_prior = TRUE)

# In case we want to save summary ! change the name if you export both summary and tidy_stan
# model_pos_xp02 <- summary(social_resp_pos)$fixed[,col2keep]
# model_pos_xp02 <- round(model_pos_xp02, 3)

# arrange output
model_pos_xp02 <- tidy_stan(social_resp_pos,
                                  typical = "mean",
                                  prob = .95)

# export output
png("tables/brms/model_pos_xp02.png", height=480, width=720)
p<-tableGrob(model_pos_xp02)
grid.arrange(p)
dev.off()

# test with rope
equi_pos_xp02_rwa <- equi_test(social_resp_pos, rope = roperwaN)
equi_pos_xp02_rwa <- equi_pos_xp02_rwa[c(10,11),]
equi_pos_xp02_rwa$ROPE <- roperwaC

equi_pos_xp02_0.5 <- equi_test(social_resp_pos, rope = c(-0.5, 0.5))
equi_pos_xp02_0.5 <- equi_pos_xp02_0.5[9,]
equi_pos_xp02_0.5$ROPE <- "-0.5, 0.5"

equi_pos_xp02 <- rbind(equi_pos_xp02_rwa, equi_pos_xp02_0.5)
equi_pos_xp02[,c(3:5)] <- round(equi_pos_xp02[,c(3:5)], 2)
ordrow <- c("3", "1", "2")
equi_pos_xp02 <- equi_pos_xp02[ordrow,]

# export test
png("tables/rope/equi_pos_xp02.png", height=480, width=720)
p<-tableGrob(equi_pos_xp02)
grid.arrange(p)
dev.off()

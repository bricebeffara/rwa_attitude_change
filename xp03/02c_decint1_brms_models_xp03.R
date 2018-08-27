#------------------------------------------------------------------------------------
# Then we run our third step model to decompose the interaction 
# between RWA and usvalence2 in the !!2 blocs!! conditioning conditions
# The interaction slope between RWA and usvalence2 includes 0 in the !!1 bloc!! condition
#------------------------------------------------------------------------------------

#############################
################## 2 blocs
#############################

#-------------
### Simple slope of RWA in the !!negative!! valence & !!2 blocs!! condition
#-------------

## first step = recode variable usvalence2
## We interpret parameters for a 0 level of others

# Coding !!negative!! valence as 0
bloc_df$usvalence2_neg <- ifelse (bloc_df$usvalence2 == -0.5, 0, 1)

bloc_resp_b2_neg <- brm(response ~ usvalence2_neg * bloc2  * RWAscore + (1|ppt) + (1|stim1), #here we change the usvalence2 variable with the recoded one
                             data = bloc_df, 
                             family = cumulative (link = "logit", threshold = "flexible"),
                             prior = priors,
                             warmup = 1000, iter = 2000,
                             chains = 4, cores = parallel::detectCores(),
                             control = list(adapt_delta = 0.8, max_treedepth = 10),
                             sample_prior = TRUE)

# In case we want to save summary ! change the name if you export both summary and tidy_stan
# model_b2_neg_xp03 <- summary(bloc_resp_b2_neg)$fixed[,col2keep]
# model_b2_neg_xp03 <- round(model_b2_neg_xp03, 3)

# arrange output
model_b2_neg_xp03 <- tidy_stan(bloc_resp_b2_neg,
                                  typical = "mean",
                                  prob = .95)

# export output
png("tables/brms/model_b2_neg_xp03.png", height=480, width=720)
p<-tableGrob(model_b2_neg_xp03)
grid.arrange(p)
dev.off()

# test with rope
equi_b2_neg_xp03_rwa <- equi_test(bloc_resp_b2_neg, rope = roperwaN)
equi_b2_neg_xp03_rwa <- equi_b2_neg_xp03_rwa[c(11,13:15),]
equi_b2_neg_xp03_rwa$ROPE <- roperwaC

equi_b2_neg_xp03_0.5 <- equi_test(bloc_resp_b2_neg, rope = c(-0.5, 0.5))
equi_b2_neg_xp03_0.5 <- equi_b2_neg_xp03_0.5[c(9,10,12),]
equi_b2_neg_xp03_0.5$ROPE <- "-0.5, 0.5"

equi_b2_neg_xp03 <- rbind(equi_b2_neg_xp03_rwa, equi_b2_neg_xp03_0.5)
equi_b2_neg_xp03[,c(3:5)] <- round(equi_b2_neg_xp03[,c(3:5)], 2)
ordrow <- c("5","6","1","7","2","3","4")
equi_b2_neg_xp03 <- equi_b2_neg_xp03[ordrow,]

# export test
png("tables/rope/equi_b2_neg_xp03.png", height=480, width=720)
p<-tableGrob(equi_b2_neg_xp03)
grid.arrange(p)
dev.off()

#-------------
### Simple slope of RWA in the !!positive!! valence & !!level 1!! conditioning condition
#-------------

## first step = recode variable usvalence2
## We interpret parameters for a 0 level of others

# Coding !!positive!! valence as 0
bloc_df$usvalence2_pos <- ifelse (bloc_df$usvalence2 == 0.5, 0, 1)

bloc_resp_b2_pos <- brm(response ~ usvalence2_pos * bloc2  * RWAscore + (1|ppt) + (1|stim1), #here we change the usvalence2 variable with the recoded one
                             data = bloc_df, 
                             family = cumulative (link = "logit", threshold = "flexible"),
                             prior = priors,
                             warmup = 1000, iter = 2000,
                             chains = 4, cores = parallel::detectCores(),
                             control = list(adapt_delta = 0.8, max_treedepth = 10),
                             sample_prior = TRUE)

# In case we want to save summary ! change the name if you export both summary and tidy_stan
# model_b2_pos_xp03 <- summary(bloc_resp_b2_pos)$fixed[,col2keep]
# model_b2_pos_xp03 <- round(model_b2_pos_xp03, 3)

# arrange output
model_b2_pos_xp03 <- tidy_stan(bloc_resp_b2_pos,
                                  typical = "mean",
                                  prob = .95)

# export output
png("tables/brms/model_b2_pos_xp03.png", height=480, width=720)
p<-tableGrob(model_b2_pos_xp03)
grid.arrange(p)
dev.off()

# test with rope
equi_b2_pos_xp03_rwa <- equi_test(bloc_resp_b2_pos, rope = roperwaN)
equi_b2_pos_xp03_rwa <- equi_b2_pos_xp03_rwa[c(11,13:15),]
equi_b2_pos_xp03_rwa$ROPE <- roperwaC

equi_b2_pos_xp03_0.5 <- equi_test(bloc_resp_b2_pos, rope = c(-0.5, 0.5))
equi_b2_pos_xp03_0.5 <- equi_b2_pos_xp03_0.5[c(9,10,12),]
equi_b2_pos_xp03_0.5$ROPE <- "-0.5, 0.5"

equi_b2_pos_xp03 <- rbind(equi_b2_pos_xp03_rwa, equi_b2_pos_xp03_0.5)
equi_b2_pos_xp03[,c(3:5)] <- round(equi_b2_pos_xp03[,c(3:5)], 2)
ordrow <- c("5","6","1","7","2","3","4")
equi_b2_pos_xp03 <- equi_b2_pos_xp03[ordrow,]

# export test
png("tables/rope/equi_b2_pos_xp03.png", height=480, width=720)
p<-tableGrob(equi_b2_pos_xp03)
grid.arrange(p)
dev.off()

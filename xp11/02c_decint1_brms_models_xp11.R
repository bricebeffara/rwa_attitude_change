#------------------------------------------------------------------------------------
# Then we run our third step model to decompose the interaction 
# between RWA and usvalence in the !!no warning!! conditioning conditions
# The interaction slope between RWA and usvalence includes 0 in the warning condition
#------------------------------------------------------------------------------------

#############################
################## No Warning
#############################

#-------------
### Simple slope of RWA in the !!negative!! valence & !!no warning!! condition
#-------------

## first step = recode variable usvalence
## We interpret parameters for a 0 level of others

# Coding !!negative!! valence as 0
warn_df$usvalence_neg <- ifelse (warn_df$usvalence == -0.5, 0, 1)

warn_resp_nowa_neg <- brm(response ~ usvalence_neg * no_warn  * RWAscore + (1|ppt) + (1|stim1), #here we change the usvalence variable with the recoded one
                             data = warn_df, 
                             family = cumulative (link = "logit", threshold = "flexible"),
                             prior = priors,
                             warmup = 1000, iter = 2000,
                             chains = 4, cores = parallel::detectCores(),
                             control = list(adapt_delta = 0.8, max_treedepth = 10),
                             sample_prior = TRUE)

# In case we want to save summary ! change the name if you export both summary and tidy_stan
# model_nowa_neg_xp11 <- summary(warn_resp_nowa_neg)$fixed[,col2keep]
# model_nowa_neg_xp11 <- round(model_nowa_neg_xp11, 3)

# arrange output
model_nowa_neg_xp11 <- tidy_stan(warn_resp_nowa_neg,
                                  typical = "mean",
                                  prob = .95)

# export output
png("tables/brms/model_nowa_neg_xp11.png", height=480, width=720)
p<-tableGrob(model_nowa_neg_xp11)
grid.arrange(p)
dev.off()

# test with rope
equi_nowa_neg_xp11_rwa <- equi_test(warn_resp_nowa_neg, rope = roperwaN)
equi_nowa_neg_xp11_rwa <- equi_nowa_neg_xp11_rwa[c(11,13:15),]
equi_nowa_neg_xp11_rwa$ROPE <- roperwaC

equi_nowa_neg_xp11_0.5 <- equi_test(warn_resp_nowa_neg, rope = c(-0.5, 0.5))
equi_nowa_neg_xp11_0.5 <- equi_nowa_neg_xp11_0.5[c(9,10,12),]
equi_nowa_neg_xp11_0.5$ROPE <- "-0.5, 0.5"

equi_nowa_neg_xp11 <- rbind(equi_nowa_neg_xp11_rwa, equi_nowa_neg_xp11_0.5)
equi_nowa_neg_xp11[,c(3:5)] <- round(equi_nowa_neg_xp11[,c(3:5)], 2)
ordrow <- c("5","6","1","7","2","3","4")
equi_nowa_neg_xp11 <- equi_nowa_neg_xp11[ordrow,]

# export test
png("tables/rope/equi_nowa_neg_xp11.png", height=480, width=720)
p<-tableGrob(equi_nowa_neg_xp11)
grid.arrange(p)
dev.off()

#-------------
### Simple slope of RWA in the !!positive!! valence & !!level 1!! conditioning condition
#-------------

## first step = recode variable usvalence
## We interpret parameters for a 0 level of others

# Coding !!positive!! valence as 0
warn_df$usvalence_pos <- ifelse (warn_df$usvalence == 0.5, 0, 1)

warn_resp_nowa_pos <- brm(response ~ usvalence_pos * no_warn  * RWAscore + (1|ppt) + (1|stim1), #here we change the usvalence variable with the recoded one
                             data = warn_df, 
                             family = cumulative (link = "logit", threshold = "flexible"),
                             prior = priors,
                             warmup = 1000, iter = 2000,
                             chains = 4, cores = parallel::detectCores(),
                             control = list(adapt_delta = 0.8, max_treedepth = 10),
                             sample_prior = TRUE)

# In case we want to save summary ! change the name if you export both summary and tidy_stan
# model_nowa_pos_xp11 <- summary(warn_resp_nowa_pos)$fixed[,col2keep]
# model_nowa_pos_xp11 <- round(model_nowa_pos_xp11, 3)

# arrange output
model_nowa_pos_xp11 <- tidy_stan(warn_resp_nowa_pos,
                                  typical = "mean",
                                  prob = .95)

# export output
png("tables/brms/model_nowa_pos_xp11.png", height=480, width=720)
p<-tableGrob(model_nowa_pos_xp11)
grid.arrange(p)
dev.off()

# test with rope
equi_nowa_pos_xp11_rwa <- equi_test(warn_resp_nowa_pos, rope = roperwaN)
equi_nowa_pos_xp11_rwa <- equi_nowa_pos_xp11_rwa[c(11,13:15),]
equi_nowa_pos_xp11_rwa$ROPE <- roperwaC

equi_nowa_pos_xp11_0.5 <- equi_test(warn_resp_nowa_pos, rope = c(-0.5, 0.5))
equi_nowa_pos_xp11_0.5 <- equi_nowa_pos_xp11_0.5[c(9,10,12),]
equi_nowa_pos_xp11_0.5$ROPE <- "-0.5, 0.5"

equi_nowa_pos_xp11 <- rbind(equi_nowa_pos_xp11_rwa, equi_nowa_pos_xp11_0.5)
equi_nowa_pos_xp11[,c(3:5)] <- round(equi_nowa_pos_xp11[,c(3:5)], 2)
ordrow <- c("5","6","1","7","2","3","4")
equi_nowa_pos_xp11 <- equi_nowa_pos_xp11[ordrow,]

# export test
png("tables/rope/equi_nowa_pos_xp11.png", height=480, width=720)
p<-tableGrob(equi_nowa_pos_xp11)
grid.arrange(p)
dev.off()

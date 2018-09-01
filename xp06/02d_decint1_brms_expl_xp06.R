#------------------------------------------------------------------------------------
# Then we run our third step models to decompose the interactions 
# between RWA and usvalencedir in alla conditions and ord2 condition
#------------------------------------------------------------------------------------

#############################
##################### overall
#############################

#-------------
### Simple slope of RWA in the !!negative!! valence & overall
#-------------

## first step = recode variable usvalencedir
## We interpret parameters for a 0 level of others

# Coding !!negative!! valence as 0
direct_df$usvalencedir_neg <- ifelse (direct_df$usvalencedir == -0.5, 0, 1)

direct_resp_neg <- brm(response ~ usvalencedir_neg * RWAscore + (1|ppt) + (1|stim1), #here we change the order variable with the recoded one
                             data = direct_df, 
                             family = cumulative (link = "logit", threshold = "flexible"),
                             prior = priors,
                             warmup = 1000, iter = 2000,
                             chains = 4, cores = parallel::detectCores(),
                             control = list(adapt_delta = 0.8, max_treedepth = 10),
                             sample_prior = TRUE)

# In case we want to save summary ! change the name if you export both summary and tidy_stan
# model_neg_xp06 <- summary(direct_resp_neg)$fixed[,col2keep]
# model_neg_xp06 <- round(model_neg_xp06, 3)

# arrange output
model_neg_xp06 <- tidy_stan(direct_resp_neg,
                                  typical = "mean",
                                  prob = .95)

# export output
png("tables/brms/model_neg_xp06.png", height=480, width=720)
p<-tableGrob(model_neg_xp06)
grid.arrange(p)
dev.off()

# test with rope
equi_neg_xp06_rwa <- equi_test(direct_resp_neg, rope = roperwaN)
equi_neg_xp06_rwa <- equi_neg_xp06_rwa[c(10:11),]
equi_neg_xp06_rwa$ROPE <- roperwaC

equi_neg_xp06_0.5 <- equi_test(direct_resp_neg, rope = c(-0.5, 0.5))
equi_neg_xp06_0.5 <- equi_neg_xp06_0.5[c(9),]
equi_neg_xp06_0.5$ROPE <- "-0.5, 0.5"

equi_neg_xp06 <- rbind(equi_neg_xp06_rwa, equi_neg_xp06_0.5)
equi_neg_xp06[,c(3:5)] <- round(equi_neg_xp06[,c(3:5)], 2)
ordrow <- c(3,1,2)
equi_neg_xp06 <- equi_neg_xp06[ordrow,]

# export test
png("tables/rope/equi_neg_xp06.png", height=480, width=720)
p<-tableGrob(equi_neg_xp06)
grid.arrange(p)
dev.off()

#-------------
### Simple slope of RWA in the !!positive!! valence overall
#-------------

## first step = recode variable usvalencedir
## We interpret parameters for a 0 level of others

# Coding !!positive!! valence as 0
direct_df$usvalencedir_pos <- ifelse (direct_df$usvalencedir == 0.5, 0, 1)

direct_resp_pos <- brm(response ~ usvalencedir_pos * RWAscore + (1|ppt) + (1|stim1), #here we change the order variable with the recoded one
                             data = direct_df, 
                             family = cumulative (link = "logit", threshold = "flexible"),
                             prior = priors,
                             warmup = 1000, iter = 2000,
                             chains = 4, cores = parallel::detectCores(),
                             control = list(adapt_delta = 0.8, max_treedepth = 10),
                             sample_prior = TRUE)

# In case we want to save summary ! change the name if you export both summary and tidy_stan
# model_pos_xp06 <- summary(direct_resp_pos)$fixed[,col2keep]
# model_pos_xp06 <- round(model_pos_xp06, 3)

# arrange output
model_pos_xp06 <- tidy_stan(direct_resp_pos,
                                  typical = "mean",
                                  prob = .95)

# export output
png("tables/brms/model_pos_xp06.png", height=480, width=720)
p<-tableGrob(model_pos_xp06)
grid.arrange(p)
dev.off()

# test with rope
equi_pos_xp06_rwa <- equi_test(direct_resp_pos, rope = roperwaN)
equi_pos_xp06_rwa <- equi_pos_xp06_rwa[c(10:11),]
equi_pos_xp06_rwa$ROPE <- roperwaC

equi_pos_xp06_0.5 <- equi_test(direct_resp_pos, rope = c(-0.5, 0.5))
equi_pos_xp06_0.5 <- equi_pos_xp06_0.5[c(9),]
equi_pos_xp06_0.5$ROPE <- "-0.5, 0.5"

equi_pos_xp06 <- rbind(equi_pos_xp06_rwa, equi_pos_xp06_0.5)
equi_pos_xp06[,c(3:5)] <- round(equi_pos_xp06[,c(3:5)], 2)
ordrow <- c(3,1,2)
equi_pos_xp06 <- equi_pos_xp06[ordrow,]

# export test
png("tables/rope/equi_pos_xp06.png", height=480, width=720)
p<-tableGrob(equi_pos_xp06)
grid.arrange(p)
dev.off()

#############################
##################### order2
#############################

#-------------
### Simple slope of RWA in the !!negative!! valence & !!order2!! condition
#-------------

direct_resp_ord2_neg <- brm(response ~ usvalencedir_neg * order2  * RWAscore + (1|ppt) + (1|stim1), #here we change the order variable with the recoded one
                             data = direct_df, 
                             family = cumulative (link = "logit", threshold = "flexible"),
                             prior = priors,
                             warmup = 1000, iter = 2000,
                             chains = 4, cores = parallel::detectCores(),
                             control = list(adapt_delta = 0.8, max_treedepth = 10),
                             sample_prior = TRUE)

# In case we want to save summary ! change the name if you export both summary and tidy_stan
# model_ord2_neg_xp06 <- summary(direct_resp_ord2_neg)$fixed[,col2keep]
# model_ord2_neg_xp06 <- round(model_ord2_neg_xp06, 3)

# arrange output
model_ord2_neg_xp06 <- tidy_stan(direct_resp_ord2_neg,
                                  typical = "mean",
                                  prob = .95)

# export output
png("tables/brms/model_ord2_neg_xp06.png", height=480, width=720)
p<-tableGrob(model_ord2_neg_xp06)
grid.arrange(p)
dev.off()

# test with rope
equi_ord2_neg_xp06_rwa <- equi_test(direct_resp_ord2_neg, rope = roperwaN)
equi_ord2_neg_xp06_rwa <- equi_ord2_neg_xp06_rwa[c(11,13:15),]
equi_ord2_neg_xp06_rwa$ROPE <- roperwaC

equi_ord2_neg_xp06_0.5 <- equi_test(direct_resp_ord2_neg, rope = c(-0.5, 0.5))
equi_ord2_neg_xp06_0.5 <- equi_ord2_neg_xp06_0.5[c(9,10,12),]
equi_ord2_neg_xp06_0.5$ROPE <- "-0.5, 0.5"

equi_ord2_neg_xp06 <- rbind(equi_ord2_neg_xp06_rwa, equi_ord2_neg_xp06_0.5)
equi_ord2_neg_xp06[,c(3:5)] <- round(equi_ord2_neg_xp06[,c(3:5)], 2)
ordrow <- c(5,6,1,7,2,3,4)
equi_ord2_neg_xp06 <- equi_ord2_neg_xp06[ordrow,]

# export test
png("tables/rope/equi_ord2_neg_xp06.png", height=480, width=720)
p<-tableGrob(equi_ord2_neg_xp06)
grid.arrange(p)
dev.off()

#-------------
### Simple slope of RWA in the !!positive!! valence & !!order2!! condition
#-------------

direct_resp_ord2_pos <- brm(response ~ usvalencedir_pos * order2  * RWAscore + (1|ppt) + (1|stim1), #here we change the order variable with the recoded one
                             data = direct_df, 
                             family = cumulative (link = "logit", threshold = "flexible"),
                             prior = priors,
                             warmup = 1000, iter = 2000,
                             chains = 4, cores = parallel::detectCores(),
                             control = list(adapt_delta = 0.8, max_treedepth = 10),
                             sample_prior = TRUE)

# In case we want to save summary ! change the name if you export both summary and tidy_stan
# model_ord2_pos_xp06 <- summary(direct_resp_ord2_pos)$fixed[,col2keep]
# model_ord2_pos_xp06 <- round(model_ord2_pos_xp06, 3)

# arrange output
model_ord2_pos_xp06 <- tidy_stan(direct_resp_ord2_pos,
                                  typical = "mean",
                                  prob = .95)

# export output
png("tables/brms/model_ord2_pos_xp06.png", height=480, width=720)
p<-tableGrob(model_ord2_pos_xp06)
grid.arrange(p)
dev.off()

# test with rope
equi_ord2_pos_xp06_rwa <- equi_test(direct_resp_ord2_pos, rope = roperwaN)
equi_ord2_pos_xp06_rwa <- equi_ord2_pos_xp06_rwa[c(11,13:15),]
equi_ord2_pos_xp06_rwa$ROPE <- roperwaC

equi_ord2_pos_xp06_0.5 <- equi_test(direct_resp_ord2_pos, rope = c(-0.5, 0.5))
equi_ord2_pos_xp06_0.5 <- equi_ord2_pos_xp06_0.5[c(9,10,12),]
equi_ord2_pos_xp06_0.5$ROPE <- "-0.5, 0.5"

equi_ord2_pos_xp06 <- rbind(equi_ord2_pos_xp06_rwa, equi_ord2_pos_xp06_0.5)
equi_ord2_pos_xp06[,c(3:5)] <- round(equi_ord2_pos_xp06[,c(3:5)], 2)
ordrow <- c(5,6,1,7,2,3,4)
equi_ord2_pos_xp06 <- equi_ord2_pos_xp06[ordrow,]

# export test
png("tables/rope/equi_ord2_pos_xp06.png", height=480, width=720)
p<-tableGrob(equi_ord2_pos_xp06)
grid.arrange(p)
dev.off()

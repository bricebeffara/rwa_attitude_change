#------------------------------------------------------------------------------------
# Then we run our third step models to decompose the interactions 
# between RWA and usvalence in the no load and load conditions
#------------------------------------------------------------------------------------

#############################
##################### No load
#############################

#-------------
### Simple slope of RWA in the !!negative!! valence & !!no load!! condition
#-------------

## first step = recode variable usvalence
## We interpret parameters for a 0 level of others

# Coding !!negative!! valence as 0
load_df$usvalence_neg <- ifelse (load_df$usvalence == -0.5, 0, 1)

load_resp_nolo_neg <- brm(response ~ usvalence_neg * no_load  * RWAscore + (1|ppt) + (1|stim1), #here we change the load_df variable with the recoded one
                             data = load_df, 
                             family = cumulative (link = "logit", threshold = "flexible"),
                             prior = priors,
                             warmup = 1000, iter = 2000,
                             chains = 4, cores = parallel::detectCores(),
                             control = list(adapt_delta = 0.8, max_treedepth = 10),
                             sample_prior = TRUE)

# In case we want to save summary ! change the name if you export both summary and tidy_stan
# model_nolo_neg_xp09 <- summary(load_resp_nolo_neg)$fixed[,col2keep]
# model_nolo_neg_xp09 <- round(model_nolo_neg_xp09, 3)

# arrange output
model_nolo_neg_xp09 <- tidy_stan(load_resp_nolo_neg,
                                  typical = "mean",
                                  prob = .95)

# export output
png("tables/brms/model_nolo_neg_xp09.png", height=480, width=720)
p<-tableGrob(model_nolo_neg_xp09)
grid.arrange(p)
dev.off()

# test with rope
equi_nolo_neg_xp09_rwa <- equi_test(load_resp_nolo_neg, rope = roperwaN)
equi_nolo_neg_xp09_rwa <- equi_nolo_neg_xp09_rwa[c(11,13:15),]
equi_nolo_neg_xp09_rwa$ROPE <- roperwaC

equi_nolo_neg_xp09_0.5 <- equi_test(load_resp_nolo_neg, rope = c(-0.5, 0.5))
equi_nolo_neg_xp09_0.5 <- equi_nolo_neg_xp09_0.5[c(9,10,12),]
equi_nolo_neg_xp09_0.5$ROPE <- "-0.5, 0.5"

equi_nolo_neg_xp09 <- rbind(equi_nolo_neg_xp09_rwa, equi_nolo_neg_xp09_0.5)
equi_nolo_neg_xp09[,c(3:5)] <- round(equi_nolo_neg_xp09[,c(3:5)], 2)
ordrow <- c(5, 6, 1, 7, 2, 3, 4)
equi_nolo_neg_xp09 <- equi_nolo_neg_xp09[ordrow,]

# export test
png("tables/rope/equi_nolo_neg_xp09.png", height=480, width=720)
p<-tableGrob(equi_nolo_neg_xp09)
grid.arrange(p)
dev.off()

#-------------
### Simple slope of RWA in the !!positive!! valence & !!no load!! condition
#-------------

## first step = recode variable usvalence
## We interpret parameters for a 0 level of others

# Coding !!positive!! valence as 0
load_df$usvalence_pos <- ifelse (load_df$usvalence == 0.5, 0, 1)

load_resp_nolo_pos <- brm(response ~ usvalence_pos * no_load  * RWAscore + (1|ppt) + (1|stim1), #here we change the load_df variable with the recoded one
                             data = load_df, 
                             family = cumulative (link = "logit", threshold = "flexible"),
                             prior = priors,
                             warmup = 1000, iter = 2000,
                             chains = 4, cores = parallel::detectCores(),
                             control = list(adapt_delta = 0.8, max_treedepth = 10),
                             sample_prior = TRUE)

# In case we want to save summary ! change the name if you export both summary and tidy_stan
# model_nolo_pos_xp09 <- summary(load_resp_nolo_pos)$fixed[,col2keep]
# model_nolo_pos_xp09 <- round(model_nolo_pos_xp09, 3)

# arrange output
model_nolo_pos_xp09 <- tidy_stan(load_resp_nolo_pos,
                                  typical = "mean",
                                  prob = .95)

# export output
png("tables/brms/model_nolo_pos_xp09.png", height=480, width=720)
p<-tableGrob(model_nolo_pos_xp09)
grid.arrange(p)
dev.off()

# test with rope
equi_nolo_pos_xp09_rwa <- equi_test(load_resp_nolo_pos, rope = roperwaN)
equi_nolo_pos_xp09_rwa <- equi_nolo_pos_xp09_rwa[c(11,13:15),]
equi_nolo_pos_xp09_rwa$ROPE <- roperwaC

equi_nolo_pos_xp09_0.5 <- equi_test(load_resp_nolo_pos, rope = c(-0.5, 0.5))
equi_nolo_pos_xp09_0.5 <- equi_nolo_pos_xp09_0.5[c(9,10,12),]
equi_nolo_pos_xp09_0.5$ROPE <- "-0.5, 0.5"

equi_nolo_pos_xp09 <- rbind(equi_nolo_pos_xp09_rwa, equi_nolo_pos_xp09_0.5)
equi_nolo_pos_xp09[,c(3:5)] <- round(equi_nolo_pos_xp09[,c(3:5)], 2)
ordrow <- c(5, 6, 1, 7, 2, 3, 4)
equi_nolo_pos_xp09 <- equi_nolo_pos_xp09[ordrow,]

# export test
png("tables/rope/equi_nolo_pos_xp09.png", height=480, width=720)
p<-tableGrob(equi_nolo_pos_xp09)
grid.arrange(p)
dev.off()

#############################
##################### Load
#############################

#-------------
### Simple slope of RWA in the !!negative!! valence & !!load!! condition
#-------------

load_resp_yelo_neg <- brm(response ~ usvalence_neg * ye_load  * RWAscore + (1|ppt) + (1|stim1), #here we change the load_df variable with the recoded one
                             data = load_df, 
                             family = cumulative (link = "logit", threshold = "flexible"),
                             prior = priors,
                             warmup = 1000, iter = 2000,
                             chains = 4, cores = parallel::detectCores(),
                             control = list(adapt_delta = 0.8, max_treedepth = 10),
                             sample_prior = TRUE)

# In case we want to save summary ! change the name if you export both summary and tidy_stan
# model_yelo_neg_xp09 <- summary(load_resp_yelo_neg)$fixed[,col2keep]
# model_yelo_neg_xp09 <- round(model_yelo_neg_xp09, 3)

# arrange output
model_yelo_neg_xp09 <- tidy_stan(load_resp_yelo_neg,
                                  typical = "mean",
                                  prob = .95)

# export output
png("tables/brms/model_yelo_neg_xp09.png", height=480, width=720)
p<-tableGrob(model_yelo_neg_xp09)
grid.arrange(p)
dev.off()

# test with rope
equi_yelo_neg_xp09_rwa <- equi_test(load_resp_yelo_neg, rope = roperwaN)
equi_yelo_neg_xp09_rwa <- equi_yelo_neg_xp09_rwa[c(11,13:15),]
equi_yelo_neg_xp09_rwa$ROPE <- roperwaC

equi_yelo_neg_xp09_0.5 <- equi_test(load_resp_yelo_neg, rope = c(-0.5, 0.5))
equi_yelo_neg_xp09_0.5 <- equi_yelo_neg_xp09_0.5[c(9,10,12),]
equi_yelo_neg_xp09_0.5$ROPE <- "-0.5, 0.5"

equi_yelo_neg_xp09 <- rbind(equi_yelo_neg_xp09_rwa, equi_yelo_neg_xp09_0.5)
equi_yelo_neg_xp09[,c(3:5)] <- round(equi_yelo_neg_xp09[,c(3:5)], 2)
ordrow <- c(5, 6, 1, 7, 2, 3, 4)
equi_yelo_neg_xp09 <- equi_yelo_neg_xp09[ordrow,]

# export test
png("tables/rope/equi_yelo_neg_xp09.png", height=480, width=720)
p<-tableGrob(equi_yelo_neg_xp09)
grid.arrange(p)
dev.off()

#-------------
### Simple slope of RWA in the !!positive!! valence & !!load!! condition
#-------------

load_resp_yelo_pos <- brm(response ~ usvalence_pos * ye_load  * RWAscore + (1|ppt) + (1|stim1), #here we change the load_df variable with the recoded one
                             data = load_df, 
                             family = cumulative (link = "logit", threshold = "flexible"),
                             prior = priors,
                             warmup = 1000, iter = 2000,
                             chains = 4, cores = parallel::detectCores(),
                             control = list(adapt_delta = 0.8, max_treedepth = 10),
                             sample_prior = TRUE)

# In case we want to save summary ! change the name if you export both summary and tidy_stan
# model_yelo_pos_xp09 <- summary(load_resp_yelo_pos)$fixed[,col2keep]
# model_yelo_pos_xp09 <- round(model_yelo_pos_xp09, 3)

# arrange output
model_yelo_pos_xp09 <- tidy_stan(load_resp_yelo_pos,
                                  typical = "mean",
                                  prob = .95)

# export output
png("tables/brms/model_yelo_pos_xp09.png", height=480, width=720)
p<-tableGrob(model_yelo_pos_xp09)
grid.arrange(p)
dev.off()

# test with rope
equi_yelo_pos_xp09_rwa <- equi_test(load_resp_yelo_pos, rope = roperwaN)
equi_yelo_pos_xp09_rwa <- equi_yelo_pos_xp09_rwa[c(11,13:15),]
equi_yelo_pos_xp09_rwa$ROPE <- roperwaC

equi_yelo_pos_xp09_0.5 <- equi_test(load_resp_yelo_pos, rope = c(-0.5, 0.5))
equi_yelo_pos_xp09_0.5 <- equi_yelo_pos_xp09_0.5[c(9,10,12),]
equi_yelo_pos_xp09_0.5$ROPE <- "-0.5, 0.5"

equi_yelo_pos_xp09 <- rbind(equi_yelo_pos_xp09_rwa, equi_yelo_pos_xp09_0.5)
equi_yelo_pos_xp09[,c(3:5)] <- round(equi_yelo_pos_xp09[,c(3:5)], 2)
ordrow <- c(5, 6, 1, 7, 2, 3, 4)
equi_yelo_pos_xp09 <- equi_yelo_pos_xp09[ordrow,]

# export test
png("tables/rope/equi_yelo_pos_xp09.png", height=480, width=720)
p<-tableGrob(equi_yelo_pos_xp09)
grid.arrange(p)
dev.off()

#------------------------------------------------------------------------------------
# Then we run our third step models to decompose the interactions 
# between RWA and usvalence in the level 1 and level 2 conditioning conditions
#------------------------------------------------------------------------------------

#############################
##################### Level 1
#############################

#-------------
### Simple slope of RWA in the !!negative!! valence & !!1 counter-conditioning!! condition
#-------------


#model
doubleone_resp_ctone_neg_lme4 <- lmer(response ~ usvalence_neg * counter_one  * RWAscore + (1|ppt) + (1|stim1),
                                   data = double_one_df)

# Save summary & confint
model_ctone_neg_xp08_lme4 <- round(cbind(summary(doubleone_resp_ctone_neg_lme4)$coefficients,
                                         confint(doubleone_resp_ctone_neg_lme4)[c(4:11),]), 2)

# export output
png("tables/lme4/model_ctone_neg_xp08_lme4.png", height=480, width=720)
p<-tableGrob(model_ctone_neg_xp08_lme4)
grid.arrange(p)
dev.off()

#-------------
### Simple slope of RWA in the !!positive!! valence & !!1 counter-conditioning!! condition
#-------------

#model
doubleone_resp_ctone_pos_lme4 <- lmer(response ~ usvalence_pos * counter_one  * RWAscore + (1|ppt) + (1|stim1),
                                   data = double_one_df)

# Save summary & confint
model_ctone_pos_xp08_lme4 <- round(cbind(summary(doubleone_resp_ctone_pos_lme4)$coefficients,
                                         confint(doubleone_resp_ctone_pos_lme4)[c(4:11),]), 2)

# export output
png("tables/lme4/model_ctone_pos_xp08_lme4.png", height=480, width=720)
p<-tableGrob(model_ctone_pos_xp08_lme4)
grid.arrange(p)
dev.off()


#############################
##################### Level 2
#############################

#-------------
### Simple slope of RWA in the !!negative!! valence & !!2 counter-conditionings!! condition
#-------------

#model
doubleone_resp_cttwo_neg_lme4 <- lmer(response ~ usvalence_neg * counter_two  * RWAscore + (1|ppt) + (1|stim1),
                                   data = double_one_df)

# Save summary & confint
model_cttwo_neg_xp08_lme4 <- round(cbind(summary(doubleone_resp_cttwo_neg_lme4)$coefficients,
                                         confint(doubleone_resp_cttwo_neg_lme4)[c(4:11),]), 2)

# export output
png("tables/lme4/model_cttwo_neg_xp08_lme4.png", height=480, width=720)
p<-tableGrob(model_cttwo_neg_xp08_lme4)
grid.arrange(p)
dev.off()

#-------------
### Simple slope of RWA in the !!positive!! valence & !!2 counter-conditionings!! condition
#-------------

#model
doubleone_resp_cttwo_pos_lme4 <- lmer(response ~ usvalence_pos * counter_two  * RWAscore + (1|ppt) + (1|stim1),
                                   data = double_one_df)

# Save summary & confint
model_cttwo_pos_xp08_lme4 <- round(cbind(summary(doubleone_resp_cttwo_pos_lme4)$coefficients,
                                         confint(doubleone_resp_cttwo_pos_lme4)[c(4:11),]), 2)

# export output
png("tables/lme4/model_cttwo_pos_xp08_lme4.png", height=480, width=720)
p<-tableGrob(model_cttwo_pos_xp08_lme4)
grid.arrange(p)
dev.off()

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
doubleonetime_resp_ctone_neg_lme4 <- lmer(response ~ usvalence_neg * counter_one  * RWAscore + (1|ppt) + (1|stim1),
                                      data = double_onetime_df)

# Save summary & confint
model_ctonetime_neg_xp08_lme4 <- round(cbind(summary(doubleonetime_resp_ctone_neg_lme4)$coefficients,
                                         confint(doubleonetime_resp_ctone_neg_lme4)[c(4:11),]), 2)

# export output
png("tables/lme4/model_ctonetime_neg_xp08_lme4.png", height=480, width=720)
p<-tableGrob(model_ctonetime_neg_xp08_lme4)
grid.arrange(p)
dev.off()

#-------------
### Simple slope of RWA in the !!positive!! valence & !!1 counter-conditioning!! condition
#-------------

#model
doubleonetime_resp_ctone_pos_lme4 <- lmer(response ~ usvalence_pos * counter_one  * RWAscore + (1|ppt) + (1|stim1),
                                      data = double_onetime_df)

# Save summary & confint
model_ctonetime_pos_xp08_lme4 <- round(cbind(summary(doubleonetime_resp_ctone_pos_lme4)$coefficients,
                                         confint(doubleonetime_resp_ctone_pos_lme4)[c(4:11),]), 2)

# export output
png("tables/lme4/model_ctonetime_pos_xp08_lme4.png", height=480, width=720)
p<-tableGrob(model_ctonetime_pos_xp08_lme4)
grid.arrange(p)
dev.off()


#############################
##################### Level 2
#############################

#-------------
### Simple slope of RWA in the !!negative!! valence & !!2 counter-conditionings!! condition
#-------------

#model
doubletimeone_resp_ctone_neg_lme4 <- lmer(response ~ usvalence_neg * counter_one  * RWAscore + (1|ppt) + (1|stim1),
                                      data = double_timeone_df)

# Save summary & confint
model_ctimeone_neg_xp08_lme4 <- round(cbind(summary(doubletimeone_resp_ctone_neg_lme4)$coefficients,
                                         confint(doubletimeone_resp_ctone_neg_lme4)[c(4:11),]), 2)

# export output
png("tables/lme4/model_ctimeone_neg_xp08_lme4.png", height=480, width=720)
p<-tableGrob(model_ctimeone_neg_xp08_lme4)
grid.arrange(p)
dev.off()

#-------------
### Simple slope of RWA in the !!positive!! valence & !!2 counter-conditionings!! condition
#-------------

#model
doubletimeone_resp_ctone_pos_lme4 <- lmer(response ~ usvalence_pos * counter_one  * RWAscore + (1|ppt) + (1|stim1),
                                      data = double_timeone_df)

# Save summary & confint
model_ctimeone_pos_xp08_lme4 <- round(cbind(summary(doubletimeone_resp_ctone_pos_lme4)$coefficients,
                                         confint(doubletimeone_resp_ctone_pos_lme4)[c(4:11),]), 2)

# export output
png("tables/lme4/model_ctimeone_pos_xp08_lme4.png", height=480, width=720)
p<-tableGrob(model_ctimeone_pos_xp08_lme4)
grid.arrange(p)
dev.off()

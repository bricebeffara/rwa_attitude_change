mxp1_bas <- lmer(CSratings ~ (1|ppt) + (1|stim), data = XP1_Greebles)
mxp1_rwa <- lmer(CSratings ~ rwa + (1|ppt) + (1|stim), data = XP1_Greebles)
mxp1_usv <- lmer(CSratings ~ usvalence + (1|ppt) + (1|stim), data = XP1_Greebles)
mxp1_int <- lmer(CSratings ~ rwa * usvalence + (1|ppt) + (1|stim), data = XP1_Greebles)




summary(mxp1_usv)
confint(mxp1_usv)

summary(mxp1_rwa)
confint(mxp1_rwa)

summary(mxp1_int)
confint(mxp1_int)

MODELXP2 <- lmer(ratings ~usvalence + (1|ppt) + (1|stim),
                 data = XP2_Greebles)
MODELRANDXP2 <- lmer(ratings ~ (1|ppt) + (1|stim),
                     data = XP2_Greebles)
anova(MODELXP2,MODELRANDXP2)
sem.model.fits(MODELXP2)
confint(MODELXP2)

summary(MODEL1)
install.packages("piecewiseSEM")
library(piecewiseSEM)
install.packages("lme4")
library(lme4)
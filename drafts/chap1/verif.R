install.packages("ordinal")
library(piecewiseSEM)

mxp1_bas <- lmer(CSratings ~ (1|ppt) + (1|stim), data = XP1_Greebles)

# usv

mxp1_usv <- lmer(CSratings ~ usvalence + (1|ppt) + (1|stim), data = XP1_Greebles)

anova(mxp1_bas, mxp1_usv)
summary(mxp1_usv)
confint(mxp1_usv)
sem.model.fits(mxp1_usv)


# rwa 

mxp1_rwa <- lmer(CSratings ~ rwa + (1|ppt) + (1|stim), data = XP1_Greebles)

anova(mxp1_bas, mxp1_rwa)
summary(mxp1_rwa)
confint(mxp1_rwa)
sem.model.fits(mxp1_rwa)

# int

mxp1_int <- lmer(CSratings ~ rwa * usvalence + (1|ppt) + (1|stim), data = XP1_Greebles)

anova(mxp1_bas, mxp1_int)
summary(mxp1_int)
confint(mxp1_int)
sem.model.fits(mxp1_int)


# int 2


mxp1_int2 <- lmer(CSratings ~ rwa : usvalence + (1|ppt) + (1|stim), data = XP1_Greebles)

anova(mxp1_bas, mxp1_int2)
summary(mxp1_int2)
confint(mxp1_int2)
sem.model.fits(mxp1_int2)


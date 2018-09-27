p_load(plyr,
       knitr,
       kableExtra,
       formattable,
       dplyr,
       webshot,
       stringr,
       install = TRUE,
       update = getOption("pac_update"),
       character.only = FALSE)

tgen_lme4 <- as.data.frame(model_gen_xp10_lme4[c(2:8),])
tgen_brms <- as.data.frame(model_gen_xp10[c(9:15),])
tgen_equi <- as.data.frame(equi_gen_xp10)

tlvone_lme4 <- as.data.frame(model_lvone_xp10_lme4[c(2:8),])
tlvone_brms <- as.data.frame(model_lvone_xp10[c(9:15),])
tlvone_equi <- as.data.frame(equi_lvone_xp10)

tlvtwo_lme4 <- as.data.frame(model_lvtwo_xp10_lme4[c(2:8),])
tlvtwo_brms <- as.data.frame(model_lvtwo_xp10[c(9:15),])
tlvtwo_equi <- as.data.frame(equi_lvtwo_xp10)

tlvone_neg_lme4 <- as.data.frame(model_lvone_neg_xp10_lme4[c(2:8),])
tlvone_neg_brms <- as.data.frame(model_lvone_neg_xp10[c(9:15),])
tlvone_neg_equi <- as.data.frame(equi_lvone_neg_xp10)

tlvone_pos_lme4 <- as.data.frame(model_lvone_pos_xp10_lme4[c(2:8),])
tlvone_pos_brms <- as.data.frame(model_lvone_pos_xp10[c(9:15),])
tlvone_pos_equi <- as.data.frame(equi_lvone_pos_xp10)

tlvtwo_neg_lme4 <- as.data.frame(model_lvtwo_neg_xp10_lme4[c(2:8),])
tlvtwo_neg_brms <- as.data.frame(model_lvtwo_neg_xp10[c(9:15),])
tlvtwo_neg_equi <- as.data.frame(equi_lvtwo_neg_xp10)

tlvtwo_pos_lme4 <- as.data.frame(model_lvtwo_pos_xp10_lme4[c(2:8),])
tlvtwo_pos_brms <- as.data.frame(model_lvtwo_pos_xp10[c(9:15),])
tlvtwo_pos_equi <- as.data.frame(equi_lvtwo_pos_xp10)


tgen_lme4$term <- tgen_brms$term
tlvone_lme4$term <- tlvone_brms$term
tlvtwo_lme4$term <- tlvtwo_brms$term
tlvone_neg_lme4$term <- tlvone_neg_brms$term
tlvone_pos_lme4$term <- tlvone_pos_brms$term
tlvtwo_neg_lme4$term <- tlvtwo_neg_brms$term
tlvtwo_pos_lme4$term <- tlvtwo_pos_brms$term

# general regression ------------------------------------------------------

tgen <- join (tgen_brms, tgen_lme4, by = "term")
tgen <- join (tgen, tgen_equi, by ="term")

tgen$"b [95% HDI]" <- paste ( format( tgen$estimate, nsmall = 2), " [",  format( tgen$hdi.low, nsmall = 2), 
                                      ", ", format(tgen$hdi.high, nsmall = 2), "]", sep = "")

tgen$"b [95% HDI]" <- gsub("\\s+", " ", str_trim(tgen$"b [95% HDI]"))
tgen$"b [95% HDI]" <- gsub("\\[ ", "\\[", str_trim(tgen$"b [95% HDI]"))

tgen$"b [95% CI]" <- paste ( format( tgen$Estimate, nsmall = 2), " [",  format( tgen$"2.5 %", nsmall = 2), 
                                    ", ", format(tgen$"97.5 %", nsmall = 2), "]", sep = "")

tgen$"b [95% CI]" <- gsub("\\s+", " ", str_trim(tgen$"b [95% CI]"))
tgen$"b [95% CI]" <- gsub("\\[ ", "\\[", str_trim(tgen$"b [95% CI]"))

# simple slope valence in the level 1 conditioning condition ----------

tlvoneA_lme4 <- format(tlvone_lme4[tlvone_lme4$term == "b_usvalence",], nsmall = 2)
tlvoneA_brms <- format(tlvone_brms[tlvone_brms$term == "b_usvalence",], nsmall = 2)
tlvoneA_equi <- format(tlvone_equi[tlvone_equi$term == "b_usvalence",], nsmall = 2)

tlvoneA <- join (tlvoneA_brms, tlvoneA_lme4, by = "term")
tlvoneA <- join (tlvoneA, tlvoneA_equi, by ="term")

tlvoneA$"b [95% HDI]" <- paste ( format( tlvoneA$estimate, nsmall = 2), " [",  format( tlvoneA$hdi.low, nsmall = 2), 
                              ", ", format(tlvoneA$hdi.high, nsmall = 2), "]", sep = "")

tlvoneA$"b [95% CI]" <- paste ( format( tlvoneA$Estimate, nsmall = 2), " [",  format( tlvoneA$"2.5 %", nsmall = 2), 
                             ", ", format(tlvoneA$"97.5 %", nsmall = 2), "]", sep = "")

# simple slope RWA in the level 1 conditioning condition ----------

tlvoneB_lme4 <- format(tlvone_lme4[tlvone_lme4$term == "b_RWAscore",], nsmall = 2)
tlvoneB_brms <- format(tlvone_brms[tlvone_brms$term == "b_RWAscore",], nsmall = 2)
tlvoneB_equi <- format(tlvone_equi[tlvone_equi$term == "b_RWAscore",], nsmall = 2)

tlvoneB <- join (tlvoneB_brms, tlvoneB_lme4, by = "term")
tlvoneB <- join (tlvoneB, tlvoneB_equi, by ="term")

tlvoneB$"b [95% HDI]" <- paste ( format( tlvoneB$estimate, nsmall = 2), " [",  format( tlvoneB$hdi.low, nsmall = 2), 
                              ", ", format(tlvoneB$hdi.high, nsmall = 2), "]", sep = "")

tlvoneB$"b [95% CI]" <- paste ( format( tlvoneB$Estimate, nsmall = 2), " [",  format( tlvoneB$"2.5 %", nsmall = 2), 
                             ", ", format(tlvoneB$"97.5 %", nsmall = 2), "]", sep = "")

# simple slope valence in the level 2 conditioning condition ----------

tlvtwoA_lme4 <- format(tlvtwo_lme4[tlvtwo_lme4$term == "b_usvalence",], nsmall = 2)
tlvtwoA_brms <- format(tlvtwo_brms[tlvtwo_brms$term == "b_usvalence",], nsmall = 2)
tlvtwoA_equi <- format(tlvtwo_equi[tlvtwo_equi$term == "b_usvalence",], nsmall = 2)

tlvtwoA <- join (tlvtwoA_brms, tlvtwoA_lme4, by = "term")
tlvtwoA <- join (tlvtwoA, tlvtwoA_equi, by ="term")

tlvtwoA$"b [95% HDI]" <- paste ( format( tlvtwoA$estimate, nsmall = 2), " [",  format( tlvtwoA$hdi.low, nsmall = 2), 
                              ", ", format(tlvtwoA$hdi.high, nsmall = 2), "]", sep = "")

tlvtwoA$"b [95% CI]" <- paste ( format( tlvtwoA$Estimate, nsmall = 2), " [",  format( tlvtwoA$"2.5 %", nsmall = 2), 
                             ", ", format(tlvtwoA$"97.5 %", nsmall = 2), "]", sep = "")

# simple slope RWA in the level 2 conditioning condition ----------

tlvtwoB_lme4 <- format(tlvtwo_lme4[tlvtwo_lme4$term == "b_RWAscore",], nsmall = 2)
tlvtwoB_brms <- format(tlvtwo_brms[tlvtwo_brms$term == "b_RWAscore",], nsmall = 2)
tlvtwoB_equi <- format(tlvtwo_equi[tlvtwo_equi$term == "b_RWAscore",], nsmall = 2)

tlvtwoB <- join (tlvtwoB_brms, tlvtwoB_lme4, by = "term")
tlvtwoB <- join (tlvtwoB, tlvtwoB_equi, by ="term")

tlvtwoB$"b [95% HDI]" <- paste ( format( tlvtwoB$estimate, nsmall = 2), " [",  format( tlvtwoB$hdi.low, nsmall = 2), 
                              ", ", format(tlvtwoB$hdi.high, nsmall = 2), "]", sep = "")

tlvtwoB$"b [95% CI]" <- paste ( format( tlvtwoB$Estimate, nsmall = 2), " [",  format( tlvtwoB$"2.5 %", nsmall = 2), 
                             ", ", format(tlvtwoB$"97.5 %", nsmall = 2), "]", sep = "")

# simple slope RWA * usvalence in the level 1 conditioning condition ----------

tlvoneI_lme4 <- format(tlvone_lme4[tlvone_lme4$term == "b_usvalence.RWAscore",], nsmall = 2)
tlvoneI_brms <- format(tlvone_brms[tlvone_brms$term == "b_usvalence.RWAscore",], nsmall = 2)
tlvoneI_equi <- format(tlvone_equi[tlvone_equi$term == "b_usvalence.RWAscore",], nsmall = 2)

tlvoneI <- join (tlvoneI_brms, tlvoneI_lme4, by = "term")
tlvoneI <- join (tlvoneI, tlvoneI_equi, by ="term")

tlvoneI$"b [95% HDI]" <- paste ( format( tlvoneI$estimate, nsmall = 2), " [",  format( tlvoneI$hdi.low, nsmall = 2), 
                                ", ", format(tlvoneI$hdi.high, nsmall = 2), "]", sep = "")

tlvoneI$"b [95% CI]" <- paste ( format( tlvoneI$Estimate, nsmall = 2), " [",  format( tlvoneI$"2.5 %", nsmall = 2), 
                               ", ", format(tlvoneI$"97.5 %", nsmall = 2), "]", sep = "")


# simple slope RWA * usvalence in the level 2 conditioning condition ----------

tlvtwoI_lme4 <- format(tlvtwo_lme4[tlvtwo_lme4$term == "b_usvalence.RWAscore",], nsmall = 2)
tlvtwoI_brms <- format(tlvtwo_brms[tlvtwo_brms$term == "b_usvalence.RWAscore",], nsmall = 2)
tlvtwoI_equi <- format(tlvtwo_equi[tlvtwo_equi$term == "b_usvalence.RWAscore",], nsmall = 2)

tlvtwoI <- join (tlvtwoI_brms, tlvtwoI_lme4, by = "term")
tlvtwoI <- join (tlvtwoI, tlvtwoI_equi, by ="term")

tlvtwoI$"b [95% HDI]" <- paste ( format( tlvtwoI$estimate, nsmall = 2), " [",  format( tlvtwoI$hdi.low, nsmall = 2), 
                                ", ", format(tlvtwoI$hdi.high, nsmall = 2), "]", sep = "")

tlvtwoI$"b [95% CI]" <- paste ( format( tlvtwoI$Estimate, nsmall = 2), " [",  format( tlvtwoI$"2.5 %", nsmall = 2), 
                               ", ", format(tlvtwoI$"97.5 %", nsmall = 2), "]", sep = "")


# simple slope RWA level 1 conditioning negative valence condition --------

tlvone_neg_rwa_lme4 <- format(tlvone_neg_lme4[tlvone_neg_lme4$term == "b_RWAscore",], nsmall = 2)
tlvone_neg_rwa_brms <- format(tlvone_neg_brms[tlvone_neg_brms$term == "b_RWAscore",], nsmall = 2)
tlvone_neg_rwa_equi <- format(tlvone_neg_equi[tlvone_neg_equi$term == "b_RWAscore",], nsmall = 2)

tlvone_neg_rwa <- join (tlvone_neg_rwa_brms, tlvone_neg_rwa_lme4, by = "term")
tlvone_neg_rwa <- join (tlvone_neg_rwa, tlvone_neg_rwa_equi, by ="term")

tlvone_neg_rwa$"b [95% HDI]" <- paste ( format( tlvone_neg_rwa$estimate, nsmall = 2), " [",  format( tlvone_neg_rwa$hdi.low, nsmall = 2), 
                                       ", ", format(tlvone_neg_rwa$hdi.high, nsmall = 2), "]", sep = "")

tlvone_neg_rwa$"b [95% CI]" <- paste ( format( tlvone_neg_rwa$Estimate, nsmall = 2), " [",  format( tlvone_neg_rwa$"2.5 %", nsmall = 2), 
                                      ", ", format(tlvone_neg_rwa$"97.5 %", nsmall = 2), "]", sep = "")

# simple slope RWA level 1 conditioning positive valence condition --------

tlvone_pos_rwa_lme4 <- format(tlvone_pos_lme4[tlvone_pos_lme4$term == "b_RWAscore",], nsmall = 2)
tlvone_pos_rwa_brms <- format(tlvone_pos_brms[tlvone_pos_brms$term == "b_RWAscore",], nsmall = 2)
tlvone_pos_rwa_equi <- format(tlvone_pos_equi[tlvone_pos_equi$term == "b_RWAscore",], nsmall = 2)

tlvone_pos_rwa <- join (tlvone_pos_rwa_brms, tlvone_pos_rwa_lme4, by = "term")
tlvone_pos_rwa <- join (tlvone_pos_rwa, tlvone_pos_rwa_equi, by ="term")

tlvone_pos_rwa$"b [95% HDI]" <- paste ( format( tlvone_pos_rwa$estimate, nsmall = 2), " [",  format( tlvone_pos_rwa$hdi.low, nsmall = 2), 
                                       ", ", format(tlvone_pos_rwa$hdi.high, nsmall = 2), "]", sep = "")

tlvone_pos_rwa$"b [95% CI]" <- paste ( format( tlvone_pos_rwa$Estimate, nsmall = 2), " [",  format( tlvone_pos_rwa$"2.5 %", nsmall = 2), 
                                      ", ", format(tlvone_pos_rwa$"97.5 %", nsmall = 2), "]", sep = "")

# simple slope RWA level 2 conditioning negative valence condition --------

tlvtwo_neg_rwa_lme4 <- format(tlvtwo_neg_lme4[tlvtwo_neg_lme4$term == "b_RWAscore",], nsmall = 2)
tlvtwo_neg_rwa_brms <- format(tlvtwo_neg_brms[tlvtwo_neg_brms$term == "b_RWAscore",], nsmall = 2)
tlvtwo_neg_rwa_equi <- format(tlvtwo_neg_equi[tlvtwo_neg_equi$term == "b_RWAscore",], nsmall = 2)

tlvtwo_neg_rwa <- join (tlvtwo_neg_rwa_brms, tlvtwo_neg_rwa_lme4, by = "term")
tlvtwo_neg_rwa <- join (tlvtwo_neg_rwa, tlvtwo_neg_rwa_equi, by ="term")

tlvtwo_neg_rwa$"b [95% HDI]" <- paste ( format( tlvtwo_neg_rwa$estimate, nsmall = 2), " [",  format( tlvtwo_neg_rwa$hdi.low, nsmall = 2), 
                                       ", ", format(tlvtwo_neg_rwa$hdi.high, nsmall = 2), "]", sep = "")

tlvtwo_neg_rwa$"b [95% CI]" <- paste ( format( tlvtwo_neg_rwa$Estimate, nsmall = 2), " [",  format( tlvtwo_neg_rwa$"2.5 %", nsmall = 2), 
                                      ", ", format(tlvtwo_neg_rwa$"97.5 %", nsmall = 2), "]", sep = "")

# simple slope RWA level 2 conditioning positive valence condition --------

tlvtwo_pos_rwa_lme4 <- format(tlvtwo_pos_lme4[tlvtwo_pos_lme4$term == "b_RWAscore",], nsmall = 2)
tlvtwo_pos_rwa_brms <- format(tlvtwo_pos_brms[tlvtwo_pos_brms$term == "b_RWAscore",], nsmall = 2)
tlvtwo_pos_rwa_equi <- format(tlvtwo_pos_equi[tlvtwo_pos_equi$term == "b_RWAscore",], nsmall = 2)

tlvtwo_pos_rwa <- join (tlvtwo_pos_rwa_brms, tlvtwo_pos_rwa_lme4, by = "term")
tlvtwo_pos_rwa <- join (tlvtwo_pos_rwa, tlvtwo_pos_rwa_equi, by ="term")

tlvtwo_pos_rwa$"b [95% HDI]" <- paste ( format( tlvtwo_pos_rwa$estimate, nsmall = 2), " [",  format( tlvtwo_pos_rwa$hdi.low, nsmall = 2), 
                                       ", ", format(tlvtwo_pos_rwa$hdi.high, nsmall = 2), "]", sep = "")

tlvtwo_pos_rwa$"b [95% CI]" <- paste ( format( tlvtwo_pos_rwa$Estimate, nsmall = 2), " [",  format( tlvtwo_pos_rwa$"2.5 %", nsmall = 2), 
                                      ", ", format(tlvtwo_pos_rwa$"97.5 %", nsmall = 2), "]", sep = "")


# join -------------------------------------------------------------------

col2k <- c("term", "b [95% HDI]", "std.error",
           "b [95% CI]", "Std. Error", "t value",
           "decision", "ROPE", "inside.rope")

tgen <- format(tgen[,col2k], nsmall = 2)
tlvoneA <- format(tlvoneA[,col2k], nsmall = 2)
tlvtwoA <- format(tlvtwoA[,col2k], nsmall = 2)
tlvoneB <- format(tlvoneB[,col2k], nsmall = 2)
tlvtwoB <- format(tlvtwoB[,col2k], nsmall = 2)
tlvoneI <- format(tlvoneI[,col2k], nsmall = 2)
tlvtwoI <- format(tlvtwoI[,col2k], nsmall = 2)
tlvone_neg_rwa <- format(tlvone_neg_rwa[,col2k], nsmall = 2)
tlvone_pos_rwa <- format(tlvone_pos_rwa[,col2k], nsmall = 2)
tlvtwo_neg_rwa <- format(tlvtwo_neg_rwa[,col2k], nsmall = 2)
tlvtwo_pos_rwa <- format(tlvtwo_pos_rwa[,col2k], nsmall = 2)

t_all <- rbind(tgen,
               tlvoneA,
               tlvtwoA,
               tlvoneB,
               tlvtwoB,
               tlvoneI,
               tlvtwoI,
               tlvone_neg_rwa,
               tlvone_pos_rwa,
               tlvtwo_neg_rwa,
               tlvtwo_pos_rwa)

t_all$term <- c("Val",
                "Niveau",
                "RWA",
                "Val × Niveau",
                "Val × RWA",
                "Niveau × RWA",
                "Val × Niveau × RWA",
                "Val <sub>niv 1</sub>",
                "Val <sub>niv 2</sub>",
                "RWA <sub>niv 1</sub>",
                "RWA <sub>niv 2</sub>",
                "(Val × RWA) <sub>niv 1</sub>",
                "(Val × RWA) <sub>niv 2</sub>",
                "RWA <sub>niv 1 neg</sub>",
                "RWA <sub>niv 1 pos</sub>",
                "RWA <sub>niv 2 neg</sub>",
                "RWA <sub>niv 2 pos</sub>")

t_all$decision <- ifelse(t_all$decision == "reject", "oui",
                         ifelse(t_all$decision == "accept", "non",
                         "indécision"))

t_all <- t_all %>%
  mutate(inside.rope = color_tile("lightgreen", "orange")(inside.rope))

t_all$"n° &#946;" <- 1:length( t_all$term)

ordc <- c("n° &#946;", "term", "b [95% HDI]", "std.error",
           "b [95% CI]", "Std. Error", "t value",
           "decision", "ROPE", "inside.rope")

t_all <- t_all[,ordc]

colnames(t_all) <- c("n° &#946;", "Paramètre", "&#946;<sub>Bayes</sub> [95% HDI]",
                     "SE<sub>Bayes</sub>", "&#946;<sub>freq</sub> [95% CI]",
                     "SE<sub>freq</sub>", "t", 
                     "&#946 &#8800; 0", "ROPE", "% &#946;<sub>Bayes</sub> &sube; ROPE")

rownames(t_all) <- NULL


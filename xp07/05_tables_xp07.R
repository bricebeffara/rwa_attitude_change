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

tgen_lme4 <- as.data.frame(model_gen_xp07_lme4[c(2:8),])
tgen_brms <- as.data.frame(model_gen_xp07[c(9:15),])
tgen_equi <- as.data.frame(equi_gen_xp07)

tctone_lme4 <- as.data.frame(model_ctone_xp07_lme4[c(2:8),])
tctone_brms <- as.data.frame(model_ctone_xp07[c(9:15),])
tctone_equi <- as.data.frame(equi_ctone_xp07)

tcttwo_lme4 <- as.data.frame(model_cttwo_xp07_lme4[c(2:8),])
tcttwo_brms <- as.data.frame(model_cttwo_xp07[c(9:15),])
tcttwo_equi <- as.data.frame(equi_cttwo_xp07)

tctone_neg_lme4 <- as.data.frame(model_ctone_neg_xp07_lme4[c(2:8),])
tctone_neg_brms <- as.data.frame(model_ctone_neg_xp07[c(9:15),])
tctone_neg_equi <- as.data.frame(equi_ctone_neg_xp07)

tctone_pos_lme4 <- as.data.frame(model_ctone_pos_xp07_lme4[c(2:8),])
tctone_pos_brms <- as.data.frame(model_ctone_pos_xp07[c(9:15),])
tctone_pos_equi <- as.data.frame(equi_ctone_pos_xp07)

tcttwo_neg_lme4 <- as.data.frame(model_cttwo_neg_xp07_lme4[c(2:8),])
tcttwo_neg_brms <- as.data.frame(model_cttwo_neg_xp07[c(9:15),])
tcttwo_neg_equi <- as.data.frame(equi_cttwo_neg_xp07)

tcttwo_pos_lme4 <- as.data.frame(model_cttwo_pos_xp07_lme4[c(2:8),])
tcttwo_pos_brms <- as.data.frame(model_cttwo_pos_xp07[c(9:15),])
tcttwo_pos_equi <- as.data.frame(equi_cttwo_pos_xp07)


tgen_lme4$term <- tgen_brms$term
tctone_lme4$term <- tctone_brms$term
tcttwo_lme4$term <- tcttwo_brms$term
tctone_neg_lme4$term <- tctone_neg_brms$term
tctone_pos_lme4$term <- tctone_pos_brms$term
tcttwo_neg_lme4$term <- tcttwo_neg_brms$term
tcttwo_pos_lme4$term <- tcttwo_pos_brms$term

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

# simple slope valence in the 1 counter-conditioning condition ----------

tctoneA_lme4 <- format(tctone_lme4[tctone_lme4$term == "b_usvalence",], nsmall = 2)
tctoneA_brms <- format(tctone_brms[tctone_brms$term == "b_usvalence",], nsmall = 2)
tctoneA_equi <- format(tctone_equi[tctone_equi$term == "b_usvalence",], nsmall = 2)

tctoneA <- join (tctoneA_brms, tctoneA_lme4, by = "term")
tctoneA <- join (tctoneA, tctoneA_equi, by ="term")

tctoneA$"b [95% HDI]" <- paste ( format( tctoneA$estimate, nsmall = 2), " [",  format( tctoneA$hdi.low, nsmall = 2), 
                              ", ", format(tctoneA$hdi.high, nsmall = 2), "]", sep = "")

tctoneA$"b [95% CI]" <- paste ( format( tctoneA$Estimate, nsmall = 2), " [",  format( tctoneA$"2.5 %", nsmall = 2), 
                             ", ", format(tctoneA$"97.5 %", nsmall = 2), "]", sep = "")

# simple slope valence in the 2 counter-conditionings condition ----------

tcttwoA_lme4 <- format(tcttwo_lme4[tcttwo_lme4$term == "b_usvalence",], nsmall = 2)
tcttwoA_brms <- format(tcttwo_brms[tcttwo_brms$term == "b_usvalence",], nsmall = 2)
tcttwoA_equi <- format(tcttwo_equi[tcttwo_equi$term == "b_usvalence",], nsmall = 2)

tcttwoA <- join (tcttwoA_brms, tcttwoA_lme4, by = "term")
tcttwoA <- join (tcttwoA, tcttwoA_equi, by ="term")

tcttwoA$"b [95% HDI]" <- paste ( format( tcttwoA$estimate, nsmall = 2), " [",  format( tcttwoA$hdi.low, nsmall = 2), 
                              ", ", format(tcttwoA$hdi.high, nsmall = 2), "]", sep = "")

tcttwoA$"b [95% CI]" <- paste ( format( tcttwoA$Estimate, nsmall = 2), " [",  format( tcttwoA$"2.5 %", nsmall = 2), 
                             ", ", format(tcttwoA$"97.5 %", nsmall = 2), "]", sep = "")

# simple slope RWA * usvalence in the 1 counter-conditioning condition ----------

tctoneI_lme4 <- format(tctone_lme4[tctone_lme4$term == "b_usvalence.RWAscore",], nsmall = 2)
tctoneI_brms <- format(tctone_brms[tctone_brms$term == "b_usvalence.RWAscore",], nsmall = 2)
tctoneI_equi <- format(tctone_equi[tctone_equi$term == "b_usvalence.RWAscore",], nsmall = 2)

tctoneI <- join (tctoneI_brms, tctoneI_lme4, by = "term")
tctoneI <- join (tctoneI, tctoneI_equi, by ="term")

tctoneI$"b [95% HDI]" <- paste ( format( tctoneI$estimate, nsmall = 2), " [",  format( tctoneI$hdi.low, nsmall = 2), 
                                ", ", format(tctoneI$hdi.high, nsmall = 2), "]", sep = "")

tctoneI$"b [95% CI]" <- paste ( format( tctoneI$Estimate, nsmall = 2), " [",  format( tctoneI$"2.5 %", nsmall = 2), 
                               ", ", format(tctoneI$"97.5 %", nsmall = 2), "]", sep = "")


# simple slope RWA * usvalence in the 2 counter-conditionings condition ----------

tcttwoI_lme4 <- format(tcttwo_lme4[tcttwo_lme4$term == "b_usvalence.RWAscore",], nsmall = 2)
tcttwoI_brms <- format(tcttwo_brms[tcttwo_brms$term == "b_usvalence.RWAscore",], nsmall = 2)
tcttwoI_equi <- format(tcttwo_equi[tcttwo_equi$term == "b_usvalence.RWAscore",], nsmall = 2)

tcttwoI <- join (tcttwoI_brms, tcttwoI_lme4, by = "term")
tcttwoI <- join (tcttwoI, tcttwoI_equi, by ="term")

tcttwoI$"b [95% HDI]" <- paste ( format( tcttwoI$estimate, nsmall = 2), " [",  format( tcttwoI$hdi.low, nsmall = 2), 
                                ", ", format(tcttwoI$hdi.high, nsmall = 2), "]", sep = "")

tcttwoI$"b [95% CI]" <- paste ( format( tcttwoI$Estimate, nsmall = 2), " [",  format( tcttwoI$"2.5 %", nsmall = 2), 
                               ", ", format(tcttwoI$"97.5 %", nsmall = 2), "]", sep = "")


# simple slope RWA 1 counter-conditioning negative valence condition --------

tctone_neg_rwa_lme4 <- format(tctone_neg_lme4[tctone_neg_lme4$term == "b_RWAscore",], nsmall = 2)
tctone_neg_rwa_brms <- format(tctone_neg_brms[tctone_neg_brms$term == "b_RWAscore",], nsmall = 2)
tctone_neg_rwa_equi <- format(tctone_neg_equi[tctone_neg_equi$term == "b_RWAscore",], nsmall = 2)

tctone_neg_rwa <- join (tctone_neg_rwa_brms, tctone_neg_rwa_lme4, by = "term")
tctone_neg_rwa <- join (tctone_neg_rwa, tctone_neg_rwa_equi, by ="term")

tctone_neg_rwa$"b [95% HDI]" <- paste ( format( tctone_neg_rwa$estimate, nsmall = 2), " [",  format( tctone_neg_rwa$hdi.low, nsmall = 2), 
                                       ", ", format(tctone_neg_rwa$hdi.high, nsmall = 2), "]", sep = "")

tctone_neg_rwa$"b [95% CI]" <- paste ( format( tctone_neg_rwa$Estimate, nsmall = 2), " [",  format( tctone_neg_rwa$"2.5 %", nsmall = 2), 
                                      ", ", format(tctone_neg_rwa$"97.5 %", nsmall = 2), "]", sep = "")

# simple slope RWA 1 counter-conditioning positive valence condition --------

tctone_pos_rwa_lme4 <- format(tctone_pos_lme4[tctone_pos_lme4$term == "b_RWAscore",], nsmall = 2)
tctone_pos_rwa_brms <- format(tctone_pos_brms[tctone_pos_brms$term == "b_RWAscore",], nsmall = 2)
tctone_pos_rwa_equi <- format(tctone_pos_equi[tctone_pos_equi$term == "b_RWAscore",], nsmall = 2)

tctone_pos_rwa <- join (tctone_pos_rwa_brms, tctone_pos_rwa_lme4, by = "term")
tctone_pos_rwa <- join (tctone_pos_rwa, tctone_pos_rwa_equi, by ="term")

tctone_pos_rwa$"b [95% HDI]" <- paste ( format( tctone_pos_rwa$estimate, nsmall = 2), " [",  format( tctone_pos_rwa$hdi.low, nsmall = 2), 
                                       ", ", format(tctone_pos_rwa$hdi.high, nsmall = 2), "]", sep = "")

tctone_pos_rwa$"b [95% CI]" <- paste ( format( tctone_pos_rwa$Estimate, nsmall = 2), " [",  format( tctone_pos_rwa$"2.5 %", nsmall = 2), 
                                      ", ", format(tctone_pos_rwa$"97.5 %", nsmall = 2), "]", sep = "")

# simple slope RWA 2 counter-conditionings negative valence condition --------

tcttwo_neg_rwa_lme4 <- format(tcttwo_neg_lme4[tcttwo_neg_lme4$term == "b_RWAscore",], nsmall = 2)
tcttwo_neg_rwa_brms <- format(tcttwo_neg_brms[tcttwo_neg_brms$term == "b_RWAscore",], nsmall = 2)
tcttwo_neg_rwa_equi <- format(tcttwo_neg_equi[tcttwo_neg_equi$term == "b_RWAscore",], nsmall = 2)

tcttwo_neg_rwa <- join (tcttwo_neg_rwa_brms, tcttwo_neg_rwa_lme4, by = "term")
tcttwo_neg_rwa <- join (tcttwo_neg_rwa, tcttwo_neg_rwa_equi, by ="term")

tcttwo_neg_rwa$"b [95% HDI]" <- paste ( format( tcttwo_neg_rwa$estimate, nsmall = 2), " [",  format( tcttwo_neg_rwa$hdi.low, nsmall = 2), 
                                       ", ", format(tcttwo_neg_rwa$hdi.high, nsmall = 2), "]", sep = "")

tcttwo_neg_rwa$"b [95% CI]" <- paste ( format( tcttwo_neg_rwa$Estimate, nsmall = 2), " [",  format( tcttwo_neg_rwa$"2.5 %", nsmall = 2), 
                                      ", ", format(tcttwo_neg_rwa$"97.5 %", nsmall = 2), "]", sep = "")

# simple slope RWA 2 counter-conditionings positive valence condition --------

tcttwo_pos_rwa_lme4 <- format(tcttwo_pos_lme4[tcttwo_pos_lme4$term == "b_RWAscore",], nsmall = 2)
tcttwo_pos_rwa_brms <- format(tcttwo_pos_brms[tcttwo_pos_brms$term == "b_RWAscore",], nsmall = 2)
tcttwo_pos_rwa_equi <- format(tcttwo_pos_equi[tcttwo_pos_equi$term == "b_RWAscore",], nsmall = 2)

tcttwo_pos_rwa <- join (tcttwo_pos_rwa_brms, tcttwo_pos_rwa_lme4, by = "term")
tcttwo_pos_rwa <- join (tcttwo_pos_rwa, tcttwo_pos_rwa_equi, by ="term")

tcttwo_pos_rwa$"b [95% HDI]" <- paste ( format( tcttwo_pos_rwa$estimate, nsmall = 2), " [",  format( tcttwo_pos_rwa$hdi.low, nsmall = 2), 
                                       ", ", format(tcttwo_pos_rwa$hdi.high, nsmall = 2), "]", sep = "")

tcttwo_pos_rwa$"b [95% CI]" <- paste ( format( tcttwo_pos_rwa$Estimate, nsmall = 2), " [",  format( tcttwo_pos_rwa$"2.5 %", nsmall = 2), 
                                      ", ", format(tcttwo_pos_rwa$"97.5 %", nsmall = 2), "]", sep = "")


# join -------------------------------------------------------------------

col2k <- c("term", "b [95% HDI]", "std.error",
           "b [95% CI]", "Std. Error", "t value",
           "decision", "ROPE", "inside.rope")

tgen <- format(tgen[,col2k], nsmall = 2)
tctoneA <- format(tctoneA[,col2k], nsmall = 2)
tcttwoA <- format(tcttwoA[,col2k], nsmall = 2)
tctoneI <- format(tctoneI[,col2k], nsmall = 2)
tcttwoI <- format(tcttwoI[,col2k], nsmall = 2)
tctone_neg_rwa <- format(tctone_neg_rwa[,col2k], nsmall = 2)
tctone_pos_rwa <- format(tctone_pos_rwa[,col2k], nsmall = 2)
tcttwo_neg_rwa <- format(tcttwo_neg_rwa[,col2k], nsmall = 2)
tcttwo_pos_rwa <- format(tcttwo_pos_rwa[,col2k], nsmall = 2)

t_all <- rbind(tgen,
               tctoneA,
               tcttwoA,
               tctoneI,
               tcttwoI,
               tctone_neg_rwa,
               tctone_pos_rwa,
               tcttwo_neg_rwa,
               tcttwo_pos_rwa)

t_all$term <- c("Val",
                "Ncc",
                "RWA",
                "Val × Ncc",
                "Ncc × RWA",
                "Lev × RWA",
                "Val × Ncc × RWA",
                "Val <sub>1 cc</sub>",
                "Val <sub>2 cc</sub>",
                "(Val × RWA) <sub>1 cc</sub>",
                "(Val × RWA) <sub>2 cc</sub>",
                "RWA <sub>1 cc neg</sub>",
                "RWA <sub>1 cc pos</sub>",
                "RWA <sub>2 cc neg</sub>",
                "RWA <sub>2 cc pos</sub>")

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
                     "&#946 &#8800; 0", "ROPE", "% &#946;<sub>Bayes</sub> dans ROPE")

rownames(t_all) <- NULL


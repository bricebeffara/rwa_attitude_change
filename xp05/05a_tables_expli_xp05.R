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

tgen_lme4 <- as.data.frame(expli_gen_xp05_lme4[c(2:8),])
tgen_brms <- as.data.frame(expli_gen_xp05[c(9:15),])
tgen_equi <- as.data.frame(equi_gen_expli__xp05)

tord1_lme4 <- as.data.frame(model_ord1_xp05_lme4[c(2:8),])
tord1_brms <- as.data.frame(model_ord1_xp05[c(9:15),])
tord1_equi <- as.data.frame(equi_ord1_xp05)

tord2_lme4 <- as.data.frame(model_ord2_xp05_lme4[c(2:8),])
tord2_brms <- as.data.frame(model_ord2_xp05[c(9:15),])
tord2_equi <- as.data.frame(equi_ord2_xp05)

tord1_lme4 <- as.data.frame(model_ord1_xp05_lme4[c(2:8),])
tord1_brms <- as.data.frame(model_ord1_xp05[c(9:15),])
tord1_equi <- as.data.frame(equi_ord1_xp05)

tord2_lme4 <- as.data.frame(model_ord2_xp05_lme4[c(2:8),])
tord2_brms <- as.data.frame(model_ord2_xp05[c(9:15),])
tord2_equi <- as.data.frame(equi_ord2_xp05)

tneg_lme4 <- as.data.frame(model_neg_xp05_lme4[c(2:4),])
tneg_brms <- as.data.frame(model_neg_xp05[c(9:11),])
tneg_equi <- as.data.frame(equi_neg_xp05)

tpos_lme4 <- as.data.frame(model_pos_xp05_lme4[c(2:4),])
tpos_brms <- as.data.frame(model_pos_xp05[c(9:11),])
tpos_equi <- as.data.frame(equi_pos_xp05)

tord1_neg_lme4 <- as.data.frame(model_ord1_neg_xp05_lme4[c(2:8),])
tord1_neg_brms <- as.data.frame(model_ord1_neg_xp05[c(9:15),])
tord1_neg_equi <- as.data.frame(equi_ord1_neg_xp05)

tord1_pos_lme4 <- as.data.frame(model_ord1_pos_xp05_lme4[c(2:8),])
tord1_pos_brms <- as.data.frame(model_ord1_pos_xp05[c(9:15),])
tord1_pos_equi <- as.data.frame(equi_ord1_pos_xp05)

tord2_neg_lme4 <- as.data.frame(model_ord2_neg_xp05_lme4[c(2:8),])
tord2_neg_brms <- as.data.frame(model_ord2_neg_xp05[c(9:15),])
tord2_neg_equi <- as.data.frame(equi_ord2_neg_xp05)

tord2_pos_lme4 <- as.data.frame(model_ord2_pos_xp05_lme4[c(2:8),])
tord2_pos_brms <- as.data.frame(model_ord2_pos_xp05[c(9:15),])
tord2_pos_equi <- as.data.frame(equi_ord2_pos_xp05)


tgen_lme4$term <- tgen_brms$term
tord1_lme4$term <- tord1_brms$term
tord2_lme4$term <- tord2_brms$term
tneg_lme4$term <- tneg_brms$term
tpos_lme4$term <- tpos_brms$term
tord1_neg_lme4$term <- tord1_neg_brms$term
tord1_pos_lme4$term <- tord1_pos_brms$term
tord2_neg_lme4$term <- tord2_neg_brms$term
tord2_pos_lme4$term <- tord2_pos_brms$term

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

# simple slope valence in the order1 condition ----------

tord1A_lme4 <- format(tord1_lme4[tord1_lme4$term == "b_usvalence",], nsmall = 2)
tord1A_brms <- format(tord1_brms[tord1_brms$term == "b_usvalence",], nsmall = 2)
tord1A_equi <- format(tord1_equi[tord1_equi$term == "b_usvalence",], nsmall = 2)

tord1A <- join (tord1A_brms, tord1A_lme4, by = "term")
tord1A <- join (tord1A, tord1A_equi, by ="term")

tord1A$"b [95% HDI]" <- paste ( format( tord1A$estimate, nsmall = 2), " [",  format( tord1A$hdi.low, nsmall = 2), 
                              ", ", format(tord1A$hdi.high, nsmall = 2), "]", sep = "")

tord1A$"b [95% CI]" <- paste ( format( tord1A$Estimate, nsmall = 2), " [",  format( tord1A$"2.5 %", nsmall = 2), 
                             ", ", format(tord1A$"97.5 %", nsmall = 2), "]", sep = "")

# simple slope valence in the order 2 condition ----------

tord2A_lme4 <- format(tord2_lme4[tord2_lme4$term == "b_usvalence",], nsmall = 2)
tord2A_brms <- format(tord2_brms[tord2_brms$term == "b_usvalence",], nsmall = 2)
tord2A_equi <- format(tord2_equi[tord2_equi$term == "b_usvalence",], nsmall = 2)

tord2A <- join (tord2A_brms, tord2A_lme4, by = "term")
tord2A <- join (tord2A, tord2A_equi, by ="term")

tord2A$"b [95% HDI]" <- paste ( format( tord2A$estimate, nsmall = 2), " [",  format( tord2A$hdi.low, nsmall = 2), 
                              ", ", format(tord2A$hdi.high, nsmall = 2), "]", sep = "")

tord2A$"b [95% CI]" <- paste ( format( tord2A$Estimate, nsmall = 2), " [",  format( tord2A$"2.5 %", nsmall = 2), 
                             ", ", format(tord2A$"97.5 %", nsmall = 2), "]", sep = "")

# simple slope RWA in the negative condition ----------

tnegA_lme4 <- format(tneg_lme4[tneg_lme4$term == "b_RWAscore",], nsmall = 2)
tnegA_brms <- format(tneg_brms[tneg_brms$term == "b_RWAscore",], nsmall = 2)
tnegA_equi <- format(tneg_equi[tneg_equi$term == "b_RWAscore",], nsmall = 2)

tnegA <- join (tnegA_brms, tnegA_lme4, by = "term")
tnegA <- join (tnegA, tnegA_equi, by ="term")

tnegA$"b [95% HDI]" <- paste ( format( tnegA$estimate, nsmall = 2), " [",  format( tnegA$hdi.low, nsmall = 2), 
                                ", ", format(tnegA$hdi.high, nsmall = 2), "]", sep = "")

tnegA$"b [95% CI]" <- paste ( format( tnegA$Estimate, nsmall = 2), " [",  format( tnegA$"2.5 %", nsmall = 2), 
                               ", ", format(tnegA$"97.5 %", nsmall = 2), "]", sep = "")

# simple slope RWAin the positive condition ----------

tposA_lme4 <- format(tpos_lme4[tpos_lme4$term == "b_RWAscore",], nsmall = 2)
tposA_brms <- format(tpos_brms[tpos_brms$term == "b_RWAscore",], nsmall = 2)
tposA_equi <- format(tpos_equi[tpos_equi$term == "b_RWAscore",], nsmall = 2)

tposA <- join (tposA_brms, tposA_lme4, by = "term")
tposA <- join (tposA, tposA_equi, by ="term")

tposA$"b [95% HDI]" <- paste ( format( tposA$estimate, nsmall = 2), " [",  format( tposA$hdi.low, nsmall = 2), 
                                ", ", format(tposA$hdi.high, nsmall = 2), "]", sep = "")

tposA$"b [95% CI]" <- paste ( format( tposA$Estimate, nsmall = 2), " [",  format( tposA$"2.5 %", nsmall = 2), 
                               ", ", format(tposA$"97.5 %", nsmall = 2), "]", sep = "")

# simple slope RWA * usvalence in the order1 condition ----------

tord1I_lme4 <- format(tord1_lme4[tord1_lme4$term == "b_usvalence.RWAscore",], nsmall = 2)
tord1I_brms <- format(tord1_brms[tord1_brms$term == "b_usvalence.RWAscore",], nsmall = 2)
tord1I_equi <- format(tord1_equi[tord1_equi$term == "b_usvalence.RWAscore",], nsmall = 2)

tord1I <- join (tord1I_brms, tord1I_lme4, by = "term")
tord1I <- join (tord1I, tord1I_equi, by ="term")

tord1I$"b [95% HDI]" <- paste ( format( tord1I$estimate, nsmall = 2), " [",  format( tord1I$hdi.low, nsmall = 2), 
                                ", ", format(tord1I$hdi.high, nsmall = 2), "]", sep = "")

tord1I$"b [95% CI]" <- paste ( format( tord1I$Estimate, nsmall = 2), " [",  format( tord1I$"2.5 %", nsmall = 2), 
                               ", ", format(tord1I$"97.5 %", nsmall = 2), "]", sep = "")


# simple slope RWA * usvalence in the order2 condition ----------

tord2I_lme4 <- format(tord2_lme4[tord2_lme4$term == "b_usvalence.RWAscore",], nsmall = 2)
tord2I_brms <- format(tord2_brms[tord2_brms$term == "b_usvalence.RWAscore",], nsmall = 2)
tord2I_equi <- format(tord2_equi[tord2_equi$term == "b_usvalence.RWAscore",], nsmall = 2)

tord2I <- join (tord2I_brms, tord2I_lme4, by = "term")
tord2I <- join (tord2I, tord2I_equi, by ="term")

tord2I$"b [95% HDI]" <- paste ( format( tord2I$estimate, nsmall = 2), " [",  format( tord2I$hdi.low, nsmall = 2), 
                                ", ", format(tord2I$hdi.high, nsmall = 2), "]", sep = "")

tord2I$"b [95% CI]" <- paste ( format( tord2I$Estimate, nsmall = 2), " [",  format( tord2I$"2.5 %", nsmall = 2), 
                               ", ", format(tord2I$"97.5 %", nsmall = 2), "]", sep = "")


# simple slope RWA order1 negative valence condition --------

tord1_neg_rwa_lme4 <- format(tord1_neg_lme4[tord1_neg_lme4$term == "b_RWAscore",], nsmall = 2)
tord1_neg_rwa_brms <- format(tord1_neg_brms[tord1_neg_brms$term == "b_RWAscore",], nsmall = 2)
tord1_neg_rwa_equi <- format(tord1_neg_equi[tord1_neg_equi$term == "b_RWAscore",], nsmall = 2)

tord1_neg_rwa <- join (tord1_neg_rwa_brms, tord1_neg_rwa_lme4, by = "term")
tord1_neg_rwa <- join (tord1_neg_rwa, tord1_neg_rwa_equi, by ="term")

tord1_neg_rwa$"b [95% HDI]" <- paste ( format( tord1_neg_rwa$estimate, nsmall = 2), " [",  format( tord1_neg_rwa$hdi.low, nsmall = 2), 
                                       ", ", format(tord1_neg_rwa$hdi.high, nsmall = 2), "]", sep = "")

tord1_neg_rwa$"b [95% CI]" <- paste ( format( tord1_neg_rwa$Estimate, nsmall = 2), " [",  format( tord1_neg_rwa$"2.5 %", nsmall = 2), 
                                      ", ", format(tord1_neg_rwa$"97.5 %", nsmall = 2), "]", sep = "")

# simple slope RWA order1 positive valence condition --------

tord1_pos_rwa_lme4 <- format(tord1_pos_lme4[tord1_pos_lme4$term == "b_RWAscore",], nsmall = 2)
tord1_pos_rwa_brms <- format(tord1_pos_brms[tord1_pos_brms$term == "b_RWAscore",], nsmall = 2)
tord1_pos_rwa_equi <- format(tord1_pos_equi[tord1_pos_equi$term == "b_RWAscore",], nsmall = 2)

tord1_pos_rwa <- join (tord1_pos_rwa_brms, tord1_pos_rwa_lme4, by = "term")
tord1_pos_rwa <- join (tord1_pos_rwa, tord1_pos_rwa_equi, by ="term")

tord1_pos_rwa$"b [95% HDI]" <- paste ( format( tord1_pos_rwa$estimate, nsmall = 2), " [",  format( tord1_pos_rwa$hdi.low, nsmall = 2), 
                                       ", ", format(tord1_pos_rwa$hdi.high, nsmall = 2), "]", sep = "")

tord1_pos_rwa$"b [95% CI]" <- paste ( format( tord1_pos_rwa$Estimate, nsmall = 2), " [",  format( tord1_pos_rwa$"2.5 %", nsmall = 2), 
                                      ", ", format(tord1_pos_rwa$"97.5 %", nsmall = 2), "]", sep = "")

# simple slope RWA order2 negative valence condition --------

tord2_neg_rwa_lme4 <- format(tord2_neg_lme4[tord2_neg_lme4$term == "b_RWAscore",], nsmall = 2)
tord2_neg_rwa_brms <- format(tord2_neg_brms[tord2_neg_brms$term == "b_RWAscore",], nsmall = 2)
tord2_neg_rwa_equi <- format(tord2_neg_equi[tord2_neg_equi$term == "b_RWAscore",], nsmall = 2)

tord2_neg_rwa <- join (tord2_neg_rwa_brms, tord2_neg_rwa_lme4, by = "term")
tord2_neg_rwa <- join (tord2_neg_rwa, tord2_neg_rwa_equi, by ="term")

tord2_neg_rwa$"b [95% HDI]" <- paste ( format( tord2_neg_rwa$estimate, nsmall = 2), " [",  format( tord2_neg_rwa$hdi.low, nsmall = 2), 
                                       ", ", format(tord2_neg_rwa$hdi.high, nsmall = 2), "]", sep = "")

tord2_neg_rwa$"b [95% CI]" <- paste ( format( tord2_neg_rwa$Estimate, nsmall = 2), " [",  format( tord2_neg_rwa$"2.5 %", nsmall = 2), 
                                      ", ", format(tord2_neg_rwa$"97.5 %", nsmall = 2), "]", sep = "")

# simple slope RWA order2 positive valence condition --------

tord2_pos_rwa_lme4 <- format(tord2_pos_lme4[tord2_pos_lme4$term == "b_RWAscore",], nsmall = 2)
tord2_pos_rwa_brms <- format(tord2_pos_brms[tord2_pos_brms$term == "b_RWAscore",], nsmall = 2)
tord2_pos_rwa_equi <- format(tord2_pos_equi[tord2_pos_equi$term == "b_RWAscore",], nsmall = 2)

tord2_pos_rwa <- join (tord2_pos_rwa_brms, tord2_pos_rwa_lme4, by = "term")
tord2_pos_rwa <- join (tord2_pos_rwa, tord2_pos_rwa_equi, by ="term")

tord2_pos_rwa$"b [95% HDI]" <- paste ( format( tord2_pos_rwa$estimate, nsmall = 2), " [",  format( tord2_pos_rwa$hdi.low, nsmall = 2), 
                                       ", ", format(tord2_pos_rwa$hdi.high, nsmall = 2), "]", sep = "")

tord2_pos_rwa$"b [95% CI]" <- paste ( format( tord2_pos_rwa$Estimate, nsmall = 2), " [",  format( tord2_pos_rwa$"2.5 %", nsmall = 2), 
                                      ", ", format(tord2_pos_rwa$"97.5 %", nsmall = 2), "]", sep = "")


# join -------------------------------------------------------------------

col2k <- c("term", "b [95% HDI]", "std.error",
           "b [95% CI]", "Std. Error", "t value",
           "decision", "ROPE", "inside.rope")

tgen <- format(tgen[,col2k], nsmall = 2)
tord1A <- format(tord1A[,col2k], nsmall = 2)
tord2A <- format(tord2A[,col2k], nsmall = 2)
tnegA <- format(tnegA[,col2k], nsmall = 2)
tposA <- format(tposA[,col2k], nsmall = 2)
tord1I <- format(tord1I[,col2k], nsmall = 2)
tord2I <- format(tord2I[,col2k], nsmall = 2)
tord1_neg_rwa <- format(tord1_neg_rwa[,col2k], nsmall = 2)
tord1_pos_rwa <- format(tord1_pos_rwa[,col2k], nsmall = 2)
tord2_neg_rwa <- format(tord2_neg_rwa[,col2k], nsmall = 2)
tord2_pos_rwa <- format(tord2_pos_rwa[,col2k], nsmall = 2)

t_all <- rbind(tgen,
               tord1A,
               tord2A,
               tnegA,
               tposA,
               tord1I,
               tord2I,
               tord1_neg_rwa,
               tord1_pos_rwa,
               tord2_neg_rwa,
               tord2_pos_rwa)

t_all$term <- c("Val",
                "Ordre",
                "RWA",
                "Val × Ordre",
                "Val × RWA",
                "Ordre × RWA",
                "Val × Ordre × RWA",
                "Val <sub>Ordre 1</sub>",
                "Val <sub>Ordre 2</sub>",
                "RWA <sub>neg</sub>",
                "RWA <sub>pos</sub>",
                "(Val × RWA) <sub>Ordre 1</sub>",
                "(Val × RWA) <sub>Ordre 2</sub>",
                "RWA <sub>Ordre 1 neg</sub>",
                "RWA <sub>Ordre 1 pos</sub>",
                "RWA <sub>Ordre 2 neg</sub>",
                "RWA <sub>Ordre 2 pos</sub>")

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


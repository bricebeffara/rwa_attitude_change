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

tgen_lme4 <- as.data.frame(direct_gen_xp06_lme4[c(2:8),])
tgen_brms <- as.data.frame(direct_gen_xp06[c(9:15),])
tgen_equi <- as.data.frame(equi_gen_direct__xp06)

tord1_lme4 <- as.data.frame(model_ord1_xp06_lme4[c(2:8),])
tord1_brms <- as.data.frame(model_ord1_xp06[c(9:15),])
tord1_equi <- as.data.frame(equi_ord1_xp06)

tord2_lme4 <- as.data.frame(model_ord2_xp06_lme4[c(2:8),])
tord2_brms <- as.data.frame(model_ord2_xp06[c(9:15),])
tord2_equi <- as.data.frame(equi_ord2_xp06)

tneg_lme4 <- as.data.frame(model_neg_xp06_lme4[c(2:4),])
tneg_brms <- as.data.frame(model_neg_xp06[c(9:11),])
tneg_equi <- as.data.frame(equi_neg_xp06)

tpos_lme4 <- as.data.frame(model_pos_xp06_lme4[c(2:4),])
tpos_brms <- as.data.frame(model_pos_xp06[c(9:11),])
tpos_equi <- as.data.frame(equi_pos_xp06)

tord2_neg_lme4 <- as.data.frame(model_ord2_neg_xp06_lme4[c(2:8),])
tord2_neg_brms <- as.data.frame(model_ord2_neg_xp06[c(9:15),])
tord2_neg_equi <- as.data.frame(equi_ord2_neg_xp06)

tord2_pos_lme4 <- as.data.frame(model_ord2_pos_xp06_lme4[c(2:8),])
tord2_pos_brms <- as.data.frame(model_ord2_pos_xp06[c(9:15),])
tord2_pos_equi <- as.data.frame(equi_ord2_pos_xp06)


tgen_lme4$term <- tgen_brms$term
tord1_lme4$term <- tord1_brms$term
tord2_lme4$term <- tord2_brms$term
tneg_lme4$term <- tneg_brms$term
tpos_lme4$term <- tpos_brms$term
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

# simple slope RWA * usvalencedir in the order1 condition ----------

tord1I_lme4 <- format(tord1_lme4[tord1_lme4$term == "b_usvalencedir.RWAscore",], nsmall = 2)
tord1I_brms <- format(tord1_brms[tord1_brms$term == "b_usvalencedir.RWAscore",], nsmall = 2)
tord1I_equi <- format(tord1_equi[tord1_equi$term == "b_usvalencedir.RWAscore",], nsmall = 2)

tord1I <- join (tord1I_brms, tord1I_lme4, by = "term")
tord1I <- join (tord1I, tord1I_equi, by ="term")

tord1I$"b [95% HDI]" <- paste ( format( tord1I$estimate, nsmall = 2), " [",  format( tord1I$hdi.low, nsmall = 2), 
                                ", ", format(tord1I$hdi.high, nsmall = 2), "]", sep = "")

tord1I$"b [95% CI]" <- paste ( format( tord1I$Estimate, nsmall = 2), " [",  format( tord1I$"2.5 %", nsmall = 2), 
                               ", ", format(tord1I$"97.5 %", nsmall = 2), "]", sep = "")

# simple slope RWA * usvalencedir in the order2 condition ----------

tord2I_lme4 <- format(tord2_lme4[tord2_lme4$term == "b_usvalencedir.RWAscore",], nsmall = 2)
tord2I_brms <- format(tord2_brms[tord2_brms$term == "b_usvalencedir.RWAscore",], nsmall = 2)
tord2I_equi <- format(tord2_equi[tord2_equi$term == "b_usvalencedir.RWAscore",], nsmall = 2)

tord2I <- join (tord2I_brms, tord2I_lme4, by = "term")
tord2I <- join (tord2I, tord2I_equi, by ="term")

tord2I$"b [95% HDI]" <- paste ( format( tord2I$estimate, nsmall = 2), " [",  format( tord2I$hdi.low, nsmall = 2), 
                                ", ", format(tord2I$hdi.high, nsmall = 2), "]", sep = "")

tord2I$"b [95% CI]" <- paste ( format( tord2I$Estimate, nsmall = 2), " [",  format( tord2I$"2.5 %", nsmall = 2), 
                               ", ", format(tord2I$"97.5 %", nsmall = 2), "]", sep = "")

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
tnegA <- format(tnegA[,col2k], nsmall = 2)
tposA <- format(tposA[,col2k], nsmall = 2)
tord1I <- format(tord1I[,col2k], nsmall = 2)
tord2I <- format(tord2I[,col2k], nsmall = 2)
tord2_neg_rwa <- format(tord2_neg_rwa[,col2k], nsmall = 2)
tord2_pos_rwa <- format(tord2_pos_rwa[,col2k], nsmall = 2)

t_all <- rbind(tgen,
               tnegA,
               tposA,
               tord1I,
               tord2I,
               tord2_neg_rwa,
               tord2_pos_rwa)

t_all$term <- c("Val",
                "Ordre",
                "RWA",
                "Val × Ordre",
                "Val × RWA",
                "Ordre × RWA",
                "Val × Ordre × RWA",
                "RWA <sub>neg</sub>",
                "RWA <sub>pos</sub>",
                "(Val × RWA) <sub>Ordre 1</sub>",
                "(Val × RWA) <sub>Ordre 2</sub>",
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


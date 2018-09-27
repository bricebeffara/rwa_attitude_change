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

tgen_lme4 <- as.data.frame(model_gen_xp09_lme4[c(2:8),])
tgen_brms <- as.data.frame(model_gen_xp09[c(9:15),])
tgen_equi <- as.data.frame(equi_gen_xp09)

tnolo_lme4 <- as.data.frame(model_nolo_xp09_lme4[c(2:8),])
tnolo_brms <- as.data.frame(model_nolo_xp09[c(9:15),])
tnolo_equi <- as.data.frame(equi_nolo_xp09)

tyelo_lme4 <- as.data.frame(model_yelo_xp09_lme4[c(2:8),])
tyelo_brms <- as.data.frame(model_yelo_xp09[c(9:15),])
tyelo_equi <- as.data.frame(equi_yelo_xp09)

tnolo_neg_lme4 <- as.data.frame(model_nolo_neg_xp09_lme4[c(2:8),])
tnolo_neg_brms <- as.data.frame(model_nolo_neg_xp09[c(9:15),])
tnolo_neg_equi <- as.data.frame(equi_nolo_neg_xp09)

tnolo_pos_lme4 <- as.data.frame(model_nolo_pos_xp09_lme4[c(2:8),])
tnolo_pos_brms <- as.data.frame(model_nolo_pos_xp09[c(9:15),])
tnolo_pos_equi <- as.data.frame(equi_nolo_pos_xp09)

tyelo_neg_lme4 <- as.data.frame(model_yelo_neg_xp09_lme4[c(2:8),])
tyelo_neg_brms <- as.data.frame(model_yelo_neg_xp09[c(9:15),])
tyelo_neg_equi <- as.data.frame(equi_yelo_neg_xp09)

tyelo_pos_lme4 <- as.data.frame(model_yelo_pos_xp09_lme4[c(2:8),])
tyelo_pos_brms <- as.data.frame(model_yelo_pos_xp09[c(9:15),])
tyelo_pos_equi <- as.data.frame(equi_yelo_pos_xp09)


tgen_lme4$term <- tgen_brms$term
tnolo_lme4$term <- tnolo_brms$term
tyelo_lme4$term <- tyelo_brms$term
tnolo_neg_lme4$term <- tnolo_neg_brms$term
tnolo_pos_lme4$term <- tnolo_pos_brms$term
tyelo_neg_lme4$term <- tyelo_neg_brms$term
tyelo_pos_lme4$term <- tyelo_pos_brms$term

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

# simple slope valence in the no load conditioning condition ----------

tnoloA_lme4 <- format(tnolo_lme4[tnolo_lme4$term == "b_usvalence",], nsmall = 2)
tnoloA_brms <- format(tnolo_brms[tnolo_brms$term == "b_usvalence",], nsmall = 2)
tnoloA_equi <- format(tnolo_equi[tnolo_equi$term == "b_usvalence",], nsmall = 2)

tnoloA <- join (tnoloA_brms, tnoloA_lme4, by = "term")
tnoloA <- join (tnoloA, tnoloA_equi, by ="term")

tnoloA$"b [95% HDI]" <- paste ( format( tnoloA$estimate, nsmall = 2), " [",  format( tnoloA$hdi.low, nsmall = 2), 
                              ", ", format(tnoloA$hdi.high, nsmall = 2), "]", sep = "")

tnoloA$"b [95% CI]" <- paste ( format( tnoloA$Estimate, nsmall = 2), " [",  format( tnoloA$"2.5 %", nsmall = 2), 
                             ", ", format(tnoloA$"97.5 %", nsmall = 2), "]", sep = "")

# simple slope valence in the load conditioning condition ----------

tyeloA_lme4 <- format(tyelo_lme4[tyelo_lme4$term == "b_usvalence",], nsmall = 2)
tyeloA_brms <- format(tyelo_brms[tyelo_brms$term == "b_usvalence",], nsmall = 2)
tyeloA_equi <- format(tyelo_equi[tyelo_equi$term == "b_usvalence",], nsmall = 2)

tyeloA <- join (tyeloA_brms, tyeloA_lme4, by = "term")
tyeloA <- join (tyeloA, tyeloA_equi, by ="term")

tyeloA$"b [95% HDI]" <- paste ( format( tyeloA$estimate, nsmall = 2), " [",  format( tyeloA$hdi.low, nsmall = 2), 
                              ", ", format(tyeloA$hdi.high, nsmall = 2), "]", sep = "")

tyeloA$"b [95% CI]" <- paste ( format( tyeloA$Estimate, nsmall = 2), " [",  format( tyeloA$"2.5 %", nsmall = 2), 
                             ", ", format(tyeloA$"97.5 %", nsmall = 2), "]", sep = "")

# simple slope RWA * usvalence in the no load conditioning condition ----------

tnoloI_lme4 <- format(tnolo_lme4[tnolo_lme4$term == "b_usvalence.RWAscore",], nsmall = 2)
tnoloI_brms <- format(tnolo_brms[tnolo_brms$term == "b_usvalence.RWAscore",], nsmall = 2)
tnoloI_equi <- format(tnolo_equi[tnolo_equi$term == "b_usvalence.RWAscore",], nsmall = 2)

tnoloI <- join (tnoloI_brms, tnoloI_lme4, by = "term")
tnoloI <- join (tnoloI, tnoloI_equi, by ="term")

tnoloI$"b [95% HDI]" <- paste ( format( tnoloI$estimate, nsmall = 2), " [",  format( tnoloI$hdi.low, nsmall = 2), 
                                ", ", format(tnoloI$hdi.high, nsmall = 2), "]", sep = "")

tnoloI$"b [95% CI]" <- paste ( format( tnoloI$Estimate, nsmall = 2), " [",  format( tnoloI$"2.5 %", nsmall = 2), 
                               ", ", format(tnoloI$"97.5 %", nsmall = 2), "]", sep = "")


# simple slope RWA * usvalence in the load conditioning condition ----------

tyeloI_lme4 <- format(tyelo_lme4[tyelo_lme4$term == "b_usvalence.RWAscore",], nsmall = 2)
tyeloI_brms <- format(tyelo_brms[tyelo_brms$term == "b_usvalence.RWAscore",], nsmall = 2)
tyeloI_equi <- format(tyelo_equi[tyelo_equi$term == "b_usvalence.RWAscore",], nsmall = 2)

tyeloI <- join (tyeloI_brms, tyeloI_lme4, by = "term")
tyeloI <- join (tyeloI, tyeloI_equi, by ="term")

tyeloI$"b [95% HDI]" <- paste ( format( tyeloI$estimate, nsmall = 2), " [",  format( tyeloI$hdi.low, nsmall = 2), 
                                ", ", format(tyeloI$hdi.high, nsmall = 2), "]", sep = "")

tyeloI$"b [95% CI]" <- paste ( format( tyeloI$Estimate, nsmall = 2), " [",  format( tyeloI$"2.5 %", nsmall = 2), 
                               ", ", format(tyeloI$"97.5 %", nsmall = 2), "]", sep = "")


# simple slope RWA no load conditioning negative valence condition --------

tnolo_neg_rwa_lme4 <- format(tnolo_neg_lme4[tnolo_neg_lme4$term == "b_RWAscore",], nsmall = 2)
tnolo_neg_rwa_brms <- format(tnolo_neg_brms[tnolo_neg_brms$term == "b_RWAscore",], nsmall = 2)
tnolo_neg_rwa_equi <- format(tnolo_neg_equi[tnolo_neg_equi$term == "b_RWAscore",], nsmall = 2)

tnolo_neg_rwa <- join (tnolo_neg_rwa_brms, tnolo_neg_rwa_lme4, by = "term")
tnolo_neg_rwa <- join (tnolo_neg_rwa, tnolo_neg_rwa_equi, by ="term")

tnolo_neg_rwa$"b [95% HDI]" <- paste ( format( tnolo_neg_rwa$estimate, nsmall = 2), " [",  format( tnolo_neg_rwa$hdi.low, nsmall = 2), 
                                       ", ", format(tnolo_neg_rwa$hdi.high, nsmall = 2), "]", sep = "")

tnolo_neg_rwa$"b [95% CI]" <- paste ( format( tnolo_neg_rwa$Estimate, nsmall = 2), " [",  format( tnolo_neg_rwa$"2.5 %", nsmall = 2), 
                                      ", ", format(tnolo_neg_rwa$"97.5 %", nsmall = 2), "]", sep = "")

# simple slope RWA no load conditioning positive valence condition --------

tnolo_pos_rwa_lme4 <- format(tnolo_pos_lme4[tnolo_pos_lme4$term == "b_RWAscore",], nsmall = 2)
tnolo_pos_rwa_brms <- format(tnolo_pos_brms[tnolo_pos_brms$term == "b_RWAscore",], nsmall = 2)
tnolo_pos_rwa_equi <- format(tnolo_pos_equi[tnolo_pos_equi$term == "b_RWAscore",], nsmall = 2)

tnolo_pos_rwa <- join (tnolo_pos_rwa_brms, tnolo_pos_rwa_lme4, by = "term")
tnolo_pos_rwa <- join (tnolo_pos_rwa, tnolo_pos_rwa_equi, by ="term")

tnolo_pos_rwa$"b [95% HDI]" <- paste ( format( tnolo_pos_rwa$estimate, nsmall = 2), " [",  format( tnolo_pos_rwa$hdi.low, nsmall = 2), 
                                       ", ", format(tnolo_pos_rwa$hdi.high, nsmall = 2), "]", sep = "")

tnolo_pos_rwa$"b [95% CI]" <- paste ( format( tnolo_pos_rwa$Estimate, nsmall = 2), " [",  format( tnolo_pos_rwa$"2.5 %", nsmall = 2), 
                                      ", ", format(tnolo_pos_rwa$"97.5 %", nsmall = 2), "]", sep = "")

# simple slope RWA load conditioning negative valence condition --------

tyelo_neg_rwa_lme4 <- format(tyelo_neg_lme4[tyelo_neg_lme4$term == "b_RWAscore",], nsmall = 2)
tyelo_neg_rwa_brms <- format(tyelo_neg_brms[tyelo_neg_brms$term == "b_RWAscore",], nsmall = 2)
tyelo_neg_rwa_equi <- format(tyelo_neg_equi[tyelo_neg_equi$term == "b_RWAscore",], nsmall = 2)

tyelo_neg_rwa <- join (tyelo_neg_rwa_brms, tyelo_neg_rwa_lme4, by = "term")
tyelo_neg_rwa <- join (tyelo_neg_rwa, tyelo_neg_rwa_equi, by ="term")

tyelo_neg_rwa$"b [95% HDI]" <- paste ( format( tyelo_neg_rwa$estimate, nsmall = 2), " [",  format( tyelo_neg_rwa$hdi.low, nsmall = 2), 
                                       ", ", format(tyelo_neg_rwa$hdi.high, nsmall = 2), "]", sep = "")

tyelo_neg_rwa$"b [95% CI]" <- paste ( format( tyelo_neg_rwa$Estimate, nsmall = 2), " [",  format( tyelo_neg_rwa$"2.5 %", nsmall = 2), 
                                      ", ", format(tyelo_neg_rwa$"97.5 %", nsmall = 2), "]", sep = "")

# simple slope RWA load conditioning positive valence condition --------

tyelo_pos_rwa_lme4 <- format(tyelo_pos_lme4[tyelo_pos_lme4$term == "b_RWAscore",], nsmall = 2)
tyelo_pos_rwa_brms <- format(tyelo_pos_brms[tyelo_pos_brms$term == "b_RWAscore",], nsmall = 2)
tyelo_pos_rwa_equi <- format(tyelo_pos_equi[tyelo_pos_equi$term == "b_RWAscore",], nsmall = 2)

tyelo_pos_rwa <- join (tyelo_pos_rwa_brms, tyelo_pos_rwa_lme4, by = "term")
tyelo_pos_rwa <- join (tyelo_pos_rwa, tyelo_pos_rwa_equi, by ="term")

tyelo_pos_rwa$"b [95% HDI]" <- paste ( format( tyelo_pos_rwa$estimate, nsmall = 2), " [",  format( tyelo_pos_rwa$hdi.low, nsmall = 2), 
                                       ", ", format(tyelo_pos_rwa$hdi.high, nsmall = 2), "]", sep = "")

tyelo_pos_rwa$"b [95% CI]" <- paste ( format( tyelo_pos_rwa$Estimate, nsmall = 2), " [",  format( tyelo_pos_rwa$"2.5 %", nsmall = 2), 
                                      ", ", format(tyelo_pos_rwa$"97.5 %", nsmall = 2), "]", sep = "")


# join -------------------------------------------------------------------

col2k <- c("term", "b [95% HDI]", "std.error",
           "b [95% CI]", "Std. Error", "t value",
           "decision", "ROPE", "inside.rope")

tgen <- format(tgen[,col2k], nsmall = 2)
tnoloA <- format(tnoloA[,col2k], nsmall = 2)
tyeloA <- format(tyeloA[,col2k], nsmall = 2)
tnoloI <- format(tnoloI[,col2k], nsmall = 2)
tyeloI <- format(tyeloI[,col2k], nsmall = 2)
tnolo_neg_rwa <- format(tnolo_neg_rwa[,col2k], nsmall = 2)
tnolo_pos_rwa <- format(tnolo_pos_rwa[,col2k], nsmall = 2)
tyelo_neg_rwa <- format(tyelo_neg_rwa[,col2k], nsmall = 2)
tyelo_pos_rwa <- format(tyelo_pos_rwa[,col2k], nsmall = 2)

t_all <- rbind(tgen,
               tnoloA,
               tyeloA,
               tnoloI,
               tyeloI,
               tnolo_neg_rwa,
               tnolo_pos_rwa,
               tyelo_neg_rwa,
               tyelo_pos_rwa)

t_all$term <- c("Val",
                "Charge",
                "RWA",
                "Val × Charge",
                "Val × RWA",
                "Charge × RWA",
                "Val × Charge × RWA",
                "Val <sub>contrôle</sub>",
                "Val <sub>charge</sub>",
                "(Val × RWA) <sub>contrôle</sub>",
                "(Val × RWA) <sub>charge</sub>",
                "RWA <sub>contrôle neg</sub>",
                "RWA <sub>contrôle pos</sub>",
                "RWA <sub>charge neg</sub>",
                "RWA <sub>charge pos</sub>")

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


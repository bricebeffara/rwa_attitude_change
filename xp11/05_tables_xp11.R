p_load(plyr,
       knitr,
       kableExtra,
       formattable,
       dplyr,
       webshot,
       qdapRegex,
       stringr,
       install = TRUE,
       update = getOption("pac_update"),
       character.only = FALSE)

tgen_lme4 <- as.data.frame(model_gen_xp11_lme4[c(2:8),])
tgen_brms <- as.data.frame(model_gen_xp11[c(9:15),])
tgen_equi <- as.data.frame(equi_gen_xp11)

tnowa_lme4 <- as.data.frame(model_nowa_xp11_lme4[c(2:8),])
tnowa_brms <- as.data.frame(model_nowa_xp11[c(9:15),])
tnowa_equi <- as.data.frame(equi_nowa_xp11)

tyewa_lme4 <- as.data.frame(model_yewa_xp11_lme4[c(2:8),])
tyewa_brms <- as.data.frame(model_yewa_xp11[c(9:15),])
tyewa_equi <- as.data.frame(equi_yewa_xp11)

tnowa_neg_lme4 <- as.data.frame(model_nowa_neg_xp11_lme4[c(2:8),])
tnowa_neg_brms <- as.data.frame(model_nowa_neg_xp11[c(9:15),])
tnowa_neg_equi <- as.data.frame(equi_nowa_neg_xp11)

tnowa_pos_lme4 <- as.data.frame(model_nowa_pos_xp11_lme4[c(2:8),])
tnowa_pos_brms <- as.data.frame(model_nowa_pos_xp11[c(9:15),])
tnowa_pos_equi <- as.data.frame(equi_nowa_pos_xp11)

tgen_lme4$term <- tgen_brms$term
tnowa_lme4$term <- tnowa_brms$term
tyewa_lme4$term <- tyewa_brms$term
tnowa_neg_lme4$term <- tnowa_neg_brms$term
tnowa_pos_lme4$term <- tnowa_pos_brms$term

# general regression ------------------------------------------------------

tgen <- join (tgen_brms, tgen_lme4, by = "term")
tgen <- join (tgen, tgen_equi, by ="term")

tgen$"b [95% HDI]" <- paste (format(tgen$estimate, nsmall = 2), " [", format(tgen$hdi.low, nsmall = 2), ", ",
                             format(tgen$hdi.high, nsmall = 2), "]", sep = "")

tgen$"b [95% HDI]" <- gsub("\\s+", " ", str_trim(tgen$"b [95% HDI]"))
tgen$"b [95% HDI]" <- gsub("\\[ ", "\\[", str_trim(tgen$"b [95% HDI]"))

ifelse ( grepl( "  \\d", tgen$"b [95% HDI]"), "  " == " ", tgen$"b [95% HDI]")

tgen$"b [95% CI]" <- paste (format( tgen$Estimate, nsmall = 2), " [", format(tgen$"2.5 %", nsmall = 2), ", ",
                            format(tgen$"97.5 %", nsmall = 2), "]", sep = "")

tgen$"b [95% CI]" <- gsub("\\s+", " ", str_trim(tgen$"b [95% CI]"))
tgen$"b [95% CI]" <- gsub("\\[ ", "\\[", str_trim(tgen$"b [95% CI]"))

# simple slope valence in the no warning condition ----------

tnowaA_lme4 <- format(tnowa_lme4[tnowa_lme4$term == "b_usvalence",], nsmall = 2)
tnowaA_brms <- format(tnowa_brms[tnowa_brms$term == "b_usvalence",], nsmall = 2)
tnowaA_equi <- format(tnowa_equi[tnowa_equi$term == "b_usvalence",], nsmall = 2)

tnowaA <- join (tnowaA_brms, tnowaA_lme4, by = "term")
tnowaA <- join (tnowaA, tnowaA_equi, by ="term")

tnowaA$"b [95% HDI]" <- paste (format(tnowaA$estimate, nsmall = 2), " [", format(tnowaA$hdi.low, nsmall = 2), ", ",
                               format(tnowaA$hdi.high, nsmall = 2), "]", sep = "")

tnowaA$"b [95% CI]" <- paste (format( tnowaA$Estimate, nsmall = 2), " [", format(tnowaA$"2.5 %", nsmall = 2), ", ",
                              format(tnowaA$"97.5 %", nsmall = 2), "]", sep = "")

# simple slope valence in the warning condition ----------

tyewaA_lme4 <- format(tyewa_lme4[tyewa_lme4$term == "b_usvalence",], nsmall = 2)
tyewaA_brms <- format(tyewa_brms[tyewa_brms$term == "b_usvalence",], nsmall = 2)
tyewaA_equi <- format(tyewa_equi[tyewa_equi$term == "b_usvalence",], nsmall = 2)

tyewaA <- join (tyewaA_brms, tyewaA_lme4, by = "term")
tyewaA <- join (tyewaA, tyewaA_equi, by ="term")

tyewaA$"b [95% HDI]" <- paste (format(tyewaA$estimate, nsmall = 2), " [", format(tyewaA$hdi.low, nsmall = 2), ", ",
                               format(tyewaA$hdi.high, nsmall = 2), "]", sep = "")

tyewaA$"b [95% CI]" <- paste (format( tyewaA$Estimate, nsmall = 2), " [", format(tyewaA$"2.5 %", nsmall = 2), ", ",
                              format(tyewaA$"97.5 %", nsmall = 2), "]", sep = "")


# simple slope RWA * usvalence in the no warning condition ----------

tnowaI_lme4 <- format(tnowa_lme4[tnowa_lme4$term == "b_usvalence.RWAscore",], nsmall = 2)
tnowaI_brms <- format(tnowa_brms[tnowa_brms$term == "b_usvalence.RWAscore",], nsmall = 2)
tnowaI_equi <- format(tnowa_equi[tnowa_equi$term == "b_usvalence.RWAscore",], nsmall = 2)

tnowaI <- join (tnowaI_brms, tnowaI_lme4, by = "term")
tnowaI <- join (tnowaI, tnowaI_equi, by ="term")

tnowaI$"b [95% HDI]" <- paste (format(tnowaI$estimate, nsmall = 2), " [", format(tnowaI$hdi.low, nsmall = 2), ", ",
                               format(tnowaI$hdi.high, nsmall = 2), "]", sep = "")

tnowaI$"b [95% CI]" <- paste (format( tnowaI$Estimate, nsmall = 2), " [", format(tnowaI$"2.5 %", nsmall = 2), ", ",
                              format(tnowaI$"97.5 %", nsmall = 2), "]", sep = "")


# simple slope RWA * usvalence in the warning condition ----------

tyewaI_lme4 <- format(tyewa_lme4[tyewa_lme4$term == "b_usvalence.RWAscore",], nsmall = 2)
tyewaI_brms <- format(tyewa_brms[tyewa_brms$term == "b_usvalence.RWAscore",], nsmall = 2)
tyewaI_equi <- format(tyewa_equi[tyewa_equi$term == "b_usvalence.RWAscore",], nsmall = 2)

tyewaI <- join (tyewaI_brms, tyewaI_lme4, by = "term")
tyewaI <- join (tyewaI, tyewaI_equi, by ="term")

tyewaI$"b [95% HDI]" <- paste (format(tyewaI$estimate, nsmall = 2), " [", format(tyewaI$hdi.low, nsmall = 2), ", ",
                               format(tyewaI$hdi.high, nsmall = 2), "]", sep = "")

tyewaI$"b [95% CI]" <- paste (format( tyewaI$Estimate, nsmall = 2), " [", format(tyewaI$"2.5 %", nsmall = 2), ", ",
                              format(tyewaI$"97.5 %", nsmall = 2), "]", sep = "")


# simple slope RWA no warning negative valence condition --------

tnowa_neg_rwa_lme4 <- format(tnowa_neg_lme4[tnowa_neg_lme4$term == "b_RWAscore",], nsmall = 2)
tnowa_neg_rwa_brms <- format(tnowa_neg_brms[tnowa_neg_brms$term == "b_RWAscore",], nsmall = 2)
tnowa_neg_rwa_equi <- format(tnowa_neg_equi[tnowa_neg_equi$term == "b_RWAscore",], nsmall = 2)

tnowa_neg_rwa <- join (tnowa_neg_rwa_brms, tnowa_neg_rwa_lme4, by = "term")
tnowa_neg_rwa <- join (tnowa_neg_rwa, tnowa_neg_rwa_equi, by ="term")

tnowa_neg_rwa$"b [95% HDI]" <- paste (format(tnowa_neg_rwa$estimate, nsmall = 2), " [", format(tnowa_neg_rwa$hdi.low, nsmall = 2), ", ",
                                      format(tnowa_neg_rwa$hdi.high, nsmall = 2), "]", sep = "")

tnowa_neg_rwa$"b [95% CI]" <- paste (format( tnowa_neg_rwa$Estimate, nsmall = 2), " [", format(tnowa_neg_rwa$"2.5 %", nsmall = 2), ", ",
                                     format(tnowa_neg_rwa$"97.5 %", nsmall = 2), "]", sep = "")

# simple slope RWA no warning positive valence condition --------

tnowa_pos_rwa_lme4 <- format(tnowa_pos_lme4[tnowa_pos_lme4$term == "b_RWAscore",], nsmall = 2)
tnowa_pos_rwa_brms <- format(tnowa_pos_brms[tnowa_pos_brms$term == "b_RWAscore",], nsmall = 2)
tnowa_pos_rwa_equi <- format(tnowa_pos_equi[tnowa_pos_equi$term == "b_RWAscore",], nsmall = 2)

tnowa_pos_rwa <- join (tnowa_pos_rwa_brms, tnowa_pos_rwa_lme4, by = "term")
tnowa_pos_rwa <- join (tnowa_pos_rwa, tnowa_pos_rwa_equi, by ="term")

tnowa_pos_rwa$"b [95% HDI]" <- paste (format(tnowa_pos_rwa$estimate, nsmall = 2), " [", format(tnowa_pos_rwa$hdi.low, nsmall = 2), ", ",
                                      format(tnowa_pos_rwa$hdi.high, nsmall = 2), "]", sep = "")

tnowa_pos_rwa$"b [95% CI]" <- paste (format( tnowa_pos_rwa$Estimate, nsmall = 2), " [", format(tnowa_pos_rwa$"2.5 %", nsmall = 2), ", ",
                                     format(tnowa_pos_rwa$"97.5 %", nsmall = 2), "]", sep = "")



# join -------------------------------------------------------------------

col2k <- c("term", "b [95% HDI]", "std.error",
           "b [95% CI]", "Std. Error", "t value",
           "decision", "ROPE", "inside.rope")

tgen <- format(tgen[,col2k], nsmall = 2)
tnowaA <- format(tnowaA[,col2k], nsmall = 2)
tyewaA <- format(tyewaA[,col2k], nsmall = 2)
tnowaI <- format(tnowaI[,col2k], nsmall = 2)
tyewaI <- format(tyewaI[,col2k], nsmall = 2)
tnowa_neg_rwa <- format(tnowa_neg_rwa[,col2k], nsmall = 2)
tnowa_pos_rwa <- format(tnowa_pos_rwa[,col2k], nsmall = 2)


t_all <- rbind(tgen,
               tnowaA,
               tyewaA,
               tnowaI,
               tyewaI,
               tnowa_neg_rwa,
               tnowa_pos_rwa)

t_all$term <- c("Val",
                "Warn",
                "RWA",
                "Val × Warn",
                "Val × RWA",
                "Warn × RWA",
                "Val × Warn × RWA",
                "Val <sub>no warn</sub>",
                "Val <sub>warn</sub>",
                "(Val × RWA) <sub>no warn</sub>",
                "(Val × RWA) <sub>warn</sub>",
                "RWA <sub>no warn neg</sub>",
                "RWA <sub>no warn pos</sub>")

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






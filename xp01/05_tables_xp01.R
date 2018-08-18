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

tgen_lme4 <- as.data.frame(model_gen_xp01_lme4[c(2:4),])
tgen_brms <- as.data.frame(model_gen_xp01[c(9:11),])
tgen_equi <- as.data.frame(equi_gen_xp01)

tneg_lme4 <- as.data.frame(model_neg_xp01_lme4[c(2:4),])
tneg_brms <- as.data.frame(model_neg_xp01[c(9:11),])
tneg_equi <- as.data.frame(equi_neg_xp01)

tpos_lme4 <- as.data.frame(model_pos_xp01_lme4[c(2:4),])
tpos_brms <- as.data.frame(model_pos_xp01[c(9:11),])
tpos_equi <- as.data.frame(equi_pos_xp01)

tgen_lme4$term <- tgen_brms$term
tneg_lme4$term <- tneg_brms$term
tpos_lme4$term <- tpos_brms$term

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

# simple slope RWA in the negative condition ----------

tnegA_lme4 <- format(tneg_lme4[tneg_lme4$term == "b_RWAscore",], nsmall = 2)
tnegA_brms <- format(tneg_brms[tneg_brms$term == "b_RWAscore",], nsmall = 2)
tnegA_equi <- format(tneg_equi[tneg_equi$term == "b_RWAscore",], nsmall = 2)

tnegA <- join (tnegA_brms, tnegA_lme4, by = "term")
tnegA <- join (tnegA, tnegA_equi, by ="term")

tnegA$"b [95% HDI]" <- paste (format(tnegA$estimate, nsmall = 2), " [", format(tnegA$hdi.low, nsmall = 2), ", ",
                               format(tnegA$hdi.high, nsmall = 2), "]", sep = "")

tnegA$"b [95% CI]" <- paste (format( tnegA$Estimate, nsmall = 2), " [", format(tnegA$"2.5 %", nsmall = 2), ", ",
                              format(tnegA$"97.5 %", nsmall = 2), "]", sep = "")

# simple slope valence in the warning condition ----------

tposA_lme4 <- format(tpos_lme4[tpos_lme4$term == "b_RWAscore",], nsmall = 2)
tposA_brms <- format(tpos_brms[tpos_brms$term == "b_RWAscore",], nsmall = 2)
tposA_equi <- format(tpos_equi[tpos_equi$term == "b_RWAscore",], nsmall = 2)

tposA <- join (tposA_brms, tposA_lme4, by = "term")
tposA <- join (tposA, tposA_equi, by ="term")

tposA$"b [95% HDI]" <- paste (format(tposA$estimate, nsmall = 2), " [", format(tposA$hdi.low, nsmall = 2), ", ",
                               format(tposA$hdi.high, nsmall = 2), "]", sep = "")

tposA$"b [95% CI]" <- paste (format( tposA$Estimate, nsmall = 2), " [", format(tposA$"2.5 %", nsmall = 2), ", ",
                              format(tposA$"97.5 %", nsmall = 2), "]", sep = "")

# join -------------------------------------------------------------------

col2k <- c("term", "b [95% HDI]", "std.error",
           "b [95% CI]", "Std. Error", "t value",
           "decision", "ROPE", "inside.rope")

tgen <- format(tgen[,col2k], nsmall = 2)
tnegA <- format(tnegA[,col2k], nsmall = 2)
tposA <- format(tposA[,col2k], nsmall = 2)

t_all <- rbind(tgen,
               tnegA,
               tposA)

t_all$term <- c("Val",
                "RWA",
                "Val × RWA",
                "RWA <sub>neg</sub>",
                "RWA <sub>pos</sub>")

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






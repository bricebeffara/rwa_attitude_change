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

tgen_lme4 <- as.data.frame(diriat_gen_xp04_lme4[c(2:8),])
tgen_brms <- as.data.frame(diriat_gen_xp04[c(9:15),])
tgen_equi <- as.data.frame(equi_gen_diriat__xp04)

tord1_lme4 <- as.data.frame(model_ord1_xp04_lme4[c(2:4),])
tord1_brms <- as.data.frame(model_ord1_xp04[c(9:11),])
tord1_equi <- as.data.frame(equi_ord1_xp04)

tord2_lme4 <- as.data.frame(model_ord2_xp04_lme4[c(2:4),])
tord2_brms <- as.data.frame(model_ord2_xp04[c(9:11),])
tord2_equi <- as.data.frame(equi_ord2_xp04)

tgen_lme4$term <- tgen_brms$term
tord1_lme4$term <- tord1_brms$term
tord2_lme4$term <- tord2_brms$term


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

# join -------------------------------------------------------------------

col2k <- c("term", "b [95% HDI]", "std.error",
           "b [95% CI]", "Std. Error", "t value",
           "decision", "ROPE", "inside.rope")

tgen <- format(tgen[,col2k], nsmall = 2)
tord1A <- format(tord1A[,col2k], nsmall = 2)
tord2A <- format(tord2A[,col2k], nsmall = 2)

t_all <- rbind(tgen,
               tord1A,
               tord2A)

t_all$term <- c("Val",
                "Ordre",
                "RWA",
                "Val × Ordre",
                "Val × RWA",
                "Ordre × RWA",
                "Val × Ordre × RWA",
                "Val <sub>Ordre 1</sub>",
                "Val <sub>Ordre 2</sub>")

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


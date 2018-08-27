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

tgen_lme4 <- as.data.frame(model_gen_xp03_lme4[c(2:8),])
tgen_brms <- as.data.frame(model_gen_xp03[c(9:15),])
tgen_equi <- as.data.frame(equi_gen_xp03)

tb1_lme4 <- as.data.frame(model_b1_xp03_lme4[c(2:8),])
tb1_brms <- as.data.frame(model_b1_xp03[c(9:15),])
tb1_equi <- as.data.frame(equi_b1_xp03)

tb2_lme4 <- as.data.frame(model_b2_xp03_lme4[c(2:8),])
tb2_brms <- as.data.frame(model_b2_xp03[c(9:15),])
tb2_equi <- as.data.frame(equi_b2_xp03)

tb2_neg_lme4 <- as.data.frame(model_b2_neg_xp03_lme4[c(2:8),])
tb2_neg_brms <- as.data.frame(model_b2_neg_xp03[c(9:15),])
tb2_neg_equi <- as.data.frame(equi_b2_neg_xp03)

tb2_pos_lme4 <- as.data.frame(model_b2_pos_xp03_lme4[c(2:8),])
tb2_pos_brms <- as.data.frame(model_b2_pos_xp03[c(9:15),])
tb2_pos_equi <- as.data.frame(equi_b2_pos_xp03)

tgen_lme4$term <- tgen_brms$term
tb1_lme4$term <- tb1_brms$term
tb2_lme4$term <- tb2_brms$term
tb2_neg_lme4$term <- tb2_neg_brms$term
tb2_pos_lme4$term <- tb2_pos_brms$term

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

# simple slope valence in the 1 block condition ----------

tb1A_lme4 <- format(tb1_lme4[tb1_lme4$term == "b_usvalence",], nsmall = 2)
tb1A_brms <- format(tb1_brms[tb1_brms$term == "b_usvalence",], nsmall = 2)
tb1A_equi <- format(tb1_equi[tb1_equi$term == "b_usvalence",], nsmall = 2)

tb1A <- join (tb1A_brms, tb1A_lme4, by = "term")
tb1A <- join (tb1A, tb1A_equi, by ="term")

tb1A$"b [95% HDI]" <- paste (format(tb1A$estimate, nsmall = 2), " [", format(tb1A$hdi.low, nsmall = 2), ", ",
                               format(tb1A$hdi.high, nsmall = 2), "]", sep = "")

tb1A$"b [95% CI]" <- paste (format( tb1A$Estimate, nsmall = 2), " [", format(tb1A$"2.5 %", nsmall = 2), ", ",
                              format(tb1A$"97.5 %", nsmall = 2), "]", sep = "")

# simple slope valence in the 2 blocks condition ----------

tb2A_lme4 <- format(tb2_lme4[tb2_lme4$term == "b_usvalence",], nsmall = 2)
tb2A_brms <- format(tb2_brms[tb2_brms$term == "b_usvalence",], nsmall = 2)
tb2A_equi <- format(tb2_equi[tb2_equi$term == "b_usvalence",], nsmall = 2)

tb2A <- join (tb2A_brms, tb2A_lme4, by = "term")
tb2A <- join (tb2A, tb2A_equi, by ="term")

tb2A$"b [95% HDI]" <- paste (format(tb2A$estimate, nsmall = 2), " [", format(tb2A$hdi.low, nsmall = 2), ", ",
                               format(tb2A$hdi.high, nsmall = 2), "]", sep = "")

tb2A$"b [95% CI]" <- paste (format( tb2A$Estimate, nsmall = 2), " [", format(tb2A$"2.5 %", nsmall = 2), ", ",
                              format(tb2A$"97.5 %", nsmall = 2), "]", sep = "")


# simple slope RWA * usvalence in the 1 block condition ----------

tb1I_lme4 <- format(tb1_lme4[tb1_lme4$term == "b_usvalence.RWAscore",], nsmall = 2)
tb1I_brms <- format(tb1_brms[tb1_brms$term == "b_usvalence.RWAscore",], nsmall = 2)
tb1I_equi <- format(tb1_equi[tb1_equi$term == "b_usvalence.RWAscore",], nsmall = 2)

tb1I <- join (tb1I_brms, tb1I_lme4, by = "term")
tb1I <- join (tb1I, tb1I_equi, by ="term")

tb1I$"b [95% HDI]" <- paste (format(tb1I$estimate, nsmall = 2), " [", format(tb1I$hdi.low, nsmall = 2), ", ",
                               format(tb1I$hdi.high, nsmall = 2), "]", sep = "")

tb1I$"b [95% CI]" <- paste (format( tb1I$Estimate, nsmall = 2), " [", format(tb1I$"2.5 %", nsmall = 2), ", ",
                              format(tb1I$"97.5 %", nsmall = 2), "]", sep = "")


# simple slope RWA * usvalence in the 2 blocks condition ----------

tb2I_lme4 <- format(tb2_lme4[tb2_lme4$term == "b_usvalence.RWAscore",], nsmall = 2)
tb2I_brms <- format(tb2_brms[tb2_brms$term == "b_usvalence.RWAscore",], nsmall = 2)
tb2I_equi <- format(tb2_equi[tb2_equi$term == "b_usvalence.RWAscore",], nsmall = 2)

tb2I <- join (tb2I_brms, tb2I_lme4, by = "term")
tb2I <- join (tb2I, tb2I_equi, by ="term")

tb2I$"b [95% HDI]" <- paste (format(tb2I$estimate, nsmall = 2), " [", format(tb2I$hdi.low, nsmall = 2), ", ",
                               format(tb2I$hdi.high, nsmall = 2), "]", sep = "")

tb2I$"b [95% CI]" <- paste (format( tb2I$Estimate, nsmall = 2), " [", format(tb2I$"2.5 %", nsmall = 2), ", ",
                              format(tb2I$"97.5 %", nsmall = 2), "]", sep = "")


# simple slope RWA 1 block negative valence condition --------

tb2_neg_rwa_lme4 <- format(tb2_neg_lme4[tb2_neg_lme4$term == "b_RWAscore",], nsmall = 2)
tb2_neg_rwa_brms <- format(tb2_neg_brms[tb2_neg_brms$term == "b_RWAscore",], nsmall = 2)
tb2_neg_rwa_equi <- format(tb2_neg_equi[tb2_neg_equi$term == "b_RWAscore",], nsmall = 2)

tb2_neg_rwa <- join (tb2_neg_rwa_brms, tb2_neg_rwa_lme4, by = "term")
tb2_neg_rwa <- join (tb2_neg_rwa, tb2_neg_rwa_equi, by ="term")

tb2_neg_rwa$"b [95% HDI]" <- paste (format(tb2_neg_rwa$estimate, nsmall = 2), " [", format(tb2_neg_rwa$hdi.low, nsmall = 2), ", ",
                                      format(tb2_neg_rwa$hdi.high, nsmall = 2), "]", sep = "")

tb2_neg_rwa$"b [95% CI]" <- paste (format( tb2_neg_rwa$Estimate, nsmall = 2), " [", format(tb2_neg_rwa$"2.5 %", nsmall = 2), ", ",
                                     format(tb2_neg_rwa$"97.5 %", nsmall = 2), "]", sep = "")

# simple slope RWA 2 blocks positive valence condition --------

tb2_pos_rwa_lme4 <- format(tb2_pos_lme4[tb2_pos_lme4$term == "b_RWAscore",], nsmall = 2)
tb2_pos_rwa_brms <- format(tb2_pos_brms[tb2_pos_brms$term == "b_RWAscore",], nsmall = 2)
tb2_pos_rwa_equi <- format(tb2_pos_equi[tb2_pos_equi$term == "b_RWAscore",], nsmall = 2)

tb2_pos_rwa <- join (tb2_pos_rwa_brms, tb2_pos_rwa_lme4, by = "term")
tb2_pos_rwa <- join (tb2_pos_rwa, tb2_pos_rwa_equi, by ="term")

tb2_pos_rwa$"b [95% HDI]" <- paste (format(tb2_pos_rwa$estimate, nsmall = 2), " [", format(tb2_pos_rwa$hdi.low, nsmall = 2), ", ",
                                      format(tb2_pos_rwa$hdi.high, nsmall = 2), "]", sep = "")

tb2_pos_rwa$"b [95% CI]" <- paste (format( tb2_pos_rwa$Estimate, nsmall = 2), " [", format(tb2_pos_rwa$"2.5 %", nsmall = 2), ", ",
                                     format(tb2_pos_rwa$"97.5 %", nsmall = 2), "]", sep = "")



# join -------------------------------------------------------------------

col2k <- c("term", "b [95% HDI]", "std.error",
           "b [95% CI]", "Std. Error", "t value",
           "decision", "ROPE", "inside.rope")

tgen <- format(tgen[,col2k], nsmall = 2)
tb1A <- format(tb1A[,col2k], nsmall = 2)
tb2A <- format(tb2A[,col2k], nsmall = 2)
tb1I <- format(tb1I[,col2k], nsmall = 2)
tb2I <- format(tb2I[,col2k], nsmall = 2)
tb2_neg_rwa <- format(tb2_neg_rwa[,col2k], nsmall = 2)
tb2_pos_rwa <- format(tb2_pos_rwa[,col2k], nsmall = 2)


t_all <- rbind(tgen,
               tb1A,
               tb2A,
               tb1I,
               tb2I,
               tb2_neg_rwa,
               tb2_pos_rwa)

t_all$term <- c("Val",
                "Bloc",
                "RWA",
                "Val × Bloc",
                "Val × RWA",
                "Bloc × RWA",
                "Val × Bloc × RWA",
                "Val <sub>1 Bloc</sub>",
                "Val <sub>2 Blocs</sub>",
                "(Val × RWA) <sub>1 Bloc</sub>",
                "(Val × RWA) <sub>2 Blocs</sub>",
                "RWA <sub>2 Blocs neg</sub>",
                "RWA <sub>2 Blocs pos</sub>")

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






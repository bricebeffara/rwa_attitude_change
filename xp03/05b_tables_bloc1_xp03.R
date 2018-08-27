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

tgen_bloc1_lme4 <- as.data.frame(model_gen_bloc1_xp03_lme4[c(2:8),])
tgen_bloc1_brms <- as.data.frame(model_gen_bloc1_xp03[c(9:15),])
tgen_bloc1_equi <- as.data.frame(equi_gen_bloc1_xp03)

tbloc1_b1_lme4 <- as.data.frame(model_bloc1_b1_xp03_lme4[c(2:4),])
tbloc1_b1_brms <- as.data.frame(model_bloc1_b1_xp03[c(9:11),])
tbloc1_b1_equi <- as.data.frame(equi_bloc1_b1_xp03)

tbloc1_b2_lme4 <- as.data.frame(model_bloc1_b2_xp03_lme4[c(2:4),])
tbloc1_b2_brms <- as.data.frame(model_bloc1_b2_xp03[c(9:11),])
tbloc1_b2_equi <- as.data.frame(equi_bloc1_b2_xp03)

tbloc1_neg_lme4 <- as.data.frame(model_bloc1_neg_xp03_lme4[c(2:4),])
tbloc1_neg_brms <- as.data.frame(model_bloc1_neg_xp03[c(9:11),])
tbloc1_neg_equi <- as.data.frame(equi_bloc1_neg_xp03)

tbloc1_pos_lme4 <- as.data.frame(model_bloc1_pos_xp03_lme4[c(2:4),])
tbloc1_pos_brms <- as.data.frame(model_bloc1_pos_xp03[c(9:11),])
tbloc1_pos_equi <- as.data.frame(equi_bloc1_pos_xp03)

tgen_bloc1_lme4$term <- tgen_bloc1_brms$term
tbloc1_b1_lme4$term <- tbloc1_b1_brms$term
tbloc1_b2_lme4$term <- tbloc1_b2_brms$term
tbloc1_neg_lme4$term <- tbloc1_neg_brms$term
tbloc1_pos_lme4$term <- tbloc1_pos_brms$term

# general regression ------------------------------------------------------

tgen_bloc1 <- join (tgen_bloc1_brms, tgen_bloc1_lme4, by = "term")
tgen_bloc1 <- join (tgen_bloc1, tgen_bloc1_equi, by ="term")

tgen_bloc1$"b [95% HDI]" <- paste (format(tgen_bloc1$estimate, nsmall = 2), " [", format(tgen_bloc1$hdi.low, nsmall = 2), ", ",
                             format(tgen_bloc1$hdi.high, nsmall = 2), "]", sep = "")

tgen_bloc1$"b [95% HDI]" <- gsub("\\s+", " ", str_trim(tgen_bloc1$"b [95% HDI]"))
tgen_bloc1$"b [95% HDI]" <- gsub("\\[ ", "\\[", str_trim(tgen_bloc1$"b [95% HDI]"))

ifelse ( grepl( "  \\d", tgen_bloc1$"b [95% HDI]"), "  " == " ", tgen_bloc1$"b [95% HDI]")

tgen_bloc1$"b [95% CI]" <- paste (format( tgen_bloc1$Estimate, nsmall = 2), " [", format(tgen_bloc1$"2.5 %", nsmall = 2), ", ",
                            format(tgen_bloc1$"97.5 %", nsmall = 2), "]", sep = "")

tgen_bloc1$"b [95% CI]" <- gsub("\\s+", " ", str_trim(tgen_bloc1$"b [95% CI]"))
tgen_bloc1$"b [95% CI]" <- gsub("\\[ ", "\\[", str_trim(tgen_bloc1$"b [95% CI]"))

# simple slope valence in the 1 block condition ----------

tbloc1_b1A_lme4 <- format(tbloc1_b1_lme4[tbloc1_b1_lme4$term == "b_usvalence1",], nsmall = 2)
tbloc1_b1A_brms <- format(tbloc1_b1_brms[tbloc1_b1_brms$term == "b_usvalence1",], nsmall = 2)
tbloc1_b1A_equi <- format(tbloc1_b1_equi[tbloc1_b1_equi$term == "b_usvalence1",], nsmall = 2)

tbloc1_b1A <- join (tbloc1_b1A_brms, tbloc1_b1A_lme4, by = "term")
tbloc1_b1A <- join (tbloc1_b1A, tbloc1_b1A_equi, by ="term")

tbloc1_b1A$"b [95% HDI]" <- paste (format(tbloc1_b1A$estimate, nsmall = 2), " [", format(tbloc1_b1A$hdi.low, nsmall = 2), ", ",
                               format(tbloc1_b1A$hdi.high, nsmall = 2), "]", sep = "")

tbloc1_b1A$"b [95% CI]" <- paste (format( tbloc1_b1A$Estimate, nsmall = 2), " [", format(tbloc1_b1A$"2.5 %", nsmall = 2), ", ",
                              format(tbloc1_b1A$"97.5 %", nsmall = 2), "]", sep = "")

# simple slope valence in the 2 blocks condition ----------

tbloc1_b2A_lme4 <- format(tbloc1_b2_lme4[tbloc1_b2_lme4$term == "b_usvalence1",], nsmall = 2)
tbloc1_b2A_brms <- format(tbloc1_b2_brms[tbloc1_b2_brms$term == "b_usvalence1",], nsmall = 2)
tbloc1_b2A_equi <- format(tbloc1_b2_equi[tbloc1_b2_equi$term == "b_usvalence1",], nsmall = 2)

tbloc1_b2A <- join (tbloc1_b2A_brms, tbloc1_b2A_lme4, by = "term")
tbloc1_b2A <- join (tbloc1_b2A, tbloc1_b2A_equi, by ="term")

tbloc1_b2A$"b [95% HDI]" <- paste (format(tbloc1_b2A$estimate, nsmall = 2), " [", format(tbloc1_b2A$hdi.low, nsmall = 2), ", ",
                               format(tbloc1_b2A$hdi.high, nsmall = 2), "]", sep = "")

tbloc1_b2A$"b [95% CI]" <- paste (format( tbloc1_b2A$Estimate, nsmall = 2), " [", format(tbloc1_b2A$"2.5 %", nsmall = 2), ", ",
                              format(tbloc1_b2A$"97.5 %", nsmall = 2), "]", sep = "")


# simple slope RWA negative valence condition --------

tbloc1_neg_rwa_lme4 <- format(tbloc1_neg_lme4[tbloc1_neg_lme4$term == "b_RWAscore",], nsmall = 2)
tbloc1_neg_rwa_brms <- format(tbloc1_neg_brms[tbloc1_neg_brms$term == "b_RWAscore",], nsmall = 2)
tbloc1_neg_rwa_equi <- format(tbloc1_neg_equi[tbloc1_neg_equi$term == "b_RWAscore",], nsmall = 2)

tbloc1_neg_rwa <- join (tbloc1_neg_rwa_brms, tbloc1_neg_rwa_lme4, by = "term")
tbloc1_neg_rwa <- join (tbloc1_neg_rwa, tbloc1_neg_rwa_equi, by ="term")

tbloc1_neg_rwa$"b [95% HDI]" <- paste (format(tbloc1_neg_rwa$estimate, nsmall = 2), " [", format(tbloc1_neg_rwa$hdi.low, nsmall = 2), ", ",
                                      format(tbloc1_neg_rwa$hdi.high, nsmall = 2), "]", sep = "")

tbloc1_neg_rwa$"b [95% CI]" <- paste (format( tbloc1_neg_rwa$Estimate, nsmall = 2), " [", format(tbloc1_neg_rwa$"2.5 %", nsmall = 2), ", ",
                                     format(tbloc1_neg_rwa$"97.5 %", nsmall = 2), "]", sep = "")

# simple slope RWA positive valence condition --------

tbloc1_pos_rwa_lme4 <- format(tbloc1_pos_lme4[tbloc1_pos_lme4$term == "b_RWAscore",], nsmall = 2)
tbloc1_pos_rwa_brms <- format(tbloc1_pos_brms[tbloc1_pos_brms$term == "b_RWAscore",], nsmall = 2)
tbloc1_pos_rwa_equi <- format(tbloc1_pos_equi[tbloc1_pos_equi$term == "b_RWAscore",], nsmall = 2)

tbloc1_pos_rwa <- join (tbloc1_pos_rwa_brms, tbloc1_pos_rwa_lme4, by = "term")
tbloc1_pos_rwa <- join (tbloc1_pos_rwa, tbloc1_pos_rwa_equi, by ="term")

tbloc1_pos_rwa$"b [95% HDI]" <- paste (format(tbloc1_pos_rwa$estimate, nsmall = 2), " [", format(tbloc1_pos_rwa$hdi.low, nsmall = 2), ", ",
                                      format(tbloc1_pos_rwa$hdi.high, nsmall = 2), "]", sep = "")

tbloc1_pos_rwa$"b [95% CI]" <- paste (format( tbloc1_pos_rwa$Estimate, nsmall = 2), " [", format(tbloc1_pos_rwa$"2.5 %", nsmall = 2), ", ",
                                     format(tbloc1_pos_rwa$"97.5 %", nsmall = 2), "]", sep = "")



# join -------------------------------------------------------------------

col2k <- c("term", "b [95% HDI]", "std.error",
           "b [95% CI]", "Std. Error", "t value",
           "decision", "ROPE", "inside.rope")

tgen_bloc1 <- format(tgen_bloc1[,col2k], nsmall = 2)
tbloc1_b1A <- format(tbloc1_b1A[,col2k], nsmall = 2)
tbloc1_b2A <- format(tbloc1_b2A[,col2k], nsmall = 2)
tbloc1_neg_rwa <- format(tbloc1_neg_rwa[,col2k], nsmall = 2)
tbloc1_pos_rwa <- format(tbloc1_pos_rwa[,col2k], nsmall = 2)


t_all_bloc1 <- rbind(tgen_bloc1,
               tbloc1_b1A,
               tbloc1_b2A,
               tbloc1_neg_rwa,
               tbloc1_pos_rwa)

t_all_bloc1$term <- c("Val",
                "Bloc",
                "RWA",
                "Val × Bloc",
                "Val × RWA",
                "Bloc × RWA",
                "Val × Bloc × RWA",
                "Val <sub>1 Bloc</sub>",
                "Val <sub>2 Blocs</sub>",
                "RWA <sub>neg</sub>",
                "RWA <sub>pos</sub>")

t_all_bloc1$decision <- ifelse(t_all_bloc1$decision == "reject", "oui",
                         ifelse(t_all_bloc1$decision == "accept", "non",
                         "indécision"))

t_all_bloc1 <- t_all_bloc1 %>%
  mutate(inside.rope = color_tile("lightgreen", "orange")(inside.rope))

t_all_bloc1$"n° &#946;" <- 1:length( t_all_bloc1$term)

ordc <- c("n° &#946;", "term", "b [95% HDI]", "std.error",
          "b [95% CI]", "Std. Error", "t value",
          "decision", "ROPE", "inside.rope")

t_all_bloc1 <- t_all_bloc1[,ordc]

colnames(t_all_bloc1) <- c("n° &#946;", "Paramètre", "&#946;<sub>Bayes</sub> [95% HDI]",
                     "SE<sub>Bayes</sub>", "&#946;<sub>freq</sub> [95% CI]",
                     "SE<sub>freq</sub>", "t", 
                     "&#946 &#8800; 0", "ROPE", "% &#946;<sub>Bayes</sub> dans ROPE")

rownames(t_all_bloc1) <- NULL






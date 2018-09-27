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

tapt_gen_lme4 <- as.data.frame(apt_gen_xp05_lme4[c(2:8),])
tapt_gen_brms <- as.data.frame(apt_gen_xp05[c(2:8),])
tapt_gen_equi <- as.data.frame(equi_gen_xp05)

tapt_gen_lme4$term <- tapt_gen_brms$term
tapt_gen_equi$term <- tapt_gen_brms$term


# apt_general regression ------------------------------------------------------

tapt_gen <- join (tapt_gen_brms, tapt_gen_lme4, by = "term")
tapt_gen <- join (tapt_gen, tapt_gen_equi, by ="term")

tapt_gen$"b [95% HDI]" <- paste ( format( tapt_gen$estimate, nsmall = 2), " [",  format( tapt_gen$hdi.low, nsmall = 2), 
                                      ", ", format(tapt_gen$hdi.high, nsmall = 2), "]", sep = "")

tapt_gen$"b [95% HDI]" <- gsub("\\s+", " ", str_trim(tapt_gen$"b [95% HDI]"))
tapt_gen$"b [95% HDI]" <- gsub("\\[ ", "\\[", str_trim(tapt_gen$"b [95% HDI]"))

tapt_gen$"b [95% CI]" <- paste ( format( tapt_gen$Estimate, nsmall = 2), " [",  format( tapt_gen$"2.5 %", nsmall = 2), 
                                    ", ", format(tapt_gen$"97.5 %", nsmall = 2), "]", sep = "")

tapt_gen$"b [95% CI]" <- gsub("\\s+", " ", str_trim(tapt_gen$"b [95% CI]"))
tapt_gen$"b [95% CI]" <- gsub("\\[ ", "\\[", str_trim(tapt_gen$"b [95% CI]"))

# join -------------------------------------------------------------------

col2k <- c("term", "b [95% HDI]", "std.error",
           "b [95% CI]", "Std. Error", "t value",
           "decision", "ROPE", "inside.rope")

tapt_gen <- format(tapt_gen[,col2k], nsmall = 2)

tapt_gen$term <- c("Congr",
                "Ordre",
                "RWA",
                "Congr × Ordre",
                "Congr × RWA",
                "Ordre × RWA",
                "Congr × Ordre × RWA")

tapt_gen$decision <- ifelse(tapt_gen$decision == "reject", "oui",
                         ifelse(tapt_gen$decision == "accept", "non",
                         "indécision"))

tapt_gen <- tapt_gen %>%
  mutate(inside.rope = color_tile("lightgreen", "orange")(inside.rope))

tapt_gen$"n° &#946;" <- 1:length( tapt_gen$term)

ordc <- c("n° &#946;", "term", "b [95% HDI]", "std.error",
           "b [95% CI]", "Std. Error", "t value",
           "decision", "ROPE", "inside.rope")

tapt_gen <- tapt_gen[,ordc]

colnames(tapt_gen) <- c("n° &#946;", "Paramètre", "&#946;<sub>Bayes</sub> [95% HDI]",
                     "SE<sub>Bayes</sub>", "&#946;<sub>freq</sub> [95% CI]",
                     "SE<sub>freq</sub>", "t", 
                     "&#946 &#8800; 0", "ROPE", "% &#946;<sub>Bayes</sub> &sube; ROPE")

rownames(tapt_gen) <- NULL


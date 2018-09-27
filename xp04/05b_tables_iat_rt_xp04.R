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

tiat_gen_lme4 <- as.data.frame(iat_gen_xp04_lme4[c(2:8),])
tiat_gen_brms <- as.data.frame(iat_gen_xp04[c(2:8),])
tiat_gen_equi <- as.data.frame(equi_gen_xp04)

tiat_gen_lme4$term <- tiat_gen_brms$term
tiat_gen_equi$term <- tiat_gen_brms$term


# iat_general regression ------------------------------------------------------

tiat_gen <- join (tiat_gen_brms, tiat_gen_lme4, by = "term")
tiat_gen <- join (tiat_gen, tiat_gen_equi, by ="term")

tiat_gen$"b [95% HDI]" <- paste ( format( tiat_gen$estimate, nsmall = 2), " [",  format( tiat_gen$hdi.low, nsmall = 2), 
                                      ", ", format(tiat_gen$hdi.high, nsmall = 2), "]", sep = "")

tiat_gen$"b [95% HDI]" <- gsub("\\s+", " ", str_trim(tiat_gen$"b [95% HDI]"))
tiat_gen$"b [95% HDI]" <- gsub("\\[ ", "\\[", str_trim(tiat_gen$"b [95% HDI]"))

tiat_gen$"b [95% CI]" <- paste ( format( tiat_gen$Estimate, nsmall = 2), " [",  format( tiat_gen$"2.5 %", nsmall = 2), 
                                    ", ", format(tiat_gen$"97.5 %", nsmall = 2), "]", sep = "")

tiat_gen$"b [95% CI]" <- gsub("\\s+", " ", str_trim(tiat_gen$"b [95% CI]"))
tiat_gen$"b [95% CI]" <- gsub("\\[ ", "\\[", str_trim(tiat_gen$"b [95% CI]"))

# join -------------------------------------------------------------------

col2k <- c("term", "b [95% HDI]", "std.error",
           "b [95% CI]", "Std. Error", "t value",
           "decision", "ROPE", "inside.rope")

tiat_gen <- format(tiat_gen[,col2k], nsmall = 2)

tiat_gen$term <- c("Cong",
                "Ordre",
                "RWA",
                "Cong × Ordre",
                "Cong × RWA",
                "Ordre × RWA",
                "Cong × Ordre × RWA")

tiat_gen$decision <- ifelse(tiat_gen$decision == "reject", "oui",
                         ifelse(tiat_gen$decision == "accept", "non",
                         "indécision"))

tiat_gen <- tiat_gen %>%
  mutate(inside.rope = color_tile("lightgreen", "orange")(inside.rope))

tiat_gen$"n° &#946;" <- 1:length( tiat_gen$term)

ordc <- c("n° &#946;", "term", "b [95% HDI]", "std.error",
           "b [95% CI]", "Std. Error", "t value",
           "decision", "ROPE", "inside.rope")

tiat_gen <- tiat_gen[,ordc]

colnames(tiat_gen) <- c("n° &#946;", "Paramètre", "&#946;<sub>Bayes</sub> [95% HDI]",
                     "SE<sub>Bayes</sub>", "&#946;<sub>freq</sub> [95% CI]",
                     "SE<sub>freq</sub>", "t", 
                     "&#946 &#8800; 0", "ROPE", "% &#946;<sub>Bayes</sub> &sube; ROPE")

rownames(tiat_gen) <- NULL


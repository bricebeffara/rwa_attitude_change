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

tamp_gen_lme4 <- as.data.frame(amp_gen_xp06_lme4[c(1:7),])
tamp_gen_brms <- as.data.frame(amp_gen_xp06[c(2:8),])
tamp_gen_equi <- as.data.frame(equi_gen_xp06)

tamp_gen_lme4$term <- tamp_gen_brms$term
tamp_gen_equi$term <- tamp_gen_brms$term


# amp_general regression ------------------------------------------------------

tamp_gen <- join (tamp_gen_brms, tamp_gen_lme4, by = "term")
tamp_gen <- join (tamp_gen, tamp_gen_equi, by ="term")

tamp_gen$"b [95% HDI]" <- paste ( format( tamp_gen$estimate, nsmall = 2), " [",  format( tamp_gen$hdi.low, nsmall = 2), 
                                      ", ", format(tamp_gen$hdi.high, nsmall = 2), "]", sep = "")

tamp_gen$"b [95% HDI]" <- gsub("\\s+", " ", str_trim(tamp_gen$"b [95% HDI]"))
tamp_gen$"b [95% HDI]" <- gsub("\\[ ", "\\[", str_trim(tamp_gen$"b [95% HDI]"))

tamp_gen$"b [95% CI]" <- paste ( format( tamp_gen$Estimate, nsmall = 2), " [",  format( tamp_gen$"2.5 %", nsmall = 2), 
                                    ", ", format(tamp_gen$"97.5 %", nsmall = 2), "]", sep = "")

tamp_gen$"b [95% CI]" <- gsub("\\s+", " ", str_trim(tamp_gen$"b [95% CI]"))
tamp_gen$"b [95% CI]" <- gsub("\\[ ", "\\[", str_trim(tamp_gen$"b [95% CI]"))

# join -------------------------------------------------------------------

col2k <- c("term", "b [95% HDI]", "std.error",
           "b [95% CI]", "Std. Error", "z value",
           "decision", "ROPE", "inside.rope")

tamp_gen <- format(tamp_gen[,col2k], nsmall = 2)

tamp_gen$term <- c("Val",
                "Ordre",
                "RWA",
                "Val × Ordre",
                "Val × RWA",
                "Ordre × RWA",
                "Val × Ordre × RWA")

tamp_gen$decision <- ifelse(tamp_gen$decision == "reject", "oui",
                         ifelse(tamp_gen$decision == "accept", "non",
                         "indécision"))

tamp_gen <- tamp_gen %>%
  mutate(inside.rope = color_tile("lightgreen", "orange")(inside.rope))

tamp_gen$"n° &#946;" <- 1:length( tamp_gen$term)

ordc <- c("n° &#946;", "term", "b [95% HDI]", "std.error",
           "b [95% CI]", "Std. Error", "z value",
           "decision", "ROPE", "inside.rope")

tamp_gen <- tamp_gen[,ordc]

colnames(tamp_gen) <- c("n° &#946;", "Paramètre", "&#946;<sub>Bayes</sub> [95% HDI]",
                     "SE<sub>Bayes</sub>", "&#946;<sub>freq</sub> [95% CI]",
                     "SE<sub>freq</sub>", "z", 
                     "&#946 &#8800; 0", "ROPE", "% &#946;<sub>Bayes</sub> &sube; ROPE")

rownames(tamp_gen) <- NULL


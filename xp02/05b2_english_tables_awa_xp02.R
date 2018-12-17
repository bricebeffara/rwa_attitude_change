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

tawa_lme4 <- as.data.frame(model_awa_xp02_lme4[c(1:8),])
tawa_brms <- as.data.frame(model_awa_xp02[c(1:8),])
tawa_equi <- as.data.frame(equi_awa_xp02)

tawa_lme4$term <- tawa_brms$term
tawa_equi$term <- tawa_brms$term

# general regression ------------------------------------------------------

tawa <- join (tawa_brms, tawa_lme4, by = "term")
tawa <- join (tawa, tawa_equi, by ="term")

tawa$"b [95% HDI]" <- paste (format(tawa$estimate, nsmall = 2), " [", format(tawa$hdi.low, nsmall = 2), ", ",
                             format(tawa$hdi.high, nsmall = 2), "]", sep = "")

tawa$"b [95% HDI]" <- gsub("\\s+", " ", str_trim(tawa$"b [95% HDI]"))
tawa$"b [95% HDI]" <- gsub("\\[ ", "\\[", str_trim(tawa$"b [95% HDI]"))

ifelse ( grepl( "  \\d", tawa$"b [95% HDI]"), "  " == " ", tawa$"b [95% HDI]")

tawa$"b [95% CI]" <- paste (format( tawa$Estimate, nsmall = 2), " [", format(tawa$"2.5 %", nsmall = 2), ", ",
                            format(tawa$"97.5 %", nsmall = 2), "]", sep = "")

tawa$"b [95% CI]" <- gsub("\\s+", " ", str_trim(tawa$"b [95% CI]"))
tawa$"b [95% CI]" <- gsub("\\[ ", "\\[", str_trim(tawa$"b [95% CI]"))

# join -------------------------------------------------------------------

col2k <- c("term", "b [95% HDI]", "std.error",
           "b [95% CI]", "Std. Error", "t value",
           "decision", "ROPE", "inside.rope")

tawa$inside.rope <- as.numeric (tawa$inside.rope)

tawa$inside.rope <- round2 (tawa$inside.rope, n=2)

tawa <- format(tawa[,col2k], nsmall = 2)

t_all_awa <- tawa

t_all_awa$term <- c("Intercept",
                    "EC",
                    "Soc",
                    "RWA",
                    "EC × Soc",
                    "EC × RWA",
                    "Soc × RWA",
                    "EC × Soc × RWA")

t_all_awa$decision <- ifelse(t_all_awa$decision == "reject", "yes",
                             ifelse(t_all_awa$decision == "accept", "no",
                                    "indecision"))

t_all_awa <- t_all_awa %>%
  mutate(inside.rope = color_tile("lightgreen", "orange")(inside.rope))

t_all_awa$"n° &#946;" <- 1:length( t_all_awa$term)

ordc <- c("n° &#946;", "term", "b [95% HDI]", "std.error",
          "b [95% CI]", "Std. Error", "t value",
          "decision", "ROPE", "inside.rope")

t_all_awa <- t_all_awa[,ordc]

colnames(t_all_awa) <- c("n° &#946;", "Parameter", "&#946;<sub>Bayes</sub> [95% HDI]",
                         "SE<sub>Bayes</sub>", "&#946;<sub>freq</sub> [95% CI]",
                         "SE<sub>freq</sub>", "t", 
                         "&#946 &#8800; 0", "ROPE", "% &#946;<sub>Bayes</sub> &sube; ROPE")

rownames(t_all_awa) <- NULL






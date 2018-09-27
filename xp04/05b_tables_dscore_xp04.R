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

tgen_lme4 <- as.data.frame(tab_modD)
tgen_brms <- as.data.frame(tab_iat_d[c(1:4),])

tgen_lme4$term <- tgen_brms$term



# general regression ------------------------------------------------------

tgen <- join (tgen_brms, tgen_lme4, by = "term")

tgen$"b [95% HDI]" <- paste ( format( tgen$estimate, nsmall = 2), " [",  format( tgen$hdi.low, nsmall = 2), 
                              ", ", format(tgen$hdi.high, nsmall = 2), "]", sep = "")

tgen$"b [95% HDI]" <- gsub("\\s+", " ", str_trim(tgen$"b [95% HDI]"))
tgen$"b [95% HDI]" <- gsub("\\[ ", "\\[", str_trim(tgen$"b [95% HDI]"))

tgen$"b [95% CI]" <- paste ( format( tgen$Estimate, nsmall = 2), " [",  format( tgen$"2.5 %", nsmall = 2), 
                             ", ", format(tgen$"97.5 %", nsmall = 2), "]", sep = "")

tgen$"b [95% CI]" <- gsub("\\s+", " ", str_trim(tgen$"b [95% CI]"))
tgen$"b [95% CI]" <- gsub("\\[ ", "\\[", str_trim(tgen$"b [95% CI]"))


# join -------------------------------------------------------------------

col2k <- c("term", "b [95% HDI]", "std.error",
           "b [95% CI]", "Std. Error", "t value")

tgen <- format(tgen[,col2k], nsmall = 2)

tgen$term <- c("Cong",
                "Ordre",
                "RWA",
                "RWA × Ordre")

tgen$"n° &#946;" <- 1:length( tgen$term)

ordc <- c("n° &#946;", "term", "b [95% HDI]", "std.error",
          "b [95% CI]", "Std. Error", "t value")

tgen <- tgen[,ordc]

colnames(tgen) <- c("n° &#946;", "Paramètre", "&#946;<sub>Bayes</sub> [95% HDI]",
                     "SE<sub>Bayes</sub>", "&#946;<sub>freq</sub> [95% CI]",
                     "SE<sub>freq</sub>", "t")

rownames(tgen) <- NULL


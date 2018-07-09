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

tgen_lme4 <- as.data.frame(model_gen_xp08_lme4[c(2:8),])
tgen_brms <- as.data.frame(model_gen_xp08[c(9:15),])
tgen_equi <- as.data.frame(equi_gen_xp08)

tone_lme4 <- as.data.frame(model_one_xp08_lme4[c(2:8),])
tone_brms <- as.data.frame(model_one_xp08[c(9:15),])
tone_equi <- as.data.frame(equi_one_xp08)

tonetime_lme4 <- as.data.frame(model_onetime_xp08_lme4[c(2:8),])
tonetime_brms <- as.data.frame(model_onetime_xp08[c(9:15),])
tonetime_equi <- as.data.frame(equi_onetime_xp08)

ttimeone_lme4 <- as.data.frame(model_timeone_xp08_lme4[c(2:8),])
ttimeone_brms <- as.data.frame(model_timeone_xp08[c(9:15),])
ttimeone_equi <- as.data.frame(equi_timeone_xp08)

tctone_lme4 <- as.data.frame(model_ctone_xp08_lme4[c(2:8),])
tctone_brms <- as.data.frame(model_ctone_xp08[c(9:15),])
tctone_equi <- as.data.frame(equi_ctone_xp08)

tcttwo_lme4 <- as.data.frame(model_cttwo_xp08_lme4[c(2:8),])
tcttwo_brms <- as.data.frame(model_cttwo_xp08[c(9:15),])
tcttwo_equi <- as.data.frame(equi_cttwo_xp08)

tctonetime_lme4 <- as.data.frame(model_ctonetime_xp08_lme4[c(2:8),])
tctonetime_brms <- as.data.frame(model_ctonetime_xp08[c(9:15),])
tctonetime_equi <- as.data.frame(equi_ctonetime_xp08)

tcttimeone_lme4 <- as.data.frame(model_cttimeone_xp08_lme4[c(2:8),])
tcttimeone_brms <- as.data.frame(model_cttimeone_xp08[c(9:15),])
tcttimeone_equi <- as.data.frame(equi_cttimeone_xp08)

tctone_neg_lme4 <- as.data.frame(model_ctone_neg_xp08_lme4[c(2:8),])
tctone_neg_brms <- as.data.frame(model_ctone_neg_xp08[c(9:15),])
tctone_neg_equi <- as.data.frame(equi_ctone_neg_xp08)

tctone_pos_lme4 <- as.data.frame(model_ctone_pos_xp08_lme4[c(2:8),])
tctone_pos_brms <- as.data.frame(model_ctone_pos_xp08[c(9:15),])
tctone_pos_equi <- as.data.frame(equi_ctone_pos_xp08)

tcttwo_neg_lme4 <- as.data.frame(model_cttwo_neg_xp08_lme4[c(2:8),])
tcttwo_neg_brms <- as.data.frame(model_cttwo_neg_xp08[c(9:15),])
tcttwo_neg_equi <- as.data.frame(equi_cttwo_neg_xp08)

tcttwo_pos_lme4 <- as.data.frame(model_cttwo_pos_xp08_lme4[c(2:8),])
tcttwo_pos_brms <- as.data.frame(model_cttwo_pos_xp08[c(9:15),])
tcttwo_pos_equi <- as.data.frame(equi_cttwo_pos_xp08)

tctonetime_neg_lme4 <- as.data.frame(model_ctonetime_neg_xp08_lme4[c(2:8),])
tctonetime_neg_brms <- as.data.frame(model_ctonetime_neg_xp08[c(9:15),])
tctonetime_neg_equi <- as.data.frame(equi_ctonetime_neg_xp08)

tctonetime_pos_lme4 <- as.data.frame(model_ctonetime_pos_xp08_lme4[c(2:8),])
tctonetime_pos_brms <- as.data.frame(model_ctonetime_pos_xp08[c(9:15),])
tctonetime_pos_equi <- as.data.frame(equi_ctonetime_pos_xp08)

tcttimeone_neg_lme4 <- as.data.frame(model_ctimeone_neg_xp08_lme4[c(2:8),])
tcttimeone_neg_brms <- as.data.frame(model_timeone_neg_xp08[c(9:15),])
tcttimeone_neg_equi <- as.data.frame(equi_timeone_neg_xp08)

tcttimeone_pos_lme4 <- as.data.frame(model_ctimeone_pos_xp08_lme4[c(2:8),])
tcttimeone_pos_brms <- as.data.frame(model_timeone_pos_xp08[c(9:15),])
tcttimeone_pos_equi <- as.data.frame(equi_timeone_pos_xp08)


tgen_lme4$term <- tgen_brms$term
tone_lme4$term <- tone_brms$term 
tonetime_lme4$term <- tonetime_brms$term
ttimeone_lme4$term <- ttimeone_brms$term
tctone_lme4$term <- tctone_brms$term
tcttwo_lme4$term <- tcttwo_brms$term
tctonetime_lme4$term <- tctonetime_brms$term
tcttimeone_lme4$term <- tcttimeone_brms$term
tctone_neg_lme4$term <- tctone_neg_brms$term
tctone_pos_lme4$term <- tctone_pos_brms$term
tcttwo_neg_lme4$term <- tcttwo_neg_brms$term
tcttwo_pos_lme4$term <- tcttwo_pos_brms$term
tctonetime_neg_lme4$term <- tctonetime_neg_brms$term 
tctonetime_pos_lme4$term <- tctonetime_pos_brms$term
tcttimeone_neg_lme4$term <- tcttimeone_neg_brms$term
tcttimeone_pos_lme4$term <- tcttimeone_pos_brms$term

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

# Level 2 interaction 1 counter-conditioning vs. 2 ----------

tone_lme4 <- format(tone_lme4[tone_lme4$term == "b_usvalence.counter2.RWAscore",], nsmall = 2)
tone_brms <- format(tone_brms[tone_brms$term == "b_usvalence.counter2.RWAscore",], nsmall = 2)
tone_equi <- format(tone_equi[tone_equi$term == "b_usvalence.counter2.RWAscore",], nsmall = 2)

tone <- join (tone_brms, tone_lme4, by = "term")
tone <- join (tone, tone_equi, by ="term")

tone$"b [95% HDI]" <- paste ( format( tone$estimate, nsmall = 2), " [",  format( tone$hdi.low, nsmall = 2), 
                              ", ", format(tone$hdi.high, nsmall = 2), "]", sep = "")

tone$"b [95% CI]" <- paste ( format( tone$Estimate, nsmall = 2), " [",  format( tone$"2.5 %", nsmall = 2), 
                             ", ", format(tone$"97.5 %", nsmall = 2), "]", sep = "")

# Level 2 interaction 1 counter-conditioning + delay vs. 2 ----------

tonetime_lme4 <- format(tonetime_lme4[tonetime_lme4$term == "b_usvalence.counter2.RWAscore",], nsmall = 2)
tonetime_brms <- format(tonetime_brms[tonetime_brms$term == "b_usvalence.counter2.RWAscore",], nsmall = 2)
tonetime_equi <- format(tonetime_equi[tonetime_equi$term == "b_usvalence.counter2.RWAscore",], nsmall = 2)

tonetime <- join (tonetime_brms, tonetime_lme4, by = "term")
tonetime <- join (tonetime, tonetime_equi, by ="term")

tonetime$"b [95% HDI]" <- paste ( format( tonetime$estimate, nsmall = 2), " [",  format( tonetime$hdi.low, nsmall = 2), 
                                  ", ", format(tonetime$hdi.high, nsmall = 2), "]", sep = "")

tonetime$"b [95% CI]" <- paste ( format( tonetime$Estimate, nsmall = 2), " [",  format( tonetime$"2.5 %", nsmall = 2), 
                                 ", ", format(tonetime$"97.5 %", nsmall = 2), "]", sep = "")

# Level 2 interaction delay + 1 counter-conditioning vs. 2 ----------

ttimeone_lme4 <- format(ttimeone_lme4[ttimeone_lme4$term == "b_usvalence.counter2.RWAscore",], nsmall = 2)
ttimeone_brms <- format(ttimeone_brms[ttimeone_brms$term == "b_usvalence.counter2.RWAscore",], nsmall = 2)
ttimeone_equi <- format(ttimeone_equi[ttimeone_equi$term == "b_usvalence.counter2.RWAscore",], nsmall = 2)

ttimeone <- join (ttimeone_brms, ttimeone_lme4, by = "term")
ttimeone <- join (ttimeone, ttimeone_equi, by ="term")

ttimeone$"b [95% HDI]" <- paste ( format( ttimeone$estimate, nsmall = 2), " [",  format( ttimeone$hdi.low, nsmall = 2), 
                                  ", ", format(ttimeone$hdi.high, nsmall = 2), "]", sep = "")

ttimeone$"b [95% CI]" <- paste ( format( ttimeone$Estimate, nsmall = 2), " [",  format( ttimeone$"2.5 %", nsmall = 2), 
                                 ", ", format(ttimeone$"97.5 %", nsmall = 2), "]", sep = "")


# simple slope RWA * usvalence in the 1 counter-conditioning condition ----------

tctoneI_lme4 <- format(tctone_lme4[tctone_lme4$term == "b_usvalence.RWAscore",], nsmall = 2)
tctoneI_brms <- format(tctone_brms[tctone_brms$term == "b_usvalence.RWAscore",], nsmall = 2)
tctoneI_equi <- format(tctone_equi[tctone_equi$term == "b_usvalence.RWAscore",], nsmall = 2)

tctoneI <- join (tctoneI_brms, tctoneI_lme4, by = "term")
tctoneI <- join (tctoneI, tctoneI_equi, by ="term")

tctoneI$"b [95% HDI]" <- paste ( format( tctoneI$estimate, nsmall = 2), " [",  format( tctoneI$hdi.low, nsmall = 2), 
                                ", ", format(tctoneI$hdi.high, nsmall = 2), "]", sep = "")

tctoneI$"b [95% CI]" <- paste ( format( tctoneI$Estimate, nsmall = 2), " [",  format( tctoneI$"2.5 %", nsmall = 2), 
                               ", ", format(tctoneI$"97.5 %", nsmall = 2), "]", sep = "")


# simple slope RWA * usvalence in the 2 counter-conditionings condition ----------

tcttwoI_lme4 <- format(tcttwo_lme4[tcttwo_lme4$term == "b_usvalence.RWAscore",], nsmall = 2)
tcttwoI_brms <- format(tcttwo_brms[tcttwo_brms$term == "b_usvalence.RWAscore",], nsmall = 2)
tcttwoI_equi <- format(tcttwo_equi[tcttwo_equi$term == "b_usvalence.RWAscore",], nsmall = 2)

tcttwoI <- join (tcttwoI_brms, tcttwoI_lme4, by = "term")
tcttwoI <- join (tcttwoI, tcttwoI_equi, by ="term")

tcttwoI$"b [95% HDI]" <- paste ( format( tcttwoI$estimate, nsmall = 2), " [",  format( tcttwoI$hdi.low, nsmall = 2), 
                                ", ", format(tcttwoI$hdi.high, nsmall = 2), "]", sep = "")

tcttwoI$"b [95% CI]" <- paste ( format( tcttwoI$Estimate, nsmall = 2), " [",  format( tcttwoI$"2.5 %", nsmall = 2), 
                               ", ", format(tcttwoI$"97.5 %", nsmall = 2), "]", sep = "")

# simple slope RWA * usvalence in the 1 counter-conditioning + delay condition ----------

tctonetimeI_lme4 <- format(tctonetime_lme4[tctonetime_lme4$term == "b_usvalence.RWAscore",], nsmall = 2)
tctonetimeI_brms <- format(tctonetime_brms[tctonetime_brms$term == "b_usvalence.RWAscore",], nsmall = 2)
tctonetimeI_equi <- format(tctonetime_equi[tctonetime_equi$term == "b_usvalence.RWAscore",], nsmall = 2)

tctonetimeI <- join (tctonetimeI_brms, tctonetimeI_lme4, by = "term")
tctonetimeI <- join (tctonetimeI, tctonetimeI_equi, by ="term")

tctonetimeI$"b [95% HDI]" <- paste ( format( tctonetimeI$estimate, nsmall = 2), " [",  format( tctonetimeI$hdi.low, nsmall = 2), 
                                     ", ", format(tctonetimeI$hdi.high, nsmall = 2), "]", sep = "")

tctonetimeI$"b [95% CI]" <- paste ( format( tctonetimeI$Estimate, nsmall = 2), " [",  format( tctonetimeI$"2.5 %", nsmall = 2), 
                                    ", ", format(tctonetimeI$"97.5 %", nsmall = 2), "]", sep = "")


# simple slope RWA * usvalence in the delay + 1 counter-conditioning condition ----------

tcttimeoneI_lme4 <- format(tcttimeone_lme4[tcttimeone_lme4$term == "b_usvalence.RWAscore",], nsmall = 2)
tcttimeoneI_brms <- format(tcttimeone_brms[tcttimeone_brms$term == "b_usvalence.RWAscore",], nsmall = 2)
tcttimeoneI_equi <- format(tcttimeone_equi[tcttimeone_equi$term == "b_usvalence.RWAscore",], nsmall = 2)

tcttimeoneI <- join (tcttimeoneI_brms, tcttimeoneI_lme4, by = "term")
tcttimeoneI <- join (tcttimeoneI, tcttimeoneI_equi, by ="term")

tcttimeoneI$"b [95% HDI]" <- paste ( format( tcttimeoneI$estimate, nsmall = 2), " [",  format( tcttimeoneI$hdi.low, nsmall = 2), 
                                     ", ", format(tcttimeoneI$hdi.high, nsmall = 2), "]", sep = "")

tcttimeoneI$"b [95% CI]" <- paste ( format( tcttimeoneI$Estimate, nsmall = 2), " [",  format( tcttimeoneI$"2.5 %", nsmall = 2), 
                                    ", ", format(tcttimeoneI$"97.5 %", nsmall = 2), "]", sep = "")

# simple slope RWA 1 counter-conditioning + delay negative valence condition --------

tctonetime_neg_rwa_lme4 <- format(tctonetime_neg_lme4[tctonetime_neg_lme4$term == "b_RWAscore",], nsmall = 2)
tctonetime_neg_rwa_brms <- format(tctonetime_neg_brms[tctonetime_neg_brms$term == "b_RWAscore",], nsmall = 2)
tctonetime_neg_rwa_equi <- format(tctonetime_neg_equi[tctonetime_neg_equi$term == "b_RWAscore",], nsmall = 2)

tctonetime_neg_rwa <- join (tctonetime_neg_rwa_brms, tctonetime_neg_rwa_lme4, by = "term")
tctonetime_neg_rwa <- join (tctonetime_neg_rwa, tctonetime_neg_rwa_equi, by ="term")

tctonetime_neg_rwa$"b [95% HDI]" <- paste ( format( tctonetime_neg_rwa$estimate, nsmall = 2), " [",  format( tctonetime_neg_rwa$hdi.low, nsmall = 2), 
                                            ", ", format(tctonetime_neg_rwa$hdi.high, nsmall = 2), "]", sep = "")

tctonetime_neg_rwa$"b [95% CI]" <- paste ( format( tctonetime_neg_rwa$Estimate, nsmall = 2), " [",  format( tctonetime_neg_rwa$"2.5 %", nsmall = 2), 
                                           ", ", format(tctonetime_neg_rwa$"97.5 %", nsmall = 2), "]", sep = "")

# simple slope RWA 1 counter-conditioning + delay positive valence condition --------

tctonetime_pos_rwa_lme4 <- format(tctonetime_pos_lme4[tctonetime_pos_lme4$term == "b_RWAscore",], nsmall = 2)
tctonetime_pos_rwa_brms <- format(tctonetime_pos_brms[tctonetime_pos_brms$term == "b_RWAscore",], nsmall = 2)
tctonetime_pos_rwa_equi <- format(tctonetime_pos_equi[tctonetime_pos_equi$term == "b_RWAscore",], nsmall = 2)

tctonetime_pos_rwa <- join (tctonetime_pos_rwa_brms, tctonetime_pos_rwa_lme4, by = "term")
tctonetime_pos_rwa <- join (tctonetime_pos_rwa, tctonetime_pos_rwa_equi, by ="term")

tctonetime_pos_rwa$"b [95% HDI]" <- paste ( format( tctonetime_pos_rwa$estimate, nsmall = 2), " [",  format( tctonetime_pos_rwa$hdi.low, nsmall = 2), 
                                            ", ", format(tctonetime_pos_rwa$hdi.high, nsmall = 2), "]", sep = "")

tctonetime_pos_rwa$"b [95% CI]" <- paste ( format( tctonetime_pos_rwa$Estimate, nsmall = 2), " [",  format( tctonetime_pos_rwa$"2.5 %", nsmall = 2), 
                                           ", ", format(tctonetime_pos_rwa$"97.5 %", nsmall = 2), "]", sep = "")

# simple slope RWA delay + 1 counter-conditioning negative valence condition --------

tcttimeone_neg_rwa_lme4 <- format(tcttimeone_neg_lme4[tcttimeone_neg_lme4$term == "b_RWAscore",], nsmall = 2)
tcttimeone_neg_rwa_brms <- format(tcttimeone_neg_brms[tcttimeone_neg_brms$term == "b_RWAscore",], nsmall = 2)
tcttimeone_neg_rwa_equi <- format(tcttimeone_neg_equi[tcttimeone_neg_equi$term == "b_RWAscore",], nsmall = 2)

tcttimeone_neg_rwa <- join (tcttimeone_neg_rwa_brms, tcttimeone_neg_rwa_lme4, by = "term")
tcttimeone_neg_rwa <- join (tcttimeone_neg_rwa, tcttimeone_neg_rwa_equi, by ="term")

tcttimeone_neg_rwa$"b [95% HDI]" <- paste ( format( tcttimeone_neg_rwa$estimate, nsmall = 2), " [",  format( tcttimeone_neg_rwa$hdi.low, nsmall = 2), 
                                            ", ", format(tcttimeone_neg_rwa$hdi.high, nsmall = 2), "]", sep = "")

tcttimeone_neg_rwa$"b [95% CI]" <- paste ( format( tcttimeone_neg_rwa$Estimate, nsmall = 2), " [",  format( tcttimeone_neg_rwa$"2.5 %", nsmall = 2), 
                                           ", ", format(tcttimeone_neg_rwa$"97.5 %", nsmall = 2), "]", sep = "")

# simple slope RWA delay + 1 counter-conditioning positive valence condition --------

tcttimeone_pos_rwa_lme4 <- format(tcttimeone_pos_lme4[tcttimeone_pos_lme4$term == "b_RWAscore",], nsmall = 2)
tcttimeone_pos_rwa_brms <- format(tcttimeone_pos_brms[tcttimeone_pos_brms$term == "b_RWAscore",], nsmall = 2)
tcttimeone_pos_rwa_equi <- format(tcttimeone_pos_equi[tcttimeone_pos_equi$term == "b_RWAscore",], nsmall = 2)

tcttimeone_pos_rwa <- join (tcttimeone_pos_rwa_brms, tcttimeone_pos_rwa_lme4, by = "term")
tcttimeone_pos_rwa <- join (tcttimeone_pos_rwa, tcttimeone_pos_rwa_equi, by ="term")

tcttimeone_pos_rwa$"b [95% HDI]" <- paste ( format( tcttimeone_pos_rwa$estimate, nsmall = 2), " [",  format( tcttimeone_pos_rwa$hdi.low, nsmall = 2), 
                                            ", ", format(tcttimeone_pos_rwa$hdi.high, nsmall = 2), "]", sep = "")

tcttimeone_pos_rwa$"b [95% CI]" <- paste ( format( tcttimeone_pos_rwa$Estimate, nsmall = 2), " [",  format( tcttimeone_pos_rwa$"2.5 %", nsmall = 2), 
                                           ", ", format(tcttimeone_pos_rwa$"97.5 %", nsmall = 2), "]", sep = "")


# simple slope RWA 1 counter-conditioning negative valence condition --------

tctone_neg_rwa_lme4 <- format(tctone_neg_lme4[tctone_neg_lme4$term == "b_RWAscore",], nsmall = 2)
tctone_neg_rwa_brms <- format(tctone_neg_brms[tctone_neg_brms$term == "b_RWAscore",], nsmall = 2)
tctone_neg_rwa_equi <- format(tctone_neg_equi[tctone_neg_equi$term == "b_RWAscore",], nsmall = 2)

tctone_neg_rwa <- join (tctone_neg_rwa_brms, tctone_neg_rwa_lme4, by = "term")
tctone_neg_rwa <- join (tctone_neg_rwa, tctone_neg_rwa_equi, by ="term")

tctone_neg_rwa$"b [95% HDI]" <- paste ( format( tctone_neg_rwa$estimate, nsmall = 2), " [",  format( tctone_neg_rwa$hdi.low, nsmall = 2), 
                                       ", ", format(tctone_neg_rwa$hdi.high, nsmall = 2), "]", sep = "")

tctone_neg_rwa$"b [95% CI]" <- paste ( format( tctone_neg_rwa$Estimate, nsmall = 2), " [",  format( tctone_neg_rwa$"2.5 %", nsmall = 2), 
                                      ", ", format(tctone_neg_rwa$"97.5 %", nsmall = 2), "]", sep = "")

# simple slope RWA 1 counter-conditioning positive valence condition --------

tctone_pos_rwa_lme4 <- format(tctone_pos_lme4[tctone_pos_lme4$term == "b_RWAscore",], nsmall = 2)
tctone_pos_rwa_brms <- format(tctone_pos_brms[tctone_pos_brms$term == "b_RWAscore",], nsmall = 2)
tctone_pos_rwa_equi <- format(tctone_pos_equi[tctone_pos_equi$term == "b_RWAscore",], nsmall = 2)

tctone_pos_rwa <- join (tctone_pos_rwa_brms, tctone_pos_rwa_lme4, by = "term")
tctone_pos_rwa <- join (tctone_pos_rwa, tctone_pos_rwa_equi, by ="term")

tctone_pos_rwa$"b [95% HDI]" <- paste ( format( tctone_pos_rwa$estimate, nsmall = 2), " [",  format( tctone_pos_rwa$hdi.low, nsmall = 2), 
                                       ", ", format(tctone_pos_rwa$hdi.high, nsmall = 2), "]", sep = "")

tctone_pos_rwa$"b [95% CI]" <- paste ( format( tctone_pos_rwa$Estimate, nsmall = 2), " [",  format( tctone_pos_rwa$"2.5 %", nsmall = 2), 
                                      ", ", format(tctone_pos_rwa$"97.5 %", nsmall = 2), "]", sep = "")

# simple slope RWA 2 counter-conditionings negative valence condition --------

tcttwo_neg_rwa_lme4 <- format(tcttwo_neg_lme4[tcttwo_neg_lme4$term == "b_RWAscore",], nsmall = 2)
tcttwo_neg_rwa_brms <- format(tcttwo_neg_brms[tcttwo_neg_brms$term == "b_RWAscore",], nsmall = 2)
tcttwo_neg_rwa_equi <- format(tcttwo_neg_equi[tcttwo_neg_equi$term == "b_RWAscore",], nsmall = 2)

tcttwo_neg_rwa <- join (tcttwo_neg_rwa_brms, tcttwo_neg_rwa_lme4, by = "term")
tcttwo_neg_rwa <- join (tcttwo_neg_rwa, tcttwo_neg_rwa_equi, by ="term")

tcttwo_neg_rwa$"b [95% HDI]" <- paste ( format( tcttwo_neg_rwa$estimate, nsmall = 2), " [",  format( tcttwo_neg_rwa$hdi.low, nsmall = 2), 
                                       ", ", format(tcttwo_neg_rwa$hdi.high, nsmall = 2), "]", sep = "")

tcttwo_neg_rwa$"b [95% CI]" <- paste ( format( tcttwo_neg_rwa$Estimate, nsmall = 2), " [",  format( tcttwo_neg_rwa$"2.5 %", nsmall = 2), 
                                      ", ", format(tcttwo_neg_rwa$"97.5 %", nsmall = 2), "]", sep = "")

# simple slope RWA 2 counter-conditionings positive valence condition --------

tcttwo_pos_rwa_lme4 <- format(tcttwo_pos_lme4[tcttwo_pos_lme4$term == "b_RWAscore",], nsmall = 2)
tcttwo_pos_rwa_brms <- format(tcttwo_pos_brms[tcttwo_pos_brms$term == "b_RWAscore",], nsmall = 2)
tcttwo_pos_rwa_equi <- format(tcttwo_pos_equi[tcttwo_pos_equi$term == "b_RWAscore",], nsmall = 2)

tcttwo_pos_rwa <- join (tcttwo_pos_rwa_brms, tcttwo_pos_rwa_lme4, by = "term")
tcttwo_pos_rwa <- join (tcttwo_pos_rwa, tcttwo_pos_rwa_equi, by ="term")

tcttwo_pos_rwa$"b [95% HDI]" <- paste ( format( tcttwo_pos_rwa$estimate, nsmall = 2), " [",  format( tcttwo_pos_rwa$hdi.low, nsmall = 2), 
                                       ", ", format(tcttwo_pos_rwa$hdi.high, nsmall = 2), "]", sep = "")

tcttwo_pos_rwa$"b [95% CI]" <- paste ( format( tcttwo_pos_rwa$Estimate, nsmall = 2), " [",  format( tcttwo_pos_rwa$"2.5 %", nsmall = 2), 
                                      ", ", format(tcttwo_pos_rwa$"97.5 %", nsmall = 2), "]", sep = "")


# join -------------------------------------------------------------------

col2k <- c("term", "b [95% HDI]", "std.error",
           "b [95% CI]", "Std. Error", "t value",
           "decision", "ROPE", "inside.rope")

tgen <- format(tgen[,col2k], nsmall = 2)
tone <- format(tone[,col2k], nsmall = 2)
tonetime <- format(tonetime[,col2k], nsmall = 2)
ttimeone <- format(ttimeone[,col2k], nsmall = 2)
tctoneI <- format(tctoneI[,col2k], nsmall = 2)
tcttwoI <- format(tcttwoI[,col2k], nsmall = 2)
tctonetimeI <- format(tctonetimeI[,col2k], nsmall = 2)
tcttimeoneI <- format(tcttimeoneI[,col2k], nsmall = 2)
tctone_neg_rwa <- format(tctone_neg_rwa[,col2k], nsmall = 2)
tctone_pos_rwa <- format(tctone_pos_rwa[,col2k], nsmall = 2)
tcttwo_neg_rwa <- format(tcttwo_neg_rwa[,col2k], nsmall = 2)
tcttwo_pos_rwa <- format(tcttwo_pos_rwa[,col2k], nsmall = 2)
tctonetime_neg_rwa <- format(tctonetime_neg_rwa[,col2k], nsmall = 2)
tctonetime_pos_rwa <- format(tctonetime_pos_rwa[,col2k], nsmall = 2)
tcttimeone_neg_rwa <- format(tcttimeone_neg_rwa[,col2k], nsmall = 2)
tcttimeone_pos_rwa <- format(tcttimeone_pos_rwa[,col2k], nsmall = 2)

t_all <- rbind(tgen,
               tone,
               tonetime,
               ttimeone,
               tctoneI,
               tcttwoI,
               tctonetimeI,
               tcttimeoneI,
               tctone_neg_rwa,
               tctone_pos_rwa,
               tcttwo_neg_rwa,
               tcttwo_pos_rwa,
               tctonetime_neg_rwa,
               tctonetime_pos_rwa,
               tcttimeone_neg_rwa,
               tcttimeone_pos_rwa)

t_all$term <- c("Val",
                "Scc<sub>constraste</sub>",
                "RWA",
                "Val × Scc<sub>constraste</sub>",
                "Val × RWA",
                "Scc<sub>constraste</sub> × RWA",
                "Val ×  Scc<sub>constraste</sub> × RWA",
                "Val ×  Scc<sub>1 vs. 2</sub> × RWA",
                "Val ×  Scc<sub>1 + t vs. 2</sub> × RWA",
                "Val ×  Scc<sub>t + 1 vs. 2</sub> × RWA",
                "(Val × RWA) <sub>1 cc</sub>",
                "(Val × RWA) <sub>2 cc</sub>",
                "(Val × RWA) <sub>1 cc + t</sub>",
                "(Val × RWA) <sub>t + 1 cc</sub>",
                "RWA <sub>1 cc neg</sub>",
                "RWA <sub>1 cc pos</sub>",
                "RWA <sub>2 cc neg</sub>",
                "RWA <sub>2 cc pos</sub>",
                "RWA <sub>1 cc + t neg</sub>",
                "RWA <sub>1 cc + t pos</sub>",
                "RWA <sub>t + 1 cc neg</sub>",
                "RWA <sub>t + 1 cc pos</sub>")

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


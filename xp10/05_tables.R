p_load(plyr,
       knitr,
       kableExtra,
       formattable,
       dplyr,
       webshot,
       install = TRUE,
       update = getOption("pac_update"),
       character.only = FALSE)

tgen_lme4 <- as.data.frame(model_gen_xp10_lme4[c(2:8),])
tgen_brms <- as.data.frame(model_gen_xp10[c(9:15),])
tgen_equi <- as.data.frame(equi_gen_xp10)

tuspos_lme4 <- as.data.frame(model_uspos_xp10_lme4[c(2:8),])
tuspos_brms <- as.data.frame(model_uspos_xp10[c(9:15),])
tuspos_equi <- as.data.frame(equi_uspos_xp10)

tusneg_lme4 <- as.data.frame(model_usneg_xp10_lme4[c(2:8),])
tusneg_brms <- as.data.frame(model_usneg_xp10[c(9:15),])
tusneg_equi <- as.data.frame(equi_usneg_xp10)

tdir_lme4 <- as.data.frame(model_dir_xp10_lme4[c(2:8),])
tdir_brms <- as.data.frame(model_dir_xp10[c(9:15),])
tdir_equi <- as.data.frame(equi_dir_xp10)

tind_lme4 <- as.data.frame(model_ind_xp10_lme4[c(2:8),])
tind_brms <- as.data.frame(model_ind_xp10[c(9:15),])
tind_equi <- as.data.frame(equi_ind_xp10)

tuspos_dir_lme4 <- as.data.frame(model_uspos_dir_xp10_lme4[c(2:8),])
tuspos_dir_brms <- as.data.frame(model_uspos_dir_xp10[c(9:15),])
tuspos_dir_equi <- as.data.frame(equi_uspos_dir_xp10)

tuspos_ind_lme4 <- as.data.frame(model_uspos_ind_xp10_lme4[c(2:8),])
tuspos_ind_brms <- as.data.frame(model_uspos_ind_xp10[c(9:15),])
tuspos_ind_equi <- as.data.frame(equi_uspos_ind_xp10)


tgen_lme4$term <- tgen_brms$term
tuspos_lme4$term <- tuspos_brms$term
tusneg_lme4$term <- tusneg_brms$term
tdir_lme4$term <- tdir_brms$term
tind_lme4$term <- tind_brms$term
tuspos_dir_lme4$term <- tuspos_dir_brms$term
tuspos_ind_lme4$term <- tuspos_ind_brms$term

# general regression ------------------------------------------------------

tgen <- join (tgen_brms, tgen_lme4, by = "term")
tgen <- join (tgen, tgen_equi, by ="term")

tgen$"b [95% HDI]" <- paste (tgen$estimate, " ", "[", tgen$hdi.low, ",",
                                      " ", tgen$hdi.high, "]", sep = "")

tgen$"b [95% CI]" <- paste (tgen$Estimate, " ", "[", tgen$"2.5 %", ",",
                                    " ", tgen$"97.5 %", "]", sep = "")

# simple slope valence in the level 1 conditioning condition ----------

tdirA_lme4 <- tdir_lme4[tdir_lme4$term == "b_usvalence",]
tdirA_brms <- tdir_brms[tdir_brms$term == "b_usvalence",]
tdirA_equi <- tdir_equi[tdir_equi$term == "b_usvalence",]

tdirA <- join (tdirA_brms, tdirA_lme4, by = "term")
tdirA <- join (tdirA, tdirA_equi, by ="term")

tdirA$"b [95% HDI]" <- paste (tdirA$estimate, " ", "[", tdirA$hdi.low, ",",
                              " ", tdirA$hdi.high, "]", sep = "")

tdirA$"b [95% CI]" <- paste (tdirA$Estimate, " ", "[", tdirA$"2.5 %", ",",
                             " ", tdirA$"97.5 %", "]", sep = "")

# simple slope RWA in the level 1 conditioning condition ----------

tdirB_lme4 <- tdir_lme4[tdir_lme4$term == "b_RWAscore",]
tdirB_brms <- tdir_brms[tdir_brms$term == "b_RWAscore",]
tdirB_equi <- tdir_equi[tdir_equi$term == "b_RWAscore",]

tdirB <- join (tdirB_brms, tdirB_lme4, by = "term")
tdirB <- join (tdirB, tdirB_equi, by ="term")

tdirB$"b [95% HDI]" <- paste (tdirB$estimate, " ", "[", tdirB$hdi.low, ",",
                              " ", tdirB$hdi.high, "]", sep = "")

tdirB$"b [95% CI]" <- paste (tdirB$Estimate, " ", "[", tdirB$"2.5 %", ",",
                             " ", tdirB$"97.5 %", "]", sep = "")

# simple slope valence in the level 2 conditioning condition ----------

tindA_lme4 <- tind_lme4[tind_lme4$term == "b_usvalence",]
tindA_brms <- tind_brms[tind_brms$term == "b_usvalence",]
tindA_equi <- tind_equi[tind_equi$term == "b_usvalence",]

tindA <- join (tindA_brms, tindA_lme4, by = "term")
tindA <- join (tindA, tindA_equi, by ="term")

tindA$"b [95% HDI]" <- paste (tindA$estimate, " ", "[", tindA$hdi.low, ",",
                              " ", tindA$hdi.high, "]", sep = "")

tindA$"b [95% CI]" <- paste (tindA$Estimate, " ", "[", tindA$"2.5 %", ",",
                             " ", tindA$"97.5 %", "]", sep = "")

# simple slope RWA in the level 2 conditioning condition ----------

tindB_lme4 <- tind_lme4[tind_lme4$term == "b_RWAscore",]
tindB_brms <- tind_brms[tind_brms$term == "b_RWAscore",]
tindB_equi <- tind_equi[tind_equi$term == "b_RWAscore",]

tindB <- join (tindB_brms, tindB_lme4, by = "term")
tindB <- join (tindB, tindB_equi, by ="term")

tindB$"b [95% HDI]" <- paste (tindB$estimate, " ", "[", tindB$hdi.low, ",",
                              " ", tindB$hdi.high, "]", sep = "")

tindB$"b [95% CI]" <- paste (tindB$Estimate, " ", "[", tindB$"2.5 %", ",",
                             " ", tindB$"97.5 %", "]", sep = "")

# simple slope RWA * spreading in the positive valence condition ----------

tuspos_lme4 <- tuspos_lme4[tuspos_lme4$term == "b_spreading.RWAscore",]
tuspos_brms <- tuspos_brms[tuspos_brms$term == "b_spreading.RWAscore",]
tuspos_equi <- tuspos_equi[tuspos_equi$term == "b_spreading.RWAscore",]

tuspos <- join (tuspos_brms, tuspos_lme4, by = "term")
tuspos <- join (tuspos, tuspos_equi, by ="term")

tuspos$"b [95% HDI]" <- paste (tuspos$estimate, " ", "[", tuspos$hdi.low, ",",
                             " ", tuspos$hdi.high, "]", sep = "")

tuspos$"b [95% CI]" <- paste (tuspos$Estimate, " ", "[", tuspos$"2.5 %", ",",
                              " ", tuspos$"97.5 %", "]", sep = "")

# simple slope RWA * spreading in the negative valence condition ----------

tusneg_lme4 <- tusneg_lme4[tusneg_lme4$term == "b_spreading.RWAscore",]
tusneg_brms <- tusneg_brms[tusneg_brms$term == "b_spreading.RWAscore",]
tusneg_equi <- tusneg_equi[tusneg_equi$term == "b_spreading.RWAscore",]

tusneg <- join (tusneg_brms, tusneg_lme4, by = "term")
tusneg <- join (tusneg, tusneg_equi, by ="term")

tusneg$"b [95% HDI]" <- paste (tusneg$estimate, " ", "[", tusneg$hdi.low, ",",
                               " ", tusneg$hdi.high, "]", sep = "")

tusneg$"b [95% CI]" <- paste (tusneg$Estimate, " ", "[", tusneg$"2.5 %", ",",
                              " ", tusneg$"97.5 %", "]", sep = "")


# simple slope RWA positive valence level 1 conditioning condition --------

tuspos_dir_lme4 <- tuspos_dir_lme4[tuspos_dir_lme4$term == "b_RWAscore",]
tuspos_dir_brms <- tuspos_dir_brms[tuspos_dir_brms$term == "b_RWAscore",]
tuspos_dir_equi <- tuspos_dir_equi[tuspos_dir_equi$term == "b_RWAscore",]

tuspos_dir <- join (tuspos_dir_brms, tuspos_dir_lme4, by = "term")
tuspos_dir <- join (tuspos_dir, tuspos_dir_equi, by ="term")

tuspos_dir$"b [95% HDI]" <- paste (tuspos_dir$estimate, " ", "[", tuspos_dir$hdi.low, ",",
                                   " ", tuspos_dir$hdi.high, "]", sep = "")

tuspos_dir$"b [95% CI]" <- paste (tuspos_dir$Estimate, " ", "[", tuspos_dir$"2.5 %", ",",
                                  " ", tuspos_dir$"97.5 %", "]", sep = "")

# simple slope RWA positive valence level 2 conditioning condition --------

tuspos_ind_lme4 <- tuspos_ind_lme4[tuspos_ind_lme4$term == "b_RWAscore",]
tuspos_ind_brms <- tuspos_ind_brms[tuspos_ind_brms$term == "b_RWAscore",]
tuspos_ind_equi <- tuspos_ind_equi[tuspos_ind_equi$term == "b_RWAscore",]

tuspos_ind <- join (tuspos_ind_brms, tuspos_ind_lme4, by = "term")
tuspos_ind <- join (tuspos_ind, tuspos_ind_equi, by ="term")

tuspos_ind$"b [95% HDI]" <- paste (tuspos_ind$estimate, " ", "[", tuspos_ind$hdi.low, ",",
                                   " ", tuspos_ind$hdi.high, "]", sep = "")

tuspos_ind$"b [95% CI]" <- paste (tuspos_ind$Estimate, " ", "[", tuspos_ind$"2.5 %", ",",
                                  " ", tuspos_ind$"97.5 %", "]", sep = "")


# join -------------------------------------------------------------------

col2k <- c("term", "b [95% HDI]", "std.error",
           "b [95% CI]", "Std. Error", "t value",
           "decision", "inside.rope")

tgen <- tgen[,col2k]
tdirA <- tdirA[,col2k]
tindA <- tindA[,col2k]
tdirB <- tdirB[,col2k]
tindB <- tindB[,col2k]
tuspos <- tuspos[,col2k]
tusneg <- tusneg[,col2k]
tuspos_dir <- tuspos_dir[,col2k]
tuspos_ind <- tuspos_ind[,col2k]

t_all <- rbind(tgen, tdirA, tindA, tdirB, tindB, tuspos, tusneg, tuspos_dir, tuspos_ind)

t_all$term <- c("Val",
                "Lev",
                "RWA",
                "Val × Lev",
                "Val × RWA",
                "Lev × RWA",
                "Val × Lev × RWA",
                "Val <sub>lev 1</sub>",
                "Val <sub>lev 2</sub>",
                "RWA <sub>lev 1</sub>",
                "RWA <sub>lev 2</sub>",
                "(Lev × RWA) <sub>pos</sub>",
                "(Lev × RWA) <sub>neg</sub>",
                "RWA <sub>pos lev 1</sub>",
                "RWA <sub>pos lev 2</sub>")

t_all$decision <- ifelse(t_all$decision == "reject", "oui",
                         ifelse(t_all$decision == "accept", "non",
                         "indécision"))

t_all <- t_all %>%
  mutate(inside.rope = color_tile("lightgreen", "orange")(inside.rope))

colnames(t_all) <- c("Paramètre", "&#946;<sub>Bayes</sub> [95% HDI]",
                     "SE<sub>Bayes</sub>", "&#946;<sub>freq</sub> [95% CI]",
                     "SE<sub>freq</sub>", "t", 
                     "&#946 &#8800; 0", "% &#946;<sub>Bayes</sub> dans rope")






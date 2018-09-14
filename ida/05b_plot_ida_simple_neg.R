############################
####### FOREST PLOTS #######
############################

# Loading packages needed (and installing if necessary) for this part

if (!require("pacman")) install.packages("pacman")
p_load(ggridges,
       install = TRUE,
       update = getOption("pac_update"),
       character.only = FALSE)

#### Forest plot for RWA simple slopes in negative condition ####

datIDAneg <- data.frame( value = c(posterior_samples(XP01_resp_neg, "b")$"b_RWAscore",
                                posterior_samples(XP02_resp_neg, "b")$"b_RWAscore",
                                posterior_samples(XP03_resp_neg, "b")$"b_RWAscore",
                                posterior_samples(XP04_resp_neg, "b")$"b_RWAscore",
                                posterior_samples(XP05_resp_neg, "b")$"b_RWAscore",
                                posterior_samples(XP06_resp_neg, "b")$"b_RWAscore",
                                posterior_samples(XP07_resp_neg, "b")$"b_RWAscore",
                                posterior_samples(XP08_resp_neg, "b")$"b_RWAscore",
                                posterior_samples(XP09_resp_neg, "b")$"b_RWAscore",
                                posterior_samples(XP11_resp_neg, "b")$"b_RWAscore",
                                posterior_samples(IDA_resp_neg, "b")$"b_RWAscore"
),
type = c(rep("Expérience 1",4000),rep("Expérience 2",4000),
         rep("Expérience 3",4000),rep("Expérience 4",4000),
         rep("Expérience 5",4000),rep("Expérience 6",4000),
         rep("Expérience 7",4000),rep("Expérience 8",4000),
         rep("Expérience 9",4000),rep("Expérience 10",4000),
         rep("Analyse intégrative",4000)
)
)


# load fonts and themes
hrbrthemes::import_roboto_condensed()
loadfonts()

hdi_neg01 <- HDInterval::hdi(posterior_samples(XP01_resp_neg, "b")$"b_RWAscore",
                credMass = 0.95)
hdi_neg01m <- mean(posterior_samples(XP01_resp_neg, "b")$"b_RWAscore",
               credMass = 0.95)

hdi_neg02 <- HDInterval::hdi(posterior_samples(XP02_resp_neg, "b")$"b_RWAscore",
                         credMass = 0.95)
hdi_neg02m <- mean(posterior_samples(XP02_resp_neg, "b")$"b_RWAscore",
               credMass = 0.95)

hdi_neg03 <- HDInterval::hdi(posterior_samples(XP03_resp_neg, "b")$"b_RWAscore",
                        credMass = 0.95)
hdi_neg03m <- mean(posterior_samples(XP03_resp_neg, "b")$"b_RWAscore",
               credMass = 0.95)

hdi_neg04 <- HDInterval::hdi(posterior_samples(XP04_resp_neg, "b")$"b_RWAscore",
                         credMass = 0.95)
hdi_neg04m <- mean(posterior_samples(XP04_resp_neg, "b")$"b_RWAscore",
               credMass = 0.95)

hdi_neg05 <- HDInterval::hdi(posterior_samples(XP05_resp_neg, "b")$"b_RWAscore",
                         credMass = 0.95)
hdi_neg05m <- mean(posterior_samples(XP05_resp_neg, "b")$"b_RWAscore",
               credMass = 0.95)

hdi_neg06 <- HDInterval::hdi(posterior_samples(XP06_resp_neg, "b")$"b_RWAscore",
                         credMass = 0.95)
hdi_neg06m <- mean(posterior_samples(XP06_resp_neg, "b")$"b_RWAscore",
               credMass = 0.95)

hdi_neg07 <- HDInterval::hdi(posterior_samples(XP07_resp_neg, "b")$"b_RWAscore",
                         credMass = 0.95)
hdi_neg07m <- mean(posterior_samples(XP07_resp_neg, "b")$"b_RWAscore",
               credMass = 0.95)

hdi_neg08 <- HDInterval::hdi(posterior_samples(XP08_resp_neg, "b")$"b_RWAscore",
                         credMass = 0.95)
hdi_neg08m <- mean(posterior_samples(XP08_resp_neg, "b")$"b_RWAscore",
               credMass = 0.95)

hdi_neg09 <- HDInterval::hdi(posterior_samples(XP09_resp_neg, "b")$"b_RWAscore",
                         credMass = 0.95)
hdi_neg09m <- mean(posterior_samples(XP09_resp_neg, "b")$"b_RWAscore",
               credMass = 0.95)

hdi_neg11 <- HDInterval::hdi(posterior_samples(XP11_resp_neg, "b")$"b_RWAscore",
                         credMass = 0.95)
hdi_neg11m <- mean(posterior_samples(XP11_resp_neg, "b")$"b_RWAscore",
               credMass = 0.95)

hdi_negIDA <- HDInterval::hdi(posterior_samples(IDA_resp_neg, "b")$"b_RWAscore",
                         credMass = 0.95)
hdi_negIDAm <- mean(posterior_samples(IDA_resp_neg, "b")$"b_RWAscore",
               credMass = 0.95)

nbppt01 <- length(unique(XP01$ppt))
nbppt02 <- length(unique(XP02$ppt))
nbppt03 <- length(unique(XP03$ppt))
nbppt04 <- length(unique(XP04$ppt))
nbppt05 <- length(unique(XP05$ppt))
nbppt06 <- length(unique(XP06$ppt))
nbppt07 <- length(unique(XP07$ppt))
nbppt08 <- length(unique(XP08$ppt))
nbppt09 <- length(unique(XP09$ppt))
nbppt10 <- length(unique(XP11$ppt))
nbpptIDA <- length(unique(IDA$ppt))


plot_IDAneg <- datIDAneg %>% 
  mutate(type = factor(type, levels = rev(unique(datIDAneg$type)))) %>% 
  ggplot(aes(x=value, y=type, fill=factor(..quantile..))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE, quantiles = c(0.025, 0.975),
                      scale = 0.9, rel_min_height = 0.01, color = "#A0A0A0A0") +
  scale_fill_manual(name = "Probability", values = c("#A0A0A0A0", "#A0A0A0A0", "#A0A0A0A0"),
                    labels = c("(0, 0.025]", "(0.025, 0.975]", "(0.975, 1]")) +
  annotate("rect", xmin = -0.7, xmax = 0.8, ymin = 1.94, ymax = 1.96, fill = "black", alpha = 0.7) +
  annotate("rect", xmin = -0.1, xmax = 0.1, ymin = 0, ymax = Inf, fill = "blue", alpha = 0.2) +
  annotate("errorbarh", y = 11, xmin = hdi_neg01[1], xmax = hdi_neg01[2], colour = "grey10", size = 0.6, height = 0) +
  annotate("point", y = 11, x = hdi_neg01m, colour = "grey10", size = 1.2) +
  annotate("errorbarh", y = 10, xmin = hdi_neg02[1], xmax = hdi_neg02[2], colour = "grey10", size = 0.6, height = 0) +
  annotate("point", y = 10, x = hdi_neg02m, colour = "grey10", size = 1.2) +
  annotate("errorbarh", y = 9, xmin = hdi_neg03[1], xmax = hdi_neg03[2], colour = "grey10", size = 0.6, height = 0) +
  annotate("point", y = 9, x = hdi_neg03m, colour = "grey10", size = 1.2) +
  annotate("errorbarh", y = 8, xmin = hdi_neg04[1], xmax = hdi_neg04[2], colour = "grey10", size = 0.6, height = 0) +
  annotate("point", y = 8, x = hdi_neg04m, colour = "grey10", size = 1.2) +
  annotate("errorbarh", y = 7, xmin = hdi_neg05[1], xmax = hdi_neg05[2], colour = "grey10", size = 0.6, height = 0) +
  annotate("point", y = 7, x = hdi_neg05m, colour = "grey10", size = 1.2) +
  annotate("errorbarh", y = 6, xmin = hdi_neg06[1], xmax = hdi_neg06[2], colour = "grey10", size = 0.6, height = 0) +
  annotate("point", y = 6, x = hdi_neg06m, colour = "grey10", size = 1.2) +
  annotate("errorbarh", y = 5, xmin = hdi_neg07[1], xmax = hdi_neg07[2], colour = "grey10", size = 0.6, height = 0) +
  annotate("point", y = 5, x = hdi_neg07m, colour = "grey10", size = 1.2) +
  annotate("errorbarh", y = 4, xmin = hdi_neg08[1], xmax = hdi_neg08[2], colour = "grey10", size = 0.6, height = 0) +
  annotate("point", y = 4, x = hdi_neg08m, colour = "grey10", size = 1.2) +
  annotate("errorbarh", y = 3, xmin = hdi_neg09[1], xmax = hdi_neg09[2], colour = "grey10", size = 0.6, height = 0) +
  annotate("point", y = 3, x = hdi_neg09m, colour = "grey10", size = 1.2) +
  annotate("errorbarh", y = 2, xmin = hdi_neg11[1], xmax = hdi_neg11[2], colour = "grey10", size = 0.6, height = 0) +
  annotate("point", y = 2, x = hdi_neg11m, colour = "grey10", size = 1.2) +
  annotate("errorbarh", y = 1, xmin = hdi_negIDA[1], xmax = hdi_negIDA[2], colour = "grey10", size = 0.6, height = 0) +
  annotate("point", y = 1, x = hdi_negIDAm, colour = "grey10", size = 1.2) +
  annotate("text", label = paste("N = ", nbppt01), x = 0.85, y = 11.2, colour = "grey10", size = 3.2, family = font_rc_light) +
  annotate("text", label = paste("N = ", nbppt02), x = 0.85, y = 10.2, colour = "grey10", size = 3.2, family = font_rc_light) +
  annotate("text", label = paste("N = ", nbppt03), x = 0.85, y = 09.2, colour = "grey10", size = 3.2, family = font_rc_light) +
  annotate("text", label = paste("N = ", nbppt04), x = 0.85, y = 08.2, colour = "grey10", size = 3.2, family = font_rc_light) +
  annotate("text", label = paste("N = ", nbppt05), x = 0.85, y = 07.2, colour = "grey10", size = 3.2, family = font_rc_light) +
  annotate("text", label = paste("N = ", nbppt06), x = 0.85, y = 06.2, colour = "grey10", size = 3.2, family = font_rc_light) +
  annotate("text", label = paste("N = ", nbppt07), x = 0.85, y = 05.2, colour = "grey10", size = 3.2, family = font_rc_light) +
  annotate("text", label = paste("N = ", nbppt08), x = 0.85, y = 04.2, colour = "grey10", size = 3.2, family = font_rc_light) +
  annotate("text", label = paste("N = ", nbppt09), x = 0.85, y = 03.2, colour = "grey10", size = 3.2, family = font_rc_light) +
  annotate("text", label = paste("N = ", nbppt10), x = 0.85, y = 02.2, colour = "grey10", size = 3.2, family = font_rc_light) +
  annotate("text", label = paste("N = ", nbpptIDA), x = 0.85, y = 01.2, colour = "grey10", size = 3.2, family = font_rc_light) +
  geom_vline(xintercept = 0, linetype="dashed", color = "red") + 
  labs(x = expression(beta[RWA["Valence négative"]]),
       y = "Expérience")+
  scale_x_continuous(breaks=seq(-0.75, 0.75, 0.25)) +
  theme_ipsum_rc(base_size = 13,
                 subtitle_size = 20,
                 axis_title_size = 15) +
  theme(strip.background = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="none")

plot_IDAneg
ggsave("plots/forest_simple_neg.jpg", width = 35, height = 20, units = "cm")
ggsave("plots/forest_simple_neg.pdf", width = 35, height = 20, units = "cm")

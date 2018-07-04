
load_df$Charge <- ifelse (load_df$load == -0.5, "Sans", "Avec")

# negative plot -----------------------------------------------------------

load_neg <- subset(load_df, usvalence==-0.5)

data_plot_neg = load_neg %>%
  ggplot(aes(x = as.character(response), y = RWAscore,
             fill = Charge, color = Charge)) +
  #geom_boxplot(position = position_dodge(width = -1.4), alpha = 1, width = 0.1, outlier.shape = NA) +
  geom_point(position = position_jitterdodge(jitter.width = 0.2, jitter.height = 0.05, 
                                             dodge.width = -0.75), alpha = 0.4, size = 1,
             shape = 19, inherit.aes = TRUE) +
  #stat_summary(fun.y = "median", geom = "point", size = 3, color="#ff738a",
  #shape = "25"|", position = position_dodge(width = -1.4), alpha = 1) +
  #scale_discrete_manual(aesthetics = "point_shape", values = c(-0.5, 0.5)) +
  labs(x = 'Évaluations', y = 'RWA', fill="Charge cognitive", color="Charge cognitive") +
  scale_fill_manual(values=c("#73a5ff", "#50ce76")) +
  scale_color_manual(values= c("#73a5ff", "#50ce76"), guide = "none") +
  coord_cartesian(ylim=c(1,9)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10), expand=c(0.01,0)) +
  labs(subtitle="Contre-conditionnement positif") +
  theme_ipsum_rc(base_size = 13,
                 subtitle_size = 20,
                 axis_title_size = 15) +
  guides(fill = guide_legend(override.aes = list(linetype = 0)),
         color = guide_legend(override.aes = list(linetype = 0))) +
  coord_flip()

data_plot_neg <- ggMarginal(data_plot_neg, margins = "x", alpha = 0.6,
                         type = "histogram", size = 4, fill = "gray", colour = "lightgray")
  

# positive plot -----------------------------------------------------------
  
load_pos <- subset(load_df, usvalence==0.5)

data_plot_pos = load_pos %>%
  ggplot(aes(x = as.character(response), y = RWAscore,
             fill = Charge, color = Charge)) +
  #geom_boxplot(position = position_dodge(width = -1.4), alpha = 1, width = 0.1, outlier.shape = NA) +
  geom_point(position = position_jitterdodge(jitter.width = 0.2, jitter.height = 0.05, 
                                             dodge.width = -0.75), alpha = 0.4, size = 1,
             shape = 19, inherit.aes = TRUE) +
  #stat_summary(fun.y = "median", geom = "point", size = 3, color="#ff738a",
  #shape = "25"|", position = position_dodge(width = -1.4), alpha = 1) +
  #scale_discrete_manual(aesthetics = "point_shape", values = c(-0.5, 0.5)) +
  labs(x = 'Évaluations', y = 'RWA', fill="Charge cognitive", color="Charge cognitive") +
  scale_fill_manual(values=c("#73a5ff", "#50ce76")) +
  scale_color_manual(values= c("#73a5ff", "#50ce76"), guide = "none") +
  coord_cartesian(ylim=c(1,9)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10), expand=c(0.01,0)) +
  labs(subtitle="Contre-conditionnement négatif") +
  theme_ipsum_rc(base_size = 13,
                 subtitle_size = 20,
                 axis_title_size = 15) +
  guides(fill = guide_legend(override.aes = list(linetype = 0)),
         color = guide_legend(override.aes = list(linetype = 0))) +
  coord_flip()

data_plot_pos <- ggMarginal(data_plot_pos, margins = "x", alpha = 0.6,
                            type = "histogram", size = 4, fill = "gray", colour = "lightgray")

# Combine plot
data_plot_all <- ggarrange(data_plot_neg,
                           data_plot_pos,
                           ncol = 2, nrow = 1)

# uncomment to display plot
# data_plot_all

# save plot
ggsave("plots/data_plot_all_xp09_french.pdf", width = 50, height = 15, units = "cm")

# Combine with spaghetti
data_spag_all <- ggarrange(marg_plot_n,
                           marg_plot_p,
                           data_plot_neg,
                           data_plot_pos,
                           ncol = 2, nrow = 2)

# uncomment to display plot
# data_spag_all

# save plot
ggsave("plots/data_spag_xp09_french.pdf", width = 50, height = 30, units = "cm")


ggplot(compare, aes(x = Income, fill = Year)) +
  geom_histogram(binwidth = 2000, alpha = .5, position = "identity")

dist_rwa_pos <- spreading_pos %>%
  ggplot(aes(x = RWAscore, fill = Conditionnement)) +
  geom_histogram(alpha = .5, color = "white", fill = "#ff717d", size = 0) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10), expand=c(0.01,0)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 4), expand=c(0,0)) +
  labs(fill = 'Évaluations') +
  theme_ipsum_rc(base_size = 13,
                 subtitle_size = 20,
                 axis_title_size = 15) +
  theme(axis.title.y=element_blank(),
        #axis.title.x=element_blank(),
        #strip.background = element_blank(),
        #axis.text.x=element_blank(),
        #axis.text.y=element_blank(),
        #axis.ticks.y=element_blank(),
        #axis.line.y=element_blank(),
        legend.text=element_text(colour="white"))
        #panel.spacing = unit(0, "lines"),
        #strip.text.y = element_text(angle = 180, vjust = 0.2))

ggMarginal(data_plot_pos, margins = "x", alpha = 0.6,
           type = "histogram", size = 4, fill = "gray", colour = "lightgray")

dist_rwa <- unique(spreading$RWAscore)
dist_rwa <- as.data.frame(dist_rwa)
dist_rwa$y <- 1
colnames(dist_rwa) <- c("x","y")

min(unique(spreading$RWAscore))

ggMarginal(data = dist_rwa, x = "x", y = "y", margins = "x", alpha = 0.6,
           type = "boxplot", size = 4, fill = "gray", colour = "lightgray")

dist_rwa_pos

ggarrange(dist_rwa_pos, data_plot_pos,
          ncol = 1, nrow = 2,  align = "v", 
          heights = c(1, 2))

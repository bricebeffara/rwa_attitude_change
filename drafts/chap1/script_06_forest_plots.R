# Thu Nov 23 17:17:35 2017 ------------------------------
# Clean script forest plots
# Bret et al. 2017
# Right-Wing Authoritarianism Predicts weakened Attitude Change in an Evaluative Counter-conditioning Paradigm
# OSF : https://osf.io/vz78f/
# Preprint : https://psyarxiv.com/3mfa8/
#################################################################################################################

# Change the name of the models if you want to plot simple slope parameters
# Change the number of models if you want to produce plots from IDA3

dataforest <- data.frame( value = c(posterior_samples(ec01rwa, "b")$"b_rwa",
                                 posterior_samples(ec02rwa, "b")$"b_rwa",
                                 posterior_samples(ec03rwa, "b")$"b_rwa",
                                 posterior_samples(ec04rwa, "b")$"b_rwa",
                                 posterior_samples(ec05rwa, "b")$"b_rwa",
                                 posterior_samples(ec06rwa, "b")$"b_rwa",
                                 posterior_samples(ec07rwa, "b")$"b_rwa",
                                 posterior_samples(ec1t7rwa, "b")$"b_rwa"
),
type = c(rep("Experiment 1",4000),rep("Experiment 2",4000),
         rep("Experiment 3",4000),rep("Experiment 4",4000),
         rep("Experiment 5",4000),rep("Experiment 6",4000),
         rep("Experiment 7",4000),rep("Integrative analysis",4000)
)
)

setDT(dataforest)[, value.ave := mean(value), by = type]
setDT(dataforest)[, value.low := hdi(value)["lower"], by = type]
setDT(dataforest)[, value.up := hdi(value)["upper"], by = type]

dataforest$type <- factor(dataforest$type,
                        levels = c("Experiment 1", "Experiment 2",
                                   "Experiment 3", "Experiment 4",
                                   "Experiment 5", "Experiment 6",
                                   "Experiment 7", "Integrative analysis"))

dataforest$sep <- ifelse(dataforest$type == "Integrative analysis", "1", "2")

dataforest$select <- rep(1:4000,4)

# Computing Evidence Ratios and preparing for pie charts

h01rwus <- hypothesis (ec01rwaN, class = "b", "rwa = 0")
bf001rwus <- 1
bf101rwus <- 1/h01rwus$hypothesis$Evid.Ratio

prop01h0 <- round(4000/(bf101rwus+1))
prop01h1 <- round(4000-4000/(bf101rwus+1))

h02rwus <- hypothesis (ec02rwaN, class = "b", "rwa = 0")
bf002rwus <- 1
bf102rwus <- 1/h02rwus$hypothesis$Evid.Ratio

prop02h0 <- round(4000/(bf102rwus+1))
prop02h1 <- round(4000-4000/(bf102rwus+1))

h03rwus <- hypothesis (ec03rwaN, class = "b", "rwa = 0")
bf003rwus <- 1
bf103rwus <- 1/h03rwus$hypothesis$Evid.Ratio

prop03h0 <- round(4000/(bf103rwus+1))
prop03h1 <- round(4000-4000/(bf103rwus+1))

h04rwus <- hypothesis (ec04rwaP, class = "b", "rwa = 0")
bf004rwus <- 1
bf104rwus <- 1/h04rwus$hypothesis$Evid.Ratio

prop04h0 <- round(4000/(bf104rwus+1))
prop04h1 <- round(4000-4000/(bf104rwus+1))

h05rwus <- hypothesis (ec05rwaP, class = "b", "rwa = 0")
bf005rwus <- 1
bf105rwus <- 1/h05rwus$hypothesis$Evid.Ratio

prop05h0 <- round(4000/(bf105rwus+1))
prop05h1 <- round(4000-4000/(bf105rwus+1))

h06rwus <- hypothesis (ec06rwaP, class = "b", "rwa = 0")
bf006rwus <- 1
bf106rwus <- 1/h06rwus$hypothesis$Evid.Ratio

prop06h0 <- round(4000/(bf106rwus+1))
prop06h1 <- round(4000-4000/(bf106rwus+1))

h07rwus <- hypothesis (ec07rwaP, class = "b", "rwa = 0")
bf007rwus <- 1
bf107rwus <- 1/h07rwus$hypothesis$Evid.Ratio

prop07h0 <- round(4000/(bf107rwus+1))
prop07h1 <- round(4000-4000/(bf107rwus+1))

h1t7rwus <- hypothesis (ec1t7rwaP, class = "b", "rwa = 0")
bf01t7rwus <- 1
bf11t7rwus <- 1/h1t7rwus$hypothesis$Evid.Ratio

prop1t7h0 <- round(4000/(bf11t7rwus+1))
prop1t7h1 <- round(4000-4000/(bf11t7rwus+1))


dataforest$prop <- c( rep ("H1",prop01h1), rep("H0",prop01h0),
                    rep ("H1",prop02h1), rep("H0",prop02h0),
                    rep ("H1",prop03h1), rep("H0",prop03h0),
                    rep ("H1",prop04h1), rep("H0",prop04h0),
                    rep ("H1",prop05h1), rep("H0",prop05h0),
                    rep ("H1",prop06h1), rep("H0",prop06h0),
                    rep ("H1",prop07h1), rep("H0",prop07h0),
                    rep ("H1",prop1t7h1), rep("H0",prop1t7h0))

# plots HDIs

p1 <-
  ggplot(dataforest, aes(x = value))+
  geom_density(color="gray60", fill="gray60")+
  facet_grid(type ~ ., switch="y", margins = FALSE, scales = "fixed", space = "fixed")+
  geom_hline(yintercept = 0, colour = "white", size = 0.5)+
  labs(x = expression(beta[RWA["Block 1 positive"]]))+
  theme(axis.title.x = element_text(size = rel(1.5), angle = 00))+
  geom_segment(data=subset(dataforest, select == 1),
               aes(x = value.low, xend = value.up, y = 0,
                   yend = 0, group = type), colour = "black", size = 0.4)+
  geom_point(data=subset(dataforest, select == 1),
             aes(x = value.ave, y = 0, group = type),
             colour = "black", size = 2, shape=19)+
  scale_color_manual(values=c("gray20", "white"))+
  theme(axis.title.y=element_blank(),
        strip.background = element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.line.y=element_blank(),
        panel.background = element_blank(),
        legend.position="none",
        panel.spacing = unit(0, "lines"),
        strip.text.y = element_text(angle = 180, vjust = 0.2))+
  geom_text(x=-0.75, y=1, data=subset(dataforest, select == 1),
            aes(label=paste(round(value.ave,2),"[",
                            round(value.low,2),", ",round(value.up,2),"]")), size = 2.5)+
  geom_segment(aes(x = min(value), xend = max(value), y = 12,
                   yend = 12, colour = sep), size = 0.2)+
  geom_vline(xintercept = 0, colour="gray40", linetype="dashed")

# plots pie charts

p2 <-
  ggplot(data = dataforest) + 
  facet_grid(type ~ ., switch="y") +
  geom_bar(mapping = aes(x = factor(1), fill = prop), width = 0.3, colour = "gray90") + 
  coord_polar(theta = "y", start = pi)+
  scale_fill_manual(values = c("gray80", "gray60")) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.line=element_blank(),
        legend.position="none",
        panel.spacing = unit(0, "lines"),
        strip.text.y = element_blank(),
        strip.background = element_blank(),
        axis.title.x = element_text(size = rel(1.5), angle = 00, color = "white"),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor = element_blank())

ggdraw(p1)+draw_plot(p2 + theme(legend.justification = "left"), 0.44, 0.03, 1, 1)

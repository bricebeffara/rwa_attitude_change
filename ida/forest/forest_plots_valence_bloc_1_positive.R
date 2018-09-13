# Tue Aug 22 01:42:17 2017 ------------------------------
# Script data extraction and analysis for RWA/evaluative conditionning study A.G. Bret december 2016
# Scipt by B. Beffara & A.G. Bret
#################################################################################################################

##########################################################
##########################################################
# Part 3 = Plot Data
##########################################################
##########################################################

# Loading packages needed (and installing if necessary) for this part (make sure that jags is installed on your computer)
if (!"pacman" %in% installed.packages()[,"Package"] ) install.packages("pacman" )
pacman::p_load(data.table, grid, ggplot2)

############################
####### FOREST PLOTS #######
############################

#### Forest plot for interaction ####

dataint<- data.frame( value = c(posterior_samples(ec01rwaN, "b")$"b_rwa",
                                posterior_samples(ec02rwaN, "b")$"b_rwa",
                                posterior_samples(ec03rwaN, "b")$"b_rwa",
                                posterior_samples(ecmmrwaN, "b")$"b_rwa"
),
type = c(rep("Experiment 1",4000),rep("Experiment 2",4000),
         rep("Experiment 3",4000),rep("Integrative analysis",4000))
)

setDT(dataint)[, value.ave := mean(value), by = type]
setDT(dataint)[, value.low := hdi(value)["lower"], by = type]
setDT(dataint)[, value.up := hdi(value)["upper"], by = type]

dataint$type <- factor(dataint$type,
                       levels = c("Experiment 1", "Experiment 2",
                                  "Experiment 3", "Integrative analysis"))

dataint$sep <- ifelse(dataint$type == "Integrative analysis", "1", "2")

dataint$select <- rep(1:4000,4)

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

hmmrwus <- hypothesis (ecmmrwaN, class = "b", "rwa = 0")
bf0mmrwus <- 1
bf1mmrwus <- 1/hmmrwus$hypothesis$Evid.Ratio

propmmh0 <- round(4000/(bf1mmrwus+1))
propmmh1 <- round(4000-4000/(bf1mmrwus+1))

dataint$prop <- c( rep ("H1",prop01h1), rep("H0",prop01h0),
                   rep ("H1",prop02h1), rep("H0",prop02h0),
                   rep ("H1",prop03h1), rep("H0",prop03h0),
                   rep ("H1",propmmh1), rep("H0",propmmh0))

dataint$iangl <- c( rep (bf101rwus,4000),
                    rep (bf102rwus,4000),
                    rep (bf103rwus,4000),
                    rep (bf1mmrwus,4000))


p1 <-
  ggplot(dataint, aes(x = value))+
  geom_density(color="gray60", fill="gray60")+
  facet_grid(type ~ ., switch="y")+
  geom_hline(yintercept = 0, colour = "white", size = 0.5)+
  labs(x = expression(beta[RWA["Block 1 positive"]]))+
  theme(axis.title.x = element_text(size = rel(1.5), angle = 00))+
  geom_segment(data=subset(dataint, select == 1),
               aes(x = value.low, xend = value.up, y = 0,
                   yend = 0, group = type), colour = "black", size = 0.4)+
  geom_point(data=subset(dataint, select == 1),
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
  geom_text(x=0.75, y=1, data=subset(dataint, select == 1),
            aes(label=paste(round(value.ave,2),"[",
                            round(value.low,2),", ",round(value.up,2),"]")), size = 2.5)+
  geom_segment(aes(x = min(value), xend = max(value), y = 10,
                   yend = 10, colour = sep), size = 0.2)+
  geom_vline(xintercept = 0, colour="gray40", linetype="dashed")


p2 <-
  ggplot(data = dataint) + 
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

library(cowplot)
ggdraw(p1)+draw_plot(p2 + theme(legend.justification = "left"), 0.44, 0.04, 1, 1)
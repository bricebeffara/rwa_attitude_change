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

data7int<- data.frame( value = c(posterior_samples(ec01rwaN, "b")$"b_rwa",
                                 posterior_samples(ec02rwaN, "b")$"b_rwa",
                                 posterior_samples(ec03rwaN, "b")$"b_rwa",
                                 posterior_samples(ecAPTrwaP, "b")$"b_rwa",
                                 posterior_samples(ecAMPrwaP, "b")$"b_rwa",
                                 posterior_samples(ecIATrwaP, "b")$"b_rwa",
                                 posterior_samples(ecdoublerwaP, "b")$"b_rwa",
                                 posterior_samples(ecIDArwaP, "b")$"b_rwa"
),
type = c(rep("Experiment 1",4000),rep("Experiment 2",4000),
         rep("Experiment 3",4000),rep("Experiment 4",4000),
         rep("Experiment 5",4000),rep("Experiment 6",4000),
         rep("Experiment 7",4000),rep("Integrative analysis",4000)
)
)

setDT(data7int)[, value.ave := mean(value), by = type]
setDT(data7int)[, value.low := hdi(value)["lower"], by = type]
setDT(data7int)[, value.up := hdi(value)["upper"], by = type]

data7int$type <- factor(data7int$type,
                        levels = c("Experiment 1", "Experiment 2",
                                   "Experiment 3", "Experiment 4",
                                   "Experiment 5", "Experiment 6",
                                   "Experiment 7", "Integrative analysis"))

data7int$sep <- ifelse(data7int$type == "Integrative analysis", "1", "2")

data7int$select <- rep(1:4000,4)

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

hAPTrwus <- hypothesis (ecAPTrwaP, class = "b", "rwa = 0")
bf0APTrwus <- 1
bf1APTrwus <- 1/hAPTrwus$hypothesis$Evid.Ratio

propAPTh0 <- round(4000/(bf1APTrwus+1))
propAPTh1 <- round(4000-4000/(bf1APTrwus+1))

hAMPrwus <- hypothesis (ecAMPrwaP, class = "b", "rwa = 0")
bf0AMPrwus <- 1
bf1AMPrwus <- 1/hAMPrwus$hypothesis$Evid.Ratio

propAMPh0 <- round(4000/(bf1AMPrwus+1))
propAMPh1 <- round(4000-4000/(bf1AMPrwus+1))

hIATrwus <- hypothesis (ecIATrwaP, class = "b", "rwa = 0")
bf0IATrwus <- 1
bf1IATrwus <- 1/hIATrwus$hypothesis$Evid.Ratio

propIATh0 <- round(4000/(bf1IATrwus+1))
propIATh1 <- round(4000-4000/(bf1IATrwus+1))

hdoublerwus <- hypothesis (ecdoublerwaP, class = "b", "rwa = 0")
bf0doublerwus <- 1
bf1doublerwus <- 1/hdoublerwus$hypothesis$Evid.Ratio

propdoubleh0 <- round(4000/(bf1doublerwus+1))
propdoubleh1 <- round(4000-4000/(bf1doublerwus+1))

hIDArwus <- hypothesis (ecIDArwaP, class = "b", "rwa = 0")
bf0IDArwus <- 1
bf1IDArwus <- 1/hIDArwus$hypothesis$Evid.Ratio

propIDAh0 <- round(4000/(bf1IDArwus+1))
propIDAh1 <- round(4000-4000/(bf1IDArwus+1))


data7int$prop <- c( rep ("H1",prop01h1), rep("H0",prop01h0),
                    rep ("H1",prop02h1), rep("H0",prop02h0),
                    rep ("H1",prop03h1), rep("H0",prop03h0),
                    rep ("H1",propAPTh1), rep("H0",propAPTh0),
                    rep ("H1",propAMPh1), rep("H0",propAMPh0),
                    rep ("H1",propIATh1), rep("H0",propIATh0),
                    rep ("H1",propdoubleh1), rep("H0",propdoubleh0),
                    rep ("H1",propIDAh1), rep("H0",propIDAh0))


p1 <-
  ggplot(data7int, aes(x = value))+
  geom_density(color="gray60", fill="gray60")+
  facet_grid(type ~ ., switch="y", margins = FALSE, scales = "fixed", space = "fixed")+
  geom_hline(yintercept = 0, colour = "white", size = 0.5)+
  labs(x = expression(beta[RWA["Block 1 positive"]]))+
  theme(axis.title.x = element_text(size = rel(1.5), angle = 00))+
  geom_segment(data=subset(data7int, select == 1),
               aes(x = value.low, xend = value.up, y = 0,
                   yend = 0, group = type), colour = "black", size = 0.4)+
  geom_point(data=subset(data7int, select == 1),
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
  geom_text(x=-0.75, y=1, data=subset(data7int, select == 1),
            aes(label=paste(round(value.ave,2),"[",
                            round(value.low,2),", ",round(value.up,2),"]")), size = 2.5)+
  geom_segment(aes(x = min(value), xend = max(value), y = 12,
                   yend = 12, colour = sep), size = 0.2)+
  geom_vline(xintercept = 0, colour="gray40", linetype="dashed")


p2 <-
  ggplot(data = data7int) + 
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
ggdraw(p1)+draw_plot(p2 + theme(legend.justification = "left"), 0.44, 0.03, 1, 1)

###load packages####
require(plyr)
require(ggplot2)
require(scales)


####load data####
comp <- read.csv("101617_respiro_comp.csv")
####Int zero is meaningless as the respirometer spends the first interval establishing a baseline aka 0###
comp <- subset(comp, Int > 0 & Int < 67)
head(comp)
####remove cultures that flatlined before blanking###
compa <- subset(comp, Treatment!="LOM" | Rep!="C" | Carbon!="Coumarate" | Strain!="E-37")
compb <- subset(compa, Treatment!="composite" | Rep!="C" | Carbon!="Coumarate" | Strain!="E-37")
compc <- subset(compb, Treatment!="PRI" | Rep!="B" | Carbon!="Coumarate" | Strain!="E-37")
compd <- subset(compc, Treatment!="PRI" | Rep!="C" | Carbon!="Casamino" | Strain!="E-37")
tail(compd)

####convert int to day####

####get means and std devs for CO2 accum###
compe <- ddply(compd, c("Treatment", "Carbon", "Concentration", "Int", "Strain"), summarise, meanCO2=mean(CO2Accum), sdCO2=sd(CO2Accum))
head(compe)
tail(compe)
compe$Day <- (compe$Int*2.55)/24

compe <- subset(compe, Treatment=="PRI" | Treatment=="composite")

compss <- subset(compe, Strain == "E-37" | Strain == "SE45")
head(compss)

compss <- subset(compss, Int < 67)

compsslow <- subset(compss, Concentration != "400 uM-C")
head(compsslow)

###make figure for low concentrations of casamino acids###
sslowp <- ggplot(compsslow, aes(Day, meanCO2, colour=Treatment, ymin=meanCO2-sdCO2, ymax=meanCO2+sdCO2)) +
  geom_line(stat="identity") +
  geom_errorbar(width=0.1) +
  scale_color_manual(values=c("#58595B", "#FF8200")) +
  ylab('CO2 Accumulation (ug)') +
  xlab('Time (d)') +
  facet_grid(Strain~Concentration) +
  theme(panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        legend.key = element_rect(fill = "white"),
        text=element_text(size=16),
        legend.position="top",legend.direction="horizontal") +
  ggsave("110217_SSlow_present.tiff", width=9, height=6)

print(sslowp)

###make figure for 400 um-C E37 and SE45 data###
compsshigh <- subset(compss, Concentration == "400 uM-C")

head(compsshigh)
tail(compsshigh)
sshighp <- ggplot(compsshigh, aes(Day, meanCO2, colour=Treatment, ymin=meanCO2-sdCO2, ymax=meanCO2+sdCO2)) +
  geom_line(stat="identity") +
  geom_errorbar(width=0.1) +
  scale_color_manual(values=c("#58595B", "#FF8200")) +
  ylab('CO2 Accumulation (ug)') +
  xlab('Time (d)') +
facet_grid(Strain~Carbon) +
  theme(panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        legend.key = element_rect(fill = "white"),
        text=element_text(size=16),
        legend.position="top",legend.direction="horizontal") +
  ggsave("110217_SShigh_present.tiff", width=9, height=6)
print(sshighp)

####community time###
comm <- read.csv("110317_comm.csv")
commp <- ggplot(comm, aes(Day, meanCO2, colour=Treatment, ymin=meanCO2-sdCO2, ymax=meanCO2+sdCO2)) +
  geom_line(stat="identity") +
  geom_errorbar(width=0.1) +
  scale_color_manual(values=c("#58595B", "#FF8200")) +
  ylab('CO2 Accumulation (ug)') +
  xlab('Time (d)') +
  facet_grid(Carbon~Concentration) +
  theme(panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        legend.key = element_rect(fill = "white"),
        text=element_text(size=16),
        legend.position="top",legend.direction="horizontal") +
  ggsave("120817_comm_present.tiff", width=9, height=6)
print(commp)

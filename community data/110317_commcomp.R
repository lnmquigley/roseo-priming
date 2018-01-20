####load packages####
require(ggplot2)
require(dplyr)
require(plyr)
library(scales)

####import data####
commcomp = read.csv("040417_commcomp.csv")
head(commcomp)

###calculate relative abundance###
commcomp_relabund <- mutate(commcomp, Relabund = (Count/Sum))
head(commcomp_relabund)
commcomp_relabund_ave <- ddply(commcomp_relabund, c("Day", "Treatment", "Strain", "Concentration", "Carbon"), summarise, Relabundm=mean(Relabund))
head(commcomp_relabund_ave)

###get rid of accidental 100 uM dataset (forgot to dilute LOM stock)###
commcomp_relabund_man <- subset(commcomp_relabund_ave, Concentration != "100")
head(commcomp_relabund_man)

###add units to concentration###
commcomp_relabund_man$Concentration <-factor(commcomp_relabund_man$Concentration, labels = c("1 uM-C", "4 uM-C", "40 uM-C", "400 uM-C"))
head(commcomp_relabund_man)

####subset to make control plots (ABM and NOM)####
commcompcontrols <- subset(commcomp_relabund_man, Carbon =="Acetate" & Treatment !="PRI" & Treatment !="LOM" & Concentration != "40 uM-C" & Concentration !="400 uM-C")
head(commcompcontrols)
tail(commcompcontrols)

commcompcontrol <- ggplot(commcompcontrols, aes(Day, Relabundm, fill=Strain)) + 
  geom_area(position="stack") +
  scale_fill_manual(values=c("#75417E", "#FF8200", "#E65933", "#B9E1E2", "#ABC178", "#006C93")) +
  scale_y_continuous(labels=scales::percent) +
  facet_grid(Treatment~Concentration) +
  xlab("Time (d)") +
  ylab("Community Composition (Relative Abundance)") +
  theme(panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        legend.key = element_rect(fill = "white"))
ggsave("110317_control.png", width=4.75, height=6)
print(commcompcontrol)

###MAKE COMMUNITY COMPOSITION PLOTS PER LOM SOURCE###
###subset to get acetate treatments####
commcomp_relabund_man_ace <- subset(commcomp_relabund_man, Carbon == "Acetate" & Treatment !="ABM" & Treatment !="NOM")
head(commcomp_relabund_man_ace)

###make acetate plot###
commcomp_acetatep <- ggplot(commcomp_relabund_man_ace, aes(Day, Relabundm, fill=Strain)) + 
  geom_area(position="stack") +
  scale_fill_manual(values=c("#75417E", "#FF8200", "#E65933", "#B9E1E2", "#ABC178", "#006C93")) +
  scale_y_continuous(labels=scales::percent) +
  scale_x_continuous(breaks=c(0, 5, 10)) +
  ylab('Community Composition (Relative Abundance)') +
  xlab('Time (d)') +
  facet_grid(Treatment~Concentration) +
  theme(panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.key = element_rect(fill = "white"),
        text=element_text(size=16),
        legend.position="none")
ggsave("121417_acetatecommcomp.pdf", width=9, height=6)
print(commcomp_acetatep)
  
##### subset to get coumarate treatments ####
commcomp_coumarate <- subset(commcomp_relabund_man, Carbon == "Coumarate" & Treatment != "ABM" & Treatment != "NOM")
head(commcomp_coumarate)

### make coumarate plot ###
commcomp_coumaratep <- ggplot(commcomp_coumarate, aes(Day, Relabundm, fill=Strain)) + 
  geom_area(position="stack") +
  scale_fill_manual(values=c("#75417E", "#FF8200", "#E65933", "#B9E1E2", "#ABC178", "#006C93")) +
  scale_y_continuous(labels=scales::percent) +
  scale_x_continuous(breaks=c(0, 5, 10)) +
  ylab('Community Composition (relative abundance)') +
  xlab('Time (d)') +
  facet_grid(Treatment~Concentration) +
  theme(panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.key = element_rect(fill = "white"),
        text=element_text(size=16),
        legend.position="none")
ggsave("121417_coumaratecommcomp.pdf", width=9, height=6)
print(commcomp_coumaratep)

###subset to get casamino acids treatment####
commcomp_casamino <- subset(commcomp_relabund_man, Carbon == "Casamino Acids" & Treatment != "ABM" & Treatment != "NOM")
head(commcomp_casamino)

###make casamino plot###
commcomp_casaminop <- ggplot(commcomp_casamino, aes(Day, Relabundm, fill=Strain)) + 
  geom_area(position="stack") +
  scale_fill_manual(values=c("#75417E", "#FF8200", "#E65933", "#B9E1E2", "#ABC178", "#006C93")) +
  scale_y_continuous(labels=scales::percent) +
  scale_x_continuous(breaks=c(0, 5, 10)) +
  facet_grid(Treatment~Concentration) +
  ylab('Community Composition (relative abundance)') +
  xlab('Time (d)') +
  theme(panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        legend.key = element_rect(fill = "white"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        text=element_text(size=16),
        legend.position="none")
ggsave("121417_casaminocommcomp.pdf", width=9, height=6)
print(commcomp_casaminop)
  
####subset to get tryptone treatments###
commcomp_tryptone <- subset(commcomp_relabund_man, Carbon == "Tryptone" & Treatment != "ABM" & Treatment != "NOM")
head(commcomp_tryptone)

###make tryptone plot###  
commcomp_tryptonep <- ggplot(commcomp_tryptone, aes(Day, Relabundm, fill=Strain)) + 
  geom_area(position="stack") +
  scale_fill_manual(values=c("#75417E", "#FF8200", "#E65933", "#B9E1E2", "#ABC178", "#006C93")) +
  guides(fill=guide_legend(ncol=6)) +
  scale_y_continuous(labels=scales::percent) +
  scale_x_continuous(breaks=c(0, 5, 10)) +
  guides(col = guide_legend(ncol = 8)) +
  ylab('Community Composition (relative abundance)') +
  xlab('Time (d)') +
  facet_grid(Treatment~Concentration) +
  theme(panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        legend.key = element_rect(fill = "white"),
        text=element_text(size=16),
        legend.position="none")
ggsave("121417_tryptonecommcomp_2.pdf", width=9, height=6)
print(commcomp_tryptonep)

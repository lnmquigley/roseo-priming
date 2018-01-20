####load packages####
require(ggplot2)
require(vegan)
require(scales)
require(plyr)
require(DescTools)
require(lme4)
require(lmerTest)

#### load in the data ####
abund_table<-read.csv("081517_commcomp.csv", row.names=1, check.names=FALSE)
####transpose data to have sample names on rows####
abund_table<-t(abund_table)
head(abund_table)
meta_table<-read.csv("081417_commcomp_meta.csv", row.names=1, check.names=FALSE)
meta_table<-meta_table[rownames(abund_table),]
head(meta_table)

###add Shannon exponentially transformed to Effective species number to metadata table###
meta_table$Shannon <- exp(diversity(abund_table, "shannon"))
head(meta_table)
tail(meta_table)
shannonnave <- ddply(meta_table, c("Day", "Treatment", "Concentration", "Carbon"), 
                  summarise, shanave=mean(Shannon), shansd=sd(Shannon))

###get rid of NAs###
shannonnave <- subset(shannonnave, Concentration != "NA" | Carbon != "NA")
###remove controls from initial analysis###
shannonavet <- subset(shannonnave, Carbon != "Control")
shannonavet$Carbon <- c("Acetate", "Casamino Acids", "Coumarate", "Tryptone")
head(shannonavet)
tail(shannonavet)

###plot effective species number for by concentration and carbon source over the course of the incubation###
shantp <- ggplot(shannonavet, aes(Day, shanave, linetype=Treatment, colour=Treatment, ymin=shanave-shansd, ymax=shanave+shansd)) +
  geom_point() +
  geom_line() +
  scale_linetype_manual(values=c("longdash", "solid")) +
  scale_color_manual(values=c("#58595B", "#58595B")) +
  facet_grid(Carbon~Concentration) +
  scale_x_continuous(limits=c(0, 15)) +
  geom_errorbar(width=0.1) +
  ylab('Effective Species Number') +
  xlab('Time (d)') +
  theme(panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        legend.key = element_rect(fill = "white"),
        text=element_text(size=20),
        legend.position="top",legend.direction="horizontal")
print(shantp)
ggsave('102617_shannonindex_treat.png', height=9, width=9, dpi=300)

####calculate effect species number for controls###
shannonavec <- subset(shannonnave, Carbon == "Control")
head(shannonavec)
shannonavec$Treatment <- c("NOM", "NOM","ABM", "ABM")
shannonavec$Concentration <- c("1 uM-C" = "1 uM-C", "4 uM-C" = "4-400 uM-C")
head(shannonavec)
###plot effective species number for controls###
shantc <- ggplot(shannonavec, aes(Day, shanave, color=Treatment, linetype=Treatment, ymin=shanave-shansd, ymax=shanave+shansd)) +
  geom_point() +
  geom_line() +
  scale_linetype_manual(values=c("dashed", "dotted")) +
  scale_color_manual(values=c("#58595B", "#58595B")) +
  scale_x_continuous(limits=c(0, 15)) +
  facet_grid(.~Concentration) +
  geom_errorbar(width=0.1) +
  ylab('Effective Species Number') +
  xlab('Time (d)') +
  theme(panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        legend.key = element_rect(fill = "white"),
        legend.position="top",legend.direction="horizontal")
print(shantc)
  ggsave('102617_shannonindex_con.png', height=2.25, width=4.5, dpi=300)

####only run statistics on LOM and PRI###
shanLP <- subset(meta_table, Treatment == "LOM" | Treatment == "PRI")
head(shanLP)

###subset by day###
shanLP1 <-subset(shanLP, Day == 1)
head(shanLP1)
shanLP2 <-subset(shanLP, Day == 2)
head(shanLP2)
shanLP4 <-subset(shanLP, Day == 4)
head(shanLP4)
shanLP7 <-subset(shanLP, Day == 7)
head(shanLP7)
shanLP10 <-subset(shanLP, Day == 10)
head(shanLP10)
shanLP14 <-subset(shanLP, Day == 14)
head(shanLP14)

options(max.print = 999999999)

###stats for day 1####
###three way ANOVA for day 1###
shanLP1a<- aov(Shannon~Treatment*Concentration*Carbon, data=shanLP1)
summary(shanLP1a)
lsdshanLP1a <-PostHocTest(shanLP1a, method="lsd")
lsdshanLP1a
###copy and paste output into excel and save as CSV called shan1###
###Benjamini Hochberg Correction###
LSDsd1 <- read.csv("090217_shan1.csv")
head(LSDsd1)
mode(LSDsd1)
LSDspval1 <- subset(LSDsd1, select = pval)
LSDspval1 <- as.matrix(LSDspval1)
BHspval1 <- p.adjust(LSDspval1, method="BH", n=length(LSDspval1))
BHspval1 <-list(BHspval1)
BHspval1
write.csv(BHspval1,"090217_BHs1.csv")

###stats for day 2####
###three way ANOVA for day 2###
shanLP2a<- aov(Shannon~Treatment*Concentration*Carbon, data=shanLP2)
summary(shanLP2a)
lsdshanLP2a <-PostHocTest(shanLP2a, method="lsd")
lsdshanLP2a
###copy and paste output into excel and save as CSV called shan2###
###Benjamini Hochberg Correction###
LSDsd2 <- read.csv("090217_shan2.csv")
head(LSDsd2)
mode(LSDsd2)
LSDspval2 <- subset(LSDsd2, select = pval)
LSDspval2 <- as.matrix(LSDspval2)
BHspval2 <- p.adjust(LSDspval2, method="BH", n=length(LSDspval2))
BHspval2 <-list(BHspval2)
BHspval2
write.csv(BHspval2,"090217_BHs2.csv")

###stats for day 4####
###three way ANOVA for day 4###
shanLP4a<- aov(Shannon~Treatment*Concentration*Carbon, data=shanLP4)
summary(shanLP4a)
lsdshanLP4a <-PostHocTest(shanLP4a, method="lsd")
lsdshanLP4a
###copy and paste output into excel and save as CSV called shan4###
###Benjamini Hochberg Correction###
LSDsd4 <- read.csv("090217_shan4.csv")
head(LSDsd4)
mode(LSDsd4)
LSDspval4 <- subset(LSDsd4, select = pval)
LSDspval4 <- as.matrix(LSDspval4)
BHspval4 <- p.adjust(LSDspval4, method="BH", n=length(LSDspval4))
BHspval4 <-list(BHspval4)
BHspval4
write.csv(BHspval4,"090217_BHs4.csv")

###stats for day 7####
###three way ANOVA for day 7###
shanLP7a<- aov(Shannon~Treatment*Concentration*Carbon, data=shanLP7)
summary(shanLP7a)
lsdshanLP7a <-PostHocTest(shanLP7a, method="lsd")
lsdshanLP7a
###copy and paste output into excel and save as CSV called shan7###
###Benjamini Hochberg Correction###
LSDsd7 <- read.csv("090217_shan7.csv")
head(LSDsd7)
mode(LSDsd7)
LSDspval7 <- subset(LSDsd7, select = pval)
LSDspval7 <- as.matrix(LSDspval7)
BHspval7 <- p.adjust(LSDspval7, method="BH", n=length(LSDspval7))
BHspval7 <-list(BHspval7)
BHspval7
write.csv(BHspval7,"090217_BHs7.csv")

###stats for day 10####
###three way ANOVA for day 10###
shanLP10a<- aov(Shannon~Treatment*Concentration*Carbon, data=shanLP10)
summary(shanLP10a)
lsdshanLP10a <-PostHocTest(shanLP10a, method="lsd")
lsdshanLP10a
###copy and paste output into excel and save as CSV called shan10###
###Benjamini Hochberg Correction###
LSDsd10 <- read.csv("090217_shan10.csv")
head(LSDsd10)
mode(LSDsd10)
LSDspval10 <- subset(LSDsd10, select = pval)
LSDspval10 <- as.matrix(LSDspval10)
BHspval10 <- p.adjust(LSDspval10, method="BH", n=length(LSDspval10))
BHspval10 <-list(BHspval10)
BHspval10
write.csv(BHspval10,"090217_BHs10.csv")

###stats for day 14####
###three way ANOVA for day 14###
shanLP14a<- aov(Shannon~Treatment*Concentration*Carbon, data=shanLP14)
summary(shanLP14a)
lsdshanLP14a <-PostHocTest(shanLP14a, method="lsd")
lsdshanLP14a
###copy and paste output into excel and save as CSV called shan14###
###Benjamini Hochberg Correction###
LSDsd14 <- read.csv("090217_shan14.csv")
head(LSDsd14)
mode(LSDsd14)
LSDspval14 <- subset(LSDsd14, select = pval)
LSDspval14 <- as.matrix(LSDspval14)
BHspval14 <- p.adjust(LSDspval14, method="BH", n=length(LSDspval14))
BHspval14 <-list(BHspval14)
BHspval14
write.csv(BHspval14,"090217_BHs14.csv")

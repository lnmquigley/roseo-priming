####load required packages####
require(DescTools)
require(car)
require(agricolae)
require(devtools)
####import data###
SE45 <-read.csv("071817_SE45_twoweek_master.csv")
head(SE45)
E37 <- read.csv("101716_E37_twoweeks_master.csv")

###transform and subset E-37 data###
E37$trans_count <- log10(E37$count)
E37p <- subset(E37, treat == "PRI" | treat == "comp")
E37d1 <- subset(E37p, day == "1")
head(E37d1)
E37d2 <-subset(E37p, day == "2")
head(E37d2)
E37d4 <-subset(E37p, day == "4")
head(E37d4)
E37d7 <-subset(E37p, day == "7")
head(E37d7)
E37d10 <-subset(E37p, day == "10")
head(E37d10)
E37d14 <-subset(E37p, day == "14")
head(E37d14)

###use the same stats workflow for each day so will only annotate once###
###three way-ANOVA####
E37d1a<- aov(trans_count~treat*concentration*priming_agent, data=E37d1)
summary(E37d1a)
lsdE37d1a <-PostHocTest(E37d1a, method="lsd")
lsdE37d1a
###correct for multiple comparisons with Benjamini Hochberg###
LSDEd1 <- read.csv("080717_E37d1_LSD.csv")
head(LSDEd1)
mode(LSDEd1)
LSDEpval1 <- subset(LSDEd1, select = pval)
LSDEpval1 <- as.matrix(LSDEpval1)
BHEpval1 <- p.adjust(LSDEpval1, method="BH", n=length(LSDEpval1))
BHEpval1 <-list(BHEpval1)
BHEpval1
write.csv(BHEpval1,"080817_E37d1BH.csv")

### E37 day 2###
E37d2a<- aov(trans_count~treat*concentration*priming_agent, data=E37d2)
summary(E37d2a)
lsdE37d2a <-PostHocTest(E37d2a, method="lsd")
lsdE37d2a

LSDEd2 <- read.csv("080717_E37d2_LSD.csv")
head(LSDEd2)
mode(LSDEd2)
LSDEpval2 <- subset(LSDEd2, select = pval)
LSDEpval2 <- as.matrix(LSDEpval2)
BHEpval2 <- p.adjust(LSDEpval2, method="BH", n=length(LSDEpval2))
BHEpval2 <-list(BHEpval2)
BHEpval2
write.csv(BHEpval2,"080817_E37d2BH.csv")

### E37 day 4####
E37d4a<- aov(trans_count~treat*concentration*priming_agent, data=E37d4)
summary(E37d4a)
lsdE37d4a <-PostHocTest(E37d4a, method="lsd")
lsdE37d4a

LSDEd4 <- read.csv("080717_E37d4_LSD.csv")
head(LSDEd4)
mode(LSDEd4)
LSDEpval4 <- subset(LSDEd4, select = pval)
LSDEpval4 <- as.matrix(LSDEpval4)
BHEpval4 <- p.adjust(LSDEpval4, method="BH", n=length(LSDEpval4))
BHEpval4 <-list(BHEpval4)
BHEpval4
write.csv(BHEpval4,"080817_E37d4BH.csv")

####E37 day 7#####
E37d7a<- aov(trans_count~treat*concentration*priming_agent, data=E37d7)
summary(E37d7a)
lsdE37d7a <-PostHocTest(E37d7a, method="lsd")
lsdE37d7a

LSDEd7 <- read.csv("080717_E37d7_LSD.csv")
head(LSDEd7)
mode(LSDEd7)
LSDEpval7 <- subset(LSDEd7, select = pval)
LSDEpval7 <- as.matrix(LSDEpval7)
BHEpval7 <- p.adjust(LSDEpval7, method="BH", n=length(LSDEpval7))
BHEpval7 <-list(BHEpval7)
BHEpval7
write.csv(BHEpval7,"080817_E37d7BH.csv")

###E37 day 10####
E37d10a<- aov(trans_count~treat*concentration*priming_agent, data=E37d10)
summary(E37d10a)
lsdE37d10a <-PostHocTest(E37d10a, method="lsd")
lsdE37d10a

LSDEd10 <- read.csv("080717_E37d10_LSD.csv")
head(LSDEd10)
mode(LSDEd10)
LSDEpval10 <- subset(LSDEd10, select = pval)
LSDEpval10 <- as.matrix(LSDEpval10)
BHEpval10 <- p.adjust(LSDEpval10, method="BH", n=length(LSDEpval10))
BHEpval10 <-list(BHEpval10)
BHEpval10
write.csv(BHEpval10,"080817_E37d10BH.csv")

### E37 day 14####
E37d14a<- aov(trans_count~treat*concentration*priming_agent, data=E37d14)
summary(E37d14a)
lsdE37d14a <-PostHocTest(E37d14a, method="lsd")
lsdE37d14a

LSDEd14 <- read.csv("080717_E37d14_LSD.csv")
head(LSDEd14)
mode(LSDEd14)
LSDEpval14 <- subset(LSDEd14, select = pval)
LSDEpval14 <- as.matrix(LSDEpval14)
BHEpval14 <- p.adjust(LSDEpval14, method="BH", n=length(LSDEpval14))
BHEpval14 <-list(BHEpval14)
BHEpval14
write.csv(BHEpval14,"080817_E37d14BH.csv")

####transform and subset SE45 appropriately####
SE45$trans_count <- log10(SE45$count)
SE45p <- subset(SE45, treat == "PRI" | treat == "composite")
SE45d1 <- subset(SE45p, day == "1")
head(SE45d1)
SE45d2 <-subset(SE45p, day == "2")
head(SE45d2)
SE45d4 <-subset(SE45p, day == "4")
head(SE45d4)
SE45d7 <-subset(SE45p, day == "7")
head(SE45d7)
SE45d10 <-subset(SE45p, day == "10")
head(SE45d10)
SE45d14 <-subset(SE45p, day == "14")
head(SE45d14)

####Type1 SS Anova with PostHoc wrapper####
SE45d1a<- aov(trans_count~treat*concentration*priming_agent, data=SE45d1)
summary(SE45d1a)
lsdSE45d1a <-PostHocTest(SE45d1a, method="lsd") 
lsdSE45d1a ####copy and paste this into an excel sheet or figure out how to write it in like a normal####
LSDd1 <- read.csv("080717_SE45d1_LSD.csv")
head(LSDd1)
mode(LSDd1)
####only want pvalues####
LSDpval <- subset(LSDd1, select = pval)
####convert into matrix (which automatically is numeric)####
LSDpval <- as.matrix(LSDpval)
####adjust the pvalue####
BHpval <- p.adjust(LSDpval, method="BH", n=length(LSDpval))
BHpval <-list(BHpval)
BHpval
write.csv(BHpval,"080817_SE45d1BH.csv")
####full disclosure I haven't actually written the code for the export and p adjust yet but I'm sure it'll be fine###
SE45d2a<- aov(trans_count~treat*concentration*priming_agent, data=SE45d2)
summary(SE45d2a)
lsdSE45d2a <-PostHocTest(SE45d2a, method="lsd") 
lsdSE45d2a

LSDd2 <- read.csv("080717_SE45d2_LSD.csv")
head(LSDd2)
mode(LSDd2)
LSDpval2 <- subset(LSDd2, select = pval)
LSDpval2 <- as.matrix(LSDpval2)
BHpval2 <- p.adjust(LSDpval2, method="BH", n=length(LSDpval2))
BHpval2 <-list(BHpval2)
BHpval2
write.csv(BHpval2,"080817_SE45d2BH.csv")

###SE45 day 4###
SE45d4a<- aov(trans_count~treat*concentration*priming_agent, data=SE45d4)
summary(SE45d4a)
lsdSE45d4a <- PostHocTest(SE45d4a, method="lsd")
lsdSE45d4a

LSDd4 <- read.csv("080717_SE45d4_LSD.csv")
head(LSDd4)
mode(LSDd4)
LSDpval4 <- subset(LSDd4, select = pval)
LSDpval4 <- as.matrix(LSDpval4)
BHpval4 <- p.adjust(LSDpval4, method="BH", n=length(LSDpval4))
BHpval4 <-list(BHpval4)
BHpval4
write.csv(BHpval4,"080817_SE45d4BH.csv")

###SE45 day 7####
SE45d7a<- aov(trans_count~treat*concentration*priming_agent, data=SE45d7)
summary(SE45d7a)
lsdSE45d7a <- PostHocTest(SE45d7a, method="lsd")
lsdSE45d7a

LSDd7 <- read.csv("080717_SE45d7_LSD.csv")
head(LSDd7)
mode(LSDd7)
LSDpval7 <- subset(LSDd7, select = pval)
LSDpval7 <- as.matrix(LSDpval7)
BHpval7 <- p.adjust(LSDpval7, method="BH", n=length(LSDpval7))
BHpval7 <-list(BHpval7)
BHpval7
write.csv(BHpval7,"080817_SE45d7BH.csv")

###SE45 day 10###
SE45d10a<- aov(trans_count~treat*concentration*priming_agent, data=SE45d10)
summary(SE45d10a)
lsdSE45d10a <- PostHocTest(SE45d10a, method="lsd")
lsdSE45d10a

LSDd10 <- read.csv("080717_SE45d10_LSD.csv")
head(LSDd10)
mode(LSDd10)
LSDpval10 <- subset(LSDd10, select = pval)
LSDpval10 <- as.matrix(LSDpval10)
BHpval10 <- p.adjust(LSDpval10, method="BH", n=length(LSDpval10))
BHpval10 <-list(BHpval10)
BHpval10
write.csv(BHpval10,"080817_SE45d10BH.csv")

####SE45 day 14###
SE45d14a<- aov(trans_count~treat*concentration*priming_agent, data=SE45d14)
summary(SE45d14a)
lsdSE45d14 <- PostHocTest(SE45d14a, method="lsd")
lsdSE45d14

LSDd14 <- read.csv("080717_SE45d14_LSD.csv")
head(LSDd14)
mode(LSDd14)
LSDpval14 <- subset(LSDd14, select = pval)
LSDpval14 <- as.matrix(LSDpval14)
BHpval14 <- p.adjust(LSDpval14, method="BH", n=length(LSDpval14))
BHpval14 <-list(BHpval14)
BHpval14
write.csv(BHpval14,"080817_SE45d14BH.csv")

####subset and transform community data####
comm <- read.csv("040717_commsum.csv")
head(comm)
tail(comm)
comm$trans_count <- log10(comm$count)
commp <- subset(comm, treat == "PRI" | treat == "comp")
head(commp)
commd1 <- subset(commp, day == "1")
head(commd1)
commd2 <-subset(commp, day == "2")
head(commd2)
commd4 <-subset(commp, day == "4")
head(commd4)
commd7 <-subset(commp, day == "7")
head(commd7)
commd10 <-subset(commp, day == "10")
head(commd10)
commd14 <-subset(commp, day == "14")
head(commd14)

###community day 1###
commd1a<- aov(trans_count~treat*concentration*priming_agent, data=commd1)
summary(commd1a)
lsdcommd1a <-PostHocTest(commd1a, method="lsd") 
lsdcommd1a

LSDcd1 <- read.csv("080717_commd1_LSD.csv")
head(LSDcd1)
mode(LSDcd1)
LSDcpval1 <- subset(LSDcd1, select = pval)
LSDcpval1 <- as.matrix(LSDcpval1)
BHcpval1 <- p.adjust(LSDcpval1, method="BH", n=length(LSDcpval1))
BHcpval1 <-list(BHcpval1)
BHcpval1
write.csv(BHcpval1,"080817_commd1BH.csv")

###community day 2###
commd2a<- aov(trans_count~treat*concentration*priming_agent, data=commd2)
summary(commd2a)
lsdcommd2a <-PostHocTest(commd2a, method="lsd") 
lsdcommd2a

LSDcd2 <- read.csv("080717_commd2_LSD.csv")
head(LSDcd2)
mode(LSDcd2)
LSDcpval2 <- subset(LSDcd2, select = pval)
LSDcpval2 <- as.matrix(LSDcpval2)
BHcpval2 <- p.adjust(LSDcpval2, method="BH", n=length(LSDcpval2))
BHcpval2 <-list(BHcpval2)
BHcpval2
write.csv(BHcpval2,"080817_commd2BH.csv")

##community day 4###
commd4a<- aov(trans_count~treat*concentration*priming_agent, data=commd4)
summary(commd4a)
lsdcommd4a <-PostHocTest(commd4a, method="lsd") 
lsdcommd4a

LSDcd4 <- read.csv("080717_commd4_LSD.csv")
head(LSDcd4)
mode(LSDcd4)
LSDcpval4 <- subset(LSDcd4, select = pval)
LSDcpval4 <- as.matrix(LSDcpval4)
BHcpval4 <- p.adjust(LSDcpval4, method="BH", n=length(LSDcpval4))
BHcpval4 <-list(BHcpval4)
BHcpval4
write.csv(BHcpval4,"080817_commd4BH.csv")

###community day 7####
commd7a<- aov(trans_count~treat*concentration*priming_agent, data=commd7)
summary(commd7a)
lsdcommd7a <-PostHocTest(commd1a, method="lsd") 
lsdcommd7a

LSDcd7 <- read.csv("080717_commd7_LSD.csv")
head(LSDcd7)
mode(LSDcd7)
LSDcpval7 <- subset(LSDcd7, select = pval)
LSDcpval7 <- as.matrix(LSDcpval7)
BHcpval7 <- p.adjust(LSDcpval7, method="BH", n=length(LSDcpval7))
BHcpval7 <-list(BHcpval7)
BHcpval7
write.csv(BHcpval7,"080817_commd7BH.csv")

###community day 10###
commd10a<- aov(trans_count~treat*concentration*priming_agent, data=commd10)
summary(commd10a)
lsdcommd10a <-PostHocTest(commd10a, method="lsd") 
lsdcommd10a

LSDcd10 <- read.csv("080717_commd10_LSD.csv")
head(LSDcd10)
mode(LSDcd10)
LSDcpval10 <- subset(LSDcd10, select = pval)
LSDcpval10 <- as.matrix(LSDcpval10)
BHcpval10 <- p.adjust(LSDcpval10, method="BH", n=length(LSDcpval10))
BHcpval10 <-list(BHcpval10)
BHcpval10
write.csv(BHcpval10,"080817_commd10BH.csv")

###community day 14###
commd14a<- aov(trans_count~treat*concentration*priming_agent, data=commd14)
summary(commd14a)
lsdcommd14a <-PostHocTest(commd14a, method="lsd") 
lsdcommd14a

LSDcd14 <- read.csv("080717_commd14_LSD.csv")
head(LSDcd14)
mode(LSDcd14)
LSDcpval14 <- subset(LSDcd14, select = pval)
LSDcpval14 <- as.matrix(LSDcpval14)
BHcpval14 <- p.adjust(LSDcpval14, method="BH", n=length(LSDcpval14))
BHcpval14 <-list(BHcpval14)
BHcpval14
write.csv(BHcpval14,"080817_commd14BH.csv")

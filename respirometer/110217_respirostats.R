###load packages###
require(DescTools)
require(agricolae)
require(plyr)

###load data respiro=CO2 accum###
respiro <- read.csv("110217_respiro_stats.csv")
rates <- read.csv("111317_rates.csv")

head(rates)

###average rates###
rates_ave <- ddply(rates, c("Concentration", "Treatment", "Carbon", "Strain"), summarise, mean=mean(Rate), sd=sd(Rate))
head(rates_ave)
view(rates_ave)
options(max.print=1000000)

###two three-way anovas because of unbalanced experimental design###
###first up only do stats for Casamino so we can see how Concentration influences priming###
casamino <- subset(respiro, Carbon == "Casamino")
casaminoaov <-aov(CO2Accum~Treatment*Concentration*Strain, data=casamino)
summary(casaminoaov)
lsdcasa <- PostHocTest(casaminoaov, method="lsd")
lsdcasa

###BH correction###
LSDcasa <- read.csv("110317_casaminoaov.csv")
head(LSDcasa)
mode(LSDcasa)
LSDcasa <- subset(LSDcasa, select = pval)
LSDcasa <- as.matrix(LSDcasa)
BHspvalcasa <- p.adjust(LSDcasa, method="BH", n=length(LSDcasa))
BHspvalcasa <-list(BHspvalcasa)
BHspvalcasa
write.csv(BHspvalcasa, "BHcasa.csv")

###same thing but for rates###
rates <- read.csv("111317_rates.csv")
concrates <- subset(rates, Carbon == "Casamino Acids")
head(concrates)
concratesaov <-aov(Rate~Treatment*Concentration*Strain, data=concrates)
summary(concratesaov)
lsdconc <- PostHocTest(concratesaov, method="lsd")
lsdconc

LSDconc <- read.csv("111417_conc_rate_stats.csv")
head(LSDconc)
mode(LSDconc)
LSDconc <- subset(LSDconc, select = pval)
LSDconc <- as.matrix(LSDconc)
BHspvalconc <- p.adjust(LSDconc, method="BH", n=length(LSDconc))
BHspvalconc <-list(BHspvalconc)
BHspvalconc
write.csv(BHspvalconc, "BHconc.csv")

###do three-way anova only looking at 400 uM-C data to understand how LOM source influences priming###
carbrates <- subset(rates, Concentration == "400 uM-C")
head(carbrates)
carbratesaov <-aov(Rate~Treatment*Carbon*Strain, data=carbrates)
summary(carbratesaov)
lsdcarb <- PostHocTest(carbratesaov, method="lsd")
lsdcarb

LSDcarb <- read.csv("111417_carbon_rate_stats.csv")
head(LSDcarb)
mode(LSDcarb)
LSDcarb <- subset(LSDcarb, select = pval)
LSDcarb <- as.matrix(LSDcarb)
BHspvalcarb <- p.adjust(LSDcarb, method="BH", n=length(LSDcarb))
BHspvalcarb <-list(BHspvalcarb)
BHspvalcarb
write.csv(BHspvalcarb, "BHcarb.csv")











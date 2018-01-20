####load packages####
require(ggplot2)
require(vegan)
require(MASS)
require(scales)
require(plyr)
require(DescTools)

####load in data####
###these spreadsheets are the 0815 and 0814 spreadsheets subsetted by day###
###I realize this is super clunky I'll do better next time###
###this same process was repeated for each day###
abund_tabled1<-read.csv("102617_commcomp_d1.csv", row.names=1, check.names=FALSE)
abund_tabled1 <-t(abund_tabled1)
####transpose data to have sample names on rows####
head(abund_tabled1)
meta_tbld1<-read.csv("102617_commcomp_meta_d1.csv", row.names=1, check.names=FALSE)
meta_tbld1<-meta_tbld1[rownames(abund_tabled1),]
head(meta_tbld1)

###calculate beta diversity using sorenson index###
beta <- betadiver(abund_tabled1, "w")
head(beta)
###run permutational mavona###
adonis(beta~treatment*concentration*priming_agent, meta_tbld1, permutations = 1500)

#### DAY 2####
abund_tabled2<-read.csv("102617_commcomp_d2.csv", row.names=1, check.names=FALSE)
abund_tabled2 <-t(abund_tabled2)
####transpose data to have sample names on rows####
head(abund_tabled2)
meta_tbld2<-read.csv("102617_commcomp_meta_d2.csv", row.names=1, check.names=FALSE)
meta_tbld2<-meta_tbld2[rownames(abund_tabled2),]
head(meta_tbld2)

beta2 <- betadiver(abund_tabled2, "w")
head(beta2)
adonis(beta2~treatment*concentration*priming_agent, meta_tbld2, permutations = 1500)

###Day 4#####
abund_tabled4<-read.csv("102617_commcomp_d4.csv", row.names=1, check.names=FALSE)
abund_tabled4 <-t(abund_tabled4)
####transpose data to have sample names on rows####
head(abund_tabled4)
meta_tbld4<-read.csv("102617_commcomp_meta_d4.csv", row.names=1, check.names=FALSE)
meta_tbld4<-meta_tbld4[rownames(abund_tabled4),]
head(meta_tbld4)

beta4 <- betadiver(abund_tabled4, "w")
head(beta4)
adonis(beta4~treatment*concentration*priming_agent, meta_tbld4, permutations = 1500)

###Day 7#####
abund_tabled7<-read.csv("102617_commcomp_d7.csv", row.names=1, check.names=FALSE)
abund_tabled7 <-t(abund_tabled7)
####transpose data to have sample names on rows####
head(abund_tabled7)
meta_tbld7<-read.csv("102617_commcomp_meta_d7.csv", row.names=1, check.names=FALSE)
meta_tbld7<-meta_tbld7[rownames(abund_tabled7),]
head(meta_tbld7)

beta7 <- betadiver(abund_tabled7, "w")
head(beta7)
adonis(beta7~treatment*concentration*priming_agent, meta_tbld7, permutations = 1500)

###Day 10#####
abund_tabled10<-read.csv("102617_commcomp_d10.csv", row.names=1, check.names=FALSE)
abund_tabled10 <-t(abund_tabled10)
####transpose data to have sample names on rows####
head(abund_tabled10)
meta_tbld10<-read.csv("102617_commcomp_meta_d10.csv", row.names=1, check.names=FALSE)
meta_tbld10<-meta_tbld10[rownames(abund_tabled10),]
head(meta_tbld10)

beta10 <- betadiver(abund_tabled10, "w")
head(beta10)
adonis(beta10~treatment*concentration*priming_agent, meta_tbld10, permutations = 1500)

###Day 14#####
abund_tabled14<-read.csv("102617_commcomp_d14.csv", row.names=1, check.names=FALSE)
abund_tabled14 <-t(abund_tabled14)
####transpose data to have sample names on rows####
head(abund_tabled14)
meta_tbld14<-read.csv("102617_commcomp_meta_d14.csv", row.names=1, check.names=FALSE)
meta_tbld14<-meta_tbld14[rownames(abund_tabled14),]
head(meta_tbld14)

beta14 <- betadiver(abund_tabled14, "w")
head(beta14)
adonis(beta14~treatment*concentration*priming_agent, meta_tbld14, permutations = 1500)

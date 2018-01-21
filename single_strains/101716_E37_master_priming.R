####load packages####
require(plyr)
require(ggplot2)
require(scales)
####load data####
E37 <-read.csv("101716_E37_twoweeks_master.csv")
head(E37)

####average cell densities and remove day 0###
E37b <- subset(E37, Day > 0)
E37c <- ddply(E37b, c("Day", "Concentration", "Carbon", "Treatment"), summarise, countm=mean(Count), countsd=sd(Count))
head(E37c)

###make y axis read 1x10^x#####
fancy_scientific <- function(l) {
  # turn in to character string in scientific notation
  l <- format(l, scientific = TRUE)
  # quote the part before the exponent to keep all the digits
  l <- gsub("^(.*)e", "'\\1'e", l)
  # turn the 'e+' into plotmath format
  l <- gsub("e+", "%*%10^", l)
  l <- gsub("0e\\00","0",l)
  # return this as an expression
  parse(text=l)
}

####Plot with all treatments####
E37c <- ggplot(E37c, aes(Day, countm, colour=Treatment, linetype=Treatment, ymin=countm-countsd, ymax=countm+countsd)) +
  geom_errorbar(width=0.1) +
  geom_line(stat="identity") +
  ylab('Cell Density (cells/mL)') +
  xlab('Time (d)') +
  scale_colour_manual(values=c("#E65933", "#58595B", "#2197A9", "#ABC178", "#58595B")) +
  scale_linetype_manual(values=c("solid", "twodash", "solid", "solid", "solid")) +
  scale_y_log10(breaks=c(1e5, 1e6, 1e7), labels=fancy_scientific) +
  scale_x_continuous(limits = c(0,15)) +
  theme(panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        legend.key = element_rect(fill = "white"),
        text=element_text(size=20),
        legend.position="top",legend.direction="horizontal") +
  facet_grid(Carbon~Concentration)
ggsave("101717_E37_priming_log.png", width=9, height=9, dpi=300)
print(E37c)

###figure for manuscript###
###subset for PRI and composite###
E37co <- subset(E37c, Treatment == "PRI" | Treatment == "composite")
###make plot####
E37co <- ggplot(E37co, aes(Day, countm, colour=Treatment, linetype=Treatment, ymin=countm-countsd, ymax=countm+countsd)) +
  geom_errorbar(width=0.1) +
  geom_line(stat="identity") +
  ylab('Cell Density (cells/mL)') +
  xlab('Time (d)') +
  scale_colour_manual(values=c("#58595B", "#58595B")) +
  scale_linetype_manual(values=c("twodash", "solid")) +
  scale_y_log10(breaks=c(1e5, 1e6, 1e7), labels=fancy_scientific) +
  scale_x_continuous(limits=c(0,15)) +
  theme(panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        legend.key = element_rect(fill = "white"),
        text=element_text(size=20),
        legend.position="top",legend.direction="horizontal") +
  facet_grid(Carbon~Concentration)
ggsave("110317_E37present.tiff", width=9, height=9)
print(E37co)


####load packages####
require(plyr)
require(ggplot2)
require(scales)
####load data####
SE45 <-read.csv("071817_SE45_twoweek_master.csv")
head(SE45)
tail(SE45)

####make datums better###
SE45b <- ddply(SE45, c("Day", "Concentration", "Carbon", "Treatment"), summarise, countm=mean(Count), countsd=sd(Count))
head(SE45b)
tail(SE45b)

SE45c <- subset(SE45b, Day > 0)
head(SE45c)

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
####Plot faceted by LOM####
SE45c <- ggplot(SE45c, aes(Day, countm, colour=Treatment, linetype=Treatment, ymin=countm-countsd, ymax=countm+countsd)) +
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
ggsave("102717_SE45_priming_log.tiff", width=9, height=9)
print(SE45c)

####presentation mode####
head(SE45c)
SE45cp <- subset(SE45c, Treatment == "PRI" | Treatment == "composite")
head(SE45cp)

SE45cpp <- ggplot(SE45cp, aes(Day, countm, linetype=Treatment, colour=Treatment, ymin=countm-countsd, ymax=countm+countsd)) +
  geom_errorbar(width=0.1) +
  geom_line(stat="identity") +
  ylab('Cell Density (cells/mL)') +
  xlab('Time (d)') +
  scale_linetype_manual(values=c("twodash", "solid")) +
  scale_color_manual(values=c("#58595B", "#58595B")) +
  theme(panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        legend.key = element_rect(fill = "white"),
        text=element_text(size=20),
        legend.position="top",legend.direction="horizontal") +
  scale_y_log10(labels=fancy_scientific) +
  scale_x_continuous(limits = c(0,15)) +
  facet_grid(Carbon~Concentration) +
ggsave("071817_SE45_priming_present.tiff", width=9, height=9)
print(SE45cpp)

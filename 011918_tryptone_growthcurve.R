###load necessary packages###
require(plyr)
require(ggplot2)
require(scales)

###import data###
tryptone <- read.csv("011918_tryptone.csv")

###generate averages and standard deviations for OD measurements###
tryptone_ave <- ddply(tryptone, c("Time", "Strain"), summarise, ODave=mean(OD), ODsd=sd(OD))
head(tryptone_ave)
tail(tryptone_ave)

###specify strain colors###
strain.colors <-c ("E-37"="#75417E", "EE-36"="#FF8200", "ISM"="#E65933", "NAS-14.1"="#B9E1E2", "SE45"="#ABC178", "Y4I"="#006C93")

###generate plot###
tryptonep <- ggplot(tryptone_ave, aes(Time, ODave, colour=Strain, ymin=ODave-ODsd, ymax=ODave+ODsd)) +
  geom_point() +
  geom_line() +
  scale_color_manual(values=strain.colors) +
  geom_errorbar(width=0.1) +
  labs(x="Time (h)", y=expression(OD[540]^{"-"})) +
  theme(panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        text=element_text(size=16),
        legend.position = "top",
        legend.key = element_rect(fill = "white"))

ggsave("011918_tryptonegc.pdf", height=8, width=12)
  
print(tryptonep)

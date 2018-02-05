###load packages####
require(plyr)
require(dplyr)
require(ggplot2)
require(scales)

####load data####
count <- read.csv("020518_countbox.csv")
head(count)

E37 <- subset(count, Strain == "E37")
str(E37)
E37p <- ggplot(E37, aes(Experiment, Count, fill=Carbon)) +
  geom_boxplot() + 
  geom_point(position = position_jitterdodge()) +
  ylab('Cell Density (cells/mL)') +
  xlab('Experiment') +
  scale_y_log10() + 
  facet_wrap(Treatment~Concentration, nrow=2) +
  theme(panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_blank(),
        text = element_text(size=16),
        panel.background = element_rect(fill = "white"),
        legend.key = element_rect(fill = "white"))
ggsave("020518_E37boxcount.pdf", height=6, width=15)
print(E37p)

SE45 <- subset(count, Strain == "SE45")
SE45p <- ggplot(SE45, aes(Experiment, Count, fill=Carbon)) +
  geom_boxplot() + 
  geom_point(position = position_jitterdodge()) +
  ylab('Cell Density (cells/mL)') +
  xlab('Experiment') +
  scale_y_log10() + 
  facet_wrap(Treatment~Concentration, nrow=2) +
  theme(panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_blank(),
        text = element_text(size=16),
        panel.background = element_rect(fill = "white"),
        legend.key = element_rect(fill = "white"))
ggsave("020518_SE45boxcount.pdf", height=6, width=15)
print(SE45p)

comm <- subset(count, Strain == "Community")
commp <- ggplot(comm, aes(Experiment, Count, fill=Carbon)) +
  geom_boxplot() + 
  geom_point(position = position_jitterdodge()) +
  ylab('Cell Density (cells/mL)') +
  xlab('Experiment') +
  scale_y_log10(limits=c(1e5, 5e7)) + 
  facet_wrap(Treatment~Concentration, nrow=2) +
  theme(panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_blank(),
        text = element_text(size=16),
        panel.background = element_rect(fill = "white"),
        legend.key = element_rect(fill = "white"))
ggsave("020518_commboxcount.pdf", height=6, width=15)
print(commp)

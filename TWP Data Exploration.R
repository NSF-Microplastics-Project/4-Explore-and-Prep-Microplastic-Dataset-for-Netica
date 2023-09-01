
library(ggplot2)
library(readr) # read_csv command 
library(gridExtra) # grid.arrange command
library(tidyverse)


NSF.manta <- read_csv("TWP Netica Data/Particles-mL/NSF_Manta_mL.csv")
NSF.effluent <- read_csv("TWP Netica Data/Particles-mL/NSF_Effluent_mL.csv")
NSF.stormwater <- read_csv("TWP Netica Data/Particles-mL/NSF_Stormwater_mL.csv")
NSF.sediment <- read_csv("TWP Netica Data/Particles-mL/NSF_Sediment_mL.csv")


NSF.manta$RiskRegion[NSF.manta$RiskRegion == "Coyote" ] <- "Coyote Creek"
NSF.effluent$RiskRegion[NSF.effluent$RiskRegion == "Coyote" ] <- "Coyote Creek"
NSF.stormwater$RiskRegion[NSF.stormwater$RiskRegion == "Coyote" ] <- "Coyote Creek"
NSF.sediment$RiskRegion[NSF.sediment$RiskRegion == "Coyote" ] <- "Coyote Creek"


# n = 17
nrow(NSF.effluent[NSF.effluent$RiskRegion == "Suisun Bay",]) # 4
nrow(NSF.effluent[NSF.effluent$RiskRegion == "San Francisco Bay",]) # 4
nrow(NSF.effluent[NSF.effluent$RiskRegion == "San Pablo Bay",]) # 2
nrow(NSF.effluent[NSF.effluent$RiskRegion == "Coyote",]) # 7 

# n = 44
nrow(NSF.manta[NSF.manta$RiskRegion == "Suisun Bay",]) # 2
nrow(NSF.manta[NSF.manta$RiskRegion == "San Francisco Bay",]) # 17
nrow(NSF.manta[NSF.manta$RiskRegion == "San Pablo Bay",]) # 18
nrow(NSF.manta[NSF.manta$RiskRegion == "Coyote",]) # 7

# n = 18
nrow(NSF.sediment[NSF.sediment$RiskRegion == "Suisun Bay",]) # 2
nrow(NSF.sediment[NSF.sediment$RiskRegion == "San Francisco Bay",]) # 6
nrow(NSF.sediment[NSF.sediment$RiskRegion == "San Pablo Bay",]) # 5
nrow(NSF.sediment[NSF.sediment$RiskRegion == "Coyote",]) # 5

# n = 13
nrow(NSF.stormwater[NSF.stormwater$RiskRegion == "Suisun Bay",]) # 0
nrow(NSF.stormwater[NSF.stormwater$RiskRegion == "San Francisco Bay",]) # 7
nrow(NSF.stormwater[NSF.stormwater$RiskRegion == "San Pablo Bay",]) # 4
nrow(NSF.stormwater[NSF.stormwater$RiskRegion == "Coyote",]) # 2


nrow(NSF.manta[NSF.manta$Season == "Wet",]) # 23
nrow(NSF.manta[NSF.manta$Season == "Dry",]) # 21

nrow(NSF.effluent[NSF.effluent$Treatment == "secondary",]) # 8
nrow(NSF.effluent[NSF.effluent$Treatment == "tertiary",]) # 9




jpeg("1_TWP_Concentration_FrequencyDis.jpeg", width = 12, height = 12, units = 'in', res = 300)

A <- ggplot(NSF.manta, aes(x=Manta)) +
  geom_histogram(bins = 15, colour = "black", fill = "gray") +
  theme_bw() + 
  theme(axis.title.x = element_blank(), 
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12), 
        plot.title = element_text(size=16), 
        strip.text.y = element_text(size = 10, angle = 0, face="bold"), 
        axis.title=element_text(size=14)) + 
  facet_grid(RiskRegion~., labeller = label_context) +
  theme(axis.text.x = element_text(angle = 0, size = 12, vjust =1, hjust = 0.5)) +
  labs(title="Manta Trawl", y="Number of Particles") 


B <- ggplot(NSF.effluent, aes(x= Effluent)) +
  geom_histogram(bins = 15, colour = "black", fill = "gray") +
  theme_bw() + 
  theme(axis.title.x = element_blank(), 
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12), 
        plot.title = element_text(size=16), 
        strip.text.y = element_text(size = 10, angle = 0, face="bold"), 
        axis.title=element_text(size=16)) + 
  facet_grid(RiskRegion~.) +
  theme(axis.text.x = element_text(angle = 0, size = 12, vjust =1, hjust = 0.5)) +
  labs(title="Effluent", y="Number of Particles")

C <- ggplot(NSF.sediment, aes(x= Sediment)) +
  geom_histogram(bins = 15, colour = "black", fill = "gray") +
  theme_bw() + 
  theme(axis.title.x = element_blank(), 
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 14), 
        plot.title = element_text(size=16), 
        strip.text.y = element_text(size = 10, angle = 0, face="bold"), 
        axis.title=element_text(size=16)) + 
  facet_grid(RiskRegion~.) +
  theme(axis.text.x = element_text(angle = 0, size = 12, vjust =1, hjust = 0.5)) +
  labs(title="Sediment", y="Number of Particles")

D <- ggplot(NSF.stormwater, aes(x= Stormwater)) +
  geom_histogram(bins = 15, colour = "black", fill = "gray") +
  theme_bw() + 
  theme(axis.title.x = element_blank(), 
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 14), 
        plot.title = element_text(size=16), 
        strip.text.y = element_text(size = 10, angle = 0, face="bold"), 
        axis.title=element_text(size=16)) + 
  facet_grid(RiskRegion~.) +
  theme(axis.text.x = element_text(angle = 0, size = 12, vjust =1, hjust = 0.5)) +
  labs(title="Stormwater", y="Number of Particles")

grid.arrange(A, B, C, D, ncol=2, nrow=2)

dev.off()


##### Adjust Axis

A1 <- ggplot(NSF.manta, aes(x=Manta)) +
  geom_histogram(bins = 15, colour = "black", fill = "gray") +
  theme_bw() + 
  theme(axis.title.x = element_blank(), axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16), plot.title = element_text(size=20, face="bold"), strip.text.y = element_text(size = 12, face="bold"), axis.title=element_text(size=16)) + 
  facet_grid(RiskRegion~.) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title="Manta") +
  coord_cartesian(xlim = c(0,0.6), ylim = c(0,17))


B1 <- ggplot(NSF.effluent, aes(x= Effluent)) +
  geom_histogram(bins = 15, colour = "black", fill = "gray") +
  theme_bw() + 
  theme(axis.title.x = element_blank(), axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16), plot.title = element_text(size=20, face="bold"), strip.text.y = element_text(size = 12, face="bold"), axis.title=element_text(size=16)) + 
  facet_grid(RiskRegion~.) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title="Effluent") +
  coord_cartesian(xlim = c(0,0.6), ylim = c(0,17))

C1 <- ggplot(NSF.sediment, aes(x= Sediment)) +
  geom_histogram(bins = 15, colour = "black", fill = "gray") +
  theme_bw() + 
  theme(axis.title.x = element_blank(), axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16), plot.title = element_text(size=20, face="bold"), strip.text.y = element_text(size = 12, face="bold"), axis.title=element_text(size=16)) + 
  facet_grid(RiskRegion~.) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title="Sediment") +
  coord_cartesian(xlim = c(0,0.6), ylim = c(0,17))

D1 <- ggplot(NSF.stormwater, aes(x= Stormwater)) +
  geom_histogram(bins = 15, colour = "black", fill = "gray") +
  theme_bw() + 
  theme(axis.title.x = element_blank(), axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16), plot.title = element_text(size=20, face="bold"), strip.text.y = element_text(size = 12, face="bold"), axis.title=element_text(size=16)) + 
  facet_grid(RiskRegion~.) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title="Stormwater") +
  coord_cartesian(xlim = c(0,0.6),ylim = c(0,17))

grid.arrange(A1, B1, C1, D1, ncol=2, nrow=2)



# Create merged dataset
NSF.effluent.merge <- NSF.effluent[ -c(1) ] # Drop treatment column
NSF.effluent.merge <- NSF.effluent.merge %>% #rename manta column to match all other datasheets 
  rename(Particle_Con = Effluent)
NSF.effluent.merge$Sample_Matrix <- "Effluent" # Add column with sample matrix names


NSF.manta.merge <- NSF.manta[ -c(2) ] 
NSF.manta.merge <- NSF.manta.merge %>% 
  rename(Particle_Con = Manta)
NSF.manta.merge$Sample_Matrix <- "Manta"


NSF.sediment.merge <- NSF.sediment %>%
  rename(Particle_Con = Sediment)
NSF.sediment.merge$Sample_Matrix <- "Sediment"

NSF.stormwater.merge <- NSF.stormwater %>%
  rename(Particle_Con = Stormwater)
NSF.stormwater.merge$Sample_Matrix <- "Stormwater"



NSF.ALL <- rbind(NSF.manta.merge, NSF.effluent.merge, NSF.stormwater.merge, NSF.sediment.merge)


jpeg("Output Figures/TWP_Concentration_Histrogram.jpeg", width = 12, height = 11, units = 'in', res = 300)

ggplot(NSF.ALL, aes(x=Particle_Con)) +
  geom_histogram(bins = 20, colour = "black", fill = "gray") +
  theme_bw() + 
  labs(title="Potential Tire Wear Particle Concentrations") + xlab("Particle Concentrations (particles/mL)") + ylab("Sample Count") +
  theme(axis.text.x =element_text(size=14, angle=0),
        axis.text.y = element_text(size=14, angle=0),
        plot.title=element_text( colour='black', size=17, margin=margin(t=2,b=3), hjust = 0.5),
        axis.title=element_text(size=16),
        strip.text.y = element_text(size = 14, face="bold"), 
        strip.text.x = element_text(size = 14, face="bold"), 
        strip.background = element_rect(size = 1.5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_cartesian(xlim = c(0,0.6),ylim = c(0,20)) +
  facet_grid(RiskRegion~Sample_Matrix)

dev.off()



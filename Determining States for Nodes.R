# Looking for breaks 

library(ggplot2)
library(tidyverse)
library(BAMMtools) # Required for getJenksBreaks() 

# Read in data
manta <- read_csv("manta.plastic_TWP.table.csv")
effluent <- read_csv("Effluent_SampleIDtable.csv")
sediment <- read_csv("Sediment_SampleIDtable copy 1.csv") 
stormwater <- read_csv("Stormwater_SampleIDtable copy 1.csv") 


##### If not already added, we need to add the concetration column

# Sediment
# sediment$liters <- sediment$`Mass(g.dw)` / 1000 # convert grams to liters; this may need to be adjusted to consider specific sediment type 
# sediment$sedTWPconc <- sediment$TWP / sediment$liters
# sediment$sedTWPconc.mL <- sediment$TWP / sediment$liters / 1000 # Dividing particle number column by liter column


######### Particles per mililiter #########
# Stormwater
stormwater$stormTWPconc.mL <- stormwater$TWP / stormwater$Liters / 1000 

# Effluent
effluent$effTWPconc.mL <- effluent$TWP / effluent$Liters /1000 

# Manta
manta$Liters <- as.numeric(manta$Liters) 
manta$mantaTWPconc.mL <- manta$TWP / manta$Liters / 1000

# Sediment
sediment$sedimentTWPconc.mL <- 0.5*(sediment$TWP / sediment$`Mass(g.dw)`)


######### Particles per liter #########
# Effluent
effluent$effTWPconc.L <- effluent$TWP / effluent$Liters

# Stormwater
stormwater$stormTWPconc.L <- stormwater$TWP / stormwater$Liters

# Manta
manta$mantaTWPconc.L <- manta$TWP / manta$Liters


#### Distrubtions

ggplot(effluent, aes(effTWPconc.mL)) +  # TWP in Effluent 
  geom_histogram()


ggplot(manta, aes(mantaTWPconc.mL)) +  # TWP in Manta 
  geom_histogram()


ggplot(sediment, aes(sedTWPconc.mL)) +  # TWP in Sediment
  geom_histogram(bins=4)


ggplot(stormwater, aes(stormTWPconc.mL)) +  # TWP in Stormwater 
  geom_histogram()



#### Jenks optimization

# With particles per mL

getJenksBreaks(effluent$effTWPconc.mL, 4) # TWP in Effluent 

getJenksBreaks(manta$mantaTWPconc.mL, 4) # TWP in Manta 

getJenksBreaks(sediment$sedTWPconc.mL, 5)# TWP in Sediment

getJenksBreaks(stormwater$stormTWPconc.mL, 4) # TWP in Stormwater



# With particles per L

getJenksBreaks(effluent$effTWPconc.L, 4) # TWP in Effluent 

getJenksBreaks(manta$mantaTWPconc.L, 4) # TWP in Manta 

getJenksBreaks(sediment$sedTWPconc.L, 4) # TWP in Sediment

getJenksBreaks(stormwater$stormTWPconc.L, 4) # TWP in Stormwater

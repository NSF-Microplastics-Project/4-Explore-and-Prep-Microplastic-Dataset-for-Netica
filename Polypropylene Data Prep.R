# Load packages
library(tidyverse)
library(readr) # for reading in csv as tibble using read_csv
library(dplyr)
library(ggplot2)
library(lubridate) # Need to deal with dates (e.g. as.Date())


manta.PP <- read_csv("manta.PP.plastic_TWP.table.csv")
effluent.PP <- read_csv("Effluent_SampleIDtable.csv")
sediment.PP <- read_csv("Sediment_SampleIDtable copy 1.csv") 
stormwater.PP <- read_csv("Stormwater_SampleIDtable copy 1.csv") 




#### PREPPING SEDIMENT ####

sediment$liters <- sediment$`Mass(g.dw)` / 1000 # convert grams to liters; this may need to be adjusted to consider specific sediment type 

sediment$PPconc <- sediment$Polypropylene / sediment$liters # Dividing particle number column by liter column

sediment.PPsub <- sediment[c(3,11,14)] #Select columns that will be represented in the Bayesian Net

sediment.PPsub$PPconc <- signif(sediment.PPsub$PPconc, 2) # Adjust to correct significant figures: in this case that is 2 based of SFEI report

sediment.PPsub$SampleDate <- as.Date(sediment.PPsub$SampleDate, format = "%m/%d/%y") # Convert Sample Date to date and reformat to year, month, day


write.csv(sediment.PPsub, "Sediment_PPpractice.csv")



#### PPREPPING STORMWATER ####

stormwater$PPconc <- stormwater$Polypropylene / stormwater$Liters

stormwater.PPsub <- stormwater[c(4,11,13)] #Select columns that will be represented in the Bayesian Net

stormwater.PPsub$PPconc <- signif(stormwater.PPsub$PPconc, 2) # Adjust to correct significant figures: in this case that is 2 based of SFEI report

stormwater.PPsub$SampleDate <- as.Date(stormwater.PPsub$SampleDate, format = "%m/%d/%y") # Convert Sample Date to date and reformat to year, month, day


write.csv(stormwater.PPsub, "Stormwater_PPpractice.csv")



#### PPREPPING EFFLUENT ####

effluent$PPconc <- effluent$Polypropylene / effluent$Liters # Dividing particle number column by liter column

effluent.PPsub <- effluent[c(2,4,6,12)] #Select columns that will be represented in the Bayesian Net

effluent.PPsub$PPconc <- signif(effluent.PPsub$PPconc, 1) # Adjust to correct significant figures: in this case that is 2 based of SFEI report

effluent.PPsub$SampleDate <- as.Date(effluent.PPsub$SampleDate, format = "%m/%d/%y") # Convert Sample Date to date and reformat to year, month, day

effluent.PPsub <- effluent.PPsub %>%  rename(Region = Discharge.Point) # rename Discharge.Point in effluent dataframe to match region column in manta.PP dataframe

write.csv(effluent.PPsub, "Effluent_PPpractice.csv")



#### PPREPPING manta.PP ####

manta.PP$Liters <- as.numeric(manta.PP$Liters)  # liters was characters - changed to numeric
                                          # Results: liters and TWP are integers which is what we want

manta.PP$PPconc <- manta.PP$Polypropylene / manta.PP$Liters

manta.PPsub <- manta.PP[c(2,4,5,12)] #Select columns that will be represented in the Bayesian Net

manta.PPsub$PPconc <- signif(manta.PP.PPsub$PPconc, 1) # Adjust to correct significant figures: in this case that is 2 based of SFEI report

manta.PPsub$SampleDate <- as.Date(manta.PP.PPsub$SampleDate, format = "%m/%d/%y") # Convert Sample Date to date and reformat to year, month, day

write.csv(manta.PPsub, "manta.PP_PPpractice.csv")



# Note: I am not going to try to aggregate the data because it will be the same results as the attempt with the TWP. All samples were collected on the same days (i.e. TWP and other plastics). 

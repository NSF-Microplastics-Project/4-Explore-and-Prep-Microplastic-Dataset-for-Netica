
# Load packages
library(tidyverse)
library(readr) # for reading in csv as tibble using read_csv
library(dplyr)
library(ggplot2)
library(lubridate) # Need to deal with dates (e.g. as.Date())
library(BAMMtools) # Required for getJenksBreaks() 


manta <- read_csv("Input Tables/manta.plastic_TWP.table.csv")
effluent <- read_csv("Input Tables/Effluent_SampleIDtable.csv")
sediment <- read_csv("Input Tables/Sediment_SampleIDtable copy 1.csv")
stormwater <- read_csv("Input Tables/Stormwater_SampleIDtable copy 1.csv")



#### Correct the data class ####

manta$Liters <- as.numeric(manta$Liters)  # liters was characters - changed to numeric
# Results: liters and TWP are integers which is what we want



#### PREPPING SEDIMENT ####

# sediment$liters <- sediment$`Mass(g.dw)` / 1000 # convert grams to liters; this may need to be adjusted to consider specific sediment type 

# sediment$sedTWPconc <- sediment$TWP / sediment$liters # Dividing particle number column by liter column

# sediment$sedTWPconc.mL <- sediment$TWP / sediment$liters / 1000 # Convert to particles/mL


sediment$sedimentTWPconc.mL <- 0.5*(sediment$TWP / sediment$`Mass(g.dw)`) # 0.5 used because 0.5kg/L was the rough number given by Diana Lin for the sediment conversion 

sediment$SampleDate <- as.Date(sediment$SampleDate, format = "%m/%d/%y") # Convert Sample Date to date and reformat to year, month, day

sediment.sub <- sediment[c(11,13)] #Select columns that will be represented in the Bayesian Net

sediment.sub$sedimentTWPconc.mL <- signif(sediment.sub$sedimentTWPconc.mL, 2) # Adjust to correct significant figures: in this case that is 2 based of SFEI report

sediment.sub <- sediment.sub %>%  rename(Sediment = sedimentTWPconc.mL) # Rename the sediment column 

names(sediment.sub)[names(sediment.sub) == "Region"] <- "RiskRegion" 

write.csv(sediment.sub, "TWP Netica Data/Particles-mL/NSF_Sediment_mL.csv", row.names=FALSE)

getJenksBreaks(sediment.sub$Sediment, 3)



#### PPREPPING STORMWATER ####

stormwater$stormTWPconc.L <- stormwater$TWP / stormwater$Liters

stormwater$stormTWPconc.mL <- stormwater$TWP / stormwater$Liters / 1000 # Convert to particles/mL

stormwater$SampleDate <- as.Date(stormwater$SampleDate, format = "%m/%d/%y") # Convert Sample Date to date and reformat to year, month, day

stormwater.sub <- stormwater[c(11,14)] #Select columns that will be represented in the Bayesian Net

stormwater.sub$stormTWPconc.mL <- signif(stormwater.sub$stormTWPconc.mL, 2) # Adjust to correct significant figures: in this case that is 2 based of SFEI report

stormwater.sub <- stormwater.sub %>%  rename(Stormwater = stormTWPconc.mL) # Rename the stormwater column 

names(stormwater.sub)[names(stormwater.sub) == "Region"] <- "RiskRegion"

write.csv(stormwater.sub, "TWP Netica Data/Particles-mL/NSF_Stormwater_mL.csv", row.names=FALSE)

getJenksBreaks(stormwater.sub$Stormwater, 5)



#### PPREPPING EFFLUENT ####

effluent$effTWPconc.L <- effluent$TWP / effluent$Liters # Dividing particle number column by liter column

effluent$effTWPconc.mL <- effluent$TWP / effluent$Liters /1000 # Convert to particles/mL

effluent$SampleDate <- as.Date(effluent$SampleDate, format = "%m/%d/%y") # Convert Sample Date to date and reformat to year, month, day

effluent.sub <- effluent[c(4,6,13)] #Select columns that will be represented in the Bayesian Net

effluent.sub$effTWPconc.mL <- signif(effluent.sub$effTWPconc.mL, 2) # Adjust to correct significant figures: in this case that is 2 based of SFEI report

effluent.sub <- effluent.sub %>%  rename(Region = Discharge.Point) # rename Discharge.Point in effluent dataframe to match region column in manta dataframe

effluent.sub <- effluent.sub %>%  rename(Effluent = effTWPconc.mL) # Rename the effluent column 

names(effluent.sub)[names(effluent.sub) == "Region"] <- "RiskRegion"

write.csv(effluent.sub, "TWP Netica Data/Particles-mL/NSF_Effluent_mL.csv", row.names=FALSE)

getJenksBreaks(effluent.sub$Effluent, 5)



#### PPREPPING MANTA ####

manta$mantaTWPconc.L <- manta$TWP / manta$Liters

manta$mantaTWPconc.mL <- manta$TWP / manta$Liters / 1000 

manta$SampleDate <- as.Date(manta$SampleDate, format = "%m/%d/%y") # Convert Sample Date to date and reformat to year, month, day

manta.sub <- manta[c(4,5,13)] #Select columns that will be represented in the Bayesian Net

manta.sub$mantaTWPconc.mL <- signif(manta.sub$mantaTWPconc.mL, 2) # Adjust to correct significant figures: in this case that is 2 based of SFEI report

manta.sub <- manta.sub %>%  rename(Manta = mantaTWPconc.mL) # Rename the Manta column 

names(manta.sub)[names(manta.sub) == "Region"] <- "RiskRegion"

write.csv(manta.sub, "TWP Netica Data/Particles-mL/NSF_Manta_mL.csv", row.names=FALSE)

getJenksBreaks(manta.sub$Manta, 5)


#########################################################################################################
#### The code below was used to determine whether any of the data could be aggregated based on data #####

# The result was that because none of the data was collected on the same day or even within the same week, data could not be aggregated by date

######################
#### Aggregating ####

# The goal here is to see if there is anyway to combine and aggregate data to use for case file learning in netica

storm_sediment.sub  <- merge(x = stormwater.sub, y = sediment.sub, by = c("Region", "SampleDate"), all = TRUE) # Combine stormwater.sub and sediment.sub by Region and SampleDate


storm_sediment.reg_date <- aggregate(x = strom_sediment.sub[c("stormTWPconc","sedTWPconc")],
                           FUN = mean,
                           by = list(Group.date = strom_sediment.sub$SampleDate, Region = strom_sediment.sub$Region)) # I think this is what we want 


storm_sediment.sub1 <- storm_sediment.sub # make a duplicate sheet so a new month column can be added but to effect the original

storm_sediment.sub1$month <- floor_date(storm_sediment.sub1$SampleDate, "month") # Round columns by month and add month column


storm_sediment.sub1 %>%
  group_by(month) %>%
  summarize(mean = mean(stormTWPconc)) # Aggregate by month 

storm_sediment.sub1 %>%
  group_by(month) %>%
  summarize(mean = mean(sedTWPconc)) # Look at the above output to see if there are any months that they both had samples

# Conclusion: It appears to me that we can not aggregate stormwater and sediment data by week or month of sample date. No samples were collected even within the same month outside of each sample matrix





storm_manta.sub  <- merge(x = stormwater.sub, y = manta.sub, by = c("Region", "SampleDate"), all = TRUE) # Combine stormwater.sub and manta.sub by Region and SampleDate


storm_manta.reg_date <- aggregate(x = strom_manta.sub[c("stormTWPconc","mantaTWPconc")],
                                     FUN = mean,
                                     by = list(Group.date = strom_manta.sub$SampleDate, Region = strom_manta.sub$Region)) # I think this is what we want 


storm_manta.sub1 <- storm_manta.sub # make a duplicate sheet so a new month column can be added but to effect the original

storm_manta.sub1$month <- floor_date(storm_manta.sub1$SampleDate, "month") # Round columns by month and add month column


storm_manta.sub1 %>%
  group_by(month) %>%
  summarize(mean = mean(stormTWPconc)) # Aggregate by month 

storm_manta.sub1 %>%
  group_by(month) %>%
  summarize(mean = mean(mantaTWPconc)) # Look at the above output to see if there are any months that they both had samples

# Conclusion: It appears to me that we can not aggregate stormwater and manta data by week or month of sample date. No samples were collected even within the same month outside of each sample matrix





effluent_sediment.sub  <- merge(x = effluent.sub, y = sediment.sub, by = c("Region", "SampleDate"), all = TRUE) # Combine effluent.sub and sediment.sub by Region and SampleDate


effluent_sediment.reg_date <- aggregate(x = strom_manta.sub[c("effTWPconc","sedTWPconc")],
                                  FUN = mean,
                                  by = list(Group.date = effluent_sediment.sub$SampleDate, Region = effluent_sediment.sub$Region)) # I think this is what we want 


effluent_sediment.sub1 <- effluent_sediment.sub # make a duplicate sheet so a new month column can be added but to effect the original

effluent_sediment.sub1$month <- floor_date(effluent_sediment.sub1$SampleDate, "month") # Round columns by month and add month column


effluent_sediment.sub1 %>%
  group_by(month) %>%
  summarize(mean = mean(effTWPconc)) # Aggregate by month 

effluent_sediment.sub1 %>%
  group_by(month) %>%
  summarize(mean = mean(sedTWPconc)) # Look at the above output to see if there are any months that they both had samples

# Conclusion: It appears to me that we can not aggregate effluent and sediment data by week or month of sample date. No samples were collected even within the same month outside of each sample matrix





effluent_manta.sub  <- merge(x = effluent.sub, y = manta.sub, by = c("Region", "SampleDate"), all = TRUE) # Combine effluent.sub and manta.sub by Region and SampleDate


effluent_manta.reg_date <- aggregate(x = strom_manta.sub[c("effTWPconc","sedTWPconc")],
                                        FUN = mean,
                                        by = list(Group.date = effluent_manta.sub$SampleDate, Region = effluent_manta.sub$Region)) # I think this is what we want 


effluent_manta.sub1 <- effluent_manta.sub # make a duplicate sheet so a new month column can be added but to effect the original

effluent_manta.sub1$month <- floor_date(effluent_manta.sub1$SampleDate, "month") # Round columns by month and add month column


effluent_manta.sub1 %>%
  group_by(month) %>%
  summarize(mean = mean(effTWPconc)) # Aggregate by month 

effluent_manta.sub1 %>%
  group_by(month) %>%
  summarize(mean = mean(sedTWPconc)) # Look at the above output to see if there are any months that they both had samples

# Conclusion: It appears to me that we can not aggregate effluent and manta data by week or month of sample date. No samples were collected even within the same month outside of each sample matrix


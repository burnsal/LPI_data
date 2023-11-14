# Load libraries

library(tidyr)
library(dplyr)
library(ggplot2)

# read in data
spikeLPI<-read.csv("./data/Spike.NKS.8-4-2022 V2.csv")
spikeLPIGather <- read.csv("./data/Spike_LPI_Gather.csv")
spikeCanopy <- read.csv("./data/CanopyGap20192022Spike.csv")

# 

Site <- spikeLPI$Site

spikeLPI_group<-spikeLPI %>% 
  group_by(ï..Site, Transect, Date, Strip, Grazed)

deadlist<-c("AF.DEAD","AG.DEAD" ,"PF.DEAD", "ARTR4.DEAD", "ARTRV.DEAD", 
            "TECA2.DEAD","PG.DEAD", "PUTR2.DEAD","PF.DEAD" )

# Change all species to a numeric value of 1 as long as they are not listed as NA

spike_wide <- spikeLPI_group %>%
  mutate_if(is.character, funs(ifelse(is.na(.), 0, 1)))

# Do a species total count for each group- however in this case, species totals
# are shown for each point as well

df_Foliar <- spike_wide %>%
  rowwise() %>%
  mutate(Foliar = sum(c_across(TopCanopy:Lower4)))
 
head(df_Foliar)

# We need to convert the columns of TopCanopy:Lower 4 into a single row group
# that we will call "HitType" while each species listed among them will be
# designated as "HitSp" so we can get a species tally for only our desired groups

# Let's try the gather function.

spikeLPI_gather <- spikeLPI %>% 
  gather(, key="HitType", value="HitSpecies", 8:12)

# Now let's group this new data frame in the appropriate format

spikeLPI_ggroup<-spikeLPI_gather %>% 
  group_by(ï..Site, Transect, Date, Strip, Grazed)

# Let's write the data as a new .csv file to play with it in Excel temporarily

write.csv(spikeLPI_gather, "Spike LPI_Gather.csv")

# Before we pivot_wider we need to account for all the dead species from deadlist
# as well as the NA and None values- since we do not want to count those for
# total counts.

# Let's make a lookup table so we can create a list of values to distinguish

lookup <- data.frame(HitSpecies = c("AF.DEAD","AG.DEAD" ,"PF.DEAD", "ARTR4.DEAD", 
                                         "ARTRV.DEAD", "TECA2.DEAD","PG.DEAD", 
                                         "PUTR2.DEAD","PF.DEAD","None"),
                     NAHitSpecies = c("NA","NA" ,"NA", "NA", 
                                     "NA", "NA","NA", 
                                     "NA","NA","NA"),
                          stringsAsFactors = FALSE)

# Now let's change all values in the deadlist_V2 to NA- this first method is the long way

spikeLPI_gather_deadlistNA <- spikeLPI_gather 
  
spikeLPI_gather_deadlistNA[spikeLPI_gather_deadlistNA == "AF.DEAD" | spikeLPI_gather_deadlistNA == "AG.DEAD" | 
                    spikeLPI_gather_deadlistNA == "ARTR4.DEAD" | spikeLPI_gather_deadlistNA == "ARTRV.DEAD" | 
                    spikeLPI_gather_deadlistNA == "TECA2.DEAD" | spikeLPI_gather_deadlistNA == "PG.DEAD" | 
                    spikeLPI_gather_deadlistNA == "PUTR2.DEAD" | spikeLPI_gather_deadlistNA == "PF.DEAD" | 
                    spikeLPI_gather_deadlistNA == "None"] <- "NA"

# Let's try a method using a lookup table to change the values

deadlist_V2<-c("AF.DEAD","AG.DEAD" ,"PF.DEAD", "ARTR4.DEAD", "ARTRV.DEAD", 
            "TECA2.DEAD","PG.DEAD", "PUTR2.DEAD","PF.DEAD","None")

df_deadlookup <- spikeLPI_gather  

df_deadlookup[df_deadlookup == any(deadlist)] = "NA"

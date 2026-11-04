
#Purpose: combine fitness measurements taken from plants in the pollen supplement experiment (PSE) with those from plants
#in the main study to create one fitness data frame to measure fitness across all plants 

library(tidyverse)
library(ggplot2)

#downloading fitness data 
fitty <- read.csv("data/Fitness_Measurements_Main_Hesp.csv") #data for the majority of plants (those not in pollen supplement experiment)
pollen <- read.csv("data/Pollen_Fitness_Measurements.csv") #fitness data for plants in the pollen supplement experiment - PSE  (has additional info bc of the more intensive measurement of three fruits)
surv <- read.csv("data/Hesp_Colour_Scoring_2023.csv") #data on whether plant_ids are in PSE or not at whether they are treated or controls

#Cleaning Pollen fit data ----
#fixing plant_ids that were entered incorrectly (after double checking the handwritten data vs typed data )
pollen <- pollen %>% 
  mutate(row_id = row_number(),
         plant_id = if_else(row_id == 92, "KR3_153", plant_id),
         plant_id = if_else(row_id == 183, "KR5_202", plant_id),
         plant_id = if_else(row_id == 217, "KU4_230", plant_id),
         plant_id = if_else(row_id == 247, "KR2_237", plant_id),
         plant_id = if_else(row_id == 255, "KU4_190", plant_id),
         plant_id = if_else(row_id == 258, "KU1_184", plant_id),
         plant_id = if_else(row_id == 300, "KR3_144", plant_id),
         plant_id = if_else(row_id == 308, "KU4_244", plant_id),
         plant_id = if_else(row_id == 329, "KU1_153", plant_id),
         plant_id = if_else(row_id == 379, "KR2_7", plant_id),
         plant_id = if_else(row_id == 388, "KR3_246", plant_id),
         plant_id = if_else(row_id == 187, "KU1_38", plant_id),
         plant_id = if_else(row_id == 5, "delete_me", plant_id), #double the number of treated flowers removing extras
         ) %>% 
  filter(!plant_id %in% c("KU3_90", "NO TAG", "delete_me", "KU5_190")) %>%  #KU3_90:correct id cannot be identified from hand writting, KU5_190: not in the pollen experiment & dead, 
  filter(!notes %in% c("TAGGED FRUIT FROM PLANT BESIDE", "maybe 76, tag beside plant (not attached)", "BAG CONTAINS ZIPLOC BAG THAT SAYS 3 PLANTS 1 TAG NO FRUITS")) %>%  #removing plants with suspicious notes
  dplyr::select(!row_id)

pollen$plant_id <- gsub("-", "_", pollen$plant_id)

#below plant_ids were removed...
#a) after finding duplicates after combining the pollen and main study fit data (see below line )
#b) after checking whether plant_ids match with those that are recorded as being in the PSE 
temp <- left_join(pollen, surv) #looked at NAs in the sup_pollen column

pollen <- pollen %>% 
  filter(!plant_id %in% c("KR2_64", "KR2_91", "KU1_138", "KU1_179", "KU2_13", "KU4_1")) %>% #removing plant_ids that are in both the main fit data sheet and the pollen experiment data sheet (indicating some sort of error in data entry) -same as those on line 96
  filter(!plant_id %in% c("KU1_115", "KR3_1", "KR2_36", "KU3_75", "KU4_115", "KR3_46", "KR3_40", "KU1_90")) # plant ids that aren't recorded as being in PSE, but had envelopes of fruits marked as treated (indicating some sort of error in data entry)

# "KU3_5", "KR3_4", "KR2_108", "KU4_46", "KU4_12" are plant_ids on pollen data sheet that should only be in main study fit data, and did not have fruit marking of being treat
#this plants are being left in for now to join with the main study fit data, but will be removed at the end of this script before the clean pollen fit data is saved
temp <- left_join(pollen, surv) #looked at NAs in the sup_pollen column

#Cleaning Main study fit data ----
#fixing plant_ids that were entered incorrectly (after double checking the handwritten data vs typed data )
fitty <- fitty %>% 
  mutate(plant_id = str_replace(plant_id, "KR5_203", "KR5_208"),
         plant_id = str_replace(plant_id, "K42_240", "KR2_240"),
         plant_id = str_replace(plant_id, "K43_112", "KR3_112"),
         plant_id = str_replace(plant_id, "KUR_163", "KU4_163")
  ) 

#fixing or dropping duplicates
fitty <- fitty %>% 
  mutate(row_id = row_number(),
         plant_id = if_else(row_id == 409, "KR2_10", plant_id),
         plant_id = if_else(row_id == 301, "KR2_23", plant_id),
         plant_id = if_else(row_id == 573, "KR2_4", plant_id),
         plant_id = if_else(row_id == 346, "KR3_14", plant_id),
         plant_id = if_else(row_id == 366, "KR3_21", plant_id),
         plant_id = if_else(row_id == 839, "KR4_11", plant_id),
         plant_id = if_else(row_id == 837, "KR4_12", plant_id),
         plant_id = if_else(row_id == 488, "KR5_223", plant_id),
         plant_id = if_else(row_id == 212, "KR4_53", plant_id),
         plant_id = if_else(row_id == 109, "KR5_2", plant_id),
         plant_id = if_else(row_id == 1399, "KU1_191", plant_id),
         plant_id = if_else(row_id == 184, "delete_me", plant_id),
         plant_id = if_else(row_id == 1629, "delete_me", plant_id),
         plant_id = if_else(row_id == 888, "KR4_128", plant_id),
         plant_id = if_else(row_id == 646, "delete_me", plant_id),
         plant_id = if_else(row_id == 223, "KU3_15", plant_id),
         plant_id = if_else(row_id == 1483, "KU5_221", plant_id),
         plant_id = if_else(row_id == 1610, "KU3_4", plant_id),
         plant_id = if_else(row_id == 1386, "KU4_93", plant_id),
         plant_id = if_else(row_id == 194, "delete_me", plant_id),
         plant_id = if_else(row_id == 1561, "delete_me", plant_id), #sample measured twice removing second measurement)
         plant_id = if_else(row_id == 248, "delete_me", plant_id), #sample measured twice removing second measurement)
         plant_id = if_else(row_id == 247, "delete_me", plant_id) #sample measured twice removing second measurement)
  ) %>% 
  dplyr::select(!row_id)

#removing rows with duplicated plant_ids, where true id can't be verified or note about 2 stems collected (ambiguity in field of which plant the id ring belonged to)
fitty <- fitty %>% 
  filter(!plant_id %in% c("KR1_11", "KR1_117", "KR1_131", "KR1_21", "KR1_89", "KR2_169", "KR2_189", "KR3_110", "KR3_112", "KR3_151", "KR3_241", "KR3_91",
                          "KR4_140", "KR4_178", "KR5_167", "KR5_206", "KR5_240", "KU1_113", "KU1_117", "KU1_181", "KU1_41", "delete_me", "KU2_208", "KU3_155", "KU3_222",
                          "KU3_91", "KU4_196", "KU4_61", "KU4_72", "KU4_78", "KU5_1", "KU5_13", "KU5_131", "KU5_165", "KU5_17", "no label", "KU4_105", "KR4_51")) 

#removing plant_ids that are in both the main fit data sheet and the pollen experiment data sheet (indicating some sort of misentry) -same as those on line 43
fitty <- fitty %>% 
  filter(!plant_id %in% c("KR2_64", "KR2_91", "KU1_138", "KU1_179", "KU2_13", "KU4_1"))  

#removing plants with suspicuous notes 
fitty <- fitty %>% 
  filter(!notes %in% c("tag wrapped around 2 stems. both included", "snapped below repro - a repro found beside (not sure if from same)", "2P1T", "2 plants, 1 tag", "2 plants 1 tag", "2 PIT", "trampled", "dead repro missing", "repro missing", "no fruit - repro snapped", "fruit totally broken into many pieces, but no seeds", "found snapped", "dead & missing part of stem", "dead stem missing", "? found loose", "severed from stem", "stepped on", "looks weirdly flat"))

#Restructuring data frames to combine them together ----
#all plants including those in PSE are used for overall fitness analysis

#removing measurements specific to PSE
fit_pollen <- pollen %>% 
  dplyr::select(initial:stem_diam_mm, F1_length:notes) %>% 
  pivot_longer(cols = ends_with("length"),
               names_to = c("fruit_sample", "delete"),
               names_sep = "_",
               values_to = "length") %>% 
  dplyr::select(!delete)


#reorganizing fitness data into long form
length <- fitty %>% 
  dplyr::select(initial:fruit_num, ends_with("length"), notes) %>% 
  pivot_longer(cols = ends_with("length"),
               names_to = c("fruit_sample", "delete"),
               names_sep = "_",
               values_to = "length") %>% 
  dplyr::select(!delete)

open <- fitty %>% 
  dplyr::select(plant_id, ends_with("open")) %>% 
  pivot_longer(cols = ends_with("open"),
               names_to = c("fruit_sample", "delete"),
               names_sep = "_",
               values_to = "open"
  ) %>% 
  dplyr::select(!delete)
temp <- left_join(length, open)
temp <- dplyr::select(temp, !open)

fit <- rbind(temp, fit_pollen)

#Saving cleaned data frames ----

#final combined fitness data frame
#write.csv(fit, "data/full_fit.csv", row.names = FALSE) 

pollen <- pollen %>% 
  filter(!plant_id %in% c("KU3_5", "KR3_4", "KR2_108", "KU4_46", "KU4_12")) #see notes on line 46 & 47 of script

#saving a cleaned version of the pollen fit data
#write.csv(pollen, "data/clean_pollen_fitness_measurements.csv", row.names = FALSE) 

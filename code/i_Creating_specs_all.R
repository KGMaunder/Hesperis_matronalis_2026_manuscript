
#This script:
#- combines all individual spec readings into one dataset
#- doesn’t need to be run again (created the csv file “specs_all”)

library(ggplot2)
library(dplyr)
library(tidyr)
library(pavo)
library(stringr)

##Part 1 ----
#Importing and merging all the scans


#import plant id & spec file name info
plantid <- read.csv("data/Flower_ID_Hesp_2023.csv")

#Establishing pathway for the spec files for each population 
#will need to grab the raw spec file (not included in the git sync)
specpath_KU1 <- "data/Raw_Spec_Hesp_2023/KU1"
specpath_KU2 <- "data/Raw_Spec_Hesp_2023/KU2"
specpath_KU3 <- "data/Raw_Spec_Hesp_2023/KU3"
specpath_KU4 <- "data/Raw_Spec_Hesp_2023/KU4"
specpath_KU5 <- "data/Raw_Spec_Hesp_2023/KU5"
specpath_KR1 <- "data/Raw_Spec_Hesp_2023/KR1"
specpath_KR2 <- "data/Raw_Spec_Hesp_2023/KR2"
specpath_KR3 <- "data/Raw_Spec_Hesp_2023/KR3"
specpath_KR4 <- "data/Raw_Spec_Hesp_2023/KR4"
specpath_KR5 <- "data/Raw_Spec_Hesp_2023/KR5"


#Loading each spectral data set
specs_KU1 <- getspec(where = specpath_KU1,
                        ext = "txt",
                        lim = c(300, 700)
)
specs_KU2 <- getspec(where = specpath_KU2,
                     ext = "txt",
                     lim = c(300, 700)
)
specs_KU3 <- getspec(where = specpath_KU3,
                     ext = "txt",
                     lim = c(300, 700)
)
specs_KU4 <- getspec(where = specpath_KU4,
                     ext = "txt",
                     lim = c(300, 700)
)
specs_KU5 <- getspec(where = specpath_KU5,
                     ext = "txt",
                     lim = c(300, 700)
)
specs_KR1 <- getspec(where = specpath_KR1,
                     ext = "txt",
                     lim = c(300, 700)
)
specs_KR2 <- getspec(where = specpath_KR2,
                     ext = "txt",
                     lim = c(300, 700)
)
specs_KR3 <- getspec(where = specpath_KR3,
                     ext = "txt",
                     lim = c(300, 700)
)
specs_KR4 <- getspec(where = specpath_KR4,
                     ext = "txt",
                     lim = c(300, 700)
)
specs_KR5 <- getspec(where = specpath_KR5,
                     ext = "txt",
                     lim = c(300, 700)
)

#Changing spec file names so that they are simplified and match those in plantid dataset

replace_column_names <- function(data) {
  new_names <- names(data)
  new_names <- gsub("_Reflection__([0-9]+)__([0-9]+)", "_\\2", new_names)
  names(data) <- new_names
  return(data)
}

specs_KR1 <- replace_column_names(specs_KR1)
specs_KR2 <- replace_column_names(specs_KR2)
specs_KR3 <- replace_column_names(specs_KR3)
specs_KR4 <- replace_column_names(specs_KR4)
specs_KR5 <- replace_column_names(specs_KR5)
specs_KU1 <- replace_column_names(specs_KU1)
specs_KU2 <- replace_column_names(specs_KU2)
specs_KU3 <- replace_column_names(specs_KU3)
specs_KU4 <- replace_column_names(specs_KU4)
specs_KU5 <- replace_column_names(specs_KU5)
  
#combining all spec data sets together
specs_all <- merge(specs_KR1, specs_KR2, by = "wl")
specs_all <- merge(specs_all, specs_KR3, by = "wl")
specs_all <- merge(specs_all, specs_KR4, by = "wl")
specs_all <- merge(specs_all, specs_KR5, by = "wl")
specs_all <- merge(specs_all, specs_KU1, by = "wl")
specs_all <- merge(specs_all, specs_KU2, by = "wl")
specs_all <- merge(specs_all, specs_KU3, by = "wl")
specs_all <- merge(specs_all, specs_KU4, by = "wl")
specs_all <- merge(specs_all, specs_KU5, by = "wl")
 
#write.csv(specs_all, "pre_specs_all.csv", row.names = FALSE) 
 
##Part 2 ----
#Linking scans to plant id data

specs_all <- read.csv("data/pre_specs_all.csv")

#informs you if the current spec file is in the rspec format
is.rspec(specs_all)
#if it is not [FALSE], use this function to turn it into rspec
specs_all <- as.rspec(specs_all)

#turning spec data into long form
long_specs_all <-  pivot_longer(specs_all,
                                cols = starts_with("K"),
                                names_to = "spec_file",
                                values_to = "Reflection"
)

#The plantid datafiel has entries in the form of KU1_9 instead of KU1_009, this needs to be fixed before we can proceed

#need to add leading zeros to entries with only single or double-digit numbers
plantid <- plantid %>%
  mutate(
    spec_file = str_replace(
      spec_file,
      pattern = "_(\\d{1,2})$",  #Note to self: for an R string a double \\ is need to communicate \d (a single \ is interpeted as an escape charater, like in \n) 
      replacement = function(match) {
        num <- as.integer(str_sub(match, 2))  #starting at 2, i.e wont include the first character (the _)
        sprintf("_%03d", num)    #format number so that it has 3 digits       
      }
    )
  )


#merging both data sets
plantid_spec <- merge(long_specs_all, plantid, by = "spec_file") 
#changing column names so scan id and plant id can easily be separated 
plantid_spec$plantid__specid <- paste(plantid_spec$plant_id, plantid_spec$spec_file, sep = "__")

#selectimng only relevant id name and spec columns
plantid_spec <- plantid_spec %>% 
  select(wl, Reflection, plantid__specid)
#returning dataset to wide form
plantid_spec_wide <- pivot_wider(plantid_spec,
                                 names_from = "plantid__specid",
                                 values_from = "Reflection")
#checking if dataset is in the form recognized by pavo 
is.rspec(plantid_spec_wide)
#if it is not [FALSE], use this function to turn it into rspec
specs_all <- as.rspec(plantid_spec_wide, whichwl = "wl")
is.rspec(specs_all)

#saving final spec_all dataset
#write.csv(specs_all, "data/specs_all.csv", row.names = FALSE) 

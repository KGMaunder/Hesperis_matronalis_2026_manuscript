
#Purpose:
#-smooths spectral data [important]
#-specifically grabs the reflectance values at the peaks and valleys of the overall spec curves (at 435,560,690 nm) [this is a legacy of an older analysis approach and is not really used in the final analysis]
#-creates sliced_specs_all.csv [a legacy file, not used in final analysis]

library(ggplot2)
library(dplyr)
library(tidyr)
library(pavo)

specs_all <- read.csv("data/specs_all.csv")
#transforming into an rspec file (ideal file form for pavo)
specs_all <- as.rspec(specs_all)

plot(specs_all, type = "o", col = spec2rgb(specs_all))

#sub-setting each population & region
KU1 <- subset(specs_all, "KU1")
KU2 <- subset(specs_all, "KU2")
KU3 <- subset(specs_all, "KU3")
KU4 <- subset(specs_all, "KU4")
KU5 <- subset(specs_all, "KU5")
KR1 <- subset(specs_all, "KR1")
KR2 <- subset(specs_all, "KR2")
KR3 <- subset(specs_all, "KR3")
KR4 <- subset(specs_all, "KR4")
KR5 <- subset(specs_all, "KR5")
#sub-setting all urban populations
KU <- subset(specs_all, "KU")
KR <- subset(specs_all, "KR")

plot(KU1, type = "o", col = spec2rgb(KU1)) 


#Scans Per Pop ----
# create a vector with only population identity names
pop <- gsub("_\\S*", "", names(specs_all))[-1]
#allows you to see the number of scans per population
table(pop)


#Smoothing ----
#checking out the approprate amount to smooth spec data
plotsmooth(KU4,
           minsmooth = 0.05,
           maxsmooth = 0.5,
           curves = 4,
           ask = FALSE
)

#smoothing by span = 0.2 looks good
smooth_specs_all <- procspec(specs_all, opt = "smooth", span = 0.2)

#Slice - simplifying to key wavelength values ----

#looking at plot of all smoothed specs
plot(smooth_specs_all, type = "o", col = spec2rgb(smooth_specs_all))
#potential peaks and troughs
abline(v=c(435,560,690)) 

#changing file to long form
long_spec <-  pivot_longer(smooth_specs_all,
                           cols = starts_with("K"),
                           names_to = c("plant_id", "spec_id"),
                           names_sep = "__",
                           values_to = "reflectance"
                           )
#filtering to get the reflectance value at wl 435, 560, and 690
slice <- long_spec %>% 
  filter(wl==435 | wl==560 | wl==690) %>% 
  group_by(wl) 

slice <- slice %>% 
  pivot_wider(names_from = wl, 
              values_from = reflectance) 

colnames(slice)[colnames(slice) == "435"] <- "wl435" 
colnames(slice)[colnames(slice) == "560"] <- "wl560" 
colnames(slice)[colnames(slice) == "690"] <- "wl690" 

#adding depth measure for difference between the average of the 2 peaks and the valley
slice <- slice %>% 
  mutate(depth = (wl435 + wl690)/2 - wl560)

#write.csv(slice, "data/sliced_specs_all.csv")  




# Load libraries
library(tidyverse)
library(pavo)
library(ggplot2)
library(FactoMineR) # For PCA


#Data loading ----
specs_all <- read.csv("data/specs_all.csv")

#smoothing spec curves to normalize shape and remove random fluctuations (span of 0.2 was picked in previous analysis "Creating_sliced_specs.R")
specs_all <- as.rspec(specs_all)
specs_all <- procspec(specs_all, opt = "smooth", span = 0.2)


#PCA ----
#first just manipulating the structure of the data so that it is compatible with the PCA 
specs <- specs_all %>% 
  mutate(wl = paste0("wl_", wl)) %>% 
  column_to_rownames(var = "wl")

##flipping row and column axis
specs <- as.data.frame(t(specs))

##standardizing data (center and scale)
specs <- scale(specs)

##running PCA
pca_result <- PCA(specs) #PC1 explains 70% of the variance in the reflectance data

#extracting PC1 scores 
pc1_scores <- pca_result$ind$coord[, 1]

#plotting PC1 loadings to see which wavelengths contribute most to PC1 
loadings <- pca_result$var$coord[, 1]
loadings_df <- data.frame(wavelength = 300:700, loading = loadings)

ggplot(loadings_df, aes(x = wavelength, y = loading)) +
  geom_line() +
  geom_vline(xintercept = c(430, 630), linetype = "dashed", color = "red") +
  labs(title = "PC1 Loadings", x = "Wavelength (nm)", y = "Loading Weight")

#the weighting of wavelengths 430 to 630 nm are all around 1. This suggests that there isn't much variation in actual colour/hue (different wavelengths), and likely most variation is in the amount of brightness 
#most variation will likely be well represented by average changes in reflectance from wl430 to wl460

#isolating just the reflectance values betwen 430 and 630
tmp <- specs_all %>% 
  filter(wl <= 630 & wl>= 430) %>% 
  select(!wl)

#calculating the mean reflectance value across this range
mean_reflectance <- colMeans(tmp)

# Create a new dataframe with spec_file_id and mean_reflectance
reflect <- data.frame(
  spec_file = colnames(tmp),
  mean_ref = mean_reflectance,
  stringsAsFactors = FALSE
)

ggplot(reflect, aes(mean_ref)) + geom_histogram()

#write_csv(reflect, "data/reflectance.csv")

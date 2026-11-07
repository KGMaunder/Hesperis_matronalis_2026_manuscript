
Flow of R Scripts

1st: i_Creating_specs_all.R         
2nd: ii_Creating_sliced_specs.R
3rd: iii_Creating_reflectance.R
4th: iv_Creating_full_fit.R
*5th: 01_Manuscript_Analysis_Feb_2026.R  

Note: i, ii, iii, and iv scripts process data and the data frames used in the final analysis
01 is the actual final analysis


Description of R Scripts 

i_Creating_specs_all
- creates the file “specs_all.csv”
- combines all individual spec readings into one dataset

ii_Creating_sliced_specs
- creates the file “sliced_specs_all.csv”
- this data is a legacy from older approaches for calculating a single estimate of colour using reflectance at peaks
- creates dataset with the reflectance values for the spec peaks and valleys at wavelengths 435, 560, and 690
- kept to allow comparison between colour estimating methods

iii_Creating_reflectance.R
- creates the file “reflectance.csv”
- a PCA is run, and determines that reflectance across 430-630 contribute fairly equally to PC1
- thus calculate mean reflectance across this range as a single estimate of colour 
- also tells us that main difference between white, pink, and purple petals is brightness rather than hue

iv_Creating_full_fit.R
- creates the file “full_fit.csv” and "clean_pollen_fitness_measurements.csv"
- data cleaning to correct mis-entered fitness data
- combines absolute fitness measurement for all plants (i.e both those in the pollen supplement experiment and those not in it)
- saves clean fitness data files for the pollen supplemental experiment and the full fitness data with all plants


01_Manuscript_Analysis_Feb_2026.R  
- analyses and creation of plots seen in the manuscript


Description of Data Files 

Legend
!- data used in the final analysis 
@- data at an intermediate state (is partially processed and moves between scripts for further processing)
#- raw data / meta data
$- output data from final analysis


$- brms_model_summary.txt (created in 01_Manuscript_Analysis_Feb_2026.R) 
	-> output of ZOIB analysis of seed intactness data (Appendix A Table 1)
!- clean_pollen_fitness_measurements.csv (created in iv_Creating_full_fit.R, used in 01_Manuscript_Analysis_Feb_2026.R)
	-> cleaned fitness data of plants in pollen supplementation experiment, wide form, measurements of three fruits in treatment 
#- Fitness_Measurements_Main_Hesp.csv (used in iv_Creating_full_fit.R) 
	-> wide format fitness measurements for just all plants NOT in the pollen supplement experiment 
#- Flower_ID_Hesp_2023.csv (used in i_Creating_specs_all.R) 
	-> list of all the spec_id names and what plant_id they came from
!- full_fit.csv (created in iv_Creating_full_fit.R, used in 01_Manuscript_Analysis_Feb_2026.R) 
	-> the fit data for each plant (including both main and pollen treatment), long form 1 row for each of the 10 fruit per plant
!#- Hesp_Colour_Scoring_2023.csv (used in iv_Creating_full_fit.R, used in 01_Manuscript_Analysis_Feb_2026.R)
	-> colour assigned to each plant in field, whether it was a biased or unbiased sample, whether in pollen supplement experiment or not
!#- hesp_pop_info.csv (used in 01_Manuscript_Analysis_Feb_2026.R)
	-> the coordinates and night sky brightness data for each pop
#- Pollen_Fitness_Measurements.csv (used in iv_Creating_full_fit.R)
	-> the uncleaned fitness data for all the plants in the pollen supplement experiment
@- pre_specs_all.csv (created and then used in i_Creating_specs_all.R)
	-> same spec info as specs_all.csv, but columns don't include plant_id info
#- Raw_Spec_Hesp_2023.zip(used in i_Creating_specs_all.R)
	-> zip file of all raw spectra reflectance data, each spectra is an individual file, (7500 .txt files total)
!- reflectance.csv (created in iii_Creating_reflectance.R, used in 01_Manuscript_Analysis_Feb_2026.R)
	-> univariate measure of colour calculated by taking the average reflectance of light between 330 and 630 nm for each petal (three per plant_id)
$- selection_differentials.csv (created in 01_Manuscript_Analysis_Feb_2026.R)
	-> output of strength of selection analysis (Table 1)
!- sliced_specs_all.csv (created in ii_Creating_sliced_specs.R, used in 01_Manuscript_Analysis_Feb_2026.R)
	-> older measure of colour [not method used in final anlysis], reflectance values at peaks and valleys for each petal (three per plant_id)
@- specs_all.cs (created in i_Creating_specs_all.R, used in ii_Creating_sliced_specs.R, used in iii_Creating_reflectance.R)
	-> the full spectra reflectance data for each sample (three petals measured per plant_id)



Description of Variables (column names in data files)
biased -> record of whether a plant was randomly selected on the transect (0) or whether it was an off-transect plant specifically tagged to meet the minimum target of 50 plants per colour (1)
depth -> measure of colour (calculation = (wl435 + wl690)/2 - wl560
F1_length -> fruit length (excluding the stem) in cm (F1-F10 are 10 fruits selected at random from the total number of fruits)
fruit_num -> total number of developed fruits (on stem collected in the field)
initial -> initials of person who took and hand wrote the measurements
lat -> latitude of population
long -> longitude of population
mean_ref -> mean of the reflectance values between 430-630 nm
P1_bubble  -> the number of seed indentation in septum
P1_cond -> condition of the fruit pod, recorded in early fruit process and then dropped as a measurement (O - open -  carpels have detached from the septum, CL - closed - carpels fully attached to septum, PO - partially open - carpels partially adetached from septum, STEM ONLY - unclear if detached or undeveloped, UNDEVELOPED - fruit did not develope, UNCLEAR - unclear if detached or undeveloped)
P1_length -> fruit length (excluding the stem) in cm
P1_viable_seeds -> number of seeds that don't crumble after being rubbed between fingers
plant_id -> id code assigned to each individual plant 
population -> population code (KR - rural population in Kingston, KU - urban population in Kingston, 5 pops of each)
score_colour -> flower colour catagory visually assigned in the field (V - purple. P - pink, W - white)
site -> population code (KR - rural population in Kingston, KU - urban population in Kingston, 5 pops of each)
spec_file -> id code for spectral measurement (3 per plant, 3 petals)
SQM -> measure of zenith sky brightness from Falchi et al 2016, sky quality meter data (unit: mag/arcsec²)
stem_diam_mm -> stem diameter measurement, taken with calipers, recorded in mm, measured at the lowest part of the stem above the soil
stem_num -> number of stems (observed and written down in the field)
sun -> rough personal annecdote on whether populations were forested or within an open field exposed to more sun
sup_pollen -> record of whether a plant was not in the pollen supplement experiment (NA), part of the pollent supplement treatment (P), or part of the control treatment (C)
total_real_pollen_fruit -> number of  experimental fruits  (out of a total 3) that were recovered and could be measured
total_viable_seeds -> sum of viable seeds from P1, P2, and P3 (the three fruits in the pollen supp experiment)
urbaness -> catagory of whether a population is urban or rural
wl -> wavelength of light being reflected (unit = nm)
wl435 -> reflectance at wavelength 435 nm (common first peak of the spectra)
wl560 -> reflectance at wavelength 560 nm (common valley of the spectra)
wl690 -> reflectance at wavelength 690 nm (common second peak of the spectra)











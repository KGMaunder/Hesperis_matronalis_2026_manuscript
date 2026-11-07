
#Purpose:
#main analysis and production of figures in the manuscript
#analysis and plots should be in order as they appear in the manuscript

#load libraries 
library(ggplot2)
library(dplyr)
library(tidyr)
library(lme4) #for linear mixed models
library(lmerTest)
library(emmeans) #for posthoc comparison of lmm
library(patchwork) #for composite plots
library(glmmTMB) #for running gamma model with random effects
library(MASS)
library(tidyverse)
library(tidybayes)
library(ggpubr)
library(DHARMa)
library(car)
library(brms) #for Bayesian analysis of intact seed data


#Data prepping ----
spec_data <- read.csv("data/sliced_specs_all.csv")
fit_data <- read.csv("data/full_fit.csv")
pop_data <- read.csv("data/hesp_pop_info.csv")
score_data <- read.csv("data/Hesp_Colour_Scoring_2023.csv")
reflect_data <- read.csv("data/reflectance.csv")
pollen <- read.csv("data/clean_pollen_fitness_measurements.csv")

#combining data files to make a data frame for looking at colour across populations
#adding column with compatible plant_ids 
spec_data$population <- gsub("_.*", "", spec_data$plant_id) #"_.*" is regex for an underscore and then any characters that follow

reflect_data <- reflect_data %>% 
  separate_wider_delim(spec_file, "__", names = c("plant_id", "spec_id")) %>% 
  group_by(plant_id) %>% 
  mutate(median_mean_ref = median(mean_ref)) #mean_ref is the average reflectance across 430-630 nm, median here is refering to taking the median value from the 3 seperate petals that we have reflectance spectra from (using median vs mean of three values does not affect outcome of analysis)

pop_data <- pop_data %>% 
  mutate(NSB = SQM*-1) #night sky brightness data, multiplying by -1 to make it easier to interpret (now more positive values are associated with great human activity levels)

colour <- left_join(spec_data, pop_data)
colour <- left_join(colour, score_data)
colour <- left_join(colour, reflect_data)
colour <- colour %>% 
  drop_na(score_colour) #there is one flower where scored colour was not recorded

colour$score_colour <- factor( #setting a set order so graphs show white, pink, then purple
  colour$score_colour,
  levels = c("W", "P", "V") 
)

fit_data <- fit_data %>% 
  drop_na(length) %>% 
  drop_na(fruit_num)

#calculating average fruit length, dropping individual fruit measurement
fit_data <- fit_data %>% 
  group_by(plant_id) %>% 
  mutate(mean_fruit_length = mean(length), num_fruit_lengths_measured = n()) %>% 
  dplyr::select(!c(fruit_sample, length)) %>% 
  distinct()

#calculating absolute fitness - an estimate of total seeds per plant (number of fruit * mean length of fruit * 2.06 (2.06 is the estimate of the number of seeds per cm of fruit - see later analysis)
fit_data <- fit_data %>% 
  mutate(repro = mean_fruit_length*fruit_num*2.06) %>% #absolute fitness of collected stem
  mutate(repro_whole_plant = repro*stem_num) #multiplying fitness per stem by number of stems on plant

#combining fit and colour data
#this data frame has 3 rows per plant_id due to 3 petals that reflectance is measured for
fit_spec <- left_join(fit_data, colour) #for comparing fitness data to spec readings
fit_spec <- fit_spec %>% 
  drop_na(score_colour) #there is one flower where scored colour was not recorded

#this data frame has just 1 row per plant_id (and just categorical descriptor of colour)
fit_score <- left_join(fit_data, score_data)
fit_score <- left_join(fit_score, pop_data)
fit_score <- fit_score %>% 
  drop_na(score_colour)

#Adding calculation of relative fitness (specific to the average fitness of each population)
fit_score <- fit_score %>% 
  group_by(population) %>% 
  mutate(avg_pop_fit = mean(repro)) %>% 
  mutate(rel_fit = repro/avg_pop_fit) 

fit_score$score_colour <- factor(fit_score$score_colour, levels = c("W", "P", "V"))

fit_spec <- left_join(fit_spec, fit_score)

#data set with just one row per plant_id (dropping individual petal measurement of reflectance, and keeping the median value)
singular_fit_spec <- fit_spec %>% 
  dplyr::select(-c(mean_ref, X:depth)) %>% 
  distinct()

##dataset for whole plant fitness
fit_spec_wp <- fit_spec %>% 
  filter(!is.na(stem_num)) %>% 
  filter(!population %in% c("KU2", "KU3", "KU4")) %>% 
  group_by(population) %>% 
  mutate(avg_pop_fit_whole_plant = mean(repro_whole_plant)) %>% 
  mutate(rel_fit_whole_plant = repro_whole_plant/avg_pop_fit_whole_plant)
fit_spec_wp$score_colour <- factor(fit_spec_wp$score_colour,levels = c("W", "P", "V"))


#creating dataset with just one row per plant id (dropping individual petal measurement of reflectance, and keeping the median value)
singular_fit_spec_wp <- fit_spec_wp %>% 
  dplyr::select(-c(mean_ref, X:depth)) %>% 
  distinct()

#z score normalized measure of reflectance for strength of selection analysis
singular_fit_spec_wp <- singular_fit_spec_wp %>%
  mutate(z_median_mean_ref = (median_mean_ref - mean(median_mean_ref)) / sd(median_mean_ref))


#Pollen supplementation experiment data
#separating the general fit measurements (from the measurements specific to the 3 fruits in the experiment)
part2 <- pollen %>% 
  dplyr::select(plant_id:stem_diam_mm, F1_length:fruit_num) %>% 
  pivot_longer(cols = ends_with("length"),
               names_to = c("fruit_sample", "delete"),
               names_sep = "_",
               values_to = "fruit_length_cm") %>% 
  dplyr::select(!delete) %>% 
  group_by(plant_id) %>%
  mutate(mean_fruit_length = mean(fruit_length_cm)) %>% 
  dplyr::select(!c(fruit_length_cm, fruit_sample)) %>% 
  distinct() 

#reformatting the measurements for the 3 treatment fruits
part1 <- pollen %>% 
  dplyr::select(plant_id, P1_cond.:total_real_pollen_fruit) %>% 
  mutate(row_id = row_number(),
         P2_viable_seeds = if_else(row_id == 121| row_id == 123 | row_id == 151, NA, P2_viable_seeds)) %>% #removing note on viable seed count for 3 plant_ids were estimate of viable seed was combined between two fruits
  dplyr::select(!row_id)

P1 <- part1 %>% 
  dplyr::select(plant_id, P1_cond.:P1_viable_seeds) %>% 
  mutate(sample_num = 1) %>% 
  rename(cond = P1_cond., length = P1_length, bubbles = P1_bubbles, viable_seeds = P1_viable_seeds)

P2 <- part1 %>% 
  dplyr::select(plant_id, P2_cond.:P2_viable_seeds) %>% 
  mutate(sample_num = 2) %>% 
  rename(cond = P2_cond., length = P2_length, bubbles = P2_bubbles, viable_seeds = P2_viable_seeds) %>% 
  mutate(viable_seeds = as.numeric(viable_seeds))

P3 <- part1 %>% 
  dplyr::select(plant_id, P3_cond.:P3_viable_seeds) %>% 
  mutate(sample_num = 3) %>% 
  rename(cond = P3_cond., length = P3_length, bubbles = P3_bubbles, viable_seeds = P3_viable_seeds) 

#this contains the undeveloped fruit (for analysis of impact on fruit set)
part1_all <- bind_rows(P1, P2, P3)

#this contains only the developed fruit (for analysis of impact on seed set given successful fruit development)
part1 <- part1_all %>% 
  drop_na(length) 

#not really "singular", 1-3 rows per plant_id bc 1-3 fruits from pollen supplement recovered and measured (but using singular measure of colour - median_mean_reflectance)
pollen_singular <- left_join(part1, singular_fit_spec, by = "plant_id")

pollen_singular <- pollen_singular %>% 
  filter(!population == "KU2") %>% #very low sample size (population where pollen supplement methods were first trialed)
  #calculating proportion of seeds that are viable out of the possible seed set of the fruit based on the fruits length
  #bubble refers to a count of seed indentation along the septum (adding a variable for how accurate the 2.06 x fruit length seed number is compared to number of seed indentations)
  mutate(seed_survival = viable_seeds/(length*2.06), bubble_accuracy = bubbles/(length*2.06)) %>% #(2.06 is the estimated number of seeds per cm of fruit - see later analysis (Appendix B Figure 1))
  mutate(seed_survival_trunk = if_else(seed_survival > 1, 1, seed_survival)) %>% #calculation 
  mutate(sup_pollen_dummy = if_else(sup_pollen == "P", 1, 0))  
  
#to look at developed vs undeveloped fruit
fruit <- part1_all %>% 
  filter(!cond == "") %>% 
  mutate(fruit_developed = ifelse(cond == "O" | cond == "PO" | cond == "CL", "yes", "no")) %>% 
  filter(!cond == "STEM ONLY") %>% #removing plants were it was not clear whether the fruit was undeveloped or if it had fallen off plant and thus condition is unknown
  filter(!cond == "UNCLEAR") %>% #removing plants were it was not clear whether the fruit was undeveloped or if it had fallen off plant and thus condition is unknown
  mutate(fruit_developed_dummy = ifelse(fruit_developed == "no", 0, 1))

#linking to reflectance data (once again not really "singular", 1-3 rows per plant_id bc 1-3 fruits from pollen supplement recovered and measured (but using singular measure of colour - median_mean_reflectance))
fruit_dev_singular <- left_join(fruit, singular_fit_spec, by = "plant_id")
fruit_dev_singular <- fruit_dev_singular %>% 
  filter(!population == "KU2")  #very low sample size (population where pollen supplement methods were first trialed)


#Main Manuscript Analysis ----

#Summary statistics for recovered plant sample size 
with(singular_fit_spec, table(population))
temp <- singular_fit_spec %>% 
  group_by(population) %>% 
  summarise(n = n())
mean(temp$n)
median(temp$n)
min(temp$n)
max(temp$n)

#Spectral Colour Data and Flower Colour Cline ----

##Figure 1A: Frequencies of each color category in each population and/or analogous distribution of spec values in each population ----
#in the below plot/analysis the data is filtered to remove samples that were selected with bias (to increase representation of more rare colours)

#making data set of colour frequency data for each pop to make Figure 1 A plot (Figure 1B map was made in QGIS)
freq_data <- filter(singular_fit_spec, biased == 0) %>%
  group_by(population, score_colour) %>% 
  summarise(n = n()) %>% 
  mutate(prop = n / sum(n))   

freq_data$score_colour <- factor(freq_data$score_colour,levels = c("W", "P", "V"))
freq_data$population <- factor(freq_data$population,levels = c("KR5", "KR3", "KR1", "KR2", "KR4", "KU2", "KU5", "KU1", "KU3", "KU4"))

freq_data <- left_join(freq_data, pop_data)

p <- ggplot(freq_data, aes(NSB, prop, colour = score_colour, fill = score_colour)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.2, colour = NA) +
  scale_colour_manual(values = c("grey", "#ff47aa", "#b762dc"), labels = c("White", "Pink", "Purple"), name = "Score colour") +
  scale_fill_manual(values = c("grey", "#ff47aa", "#b762dc"), labels = NULL) +
  theme_classic() +
  guides(fill = "none") +
  labs(x = expression("Human activity (mag/arc"*sec^2*")"), y = "Frequency of colour in population") +
  xlim(-22, -19) +
  theme(axis.title = element_text(size = 16))
p
  
###*manuscript fig ----
ggsave("plots/Fig_1A.pdf", plot = p, width = 7, height = 4, dpi = 300)

##Figure 2: How does human colour assignment relate to spec measurements? Boxplot ----

singular_fit_spec$score_colour <- factor(singular_fit_spec$score_colour,levels = c("V", "P", "W"))

#for adding the significant markers above the boxplots
my_comparisons <- list( c("V", "P"), c("P", "W"), c("V", "W"))

p <- ggplot(singular_fit_spec, aes(score_colour, median_mean_ref)) + 
  geom_jitter(alpha = 0.3, width = 0.25, height = 0) + 
  geom_boxplot(width = 0.5) + 
  scale_x_discrete(labels = c("Purple", "Pink", "White")) +
  theme_classic() + 
  stat_compare_means(comparisons = my_comparisons, label = "p.signif") +
  labs(y = "Petal Reflectance", x= "Field assigned colour") +
  theme(axis.title = element_text(size = 14))
p
###*manuscript fig ----
ggsave("plots/Fig_2.pdf", plot = p, width = 5, height = 6, dpi = 300)

all <- lm(median_mean_ref ~ score_colour, data = singular_fit_spec)
summary(all)
Anova(all)
emm <- emmeans(all, ~ score_colour)
pairs(emm, adjust = "tukey") 

mod <- aov(median_mean_ref ~ score_colour, data = singular_fit_spec)
anova(mod)
TukeyHSD(mod)

##No Figure: Colour morph frequency vs NSB ----
#in the below plot/analysis the data is filtered to remove samples that were selected with bias (to increase representation of more rare colours)
#confirming that the populations in this study still follow the previous observed cline of increasing frequency of purple morphs in more rural environments
ggplot(filter(singular_fit_spec, biased == 0), aes(NSB, median_mean_ref)) + 
  geom_jitter() + 
  geom_smooth(method = "lm", colour = "purple") + 
  theme_classic() +
  labs(y = "purple ↔ white  (reflectance at 435 nm)", x = "rural ↔ urban (NSB)")

mod <- lmer(median_mean_ref ~ NSB + (1|population), singular_fit_spec)
summary(mod)


#Pollen Supplementation ----

##No Figure: fruit development ----

mod <- glmer(fruit_developed_dummy ~ sup_pollen * median_mean_ref + (1|plant_id), fruit_dev_singular, family = binomial)
summary(mod)

with(fruit_dev_singular, table(fruit_developed_dummy, sup_pollen, population))
with(fruit_dev_singular, table(fruit_developed_dummy, sup_pollen))

##Figure 3:Raw pollen supplementation data, optionally with model fits overlaid ----

mod <- lmer(length ~ median_mean_ref * population * sup_pollen + (1|plant_id), data = pollen_singular)
anova(mod) #anova from lmerTest (using type III)

#Plot with spec data
unique_pops <- list("KR5", "KR3", "KR2", "KU1", "KU3", "KU4")
plot_list <- list()

for (pop in unique_pops) {
  subset_data <- pollen_singular[pollen_singular$population == pop, ]
  subset_data$score_colour <- factor(subset_data$score_colour,levels = c("W", "P", "V"))
  
  plot <- ggplot(subset_data, aes(median_mean_ref, length, colour = sup_pollen)) + 
    geom_point() +
    geom_smooth(method = "lm") +
    #geom_smooth(method = "lm", formula = y ~ x + I(x^2)) +
    labs(title = paste0(pop, " (NSB: ", subset_data$NSB, ")"), y = "Fruit length (cm)", x = "Petal reflectance (purple ↔ white)", colour = "Pollen treatment") +
    scale_colour_manual(values = c("#70a400", "#ffb121"), labels = c("Control", "Supplemented")) +
    #guides(fill = "none") +
    theme_classic() +
    theme(title = element_text(size = 12)) +
    theme(text = element_text(size = 16)) +
    theme(axis.title = element_text(size = 16)) +
    theme(legend.text = element_text(size = 16)) +
    theme(legend.title = element_text(size = 16))
  
  plot_list[[pop]] <- plot
}

plot_list <- lapply(plot_list, function(p) {
  p +
    theme(legend.position = "bottom") +
    guides(fill = guide_legend(nrow = 1))  
})

combined_plot <- patchwork::wrap_plots(plot_list, ncol = 3, axis_titles = "collect") + 
  plot_layout(guides = "collect") + 
  plot_annotation(theme = theme(legend.position = "bottom"))
print(combined_plot)

###*manuscript fig ----
ggsave("plots/Fig_3.pdf", plot = combined_plot, width = 9, height = 6, dpi = 300)

#Estimating the Strength of Selection on Flower Colour ----

unique_pops <- unique(singular_fit_spec_wp$population)
results <- list()
for (pop in unique_pops) {
  subset_data <- singular_fit_spec_wp[singular_fit_spec_wp$population == pop, ]
  
  model_l <- lm(rel_fit_whole_plant ~ z_median_mean_ref, data = subset_data)
  
  model_q <- lm(rel_fit_whole_plant ~ z_median_mean_ref + I(z_median_mean_ref^2), data = subset_data)
  
  results[[pop]] <- list(
    anova = anova(model_l, model_q),
    model_l_summary = summary(model_l),
    model_q_summary = summary(model_q)
  )
}

results
#KR4 has a significant quadratic regression (everything else is non-sig)

##Table 1: Strength of selection figure data ----
unique_pops <- unique(singular_fit_spec_wp$population)
results_df <- data.frame(
  population = character(),
  beta = numeric(),
  beta_se = numeric(),
  gamma = numeric(),
  gamma_se = numeric(),
  stringsAsFactors = FALSE
)

for (pop in unique_pops) {
  subset_data <- singular_fit_spec_wp[singular_fit_spec_wp$population == pop, ]
  
  mod_l <- lm(rel_fit_whole_plant ~ z_median_mean_ref , subset_data)
  mod_q <- lm(rel_fit_whole_plant ~ z_median_mean_ref + I(z_median_mean_ref^2), subset_data)
  s_l <- summary(mod_l)
  s_q <- summary(mod_q)
  Gamma <- s_q$coefficients[3 , 1]
  Gamma_se <- s_q$coefficients[3 , 2]
  
  results_df <- rbind(results_df, data.frame(
    population = pop,
    beta = s_l$coefficients[2 , 1],
    beta_se = s_l$coefficients[2 , 2],
    gamma = Gamma*2, #multiplying by 2 for accurate reproting of gamma coeffiecient
    gamma_se = Gamma_se*2, #multiplying by 2 for accurate reproting of gamma coeffiecient
    p_l = s_l$coefficients[2 , 4],
    p_q = s_q$coefficients[3 , 4]
  ))
  
}
strength <- results_df %>% 
  mutate(b_CI_top = beta+1.96*beta_se, b_CI_bottom = beta-1.96*beta_se,
         g_CI_top = gamma+1.96*gamma_se, g_CI_bottom = gamma-1.96*gamma_se,
         sig_p_l = if_else(p_l < 0.05, "Yes", "No"),
         sig_p_q = if_else(p_q < 0.05, "Yes", "No")) 

write.csv(strength, "data/selection_differentials.csv", row.names = FALSE) 


##Figure 4: Scatter plot of fitness against spec color measure for each population, with a linear & quadratic regression fit for significant cases ----
unique_pops <- list("KR5", "KR3", "KR1", "KR2", "KR4", "KU5", "KU1")
plot_list <- list()
significant_pops_l <- c()
significant_pops_q <- c("KR4") 

#add a if/else statement to only show lines for significant pops
for (pop in unique_pops) {
  subset_data <- singular_fit_spec_wp[singular_fit_spec_wp$population == pop, ]
  
  plot <- ggplot(subset_data, aes(median_mean_ref, rel_fit_whole_plant)) + 
    geom_point(alpha = 0.7) +
    labs(title = paste0(pop, " (NSB: ", subset_data$NSB,")"), y = "Relative fitness (seed count)", x = "Petal reflectance (purple to white)") +
    theme_classic() +
    theme(title = element_text(size = 12)) +
    theme(text = element_text(size = 16)) +
    theme(axis.title = element_text(size = 16)) +
    theme(legend.text = element_text(size = 16)) +
    theme(legend.title = element_text(size = 16))
  
  
  if(pop %in% significant_pops_q) {
    plot <- plot + geom_smooth(method = "lm", formula = y ~ scale(x) + I(scale(x)^2), fill= "#ff9fff", colour = "#c600c6", linewidth = 1.5) }
  
  #  else{plot <- plot + geom_smooth(method = "lm", formula = y ~ scale(x) + I(scale(x)^2), fill= "grey88", colour = "grey77", linewidth = 1.5)} #fit line for non-significant regressions
  
  # if(pop %in% significant_pops_l) {
  #  plot <- plot + geom_smooth(method = "lm", formula = y ~ scale(x), fill = "#8bbb18", colour = "#8abb19", linewidth = 1.5) } #fit line for non-significant regressions
  
  plot_list[[pop]] <- plot
}

combined_plot <- wrap_plots(plot_list, ncol = 4, axis_titles = "collect")
print(combined_plot)
###*manuscript fig ----
ggsave("plots/Fig_4.pdf", plot = combined_plot, width = 9, height = 7, dpi = 300)


#Appendix A: Intact seeds  ----
###Appendix A Table 1: ----
#This data is right skewed data between 0-1 that includes zeros and ones
# a beta regression can't work with 0 and 1 values 
# the betareg() packages doesn't allow for random effect to be included
#thus we are using brms to run a zero-one inflated beta 

#setting priors, don't have expectations of results so are using standard weakly informative priors
all_priors <- c(
  set_prior("normal(0, 1)", class = "Intercept"),
  set_prior("normal(0, 1)", class = "b"),   # for mu 
  set_prior("normal(0, 1)", class = "b", dpar = c("zoi", "coi")), 
  set_prior("student_t(3, 0, 2.5)", class = "sd") #for random effect
  )

model <- brm(
  formula = bf(seed_survival_trunk ~ NSB + sup_pollen + scale(median_mean_ref) + (1|plant_id), 
               zoi ~ NSB + sup_pollen + scale(median_mean_ref) + (1|plant_id), # probability of being zero or a value other than zero
               coi ~ NSB + sup_pollen + scale(median_mean_ref) + (1|plant_id)), # probability of being one or a value other than one
  data = pollen_singular,
  prior = all_priors,
  family = zero_one_inflated_beta(),
  control = list(adapt_delta = 0.99))

plot(model)
#plotting posterier distribution of the NSB parameter for zoi
plot(model, variable = "b_zoi_NSB", ask = FALSE)

summary(model) #"variable name" is the reporting of that variable on mu (the effect on continuous values - not 0s or 1s)

#saving text file of model summary
summary_text <- capture.output(summary(model))
#writeLines(summary_text, "data/brms_model_summary.txt")

###Appendix A Figure1: ----
#plot for manuscript
ce_mu <- conditional_effects(model, effects = "NSB", dpar = "mu")
ce_mu_df <- ce_mu$NSB

#seperating points that are 0s and 1s vs the continuous numbers in between. Mu only includes the continuous numbers
continuous_rawdata <- subset(pollen_singular, seed_survival_trunk > 0 & seed_survival_trunk < 1)
boundary_rawdata <- subset(pollen_singular, seed_survival_trunk == 0 | seed_survival_trunk == 1)

p <- ggplot(ce_mu_df, aes(x = NSB, y = estimate__)) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__),
              fill = "#8bbb18", alpha = 0.5) +
  geom_line(color = "#4a640d", linewidth = 1.2) +
  geom_jitter(data = continuous_rawdata,
             aes(x = NSB, y = seed_survival_trunk),
             width =  0.03, height = 0, alpha = 0.5, size = 2) +
  geom_jitter(data = boundary_rawdata,
                aes(x = NSB, y = seed_survival_trunk),
                width =  0.03, height = 0, alpha = 0.5, size = 2, colour = "grey80") +
  labs(x = expression("Human activity (mag/arc"*sec^2*")"), y = "Predicted percent of intact seeds") +
  theme_classic() +
  xlim(-22, -19)
p
####*manuscript fig ----
ggsave("plots/App_A_Fig_1.pdf", plot = p, width = 5, height = 4, dpi = 300)

#reporting effect values for the predicted mean at the low and high ends of NSB
ce_mu_df[1, c("NSB", "estimate__", "lower__", "upper__")]      # lowest NSB
ce_mu_df[nrow(ce_mu_df), c("NSB", "estimate__", "lower__", "upper__")]

#Appendix B ----
###Appendix B Figure 1: Bubble to Length ----
mean(pollen_singular$bubble_accuracy, na.rm = T)
median(pollen_singular$bubble_accuracy, na.rm = T)

p <- ggplot(pollen_singular, aes(length, bubbles)) + 
  geom_point() + 
  geom_smooth(method = "lm", fill = "#8bbb18", colour = "#4a640d") +
  labs(y = "Number of seed indentations", x = "Fruit length (cm)") +
  theme_classic() +
  theme(axis.title = element_text(size = 16)) +
  theme(axis.text = element_text(size = 14))

p
####*manuscript fig ----
ggsave("plots/App_B_Fig_1.pdf", plot = p, width = 8, height = 6, dpi = 300)

#mod <- lm(bubbles ~ length, pollen_singular)
#summary(mod) #slope is 2.038 (i.e. 2.024 bubbles per cm of fruit r^2 = 0.57)

mod2 <- lmer(bubbles ~ length + (1|plant_id), pollen_singular)
summary(mod2) #slope is 2.06

###Appendix B Figure 2: Plots of mean fitness measure, +/- standard error of mean, for white, pink, purple plants ....  one panel for each population. Here making bar graphs using the categorical colors, not the spec measure of color ----

mod <- glm(repro_whole_plant ~ score_colour * population, family = Gamma(link = "log"), singular_fit_spec_wp)
Anova(mod, type = "III")

unique_pops <- list("KR5", "KR3", "KR1", "KR2", "KR4", "KU5", "KU1")
plot_list <- list()

for (pop in unique_pops) {
  subset_data <- singular_fit_spec_wp[singular_fit_spec_wp$population == pop, ]
  
  subset_data$score_colour <- factor(subset_data$score_colour,levels = c("V", "P", "W"))
  
  summary_stats <- subset_data %>%
    group_by(score_colour) %>%
    summarise(
      mean = mean(repro_whole_plant, na.rm = TRUE),
      se = sd(repro_whole_plant, na.rm = TRUE) / sqrt(n())
    )
  
  plot <- ggplot(summary_stats, aes(x = score_colour, y = mean, fill = score_colour)) +
    geom_col(width = 0.6) + 
    geom_errorbar(
      aes(ymin = mean - se, ymax = mean + se), # SE range
      width = 0.2,                             # Width of error bars
      color = "black",
      linewidth = 0.5) +
    scale_fill_manual(values = c("#b762dc", "#ff47aa", "grey")) +
    #ylim(0, 800) +
    guides(fill = "none") +
    labs(x = "Field assigned colour", y = "Mean seed count (± SE)", title = paste0(pop, " (NSB: ", subset_data$NSB, ")")) +
    theme_classic() +
    theme(axis.title = element_text(size = 16)) +
    theme(title = element_text(size = 12)) +
    theme(text = element_text(size = 16)) +
    theme(legend.text = element_text(size = 16)) +
    theme(legend.title = element_text(size = 16))
    
  
  plot_list[[pop]] <- plot
}

combined_plot <- wrap_plots(plot_list, ncol = 4, axis_titles = "collect")
print(combined_plot)

####*manuscript fig ----
ggsave("plots/App_B_Fig_2.pdf", plot = combined_plot, width = 9, height = 7, dpi = 300)


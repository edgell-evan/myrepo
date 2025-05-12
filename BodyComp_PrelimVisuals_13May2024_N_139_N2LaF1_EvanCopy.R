# Body composition analysis
#
# Author: D. Somo
# Date created: 13 May 2024
##############################################################

# Needed packages
library(tidyverse)
library(cowplot)

# Read in data
bc_data <- readr::read_csv("./DeerMice_GenerationBodyComp_Data_v1.0_DAS_Through25July2024_EvanPracticeCopy - Copy.csv")
bc_data

# ~~~~~~~~~~~~~~
#
# Data cleaning
#
# ~~~~~~~~~~~~~~
bc_data$native_alt[bc_data$native_alt == "Highlasnders"] <- "Highlanders" # Fix typo

# Counts for generation and population
table(bc_data$native_alt, bc_data$generation) # No F1 lowlanders

bc_data$native_alt[bc_data$native_alt == "Highlanders"] <- "Highlander" # Make label uniform
bc_data$native_alt[bc_data$native_alt == "Lowlanders"] <- "Lowlander" # Make label uniform

table(bc_data$native_alt, bc_data$generation)

# Number of different familes for each population and generation
table(bc_data$native_alt, bc_data$family, bc_data$generation)

table(bc_data$population)

bc_data$population[bc_data$population == "MSB" |
                     bc_data$population == "ME"] <- "MBS"

table(bc_data$population)
# ~~~~~~~~~~~~~~~~~~~~~~~~~
# End data cleaning section
# ~~~~~~~~~~~~~~~~~~~~~~~~~


### ~~~~~~~~~~~~~~~
###
### Visualize data:
###
### ~~~~~~~~~~~~~~~

### Fill by sex
bc_data |> 
  ggplot(aes(x=native_alt, y=fat_percent, fill = sex))+
  geom_jitter(width = 0.15, height = 0, 
              shape = 21, size = 4, alpha = 0.8, stroke = 1.15)+
  facet_wrap("generation")+
  scale_x_discrete(name = element_blank())+
  scale_y_continuous(name = expression(paste("Body fat(%)")),
                     limits = c(0,62),
                     breaks = seq(0,60,10))+
  theme_cowplot()

bc_data |> 
  ggplot(aes(x=native_alt, y=fat_percent, fill = sex, group = sex))+
  geom_violin()+
  facet_wrap("generation")+
  scale_x_discrete(name = element_blank())+
  scale_y_continuous(name = expression(paste("Body fat(%)")),
                     limits = c(0,62),
                     breaks = seq(0,60,10))+
  theme_cowplot()

### Fill by family <<<------- Crucial section!

# Generate a random number for each family
bc_fam_rand_data <- bc_data |> 
  group_by(family) |> 
  mutate(random_family_numbers = runif(1)) |> 
  ungroup()

bc_fam_rand_data |> 
  ggplot(aes(x=native_alt, y=fat_percent, fill = as.character(random_family_numbers)))+
  geom_jitter(width = 0.15, height = 0, 
              shape = 21, size = 4, alpha = 0.8, stroke = 1.15)+
  facet_wrap("generation")+
  guides(fill = "none")+
  scale_x_discrete(name = element_blank())+
  scale_y_continuous(name = expression(paste("Body fat (%)")),
                     limits = c(0,62),
                     breaks = seq(0,60,10))+
  theme_cowplot()

##
## ~~~~~~~~~~~~~~~~~~~~~~~~~
## Plot body comp by family
## ~~~~~~~~~~~~~~~~~~~~~~~~~
##

bc_data %>% 
  dplyr::mutate(fam_chr = as.character(family),
                pop_alt = if_else(population == "KN","Lowlander","Highlander"),
                pop_alt_fct = forcats::as_factor(pop_alt),
                pop_alt_ord_var = if_else(pop_alt == "Lowlander",1,2),
                pop_alt_ord_fct = forcats::fct_reorder(pop_alt_fct, pop_alt_ord_var)) %>% 
  ggplot(aes(x=fam_chr, y=fat_percent, fill=generation))+
  geom_boxplot()+
  geom_jitter(shape=21, size=4, alpha=0.7,
              width=0.05, height=0)+
  scale_y_continuous(name = expression(paste("Body fat (%)")),
                     limits = c(0,62),
                     breaks = seq(0,60,10))+
  scale_x_discrete(name="Family")+
  facet_wrap(~ pop_alt_ord_fct,
             scales="free")+
  theme_cowplot(font_size=18,
                line_size=1)+
  theme(axis.text.x = element_text(angle=55, vjust=1, hjust=1))

##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Plot body comp by Father's family
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##

bc_data %>% 
  dplyr::mutate(fam_dad_chr = as.character(father_fam),
                pop_alt = if_else(population == "KN","Lowlander","Highlander"),
                pop_alt_fct = forcats::as_factor(pop_alt),
                pop_alt_ord_var = if_else(pop_alt == "Lowlander",1,2),
                pop_alt_ord_fct = forcats::fct_reorder(pop_alt_fct, pop_alt_ord_var)) %>% 
  ggplot(aes(x=fam_dad_chr, y=fat_percent, fill=generation))+
  geom_boxplot()+
  geom_jitter(shape=21, size=4, alpha=0.7,
              width=0.05, height=0)+
  scale_y_continuous(name = expression(paste("Body fat (%)")),
                     limits = c(0,62),
                     breaks = seq(0,60,10))+
  scale_x_discrete(name="Family")+
  facet_wrap(~ pop_alt_ord_fct,
             scales="free")+
  theme_cowplot(font_size=18,
                line_size=1)+
  theme(axis.text.x = element_text(angle=55, vjust=1, hjust=1))


##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Plot body comp by Mother's family
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##

bc_data %>% 
  dplyr::mutate(fam_mom_chr = as.character(mother_fam),
                pop_alt = if_else(population == "KN","Lowlander","Highlander"),
                pop_alt_fct = forcats::as_factor(pop_alt),
                pop_alt_ord_var = if_else(pop_alt == "Lowlander",1,2),
                pop_alt_ord_fct = forcats::fct_reorder(pop_alt_fct, pop_alt_ord_var)) %>% 
  ggplot(aes(x=fam_mom_chr, y=fat_percent, fill=generation))+
  geom_boxplot()+
  geom_jitter(shape=21, size=4, alpha=0.7,
              width=0.05, height=0)+
  scale_y_continuous(name = expression(paste("Body fat (%)")),
                     limits = c(0,62),
                     breaks = seq(0,60,10))+
  scale_x_discrete(name="Family")+
  facet_wrap(~ pop_alt_ord_fct,
             scales="free")+
  theme_cowplot(font_size=18,
                line_size=1)+
  theme(axis.text.x = element_text(angle=55, vjust=1, hjust=1))

### Fill by age
bc_data |> 
  ggplot(aes(x=native_alt, y=fat_percent, fill = age_days))+
  geom_jitter(width = 0.15, height = 0, 
              shape = 21, size = 4, alpha = 0.8, stroke = 1.15)+
  facet_wrap("generation")+
  scale_x_discrete(name = element_blank())+
  scale_y_continuous(name = expression(paste("Body fat (%)")),
                     limits = c(0,62),
                     breaks = seq(0,60,10))+
  theme_cowplot()

# F2 F3 only
bc_data |> 
  dplyr::filter(generation != "F1") %>% 
  ggplot(aes(x=native_alt, y=fat_percent, fill = age_days))+
  geom_jitter(width = 0.15, height = 0, 
              shape = 21, size = 4, alpha = 0.8, stroke = 1.15)+
  facet_wrap("generation")+
  scale_x_discrete(name = element_blank())+
  scale_y_continuous(name = expression(paste("Body fat (%)")),
                     limits = c(0,62),
                     breaks = seq(0,60,10))+
  theme_cowplot()


### Visualize age effect
bc_data |>
  ggplot(aes(x=age_days, y=fat_percent, fill=sex, shape=generation))+
  geom_point(size=4)+
  scale_shape_manual(values = c(21,22,23))+
  scale_x_continuous(name = expression(paste("Age (days)")),
                     limits = c(0,1000),
                     breaks = seq(0,1000,200))+
  guides(fill=guide_legend(override.aes = list(shape=21)))+
  theme_cowplot()

# Just F2 F3
bc_data |>
  dplyr::filter(generation != "F1") %>% 
  ggplot(aes(x=age_days, y=fat_percent, fill=sex, shape=generation))+
  geom_point(size=4)+
  scale_shape_manual(values = c(21,22,23))+
  scale_x_continuous(name = expression(paste("Age (days)")),
                     limits = c(0,1000),
                     breaks = seq(0,1000,200))+
  guides(fill=guide_legend(override.aes = list(shape=21)))+
  theme_cowplot()


bc_data |>
  ggplot(aes(x=age_days, y=fat_percent, fill=native_alt))+
  geom_point(shape=21, size=4, alpha=0.8, stroke=1.15)+
  scale_x_continuous(name = expression(paste("Age (days)")),
                     limits = c(0,1000),
                     breaks = seq(0,1000,250))+
  facet_wrap("generation")+
  #scale_shape_manual(values = c(21,25))+
  #guides(fill=guide_legend(override.aes = list(shape=21)))+
  scale_y_continuous(name = expression(paste("Body fat (%)")),
                     limits = c(0,62),
                     breaks = seq(0,60,10))+
  theme_cowplot()

# F2 F3 only
bc_data |>
  dplyr::filter(generation != "F1") %>% 
  ggplot(aes(x=age_days, y=fat_percent, fill=native_alt))+
  geom_point(shape=21, size=4, alpha=0.8, stroke=1.15)+
  scale_x_continuous(name = expression(paste("Age (days)")),
                     limits = c(0,1000),
                     breaks = seq(0,1000,250))+
  facet_wrap("generation")+
  #scale_shape_manual(values = c(21,25))+
  #guides(fill=guide_legend(override.aes = list(shape=21)))+
  scale_y_continuous(name = expression(paste("Body fat (%)")),
                     limits = c(0,62),
                     breaks = seq(0,60,10))+
  theme_cowplot()


######### BACKGROUND ###############
# BUREAUCRAT BELIEFS AND BEHAVIOR PAPER 
# Link to overleaf project >>> https://www.overleaf.com/project/683c1bc49373bd6559bdd314
# Datasets downloaded from Afrobarometer >>> https://www.afrobarometer.org/data/data-sets/
# Date created: 02.09.2025
# Date last modified: 02.09.2025

options(scipen = 999) # disable scientific notations for numerical answers

############################################################################################## Loading R Packages ##################
packages <- c("dplyr", "ggthemes", "glue", "tidyverse", "pdftools", "here", "zoo", "estimatr", "lmtest", "xtable",
              "robotoolbox", "labelled", "haven", "readxl", "xlsx", "data.table", "knitr", "sandwich", "texreg", "ggplot2",
              "readr", "tidyverse", "tm", "Hmisc", "gtsummary", "RCT", "vtable", "modelsummary", "writexl", "sf",
              "kableExtra", "eeptools", "stargazer", "stringr", "stringi", "purrr", "gt", "broom", "tinytable", "fixest",
              "sandwich", "lmtest", "clubSandwich", "irr", "ICC", "psych", "fwildclusterboot", "tableone", "janitor")

# Install packages if they are not already installed
installed_packages <- installed.packages()[, "Package"]
missing_packages <- packages[!(packages %in% installed_packages)]

if(length(missing_packages)) {
  install.packages(missing_packages)
}

require(packages)
lapply(packages, require, character.only = TRUE)

rm(list = ls()) # clears environment

############################################################################################## Set Working Folder ##################

#set working directory
setwd(rstudioapi::getActiveProject())

############################################################################################## LOADING RAW  DATA  ##################################################
#### Read merged data
#r9 <- read_sav("00_raw_data/afrobarometer/tanzania/merged_r9_data.sav")
#r8 <- read_sav("00_raw_data/afrobarometer/tanzania/merged_r8_data.sav")
#r7 <- read_sav("00_raw_data/afrobarometer/tanzania/merged_r7_data.sav")
#r6 <- read_sav("00_raw_data/afrobarometer/tanzania/merged_r6_data.sav")
#r5 <- read_sav("00_raw_data/afrobarometer/tanzania/merged_r5_data.sav")
#r4 <- read_sav("00_raw_data/afrobarometer/tanzania/merged_r4_data.sav")
#r3 <- read_sav("00_raw_data/afrobarometer/tanzania/merged_r3_data.sav")
#r2 <- read_sav("00_raw_data/afrobarometer/tanzania/merged_r2_data.sav")
#r1 <- read_sav("00_raw_data/afrobarometer/tanzania/merged_r1_data.sav")

# Read Tanzania Data
tzr1 <- read_sav("00_raw_data/afrobarometer/tanzania/tan_r1_data.sav")
tzr2 <- read_sav("00_raw_data/afrobarometer/tanzania/tan_r2_data.sav")
tzr3 <- read_sav("00_raw_data/afrobarometer/tanzania/tan_r3_data.sav")

### Rename important variables
# round 1 data
tzr1 <- tzr1 %>% 
  rename(
    occup_r1 = Q7,
    educ_r1 = Q6,
    hh_income_r1 = Q94,
    district = DISTRICT,
    age = Q3,
    withinwt = REGIONWT
    
  ) %>% 
  mutate(
    treatment = 0,
    teacher = ifelse(occup_r1 == 9, 1, ifelse(occup_r1 == 99, NA, 0)),
    free_sch = ifelse(Q62 == 1 | Q62 == 2, 1, ifelse(Q62 == 9, NA, 0)),
    diff_sch_child_placement = ifelse(Q80AB == 3 | Q80AB == 4, 1, ifelse(Q80AB == 1 | Q80AB == 2, 0, NA)),# Difficult to find a place in primary school for a child
    diff_hh_elec_connect = ifelse(Q80AD == 3 | Q80AD == 4, 1, ifelse(Q80AD == 1 | Q80AD == 2, 0, NA)),# Difficult to connect household to electricity
    diff_sch_child_placement_cost = ifelse(Q80BB == 1, 1, ifelse(Q80BB == 8 | Q80BB == 9, NA, 0)),
    diff_sch_child_placement_dist = ifelse(Q80BB == 2, 1, ifelse(Q80BB == 8 | Q80BB == 9, NA, 0)),
    diff_sch_child_placement_both = ifelse(Q80BB == 3, 1, ifelse(Q80BB == 8 | Q80BB == 9, NA, 0)),
    teachers_corrupt = ifelse(Q83H == 1 | Q83H == 2, 1, ifelse(Q83H == 9, NA, 0)),
    police_corrupt = ifelse(Q83C == 1 | Q83C == 2, 1, ifelse(Q83C == 9, NA, 0)),
    judge_corrupt = ifelse(Q83E == 1 | Q83E == 2, 1, ifelse(Q83E == 9, NA, 0)),
    mp_corrupt = ifelse(Q83A == 1 | Q83A == 2, 1, ifelse(Q83A == 9, NA, 0)), # in Round one framed as "Elected leaders"
    #healthworker_corrupt = ifelse(Q83C == 1 | Q83C == 2, 1, ifelse(Q83C == 9, NA, 0)),
    district = str_to_title(district),
    educ = ifelse(educ_r1 <=3, "Primary", ifelse(educ_r1 >=4 & educ_r1 <=5, "Secondary", ifelse(educ_r1 == 10, NA, "Tertiary")))
  ) %>% 
  select(teacher, treatment, free_sch, diff_sch_child_placement, diff_hh_elec_connect, district, age, withinwt, educ, teachers_corrupt, police_corrupt, judge_corrupt, mp_corrupt)



# round 2 data
tzr2 <- tzr2 %>% 
  rename(
    occup_r2 = q88,
    educ_r2 = q84,
    hh_income_r2 = q90,
    age = q80
  ) %>% 
  mutate(
    treatment = 1,
    teacher = ifelse(occup_r2 == 15, 1, ifelse(occup_r2 == 999, NA, 0)),
    free_sch = ifelse(q14 == 1 | q14 == 2, 1, ifelse(q14 == 9 | q14 == 5, NA, 0)),
    diff_sch_child_placement = ifelse(q58b == 1 | q58b == 2, 1, ifelse(q58b == 3 | q58b == 4, 0, NA)),# Difficult to find a place in primary school for a child
    diff_hh_elec_connect = ifelse(q58d == 3 | q58d == 4, 1, ifelse(q58d == 1 | q58d == 2, 0, NA)),# Difficult to connect household to electricity
    teachers_corrupt = ifelse(q51i == 2 | q51i == 3, 1, ifelse(q51i == 9, NA, 0)), # question framed differently though
    police_corrupt = ifelse(q51d == 2 | q51d == 3, 1, ifelse(q51d == 9, NA, 0)), # question framed differently though
    educ = ifelse(educ_r2 <=3, "Primary", ifelse(educ_r2 >=4 & educ_r2 <=5, "Secondary", ifelse(educ_r2 == 10, NA, "Tertiary")))
  ) %>% 
  select(teacher, treatment, free_sch, diff_sch_child_placement, diff_hh_elec_connect, age, withinwt, educ, teachers_corrupt, police_corrupt)

# round 3 data
tzr3 <- tzr3 %>% 
  rename(
    occup_r3 = Q95,
    educ_r3 = Q90,
    #hh_income_r3 = q90,
    district = DISTRICT,
    age = Q1
  ) %>% 
  mutate(
    treatment = 1,
    teacher = ifelse(occup_r3 == 20, 1, ifelse(occup_r3 == 999, NA, 0)),
    free_sch = ifelse(Q10 == 1 | Q10 == 2, 1, ifelse(Q10 == 9 | Q10 == 5, NA, 0)),
    diff_sch_child_placement = ifelse(Q71D == 1 | Q71D == 2, 1, ifelse(Q71D == 3 | Q71D == 4, 0, NA)),# Difficult to find a place in primary school for a child
    diff_hh_elec_connect = ifelse(Q71B == 3 | Q71B == 4, 1, ifelse(Q71B == 1 | Q71B == 2, 0, NA)),# Difficult to connect household to electricity
    teachers_corrupt = ifelse(Q56J == 2 | Q56J == 3, 1, ifelse(Q56J == 9, NA, 0)), # question framed differently though
    police_corrupt = ifelse(Q56F == 2 | Q56F == 3, 1, ifelse(Q56F == 9, NA, 0)), # question framed differently though
    judge_corrupt = ifelse(Q56H == 2 | Q56H == 3, 1, ifelse(Q56H == 9, NA, 0)), # question framed differently though
    mp_corrupt = ifelse(Q56B == 2 | Q56B == 3, 1, ifelse(Q56B == 9, NA, 0)),
    #healthworker_corrupt = ifelse(Q56I == 2 | Q56I == 3, 1, ifelse(Q56I == 9, NA, 0)),
    paid_bribe_sch_yr = ifelse(Q57B== 0, 0, ifelse(is.na(Q57B) | Q57B == 9, NA, 1)), # paid bribe to get a child to school
    resp_knows_about_policy = ifelse(Q69A == 2, 1, ifelse(Q69A == 1, 0, NA)),
    district = str_to_title(district),
    educ = ifelse(educ_r3 <=3, "Primary", ifelse(educ_r3 >=4 & educ_r3 <=5, "Secondary", ifelse(educ_r3 == 99, NA, "Tertiary")))
  ) %>% 
  select(teacher, treatment, free_sch, diff_sch_child_placement, diff_hh_elec_connect, district, age, withinwt, educ, teachers_corrupt, police_corrupt, judge_corrupt, mp_corrupt, paid_bribe_sch_yr)

############################################################################################## ANALYSIS  ##################################################
### Append r1 and r3 dataset
tz <- bind_rows(tzr1, tzr3)

################################################# Main DID Model

did_fe <- feols(
  free_sch ~ treatment * teacher | district, # free_sch represents support for free education. 
  data = tz,
  weights = ~ withinwt,     # apply weights
  cluster = ~ district      # cluster SEs at district level
)

did_fe1 <- feols(
  free_sch ~ treatment * teacher + age + educ, # free_sch represents support for free education. 
  data = tz,
  weights = ~ withinwt,     # apply weights
  cluster = ~ district      # cluster SEs at district level
)

did_fe2 <- feols(
  free_sch ~ treatment * teacher + age + educ | district, # free_sch represents support for free education. 
  data = tz,
  weights = ~ withinwt,     # apply weights
  cluster = ~ district      # cluster SEs at district level
)

# generate latex table
texreg(list(did_fe, did_fe1, did_fe2), 
       file = "02_tables/did_beliefs_main.tex", 
       custom.note = "",
       #custom.header = list("Literacy" = 1:3, "Numeracy" = 4:6),
       custom.model.names = c("1", "2", "3"),
       caption.above = TRUE, 
       #dcolumn = TRUE,
       booktabs = TRUE,
       use.packages = FALSE,
       custom.gof.rows = list(
         "District FE" = c("Yes", "No", "Yes"),
         "Controls" = c("No", "Yes", "Yes")
       ),
       custom.coef.map = list("treatment:teacher" = "Teacher x Post",
                              "teacher" = "Teacher", 
                              "treatment" = "Post", 
                              "age" = "Age", 
                              "educSecondary" = "Secondary education",
                              "educTertiary" = "Tertiary education"
                              ),
       table = FALSE,
       digits = 3,
       ci.force = FALSE,       
       include.ci = FALSE,    
       single.row = FALSE,    
       stars = c(0.01, 0.05, 0.1),
       include.adjr = FALSE, 
       include.rsquared = FALSE, 
       include.rmse = FALSE,
       include.nclusters = FALSE
) 

################################################# Evidence of rent-seeking
#reshape data
tz_long_police <- tz %>%
  select(educ, district, teachers_corrupt, police_corrupt, age, treatment, withinwt) %>% 
  pivot_longer(
    cols = c(teachers_corrupt, police_corrupt),
    names_to = "profession",
    values_to = "corrupt"
  ) %>%
  mutate(
    teacher = if_else(profession == "teachers_corrupt", 1, 0)
  ) %>%
  select(-profession)

tz_long_judge <- tz %>%
  select(educ, district, teachers_corrupt, judge_corrupt, age, treatment, withinwt) %>% 
  pivot_longer(
    cols = c(teachers_corrupt, judge_corrupt),
    names_to = "profession",
    values_to = "corrupt"
  ) %>%
  mutate(
    teacher = if_else(profession == "teachers_corrupt", 1, 0)
  ) %>%
  select(-profession)

tz_long_mp <- tz %>%
  select(educ, district, teachers_corrupt, mp_corrupt, age, treatment, withinwt) %>% 
  pivot_longer(
    cols = c(teachers_corrupt, mp_corrupt),
    names_to = "profession",
    values_to = "corrupt"
  ) %>%
  mutate(
    teacher = if_else(profession == "teachers_corrupt", 1, 0)
  ) %>%
  select(-profession)

## Evidence of rent-seeking
did_rentseeking <- feols(corrupt ~ teacher * treatment + educ + age | district, 
                         data = tz_long_police,
                         weights = ~ withinwt)
  
did_rentseeking1 <- feols(corrupt ~ teacher * treatment + educ + age | district, 
                         data = tz_long_judge,
                         weights = ~ withinwt,
                         cluster = ~district)

did_rentseeking2 <- feols(corrupt ~ teacher * treatment + educ + age | district, 
                         data = tz_long_mp,
                         weights = ~ withinwt,
                         cluster = ~district)

# generate latex table
texreg(list(did_rentseeking, did_rentseeking1, did_rentseeking2), 
       file = "02_tables/did_rent_main.tex", 
       custom.note = "",
       #custom.header = list("Literacy" = 1:3, "Numeracy" = 4:6),
       custom.model.names = c("Police", "Judge", "Elected leader"),
       caption.above = TRUE, 
       #dcolumn = TRUE,
       booktabs = TRUE,
       use.packages = FALSE,
       custom.gof.rows = list(
         "District FE" = c("Yes", "Yes", "Yes"),
         "Controls" = c("Yes", "Yes", "Yes")
       ),
       custom.coef.map = list("teacher:treatment" = "Teacher x Post",
                              "teacher" = "Teacher", 
                              "treatment" = "Post", 
                              "age" = "Age", 
                              "educSecondary" = "Secondary education",
                              "educTertiary" = "Tertiary education"
       ),
       table = FALSE,
       digits = 3,
       ci.force = FALSE,       
       include.ci = FALSE,    
       single.row = FALSE,    
       stars = c(0.01, 0.05, 0.1),
       include.adjr = FALSE, 
       include.rsquared = FALSE, 
       include.rmse = FALSE,
       include.nclusters = FALSE
) 



############################################### graph showing evidene of rent seeking
plot <- tz %>% 
  filter(!is.na(paid_bribe_sch_yr)) %>%
  count(paid_bribe_sch_yr) %>%
  mutate(percentage = round(n/sum(n) * 100,0),
         label = ifelse(paid_bribe_sch_yr == 0, "No", "Yes")) %>%
  ggplot(aes(x = label, y = percentage)) +
  geom_col(fill = c("grey90", "grey50"), color = "black", width = 0.6) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            vjust = -0.5, size = 4, fontface = "bold") +
  labs(title = "",
       x = "Paid Bribe",
       y = "Percent (%)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14))

ggsave(filename = "03_graphs/paid_bribe_sch_yr.png", 
       plot = plot, 
       width = 10, 
       height = 10, 
       dpi = 300)





A. It is better to have free schooling for our children, even if the quality of education is low.
B. It is better to raise educational standards, even if we have to pay school fees.


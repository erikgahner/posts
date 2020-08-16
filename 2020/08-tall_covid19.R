# R script to figures in "Does being tall raise the risk of getting COVID-19?"
# Link: https://erikgahner.dk/2020/does-being-tall-raise-the-risk-of-getting-covid-19/

library("tidyverse")
library("readxl")
library("janitor")

# Data is available at: https://osf.io/v9t8a/?view_only=8531e8dd672f41e6bf532e280a2f31e6

data_uk <- read_xlsx("UK covid 2020 data.xlsx", sheet = "Individuals") %>% 
  janitor::clean_names() %>% 
  mutate(positive = ifelse(have_you_had_a_medical_diagnosis_or_positive_test_for_covid_19 == "Yes", 1, 0),
         male = ifelse(gender == "male", 1, 0),
         age = 2020 - parse_number(year_of_birth),
         tall = case_when(
           str_detect(more_about_you_and_your_situation, "in height\\? \\:Yes") ~ 1,
           str_detect(more_about_you_and_your_situation, "in height\\? \\:No") ~ 0,
           TRUE ~ NA_real_
         )) %>% 
  filter(male == 1)

data_us <- read_xlsx("US covid 2020 data.xlsx", sheet = "Individuals") %>% 
  janitor::clean_names() %>% 
  mutate(positive = ifelse(have_you_had_a_medical_diagnosis_or_positive_test_for_covid_19 == "Yes", 1, 0),
         male = ifelse(gender == "male", 1, 0),
         age = 2020 - parse_number(year_of_birth),
         tall = case_when(
           str_detect(more_about_you_and_your_situation, "in height\\? \\:Yes") ~ 1,
           str_detect(more_about_you_and_your_situation, "in height\\? \\:No") ~ 0,
           TRUE ~ NA_real_
         )) %>% 
  filter(male == 1)

table(data_uk$positive)
table(data_uk$tall)
table(data_us$positive)
table(data_us$tall)

table(data_uk$positive, data_uk$tall)
table(data_us$positive, data_us$tall)

broom::tidy(lm(positive ~ tall, data = data_uk))
broom::tidy(lm(positive ~ tall, data = data_us))

broom::tidy(lm(positive ~ tall + age, data = data_uk))
broom::tidy(lm(positive ~ tall + age, data = data_us))

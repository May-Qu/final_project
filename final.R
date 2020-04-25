#final
library(haven)
library(tidyverse)

Health_Status_and_Functioning <- read_dta("2015_data/Health_Status_and_Functioning.dta")
View(Health_Status_and_Functioning)

Happiness <- Health_Status_and_Functioning %>%
  select(ID,dc042_w3,dc043_w3,dc044_w3,dc028)%>%
  filter(!is.na(dc042_w3),!is.na(dc043_w3),!is.na(dc044_w3),!is.na(dc028))
view(Happiness)

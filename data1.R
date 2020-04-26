library(tidyverse)
library(haven)

Demographic_Background <- read_dta("/Users/zoe/Desktop/CHARLS/2015_data/Biomarker.dta") %>%
  select(ID, ba000_w2_3, ba004_w3_1) %>%
  filter(!is.na(ID), !is.na(ba000_w2_3), !is.na(ba004_w3_1))

Family_transfer <- read_dta("/Users/zoe/Desktop/CHARLS/2015_data/Family_Transfer.dta") %>%
  mutate_all(funs(ifelse(is.na(.), 0, .)))
total_support <- Family_transfer$ce009_1_1_ + Family_transfer$ce009_1_2_ + Family_transfer$ce009_1_3_ + Family_transfer$ce009_1_4_ + Family_transfer$ce009_1_5_ + Family_transfer$ce009_1_6_ + Family_transfer$ce009_1_7_ + Family_transfer$ce009_1_8_ + Family_transfer$ce009_1_9_ + Family_transfer$ce009_1_10_ + Family_transfer$ce009_1_11_ + Family_transfer$ce009_1_12_ + Family_transfer$ce009_1_13_ + Family_transfer$ce009_1_14_ + Family_transfer$ce009_1_16_ + Family_transfer$ce009_1_16_
Family_transfer <- cbind(Family_transfer, total_support) %>%
  select(ID, total_support)

Individual_Income <- read_dta("/Users/zoe/Desktop/CHARLS/2015_data/Individual_Income.dta") %>%
  select(ID, ga002, hc005, hd001) %>%
  filter(!is.na(ID), !is.na(ga002), !is.na(hc005), !is.na(hd001))

Child <- read_dta("/Users/zoe/Desktop/CHARLS/2015_data/Child.dta") %>%
  select(ID, gender, cb053_2, cb069) %>%
  filter(!is.na(ID), !is.na(gender), !is.na(cb053_2), !is.na(cb069))

Health <- read_dta("/Users/zoe/Desktop/CHARLS/2015_data/Health_Status_and_Functioning.dta") %>%
  select(ID, zda040, zda059, da002_w2_1, da041) %>%
  filter(!is.na(ID), !is.na(zda040), !is.na(zda059), !is.na(da002_w2_1), !is.na(da041))









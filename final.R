#final
library(haven)
library(tidyverse)
library(tidymodels)

Health_Status_and_Functioning <- read_dta("2015_data/Health_Status_and_Functioning.dta")
View(Health_Status_and_Functioning)

Happiness <- Health_Status_and_Functioning %>%
  select(ID,dc042_w3,dc043_w3,dc044_w3,dc028)%>%
  filter(!is.na(dc042_w3),!is.na(dc043_w3),!is.na(dc044_w3),!is.na(dc028))


H_4 <- Happiness %>%
  filter(dc043_w3!=6&dc044_w3!=6)%>%
  mutate(index =0.2*dc042_w3 + 0.2* dc043_w3 +0.2*dc044_w3 + 0.4*dc028)

H_3a <- Happiness %>%
  filter(dc043_w3==6)%>%
  mutate(index = 0.3*dc044_w3+0.3*dc042_w3+0.4*dc028)
H_3b <- Happiness %>%
  filter(dc044_w3 == 6)%>%
  mutate(index = 0.3*dc043_w3+0.3*dc042_w3+0.4*dc028)
H_2 <- Happiness%>%
  filter(dc043_w3==6&dc044_w3==6)
#No obs with two invalid answers
hindex<-rbind(H_4,H_3a,H_3b)
#weighted happiness index added


Demographic_Background <- read_dta("2015_data/Demographic_Background.dta") %>%
  select(ID, ba000_w2_3, ba004_w3_1) %>%
  filter(!is.na(ID), !is.na(ba000_w2_3), !is.na(ba004_w3_1))

Family_transfer <- read_dta("2015_data/Family_Transfer.dta") %>%
  mutate_all(funs(ifelse(is.na(.), 0, .)))
total_support <- Family_transfer$ce009_1_1_ + Family_transfer$ce009_1_2_ + Family_transfer$ce009_1_3_ + Family_transfer$ce009_1_4_ + Family_transfer$ce009_1_5_ + Family_transfer$ce009_1_6_ + Family_transfer$ce009_1_7_ + Family_transfer$ce009_1_8_ + Family_transfer$ce009_1_9_ + Family_transfer$ce009_1_10_ + Family_transfer$ce009_1_11_ + Family_transfer$ce009_1_12_ + Family_transfer$ce009_1_13_ + Family_transfer$ce009_1_14_ + Family_transfer$ce009_1_16_ + Family_transfer$ce009_1_16_
Family_transfer <- cbind(Family_transfer, total_support) %>%
  select(ID, total_support)

Individual_Income <- read_dta("2015_data/Individual_Income.dta") %>%
  select(ID, ga002, hc005,hd001) %>%
  filter(!is.na(ID), !is.na(ga002),!is.na(hc005),!is.na(hd001))

Child <- read_dta("2015_data/Child.dta") %>%
  select(ID, gender, cb069) %>%
  filter(!is.na(ID), !is.na(gender), !is.na(cb069))

Health <- read_dta("2015_data/Health_Status_and_Functioning.dta") %>%
  select(ID, zda040, zda059, da002_w2_1, da041) %>%
  filter(!is.na(ID), !is.na(zda040), !is.na(zda059), !is.na(da002_w2_1), !is.na(da041))

hindex_v1 <- inner_join(x= hindex,y= Child,by = "ID")
hindex_v1 <- select(hindex_v1,ID,index)
hindex_v2 <- inner_join(x= hindex_v1,y= Demographic_Background,by = "ID")
hindex_v3 <- inner_join(x= hindex_v2,y= Family_transfer,by = "ID")
hindex_v4 <- inner_join(x= hindex_v3,y= Health,by = "ID")
hindex_final <- inner_join(x= hindex_v4,y= Individual_Income,by = "ID")
hindex_final <- hindex_final%>%
  rename(child_gender = gender)%>%
  mutate(age = 2020 - ba004_w3_1)
#Merge the final dataset for machine learning

hindex_ml <- hindex_final[,-1]
hindex_ml <- hindex_ml[,-9]
set.seed(seed = 20200503)
split <- initial_split(hindex_ml, prop = 0.8)
index_training <- training(split) 
index_testing <- testing(split)

index_resamples <- vfold_cv(data = index_training, v = 10)

train_lm <- function(split, formula, ...) {
  analysis_data <- analysis(split)
  model <- linear_reg() %>% 
    set_engine("lm") %>% 
    fit(formula, data = analysis_data)
  assessment_data <- assessment(split)
  rmse <- bind_cols( assessment_data, predict(model, assessment_data) ) %>% 
    rmse(truth =index, estimate = .pred) %>% 
    pull(.estimate)
  return(rmse)
}


train_lm2 <- function(split, formula, ...) {
   ames_recipe <- recipe(formula, data = analysis(split)) %>% 
   step_log(ga_002,hc005,hd001,total_support) %>% 
   step_center(square_footage) %>%
   step_scale(square_footage) %>%
   prep()
  
   analysis_data <- analysis(split) %>% 
   bake(object = ames_recipe, new_data = .)
   
   model <- linear_reg() %>% 
   set_engine("lm") %>% 
   fit(formula, data = analysis_data)
   assessment_data <- assessment(split) %>% 
   bake(object = ames_recipe, new_data = .)
   rmse <- bind_cols( assessment_data, predict(model, assessment_data) ) %>% 
   rmse(truth = sale_price, estimate = .pred) %>% pull(.estimate)
  return(rmse)
}
ames_resamples %>% 
  mutate( rmse1 = map_dbl(splits, ~train_lm(split = .x, formula = index ~.)) ) %>% 
  select(id, rmse1)





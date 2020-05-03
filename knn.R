library(haven)
library(tidyverse)
library(tidymodels)

happiness <- read_csv("/Users/zoe/Desktop/happiness.csv") %>%
  filter(index != 3)

hsubset <- happiness %>%
  mutate(happy = ifelse(index > 3, "happy",
                                   "unhappy")) %>%
  mutate(happy = factor(happy)) %>%
  select(-ID)

# knn
set.seed(19920314)
split <- initial_split(data = hsubset, prop = 0.8)
happy_training <- training(split)
happy_testing <- testing(split)

recipe <-
  recipe(formula = happy ~ .,
         data = happy_training) %>% step_normalize(-happy) %>%
  prep()

model <-nearest_neighbor(mode = "classification", neighbors = 11) %>%
  set_engine(engine = "kknn") %>%
  fit(formula = happy ~., data = bake(object = recipe, new_data = happy_training))
  
prediction <- bind_cols(
  happy_testing,
  decision_tree = predict(model, bake(object = recipe,
                                      new_data = happy_testing))
)

prediction %>%
  conf_mat(truth = happy,
           estimate = .pred_class)



#cart
set.seed(20200301)
split_c <- initial_split(data = hsubset, prop = 0.8)
happy_training <- training(split)
happy_testing <- testing(split)

model_c <-
  decision_tree(mode = "classification") %>%
  set_engine(engine = "rpart") %>%
  fit(formula = happy ~., data = happy_training)

prediction <- bind_cols(
  happy_testing,
  decision_tree = predict(model, happy_testing)
)

prediction %>%
  conf_mat(truth = happy,
           estimate = .pred_class)





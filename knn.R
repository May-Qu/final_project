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



# knn & cart regression model
hr <- happiness %>%
  select(-ID)

sr <- initial_split(data = hr, prop = 0.8)
h_training <- training(sr)
h_testing <- testing(sr)

h_resamples <- vfold_cv(data = hr, v = 10)

train_imputation <- function(split, formula, model, ...) {
  analysis_data <- analysis(sr)
  if (model == "knn") {
    model <-
    nearest_neighbor(mode = "regression", 
                     neighbors = 5) %>%
      set_engine("kknn") %>%
      fit(formula, data = analysis_data)
  } else if (model == "cart") {
    model <-
      decision_tree(mode = "regression") %>%
      set_engine("rpart") %>%
      fit(formula, data = analysis_data)
  }
  assessment_data <- assessment(sr)
  rmse <- bind_cols(
    assessment_data,
    predict(model, assessment_data)
  ) %>%
    rmse(truth = index, estimate = .pred) %>%
    pull(.estimate)
  return(rmse)
}

h_resamples <- h_resamples %>%
  mutate(knn_rmse = map_dbl(.x = splits,
                            .f = ~train_imputation(split = .x,
                                              formula = index ~ . ,
                                              model = "knn")))

h_resamples <- h_resamples %>%
  mutate(cart_rmse = map_dbl(.x = splits,
                            .f = ~train_imputation(split = .x,
                                                   formula = index ~ . ,
                                                   model = "cart")))
h_resamples <- h_resamples %>%
  select(-splits)

knitr::kable(h_resamples)

h_resamples %>% 
  pivot_longer(-id) %>%
  ggplot(aes(x = id,
             y = value,
             color = name, group = name)) +
  geom_point() +
  geom_line() + 
  scale_y_continuous(limits = c(0, NA))
    
hm <-
  decision_tree(mode = "regression") %>%
  set_engine("rpart") %>%
  fit(index ~ . ,
      data = h_training)

bind_cols(
  h_testing,
  predict(hm, h_testing) 
) %>%
  metrics(truth = index, estimate = .pred)










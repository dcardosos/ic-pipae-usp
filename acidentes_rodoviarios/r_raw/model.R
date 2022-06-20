library(tidymodels)

df <- readr::read_csv("acidentes_rodoviarios/data/ocorrencias_2021.csv") |> 
  dplyr::select(-id, -data, -km, -condicao_metereologica, -uop, -regional, 
                -delegacia, -ano, -latitude, -longitude, -municipio,
                -classificacao_acidente, -feridos, -ilesos, -feridos_leves,
                -feridos_graves, -ignorados) |> 
  dplyr::filter(!is.na(br))


set.seed(123)
df_split <- initial_split(df)
train <- training(df_split)
test <- testing(df_split)


set.seed(234)
folds <- vfold_cv(train)
folds

library(usemodels)
use_ranger(mortos ~ ., data = train)


ranger_recipe <- 
  recipe(formula = mortos ~ ., data = train) %>% 
  step_string2factor(one_of("dia_semana", "uf", "causa_acidente", "tipo_acidente", 
                            "fase_dia", "sentido_via", "tipo_pista", "tracado_via", "uso_solo")) 

ranger_spec <- 
  rand_forest(mtry = tune(), min_n = tune(), trees = 100) %>% 
  set_mode("regression") %>% 
  set_engine("ranger") 

ranger_workflow <- 
  workflow() %>% 
  add_recipe(ranger_recipe) %>% 
  add_model(ranger_spec) 

set.seed(56989)
doParallel::registerDoParallel()
ranger_tune <-
  tune_grid(ranger_workflow, 
            resamples = folds, 
            grid = 5)


show_best(ranger_tune, metric = "rmse")
show_best(ranger_tune, metric = "rsq")

autoplot(ranger_tune)


final_rf <- ranger_workflow |> 
  finalize_workflow(select_best(ranger_tune))

final_rf


df_fit <- last_fit(final_rf, df_split)
df_fit

collect_metrics(df_fit)

collect_predictions(df_fit) |> 
  ggplot(aes(mortos, .pred)) + 
  geom_abline(lty = 2, color = "gray50") +
  geom_point(alpha = 0.5, color = "midnightblue") + 
  coord_fixed()


predict(df_fit$.workflow[[1]], test)

library(vip)

imp_spec <- ranger_spec |> 
  finalize_model(select_best(ranger_tune)) |> 
  set_engine("ranger", importance = "permutation")


workflow() |> 
  add_recipe(ranger_recipe) |> 
  add_model(imp_spec) |> 
  fit(train) |> 
  pull_workflow_fit() |> 
  vip(aesthetic = list(alpha = 0.8, fill = "midnightblue"))

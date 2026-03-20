library(tidymodels)

matter <- read.csv("C:/Users/sargu/Downloads/CM actual/amot.csv")
clearing <- read.csv("C:/Users/sargu/Downloads/CM actual/the_clearing.csv")
older <- read.csv("C:/Users/sargu/Downloads/CM actual/older_and_wiser.csv")

albums <-
  bind_rows(
    matter |> mutate(Category = "A Matter of Time"),
    clearing |> mutate(Category = "The Clearing"),
    older |> mutate(Category = "Older and Wiser")
  ) 

library(ggdendro)
library(heatmaply)
library(compmus)

get_conf_mat <- function(fit) {
  outcome <- .get_tune_outcome_names(fit)
  fit |> 
    collect_predictions() |> 
    conf_mat(truth = outcome, estimate = .pred_class)
}  

get_pr <- function(fit) {
  fit |> 
    conf_mat_resampled() |> 
    group_by(Prediction) |> mutate(precision = Freq / sum(Freq)) |> 
    group_by(Truth) |> mutate(recall = Freq / sum(Freq)) |> 
    ungroup() |> filter(Prediction == Truth) |> 
    select(class = Prediction, precision, recall)
}  

albums_juice <-
  recipe(
    `Track.Name` ~
      Danceability +
      Energy +
      Acousticness +
      Valence +
      `Duration..ms.`,
    data = albums
  ) |>
  step_center(all_predictors()) |>
  step_scale(all_predictors()) |> 
  # step_range(all_predictors()) |> 
  prep(albums |> mutate(`Track.Name` = str_trunc(`Track.Name`, 36))) |>
  juice() |>
  column_to_rownames("Track.Name")

albums_dist <- dist(albums_juice, method = "euclidean")

albums_dist |> 
  hclust(method = "complete") |> # Try single, average, and complete.
  dendro_data() |>
  ggdendrogram()

#classification

albums_recipe <-
  recipe(
    Album.Name ~
      Danceability +
      Energy +
      Loudness +
      Speechiness +
      Acousticness +
      Instrumentalness +
      Liveness +
      Valence +
      Tempo +
      `Duration..ms.`,
    data = albums                    # Use the same name as the previous block.
  ) |>
  step_center(all_predictors()) |>
  step_scale(all_predictors())      # Converts to z-scores.
# step_range(all_predictors())    # Sets range to [0, 1].

albums_cv <- albums |> vfold_cv(10)

forest_model <-
  rand_forest() |>
  set_mode("classification") |> 
  set_engine("ranger", importance = "impurity")

albums_forest <- 
  workflow() |> 
  add_recipe(albums_recipe) |> 
  add_model(forest_model) |> 
  fit_resamples(
    albums_cv, 
    control = control_resamples(save_pred = TRUE)
  )

albums_forest |> get_pr()

albums_forest |> get_conf_mat() |> autoplot(type = "mosaic")

albums_forest |> get_conf_mat() |> autoplot(type = "heatmap")

workflow() |> 
  add_recipe(albums_recipe) |> 
  add_model(forest_model) |> 
  fit(albums) |> 
  pluck("fit", "fit", "fit") |>
  ranger::importance() |> 
  enframe() |> 
  mutate(name = fct_reorder(name, value)) |> 
  ggplot(aes(name, value)) + 
  geom_col() + 
  coord_flip() +
  theme_minimal() +
  labs(x = NULL, y = "Importance")

albums |>
  ggplot(aes(x = Acousticness, y = Liveness, colour = Playlist, size = Energy)) +
  geom_point(alpha = 0.8) +
  scale_color_viridis_d() +
  labs(
    x = "Acousticness",
    y = "Liveness",
    size = "Energy",
    colour = "Playlist"
  )

albums_recipe <-
  recipe(
    Album.Name ~
      Energy +
      Loudness +
      Speechiness +
      Acousticness +
      `Duration..ms.`,
    data = albums                    # Use the same name as the previous block.
  ) |>
  step_center(all_predictors()) |>
  step_scale(all_predictors())      # Converts to z-scores.
# step_range(all_predictors())    # Sets range to [0, 1].

albums_cv <- albums |> vfold_cv(10)

forest_model <-
  rand_forest() |>
  set_mode("classification") |> 
  set_engine("ranger", importance = "impurity")

albums_forest <- 
  workflow() |> 
  add_recipe(albums_recipe) |> 
  add_model(forest_model) |> 
  fit_resamples(
    albums_cv, 
    control = control_resamples(save_pred = TRUE)
  )

albums_forest |> get_pr()

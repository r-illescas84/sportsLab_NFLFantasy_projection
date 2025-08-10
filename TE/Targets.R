library(nflreadr)
library(dplyr)

targets_jugador <- function(year, player_id) {
  pbp <- load_pbp(year) |> 
    filter(season_type == "REG") |> 
    filter(pass == 1) |> 
    filter(receiver_player_id == player_id) |>
    filter(!is.na(epa))
  
  total_targets <- nrow(pbp)
  jugador <- unique(pbp$receiver_player_name)
  
  return(data.frame(
    jugador = jugador,
    targets = total_targets
  ))
}

pbp_2024 <- load_pbp(2024) |>
  filter(season_type == "REG", pass == 1, complete_pass == 1) |>
  filter(!is.na(receiver_player_id))

ids_filtrados <- pbp_2024 |>
  count(receiver_player_id, name = "recepciones") |>
  filter(recepciones >= 50) |>
  pull(receiver_player_id)

resultados <- map_dfr(ids_filtrados, ~ targets_jugador(2024, .x))

resultados_ordenado <- resultados |> arrange(desc(targets))

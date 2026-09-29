library(tidyverse)
library(nflfastR)

rec_yrds_pg <- function(year, receiver_player_id) {
  pbp <- load_pbp(year)
  
  pbp_regular <- pbp |> 
    filter(season_type == "REG")
  
  juegos_jugados <- pbp_regular |> 
    filter(
      receiver_player_id == !!receiver_player_id |
        rusher_player_id == !!receiver_player_id |
        passer_player_id == !!receiver_player_id
    ) |> 
    distinct(game_id) |> 
    nrow()
  
  pbp_p <- pbp |> 
    filter(season_type == "REG") |>
    filter(pass == 1, complete_pass == 1) |> 
    filter(receiver_player_id == !!receiver_player_id) |> 
    filter(!is.na(receiving_yards))
  
  if (nrow(pbp_p) == 0) {
    return(data.frame(
      id = receiver_player_id,
      recepciones = 0
    ))
  }
  
  nombre <- unique(pbp_p$receiver_player_name)
  total_yardas <- sum(pbp_p$receiving_yards, na.rm = TRUE)
  ypg <- total_yardas / juegos_jugados
  
  return(data.frame(
    nombre = nombre,
    partidos = juegos_jugados,
    yardas = total_yardas,
    yardas_por_partido = round(ypg, 2)
  ))
}

# Prueba Jugadores 2024 con al menos 50 recepciones
# pbp_2024 <- load_pbp(2024) |>
#   filter(season_type == "REG", pass == 1, complete_pass == 1) |>
#   filter(!is.na(receiver_player_id))
# 
# ids_filtrados <- pbp_2024 |>
#   count(receiver_player_id, name = "recepciones") |>
#   filter(recepciones >= 50) |>
#   pull(receiver_player_id)
# 
# resultados <- map_dfr(ids_filtrados, ~ rec_yrds_pg(2024, .x))
# 
# resultados_ordenado <- resultados |> arrange(desc(yardas))

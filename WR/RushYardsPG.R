library(tidyverse)
library(nflfastR)

Rush_yrds_pg <- function(year, rusher_player_id){
  pbp <- load_pbp(year)
  
  pbp_regular <- pbp |> 
    filter(season_type == "REG")
  
  juegos_jugados <- pbp_regular |> 
    filter(
      receiver_player_id == !!rusher_player_id |
        rusher_player_id == !!rusher_player_id |
        passer_player_id == !!rusher_player_id
    ) |> 
    distinct(game_id) |> 
    nrow()
  
  pbp_r <- pbp |> 
    filter(season_type == "REG") |>
    filter(rush_attempt == 1) |> 
    filter(rusher_player_id == !!rusher_player_id) |> 
    filter(!is.na(epa))
  
  if (nrow(pbp_r) == 0) {
    return(data.frame(
      id = rusher_player_id,
      rush_yards = 0
    ))
  }
  
  nombre <- unique(pbp_r$rusher_player_name)
  total_yardas <- sum(pbp_r$yards_gained, na.rm = TRUE)
  ypg <- total_yardas / juegos_jugados
  
  return(data.frame(
    nombre = nombre,
    partidos = juegos_jugados,
    yardas = total_yardas,
    yardas_por_partido = round(ypg, 2)
  ))
}

#Prueba Jugadores con al menos 100 carries en 2024
# pbp_2024 <- load_pbp(2024) |>
#   filter(season_type == "REG", rush_attempt == 1) |>
#   filter(!is.na(rusher_player_id))
# 
# ids_filtrados <- pbp_2024 |>
#   count(rusher_player_id, name = "attempts") |>
#   filter(attempts >= 100) |>
#   pull(rusher_player_id)
# 
# resultadosr <- map_dfr(ids_filtrados, ~ Rush_yrds_pg(2024, .x))
# 
# resultados_ordenador <- resultadosr |> arrange(desc(yardas))

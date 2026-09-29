library(tidyverse)
library(nflfastR)

Yrd20_Rec <- function(year, receiver_player_id){
  pbp <- load_pbp(year)
  
  pbp_p <- pbp |> 
    filter(season_type == "REG") |>
    filter(pass == 1, receiving_yards >= 20) |> 
    filter(receiver_player_id == !!receiver_player_id) |> 
    filter(!is.na(epa))
  
  if (nrow(pbp_p) == 0) {
    return(data.frame(
      id = receiver_player_id,
      over20_yrd_pass = 0
    ))
  }
  
  total_20_plays <- nrow(pbp_p)
  nombre <- unique(pbp_p$receiver_player_name)
  
  
  return(data.frame(
    nombre = nombre,
    jugadas_largas = total_20_plays
  ))
}

#Prueba con todos los jugadores que tuvieron al menos 1 recepcion de 20+ yardas en 2024
# pbp_2024 <- load_pbp(2024) |>
#   filter(season_type == "REG", pass == 1, yards_gained >= 20) |>
#   filter(!is.na(receiver_player_id))
# 
# ids_unicos <- unique(pbp_2024$receiver_player_id)
# 
# resultadosr <- map_dfr(ids_unicos, ~ Yrd20_Rec(2024, .x))
# 
# resultados_ordenador <- resultadosr |> arrange(desc(jugadas_largas))


Yrd20_rush <- function(year, player_id){
  pbp <- load_pbp(year)
  
  pbp_r <- pbp |>
    filter(season_type == "REG") |>
    filter((rush == 1 | qb_scramble == 1), yards_gained >= 20) |>
    filter(rusher_player_id == !!player_id)
  
  if (nrow(pbp_r) == 0) {
    return(data.frame(
      id = player_id,
      over20_yrd_rush = 0
    ))
  }
  
  total_20_rush <- nrow(pbp_r)
  nombre <- unique(pbp_r$rusher_player_name)
  
  data.frame(
    nombre = nombre,
    acarreos_largos = total_20_rush
  )
}

#Pruebas 
pbp_2024 <- load_pbp(2024) |>
  filter(season_type == "REG",
         (rush == 1 | qb_scramble == 1),
         yards_gained >= 20)

ids_unicos <- unique(pbp_2024$rusher_player_id)

resultadosru <- purrr::map_dfr(ids_unicos, ~ Yrd20_rush(2024, .x))
resultados_ordenadoru <- resultadosru |> arrange(desc(acarreos_largos))

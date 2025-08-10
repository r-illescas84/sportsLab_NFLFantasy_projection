library(tidyverse)
library(nflfastR)

total_ints <- function(year, passer_player_id){
  pbp <- load_pbp(year)
  
  pbp_i <- pbp |> 
    filter(season_type == "REG") |>
    filter(pass == 1, interception == 1) |> 
    filter(passer_player_id == !!passer_player_id) |> 
    filter(!is.na(epa))
  
  if (nrow(pbp_i) == 0) {
    return(data.frame(
      id = passer_player_id,
      intercepciones = 0
    ))
  }
  
  total_ints <- nrow(pbp_i)
  nombre <- unique(pbp_i$passer_player_name)
  
  
  return(data.frame(
    nombre = nombre,
    intercepciones = total_ints
  ))
}

#Pruebas
pbp_2024 <- load_pbp(2024) |>
  filter(season_type == "REG", pass == 1) |>
  filter(!is.na(passer_player_id))

ids_filtrados <- pbp_2024 |>
  count(passer_player_id, name = "attempts") |>
  filter(attempts >= 100) |>
  pull(passer_player_id)

resultados_ints <- map_dfr(ids_filtrados, ~ total_ints(2024, .x))

resultados_ints_ordenado <- resultados_ints |> arrange(desc(intercepciones))

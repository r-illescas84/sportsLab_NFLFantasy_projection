library(tidyverse)
library(nflfastR)

Rec_total <- function(year, receiver_player_id){
  pbp <- load_pbp(year)
  
  pbp_p <- pbp |> 
    filter(season_type == "REG") |>
    filter(pass == 1, complete_pass == 1) |> 
    filter(receiver_player_id == !!receiver_player_id) |> 
    filter(!is.na(epa)) #Filtra Penalty plays
  
  if (nrow(pbp_p) == 0) {
    return(data.frame(
      id = receiver_player_id,
      recepciones = 0
    ))
  }
  
  total_recepciones <- nrow(pbp_p)
  nombre <- unique(pbp_p$receiver_player_name)
  
  
  return(data.frame(
    nombre = nombre,
    recepciones = total_recepciones
  ))
}

# Prueba jugadores min 50 catches
# pbp_2024 <- load_pbp(2024) |>
#   filter(season_type == "REG", pass == 1, complete_pass == 1) |>
#   filter(!is.na(receiver_player_id))
# 
# ids_filtrados <- pbp_2024 |>
#   count(receiver_player_id, name = "recepciones") |>
#   filter(recepciones >= 50) |>
#   pull(receiver_player_id)
# 
# resultados <- map_dfr(ids_filtrados, ~ Rec_total(2024, .x))
# 
# resultados_ordenado <- resultados |> arrange(desc(recepciones))

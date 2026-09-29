library(tidyverse)
library(nflfastR)

Rec_td <- function(year, receiver_player_id){
  pbp <- load_pbp(year)
  
  pbp_p <- pbp |>
    filter(season_type == "REG",
           pass_touchdown == 1,
           complete_pass == 1,
           !is.na(receiver_player_id),
           receiver_player_id == !!receiver_player_id,
           td_player_id == receiver_player_id)
  
  if (nrow(pbp_p) == 0) {
    return(data.frame(
      id = receiver_player_id,
      rec_touchdowns = 0
    ))
  }
  
  data.frame(
    nombre = unique(pbp_p$receiver_player_name),
    rec_touchdowns = nrow(pbp_p)
  )
}

#Prueba
# ids_tdrec <- load_pbp(2024) |>
#   filter(season_type == "REG",
#          pass_touchdown == 1,
#          complete_pass == 1,
#          !is.na(receiver_player_id)) |>
#   distinct(receiver_player_id) |>
#   pull(receiver_player_id)
# 
# resultados <- map_dfr(ids_tdrec, ~ Rec_td(2024, .x))
# 
# resultados_ordenado <- resultados |>
#   arrange(desc(touchdowns))

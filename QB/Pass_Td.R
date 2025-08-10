library(tidyverse)
library(nflfastR)

Pass_td <- function(year, passer_player_id){
  pbp <- load_pbp(year)
  
  pbp_pa <- pbp |>
    filter(season_type == "REG",
           pass == 1,
           pass_touchdown == 1,
           !is.na(passer_player_id),
           passer_player_id == !!passer_player_id)
  
  if (nrow(pbp_pa) == 0) {
    return(data.frame(
      id = passer_player_id,
      pass_touchdowns = 0
    ))
  }
  
  data.frame(
    nombre = unique(pbp_pa$passer_player_name),
    pass_touchdowns = nrow(pbp_pa)
  )
}

#Prueba
# pbp_2024_tdpass <- load_pbp(2024) |>
#   filter(season_type == "REG",
#          pass == 1,
#          pass_touchdown == 1,
#          !is.na(passer_player_id))
# 
# ids_con_pass_td <- pbp_2024_tdpass |>
#   distinct(passer_player_id) |>
#   pull(passer_player_id)
# 
# resultados <- map_dfr(ids_con_pass_td, ~ Pass_td(2024, .x))
# 
# resultados_ordenado <- resultados |> arrange(desc(pass_touchdowns))

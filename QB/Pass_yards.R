library(tidyverse)
library(nflfastR)

pass_yrds_pg <- function(year, passer_player_id) {
  pbp <- load_pbp(year)
  
  pbp_p <- pbp |> 
    filter(season_type == "REG") |>
    filter(pass == 1) |> 
    filter(passer_player_id == !!passer_player_id)
  
  if (nrow(pbp_p) == 0) {
    return(data.frame(
      id = passer_player_id,
      pases = 0
    ))
  }
  
  nombre <- unique(pbp_p$passer_player_name)
  total_yardas <- sum(pbp_p$passing_yards, na.rm = TRUE)
  juegos_jugados <- (n_distinct(pbp_p$game_id))
  ypg <- total_yardas / juegos_jugados
  
  return(data.frame(
    nombre = nombre,
    yardas = total_yardas,
    juegos = juegos_jugados,
    yardas_por_partido = round(ypg, 2)
  ))
}

# Prueba Min 100 Pass Attempts 2024
# pbp_2024 <- load_pbp(2024) |>
#   filter(season_type == "REG", pass == 1) |>
#   filter(!is.na(passer_player_id))
# 
# ids_filtrados <- pbp_2024 |>
#   count(passer_player_id, name = "attempts") |>
#   filter(attempts >= 100) |>
#   pull(passer_player_id)
# 
# resultados <- map_dfr(ids_filtrados, ~ pass_yrds_pg(2024, .x))
# 
# resultados_ordenado <- resultados |> arrange(desc(yardas))


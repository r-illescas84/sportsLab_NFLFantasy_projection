library(tidyverse)
library(nflfastR)

pass_stats <- function(year, passer_player_id) {
  pbp <- load_pbp(year)
  
  pbp_p <- pbp |> 
    filter(
      season_type == "REG",
      pass == 1,                     
      two_point_attempt == 0,        
      play_type != "no_play",        
      passer_player_id == !!passer_player_id
    )
  
  if (nrow(pbp_p) == 0) {
    return(data.frame(
      nombre = NA,
      attempts = 0,
      completions = 0,
      cmp_pct = 0
    ))
  }
  
  nombre <- unique(na.omit(pbp_p$passer_player_name))
  if (length(nombre) == 0) nombre <- NA_character_
  
  completions   <- sum(pbp_p$complete_pass == 1, na.rm = TRUE)
  incompletions <- sum(pbp_p$incomplete_pass == 1, na.rm = TRUE)
  interceptions <- sum(pbp_p$interception == 1, na.rm = TRUE)
  spikes        <- sum(pbp_p$qb_spike == 1, na.rm = TRUE)
  
  attempts <- completions + incompletions + interceptions + spikes
  
  cmp_pct <- if (attempts > 0) round((completions / attempts) * 100, 2) else 0
  
  return(data.frame(
    nombre = nombre[1],
    attempts = attempts,
    completions = completions,
    cmp_pct = cmp_pct
  ))
}

# pbp_2024 <- load_pbp(2024) |>
#   filter(season_type == "REG", pass == 1, !is.na(passer_player_id))
# 
# ids_filtrados <- pbp_2024 |>
#   count(passer_player_id, name = "attempts") |>
#   filter(attempts >= 100) |>
#   pull(passer_player_id)
# 
# resultados <- map_dfr(ids_filtrados, ~ pass_stats(2024, .x))
# resultados_ordenado <- resultados |> arrange(desc(attempts))

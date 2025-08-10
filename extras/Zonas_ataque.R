library(nflfastR)
library(dplyr)

Zonas_ataque <- function(year, teamabbr) {
  pbp <- load_pbp(year)
  
  #Separa tipo de jugada (Distancia y Zona)
  pbp_clean <- pbp |>
    filter(play_type %in% c("run", "pass"), !is.na(epa), posteam == teamabbr) |>
    mutate(
      categoria = case_when(
        # Carreras
        play_type == "run" & run_location == "middle" ~ "Carrera Centro",
        play_type == "run" & run_location == "left" ~ "Carrera Izquierda",
        play_type == "run" & run_location == "right" ~ "Carrera Derecha",
        
        # Screens
        play_type == "pass" & pass_location == "left" & air_yards <= 0 ~ "Screen Izquierda",
        play_type == "pass" & pass_location == "right" & air_yards <= 0 ~ "Screen Derecha",
        
        # Pase corto (menos de 5 yds)
        play_type == "pass" & pass_location == "left" & air_yards > 0 & air_yards <= 5 ~ "Pase Corto Izquierda",
        play_type == "pass" & pass_location == "middle" & air_yards > 0 & air_yards <= 5 ~ "Pase Corto Centro",
        play_type == "pass" & pass_location == "right" & air_yards > 0 & air_yards <= 5 ~ "Pase Corto Derecha",
        
        # Pase medio (de 5 a 15 yds)
        play_type == "pass" & pass_location == "left" & air_yards > 5 & air_yards <= 15 ~ "Pase Medio Izquierda",
        play_type == "pass" & pass_location == "middle" & air_yards > 5 & air_yards <= 15 ~ "Pase Medio Centro",
        play_type == "pass" & pass_location == "right" & air_yards > 5 & air_yards <= 15 ~ "Pase Medio Derecha",
        
        # Pase largo (Más de 15 yds)
        play_type == "pass" & pass_location == "left" & air_yards > 15 ~ "Pase Largo Izquierda",
        play_type == "pass" & pass_location == "middle" & air_yards > 15 ~ "Pase Largo Centro",
        play_type == "pass" & pass_location == "right" & air_yards > 15 ~ "Pase Largo Derecha",
        
        TRUE ~ NA_character_
      ),
      tipo_jugada = case_when( 
        play_type == "run" ~ "Total Carreras",
        play_type == "pass" ~ "Total Pases",
        TRUE ~ NA_character_
      )
    ) |>
    filter(!is.na(categoria))
  
  #Cuenta totales por tipo (Distancia y Tipo)
  totales <- pbp_clean |>
    group_by(posteam, categoria) |>
    summarise(Jugadas = n(), .groups = "drop")
  
  #Cuenta totales entre pase y carrera
  totales_tipo <- pbp_clean |>
    group_by(posteam, tipo_jugada) |>
    summarise(Jugadas = n(), .groups = "drop") |>
    rename(categoria = tipo_jugada)
  
  #Total de jugadas ofensivas
  total_ofensivo <- pbp_clean |>
    group_by(posteam) |>
    summarise(categoria = "Total Jugadas Ofensivas", Jugadas = n(), .groups = "drop")
  
  resumen <- bind_rows(totales, totales_tipo, total_ofensivo)
  
  total_jugadas <- resumen |>
    filter(categoria == "Total Jugadas Ofensivas") |>
    pull(Jugadas)
  
  #Porcentajes y orden de mayor a menor
  resumen <- resumen |>
    mutate(
      Porcentaje = round(100 * Jugadas / total_jugadas, 1)
    ) |>
    arrange(desc(Jugadas))
  
  return(resumen)
}


Zonas_ataque(2024, "GB")


source("C:/Users/HP15/Documents/NFL-Proyecto Fantasy/Terminadas/Pass_Td.R")
source("C:/Users/HP15/Documents/NFL-Proyecto Fantasy/Terminadas/Rush_Td.R")
source("C:/Users/HP15/Documents/NFL-Proyecto Fantasy/Terminadas/Rec_Td.R")


Total_td <- function(year, player_id) {
  total <- 0
  nombre <- NULL
  
  pass <- tryCatch({
    df <- Pass_td(year, player_id)
    total <- total + df$pass_touchdowns
    nombre <- df$nombre
  }, error = function(e) {
    total <- total + 0
  })
  
  rush <- tryCatch({
    df <- Rush_td(year, player_id)
    total <- total + df$rushing_touchdowns
    if (is.null(nombre)) nombre <- df$nombre
  }, error = function(e) {
    total <- total + 0
  })
  
  rec <- tryCatch({
    df <- Rec_td(year, player_id)
    total <- total + df$rec_touchdowns
    if (is.null(nombre)) nombre <- df$nombre
  }, error = function(e) {
    total <- total + 0
  })
  
  return(data.frame(
    nombre = nombre,
    total_touchdowns = total
  ))
}


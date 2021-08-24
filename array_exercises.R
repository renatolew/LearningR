temperatura <- c(22, 35, 44, 30)
temperatura2 <- c(22, 35, 44, 30, 28, 18, NA, 15, 35)


# Função que retorna posição dos valores pares.

position_even <- function(array) {
  even_number <- array %% 2 == 0
  position_even_number <- which(even_number)
  return(position_even_number)
}


position_even(temperatura)

# Função para substituir valores vazios pela média dos demais valores.

replace_empty <- function(array) {
  empty_position <- which(is.na(array))
  array[empty_position] <- mean(array, na.rm = TRUE)
  return(array)
}

replace_empty(temperatura2)

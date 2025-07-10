## STAT385 Final Project - Bingo ##
# Jackson Fleege #

#-----------------------------------------------------------#

# creates a vector of 100 numbers that will be used to populates the players bingo card
number_pool <- c(1:100)

# samples the number pool with 25 numbers, making the players card
rough_player_card <- sample(number_pool, 25)

# makes the card fancier for aesthetic 
card_banner <- list(c("B", "I", "N", "G", "O"), c("B", "I", "N", "G", "O"))

# builds the players card with the sample from the number pool
temp_player_card <- matrix(data = rough_player_card, nrow = 5, ncol = 5, byrow = FALSE, dimnames = card_banner)
temp_player_card[3,3] <- "FREE"

# transforms the data into a proper data frame
player_card <- as.data.frame(temp_player_card)

#-----------------------------------------------------------#

# draws numbers that will be used to cross check with the players card
number_called <- number_pool

# creates a function that removes a number from the card if matched
called_number <- function() {
  sample(number_called, 1, replace = FALSE)
  number_called <<- number_called[number_called != called_number]
  return(called_number)
}

# checks the number against the players card and if matched marks with an X
mark_card <- function(card, number) {
  card[card == number] <- "X"
  return(card)
}

# checks for a bingo
check_for_bingo <- function(card) {
  is_bingo <- function(line) all(line == "X" | line == "FREE")
  
  # checks for a bingo in rows or columns
  rows_bingo <- any(apply(card, 1, is_bingo))
  cols_bingo <- any(apply(card, 2, is_bingo))
  
  # checks diagonals for bingo
  diag1 <- diag(as.matrix(card))
  diag2 <- diag(as.matrix(card)[, ncol(card):1])
  diag_bingo <- is_bingo(diag1) || is_bingo(diag2)
  
  return(rows_bingo || cols_bingo || diag_bingo)
}

#-----------------------------------------------------------#
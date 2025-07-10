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



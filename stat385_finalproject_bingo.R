## STAT385 Final Project - Bingo ##
# Jackson Fleege #

#-----------------------------------------------------------#

# creates a vector of 100 numbers that will be used to populates the players bingo card
number_pool <- c(1:100)

# samples the number pool with 25 numbers, making the players card
rough_player_card <- sample(number_pool, 25)

# makes the card fancier
card_banner <- list(c("B", "I", "N", "G", "O"), c("B", "I", "N", "G", "O"))

# builds the players card with the sample from the number pool
temp_player_card <- matrix(data = rough_player_card, nrow = 5, ncol = 5, byrow = FALSE, dimnames = card_banner)
temp_player_card[3,3] <- "FREE"

# transforms the data into a proper data frame
player_card <- as.data.frame(temp_player_card)

#-----------------------------------------------------------#

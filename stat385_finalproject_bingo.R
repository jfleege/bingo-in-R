## STAT385 Final Project - Bingo ##
# Jackson Fleege #

#-----------------------------------------------------------#

# asks how many players will be playing
get_number_of_players <- function() {
  num <- as.numeric(readline("How many people will be playing today? (1 or 2): "))
  if (is.na(num) || !(num %in% c(1, 2))) {
    cat("Invalid input. Starting a 1 player game.\n")
    num <- 1
  }
  return(num)
}

# helper function to create a bingo board
create_board <- function() {
  number_pool <- c(1:50)
  rough_player_card <- sample(number_pool, 25)
  card_banner <- list(c("B", "I", "N", "G", "O"), c("B", "I", "N", "G", "O"))
  temp_player_card <- matrix(data = rough_player_card, nrow = 5, ncol = 5, byrow = FALSE, dimnames = card_banner)
  temp_player_card[3,3] <- "FREE"
  return(as.data.frame(temp_player_card))
}

# marks a card if number matches
mark_card <- function(card, number) {
  card[card == number] <- "X"
  return(card)
}

# checks if a card has a bingo
check_for_bingo <- function(card) {
  is_bingo <- function(line) all(line == "X" | line == "FREE")
  rows_bingo <- any(apply(card, 1, is_bingo))
  cols_bingo <- any(apply(card, 2, is_bingo))
  diag1 <- diag(as.matrix(card))
  diag2 <- diag(as.matrix(card)[, ncol(card):1])
  diag_bingo <- is_bingo(diag1) || is_bingo(diag2)
  return(rows_bingo || cols_bingo || diag_bingo)
}

#-----------------------------------------------------------#

# loops the above functions to play a game of bingo
play_bingo <- function() {
  repeat {
    # grabs the number of players for the game
    number_of_players <- get_number_of_players()
    
    # makes the player's boards
    player_cards <- list()
    for (i in 1:number_of_players) {
      player_cards[[i]] <- create_board()
    }
    
    # manufactures a number pool that that players will play with
    number_pool <- 1:50
    
    # draws numbers that will be used to cross check with the players card
    called_number <- function() {
      drawn <- sample(number_pool, 1)  # draw one random number
      number_pool <<- setdiff(number_pool, drawn)  # remove drawn number from pool
      return(drawn)
    }
    
    # gameplay loops; repeats the functions until there's an outcome
    while (TRUE) {
      if (length(number_pool) == 0) {
        cat("No more numbers left to call. Thank you for playing today. Game Over! \n")
        break
      }
      
      # draws the next number
      next_called_number <- called_number()
      cat("\n... and the next number is ....", next_called_number, "\n")
      
      # fills X's in spots necessary
      for (i in 1:number_of_players) {
        player_cards[[i]] <- mark_card(player_cards[[i]], next_called_number)
      }
      
      # prints the live boards
      cat("\nUpdated Board(s):\n")
      if (number_of_players == 2) {
        # combines the player boards side by side with a spacer
        combined_boards <- cbind(
          Player1 = as.matrix(player_cards[[1]]),
          " " = rep("", 5),  # spacer column for readability
          Player2 = as.matrix(player_cards[[2]])
        )
        print(as.data.frame(combined_boards), row.names = FALSE)
      } else {
        # if there's only one player, print their board regularly
        print(player_cards[[1]])
      }
      
      # checks win condition
      winner_found <- FALSE
      for (i in 1:number_of_players) {
        if (check_for_bingo(player_cards[[i]])) {
          cat("\n WE HAVE A BINGO!!! Player", i, "wins! \n")
          winner_found <- TRUE
          break
        }
      }
      
      # if there's a winner, breaks from running so that the game ends
      if (winner_found) break
      
      # draws next number
      readline(prompt = "Please press ENTER to call the next number.")
    }
    
    # ask player(s) if they would like to play again
    restart <- readline(prompt = "\nWould you like to play again? (Y/N): ")
    if (tolower(restart) != "y") {
      cat("Thank you for playing! Have a great day! \n")
      break
    } else {
      cat("\nBeginning new game shortly. Get your cards ready!! \n")
    }
  }
}

#-----------------------------------------------------------#

# start game
play_bingo()

## STAT385 Final Project - Bingo ##
# Jackson Fleege #

# presentation link can be accessed here:
# https://drive.google.com/file/d/1U9aoijZq-dZPMHMMHJWvUOByBxp_O-i2/view?usp=drive_link

#-----------------------------------------------------------#

# asks how many players will be playing
get_number_of_players <- function() {
  num <- as.numeric(readline("How many people will be playing today? (1, 2, 3, 4): "))
  if (is.na(num) || !(num %in% c(1, 2, 3, 4))) {
    cat("Invalid input. Starting a 1 player game.\n")
    num <- 1
  }
  return(num)
}

# helper function to create a bingo board
create_board <- function() {
  number_pool <- c(1:50)
  rough_player_card <- sample(number_pool, 25)
  
  temp_player_card <- matrix(
    data = rough_player_card, 
    nrow = 5, ncol = 5, 
    byrow = FALSE
  )
  
  # set column names for B I N G O
  colnames(temp_player_card) <- c("B", "I", "N", "G", "O")
  
  # add FREE space in the middle
  temp_player_card[3,3] <- "FREE"
  
  # convert to data frame without row names
  temp_player_card <- as.data.frame(temp_player_card, row.names = NULL)
  
  return(temp_player_card)
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

# tracks how many moves a player has until they win the game
moves_till_bingo <- function(card) {
  counts <- c()
  
  # checks the number of unmarked spaces in the rows
  for (r in 1:nrow(card)) {
    line <- card[r, ]
    counts <- c(counts, sum(line != "X" & line != "FREE"))
  }
  
  # checks the number of unmarked spaces in the columns
  for (c in 1:ncol(card)) {
    line <- card[, c]
    counts <- c(counts, sum(line != "X" & line != "FREE"))
  }
  
  # checks the number of unmarked spaces in the diagonals
  diag1 <- diag(as.matrix(card))
  diag2 <- diag(as.matrix(card)[, ncol(card):1])
  counts <- c(counts,
              sum(diag1 != "X" & diag1 != "FREE"),
              sum(diag2 != "X" & diag2 != "FREE"))
  
  return(min(counts))
}

# initializes the high score so that it can be eventually tracked
high_score <- NULL

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
    
    # initializes the move count so that it starts at 0 every game
    move_count <- 0
    
    # gameplay loops; repeats the functions until there's an outcome
    while (TRUE) {
      if (length(number_pool) == 0) {
        cat("No more numbers left to call. Thank you for playing today. Game Over! \n")
        break
      }
      
      # draws the next number
      next_called_number <- called_number()
      cat("\n... and the next number is ....", next_called_number, "\n")
      
      # keeps track of the number of moves performed each game
      move_count <- move_count + 1
      cat("Move number: ", move_count, "\n")
      
      # fills X's in spots necessary
      for (i in 1:number_of_players) {
        player_cards[[i]] <- mark_card(player_cards[[i]], next_called_number)
      }
      
      # prints the live boards
      cat("\n================== PLAYER BOARDS ==================\n\n")
      
      for (i in 1:number_of_players) {
        cat("========== PLAYER", i, "==========\n")
        print(player_cards[[i]], row.names = FALSE)
        cat("\n")  # spacer between boards
      }
      
      # tracks how many moves a player has until they win the game
      cat("\nMoves till Bingo:\n")
      for (i in 1:number_of_players) {
        m_t_b <- moves_till_bingo(player_cards[[i]])
        cat("Player", i, "needs", m_t_b, "more move(s) to win!\n")
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
      
      # updates the high score once a winner is found
      if (winner_found) {
        if (is.null(high_score) || move_count < high_score) {
          high_score <<- move_count
          cat("NEW HIGH SCORE! Winner in", high_score, "moves! \n")
        } else {
          cat("Score to beat:", high_score, "moves\n")
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

# run this line to start the game
play_bingo()



########################################### Game ###############################################

# Define the crap_game function
crap_game <- function(n = 100) {
  # Initialize a list to store summaries for each game
  game_summaries <- list()

  for (i in 1:n) {
    id <- 1   # Initialize the roll counter (number of rolls in the game)
    outcome <- NA   # The final outcome of the game (win or lose)
    rolls <- 0      # Number of rolls in the current game
    point <- NA     # Initial point is NA by default

    # Roll the first dice (initial roll)
    roll1 <- roll_dice()
    rolls <- rolls + 1  # Increment the roll counter

    # Check if it's a win or lose on the first roll
    if (roll1 %in% c(2, 3, 12)) {
      outcome <- "lose"  # Immediate loss
    } else if (roll1 %in% c(7, 11)) {
      outcome <- "win"   # Immediate win
    } else {
      # If no immediate win/lose, set the point to the first roll
      point <- roll1

      # Otherwise, keep rolling until the point is matched or a 7 is rolled
      while (TRUE) {
        roll2 <- roll_dice()  # Roll the dice again
        rolls <- rolls + 1  # Increment the roll counter

        if (roll2 == 7) {
          outcome <- "lose"  # 7 means lose
          break
        } else if (roll2 == point) {
          outcome <- "win"   # Point matched, win
          break
        }
      }
    }

    # Create a summary of the game
    game_summary <- data.frame(
      GameID = i,       # Game identifier
      Point = point,  # The initial roll (point), NA if win/lose on first roll
      TotalRolls = rolls,   # Total number of rolls (1 if immediate win/lose)
      Outcome = outcome     # Final outcome (win or lose)
    )

    # Store the summary for this game in the list
    game_summaries[[i]] <- game_summary
  }

  # Combine all game summaries into a single data frame
  final_summary <- do.call(rbind, game_summaries)

  # Return the final summarized results for all games
  return(final_summary)
}

# Example: Run the function for 10 games
x= crap_game(n = 10)
x

###### This function summarizes the data frame and calculate the winning percentage. ##########

summarize_probability = function(df){
  wins = numeric()
  for (i in 1:nrow(df)){
    win = df[4][[1]][i]
    if (win == "win"){
      wins = append(wins, 1)
    }
    else{
      wins = append(wins, 0)
    }
  }
  y = (sum(wins)/(nrow(df)))*100
  x = paste("Your winning percentage after playing ",nrow(df), " games is: ", y, "%" )
  return(x)
}

k = summarize_probability(x)
k



############### The variation of probability of winning for each game.#########################

## simulate 10 trials of 1000 games in each trail and calculate the probability of them.
## the function produces the all the probabilities for the specified trails and return a histogram
## of the frequency distribution.

variance_prob = function(n, iter = 10){
  w = character()
  for (i in 1:iter){
    xx = crap_game(n)
    k = summarize_probability(xx)
    prob = str_extract(str_squish(k), "\\d+(\\.\\d+)?\\s?%")
    w = append(w, prob)
  }
  prob_numeric = as.numeric(gsub("%", "", w))
  print(prob_numeric)
  var_p = paste("variance is: ", var(prob_numeric)," and the Standard Dev : ", sd(prob_numeric))
  print(var_p)

  return(hist(prob_numeric, col = "blue", main = "Histogram of percentage of winning",
             xlab = "percentage of winning"))
}

var_p = variance_prob(n = 10, iter = 100)



############## Analysis on the percentage of winning the game in the 1st attempt ##############


probability_first_attempt = function(df){

  wins_df = df%>%
    filter(TotalRolls == 1) %>%
    mutate(win_status = ifelse(Outcome == "win", 1, 0))

  y = (sum(wins_df$win_status/(nrow(df))))*100
  x = paste("Your winning percentage after playing 1st time",nrow(df), " games is: ", y, "%" )
  return(x)
}

## returns the probability of winning at the 1st attempt.

probability_first_attempt(x)


############# probability of not winning at the 1st attempt, but eventually wins ##############

x2 = crap_game(1000)


probability_notFirst_attempt = function(df){

  wins_df = df%>%
    filter(TotalRolls == 1) %>%
    mutate(win_status = ifelse(Outcome == "win", 1, 0))

  y = ((nrow(wins_df))/nrow(df))*100
  x = paste("Your winning percentage after not winning the 1st time",nrow(df), " games is: ", y, "%" )
  return(x)
}

probability_notFirst_attempt(x2)











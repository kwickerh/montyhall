


#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors 
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two 
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous 
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies. 
#'
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
} 



#' @title
#'    The contestant selects one of the three doors.
#' @description
#'    'select_door()' will return a number between 1 and 3. This number 
#'    represents the position of a door with either a goat or car 
#'    behind it. It is the contestant's first choice in the game.
#' @details
#'    This is the first step of the actual game. The contestant will
#'    have the opportunity to switch their door number or keep the original
#'    one after the host makes their choice.
#' @param ... no arguments are used by the function
#' @return 
#'    The function returns a numeric vector representing the door
#'    selected by the contestant.
#' @examples
#'     select_door()
#' @export
select_door <- function( )
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#'    Host opens a goat door.
#' @description
#'    'open_goat_door()' will return a numeric value between 1 and 3. It cannot
#'    be the door selected by the contestant, nor can it be the door with
#'    the car behind it. If the contestant picked the car, the function
#'    will return either of the two remaining doors; however, if the contestant
#'    picked a goat, it must return the final door with a goat behind it.
#' @details
#'    The host must select a door with a goat behind it, giving the contestant
#'    an opportunity to either switch doors or keep their original choice.
#' @param
#'    The parameters of this function are game (which identifies the position
#'    of the cars/goats for the specific game) and a.pick (the contestant's
#'    choice)
#' @return 
#'    The function returns a numeric value between 1 and 3. It will not be 
#'    the same as the contestant's choice, nor will it be the number that
#'    represents the car door. It must be a goat.
#' @examples
#'     this.game <- c("goat","car","goat")
#'     my.initial.pick <- 1
#'     open_goat_door( this.game, my.initial.pick ) # should be 3
#' @export
open_goat_door <- function( game, a.pick )
{
   doors <- c(1,2,3)
   # if contestant selected car,
   # randomly select one of two goats 
   if( game[ a.pick ] == "car" )
   { 
     goat.doors <- doors[ game != "car" ] 
     opened.door <- sample( goat.doors, size=1 )
   }
   if( game[ a.pick ] == "goat" )
   { 
     opened.door <- doors[ game != "car" & doors != a.pick ] 
   }
   return( opened.door ) # number between 1 and 3
}



#' @title
#'    The contestant's final choice.
#' @description
#'    'change_door()' will return the final selection of the contestant based 
#'    off whether the contestant chooses to keep their original door (stay)
#'    or change it (switch).
#' @details
#'    The contestant now makes their final choice to either stay with their
#'    original door or switch to the lat one remaining.
#' @param 
#'    The argument for this function is stay=TRUE or stay=FALSE, and it also
#'    contains opened.door (the door number opened by the host) and a.pick (the
#'    door originally selected by the contestant) to return the final pick.
#' @return
#'    This function returns a numeric value between 1 and 3. If stay=TRUE,
#'    it will return the value of the door the contestant chose. If stay=FALSE,
#'    it will return the remaining door.
#' @examples
#'     change_door( stay=T, opened.door=1, a.pick=3 )
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
   doors <- c(1,2,3) 
   
   if( stay )
   {
     final.pick <- a.pick
   }
   if( ! stay )
   {
     final.pick <- doors[ doors != opened.door & doors != a.pick ] 
   }
  
   return( final.pick )  # number between 1 and 3
}



#' @title
#'    Determine outcome of the game.
#' @description
#'    'determine_winner' let's us know if the contestant won or loss. To win,
#'    the final pick must be the car; otherwise the contestant loss.
#' @details
#'    The game concludes by revealing what was behind the door of the final pick.
#' @param 
#'    This function relies on the arguments final.pick (the door number the
#'    contestant ultimately chose) and game, which contains what is behind
#'    each door.
#' @return 
#'    This function will return a character value, either WIN or LOSE.
#' @examples
#'     this.game <- c("goat","car","goat")
#'     determine_winner( final.pick=1, game=this.game )
#' @export
determine_winner <- function( final.pick, game )
{
   if( game[ final.pick ] == "car" )
   {
      return( "WIN" )
   }
   if( game[ final.pick ] == "goat" )
   {
      return( "LOSE" )
   }
}





#' @title
#'    Game simulation
#' @description
#'    The play_game() function runs a simulation of a game. It goes 
#'    through each step in order and returns the results.
#' @details
#'    The simulation runs through a Monty Hall scenario, returning
#'    the result of the game in a scenario where the contestant
#'    stayed and a scenario where the contestant switched.
#' @param ...no arguments used for this function
#' @return 
#'    The return is a table showing 'stay' and 'switch' in the first
#'    column, and the results in the second column.
#' @examples
#'     play_game()
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )
  
  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}




#' @title
#'    Simulation loop
#' @description
#'    The play_n_games function will run a simulation of the game a number 
#'    of times set by the user ('n). It will return a table with the number
#'    of wins and losses for both the 'stay' and 'switch' strategies.
#' @details
#'    In order to determine the probability of winning with each strategy,
#'    we can run the Monty Hall scenario 'n' number of times.
#' @param 
#'    The argument for this function is n=100.
#' @return
#'    The function returns a table showing the number of wins and losses 
#'    when someone chooses to stay and when someone chooses to switch in 'n'
#'    scenarios. 
#' @examples
#'     play_n_games(n=100)
#' @export
play_n_games <- function( n=100 )
{
  
  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome 
    loop.count <- loop.count + 1
  }
  
  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>% 
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 ) %>% 
  print()
  
  return( results.df )

}

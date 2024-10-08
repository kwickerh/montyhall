% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/monty-hall-pkg-Wickerham.R
\name{play_n_games}
\alias{play_n_games}
\title{Simulation loop}
\usage{
play_n_games(n = 100)
}
\arguments{
\item{The}{argument for this function is n=100.}
}
\value{
The function returns a table showing the number of wins and losses
when someone chooses to stay and when someone chooses to switch in 'n'
scenarios.
}
\description{
The play_n_games function will run a simulation of the game a number
of times set by the user ('n). It will return a table with the number
of wins and losses for both the 'stay' and 'switch' strategies.
}
\details{
In order to determine the probability of winning with each strategy,
we can run the Monty Hall scenario 'n' number of times.
}
\examples{
    play_n_games(n=100)
}

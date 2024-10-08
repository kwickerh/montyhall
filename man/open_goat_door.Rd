% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/monty-hall-pkg-Wickerham.R
\name{open_goat_door}
\alias{open_goat_door}
\title{Host opens a goat door.}
\usage{
open_goat_door(game, a.pick)
}
\arguments{
\item{The}{parameters of this function are game (which identifies the position
of the cars/goats for the specific game) and a.pick (the contestant's
choice)}
}
\value{
The function returns a numeric value between 1 and 3. It will not be
the same as the contestant's choice, nor will it be the number that
represents the car door. It must be a goat.
}
\description{
'open_goat_door()' will return a numeric value between 1 and 3. It cannot
be the door selected by the contestant, nor can it be the door with
the car behind it. If the contestant picked the car, the function
will return either of the two remaining doors; however, if the contestant
picked a goat, it must return the final door with a goat behind it.
}
\details{
The host must select a door with a goat behind it, giving the contestant
an opportunity to either switch doors or keep their original choice.
}
\examples{
    this.game <- c("goat","car","goat")
    my.initial.pick <- 1
    open_goat_door( this.game, my.initial.pick ) # should be 3
}

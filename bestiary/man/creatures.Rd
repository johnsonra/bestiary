\name{creatures}

\alias{creatures}

\docType{data}

\title{
DnD 5e Creatures
}

\description{
DnD 5e creatures for planning encounters.
}

\usage{data("creatures")}

\format{
  A data frame with 384 observations on the following 22 variables.
  \describe{
    \item{\code{name}}{Creature species name.}
    \item{\code{attitude}}{Takes one of the following values: any, chaotic, lawful, neutral, non-lawful.}
    \item{\code{morality}}{Takes one of the following values: any, evil, good, neutral, non-good.}
    \item{\code{size}}{Takes one of the following values: gargantuan, huge, large, medium, small, tiny.}
    \item{\code{type}}{Takes one of the following values: aberration, beast, celestial, construct, dragon, elemental, fey, fiend, giant, humanoid, monstrosity, ooze, plant, swarm, undead.}
    \item{\code{tags}}{A list of tags (e.g. c('large', aberration', 'cr10'))}
    \item{\code{armor.class}}{Armor class}
    \item{\code{HPsummary}}{Rules for determining it points.}
    \item{\code{HPmean}}{Mean hit points for creatures of this species.}
    \item{\code{HPnDice}}{Number of hit point dice to roll.}
    \item{\code{HPdie}}{Number of sides on the hit point dice.}
    \item{\code{HPmod}}{Hit point modifier.}
    \item{\code{speed}}{Speed in feet per turn.}
    \item{\code{STRmod}}{Strength modifier.}
    \item{\code{DEXmod}}{Dexterity modifier.}
    \item{\code{CONmod}}{Constitution modifier.}
    \item{\code{INTmod}}{Intellegence modifier.}
    \item{\code{WISmod}}{Wisdom modifier.}
    \item{\code{CHAmod}}{Charisma modifier.}
    \item{\code{challenge}}{Challenge rating (in 1:20).}
    \item{\code{xp}}{Experience points.}
    \item{\code{description}}{Additional details specific to this species.}
  }
}

\source{
This creature list was forked from github.com/chisaipete/bestiary and pulled together into this R dataset.
}

\examples{
data(creatures)
}

\keyword{datasets}

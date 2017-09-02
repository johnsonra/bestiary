\name{Interactions}

\alias{attack}
\alias{damage}
\alias{savingThrow}

\title{
Interactions with creature objects
}

\description{
These functions allow interactions with creature objects without giving away too many details about the creature.
}

\usage{
attack(x, ar, n = 1)
damage(x, dam, n)
savingThrow(x, dc, dcMod, n = 1)
}

\arguments{
  \item{x}{A creature object.}
  \item{ar}{Attack roll.}
  \item{n}{Index identifying which creature to attack.}
  \item{dam}{Damage dealt to the creature.}
  \item{dc}{Difficulty class.}
  \item{dcMod}{Difficulty class modifier for the creature (see Details).}
}

\details{
dcMod:
}

\value{
What is returned...
}

\author{
Randy Johnson
}

\examples{
ogres <- rencounter(2, 'Ogre')

# Successful attack
# Now we have more information about these creatures' armor class
(ogres <- attack(ogres, 12))

# Assign damage
(ogres <- damage(ogres, 23, 1))

# Ogre 2 makes a dexterity saving throw
savingThrow(ogres, 13, 'DEXmod', 2)
}

\keyword{utilities}

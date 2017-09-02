\name{Encounters}

\alias{rencounter}
\alias{rcreatures}

\title{
Functions to set up encounters
}

\description{
These functions (more to come) can be used to set up planned or random encounters.
}

\usage{
rcreatures(n, cname)
rencounter(n, ...)
}

\arguments{
  \item{n}{Number of creatures to initialize.}
  \item{cname}{Name of species.}
  \item{...}{Values to filter on. See details for more information.}
}

\details{
Need to add details on the ... argument of rencounter(). See code for details until I get around to this.

See print method for what is shown to the user when printing a creature object. Most details are kept hidden when interacting with a creature using the functions in this package.
}

\value{
A creature object list of length n. Each creature has the following values:
\item{name}{Name of creature (e.g. Ogre 1)}
\item{HP}{Creature's hit points}
\item{damage}{Damage taken (initializes to 0)}
\item{AC}{Creature's armor class}
\item{minACthresh}{Minimum armor class based on attempted attacks (initializes to 0)}
\item{maxACthresh}{Maximum armor class based on attempted attacks (initializes to 0)}
\item{speed}{Speed}
\item{initiative}{Initiative}
\item{xp}{Experience points}
\item{STRmod}{Strength modifier}
\item{DEXmod}{Dexterity modifier}
\item{CONmod}{Consitution modifier}
\item{INTmod}{Inetelligence modifier}
\item{WISmod}{Wisdom modifier}
\item{CHAmod}{Charisma modifier}
\item{details}{Additional details}
\item{class}{'creature'}
}

\author{
Randy Johnson
}

\examples{
ogres <- rcreatures(2, 'Ogre')

print(ogres)
}

\keyword{datagen}

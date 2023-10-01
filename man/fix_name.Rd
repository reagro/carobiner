\name{fix_name}

\alias{fix_name}

\title{
Fix character variables
}

\description{
Fix character variables by removing extraneous whitespace and possibly changing capitalization.

When using title case, words starting after an "-" are capitalized. A letter preceding an apostrophe (') is not capitalized. The words "and", "the" and "of" are not capitalized. Single letters alternated with dots (abbreviations) are capitalized.

Alternatively, you can get title case with \code{tools::toTitleCase(fix_name(x, "lower"))}
}


\usage{
fix_name(x, case="", skip="", lowothers=TRUE)
}
 
\arguments{
  \item{x}{character}
  \item{case}{character. If this is not \code{""}, the capitalization of the words is changed. You can use "first", "lower", or "title"}
  \item{skip}{character. Additional words to skip when case="title" (see details)}
  \item{lowothers}{logical. Only relevant when case is "first" or "title". Should all other characters be made lowercase?}
}

\value{
character
}


\examples{
d <- c("aruba", "BOM BINI", "saint-vincent and the grenadines",
       "the u.s. of america", "côte d'ivoire")
fix_name(d, "title")
fix_name(d, "title", skip="bini")
}



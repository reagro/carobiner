\name{capitalize_words}

\alias{capitalize_words}

\title{
Capitalize words
}

\description{
Change words spelling to Title Case. Words starting after an "-" are capitalized. A letter preceding an apostrophe (') is not capitalized. The words "and", "the" and "of" are not capitalized. Single letters alternated with dots (abbreviations) are capitalized.
}


\usage{
capitalize_words(x, skip="")
}
 
\arguments{
  \item{x}{character}
  \item{skip}{additional words to skip. The case of these words is ignored}
}

\value{
character
}


\examples{
d <- c("aruba", "BOM BINI", "saint-vincent and the grenadines", "the u.s. of america", "côte d'ivoire")
capitalize_words(d)
capitalize_words(d, skip="bini")
}



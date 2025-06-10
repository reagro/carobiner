\name{eng_months_to_nr}

\alias{eng_months_to_nr}

\title{
Change months in English to a month number.
}

\description{
Change months in English to a month number. E.g. March to 3.
You should use this function instead of \code{\%B} or \code{\%b} because these only work well on a computer set to the English language.
}


\usage{
eng_months_to_nr(x)
}
 
\arguments{
  \item{x}{character. (abbreviated) month names}
}

\value{
character
}


\examples{
eng_months_to_nr(c("January", "Feb", "Apr", "May"))
}



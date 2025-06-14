\name{carob_vocabulary}

\alias{carob_vocabulary}

\title{
Generate a carob_vocabulary Carob script
}

\description{
Adjust the Carob vocabulary. Normally by adding additional terms from another vocabulary
}


\usage{
carob_vocabulary(x=NULL, save=FALSE, add=TRUE, restore=FALSE)
}
 
\arguments{
  \item{x}{character. The vocabulary (ies) to use}
  \item{save}{logical. Save the values of \code{x} so that it also gets used in future sessions?}
  \item{add}{logical. Add this to the default vocabulary?}
  \item{reset}{logical. Reset to the default settings? Setting this to \code{TRUE} overrides the other settings}
}

\value{
character, if \code{is.null(x)}
}

\examples{
	f <- system.file("voc", package="vocal")
	carob_vocabulary(c(f, "github:carob-data/terminag"))
	get_vocabulary()
}
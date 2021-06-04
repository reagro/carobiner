\name{get_function}

\alias{get_function}

\title{
Get a function
}

\description{
Get a data processing function that is shared bewteen scripts. The function must exist in a file called \code{_functions.R}
}


\usage{
get_function(name, path, group="")
}
 
\arguments{
  \item{name}{character. The name of the function}
  \item{path}{character. The path to the Carob repo}
  \item{group}{character. Optional, a group name}
}

\value{
function
}


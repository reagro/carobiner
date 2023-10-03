\name{get_license}

\alias{get_license}

\title{
Get the license of a dataset
}

\description{
Get the license information from the object created in \code{\link{get_metadata}}. The fields "license" and "termsOfUse" are concatenated, except if license is "NONE". Create Common license descriptions are abbreviated to their standard form. 
}


\usage{
get_license(x)
}
 
\arguments{
  \item{x}{list. Created with \code{\link{get_metadata}}}
}

\value{
character
}

\examples{
get the license of dataset labeled [dset] with a metadata object labeled [js]: 

dset$license <-carobiner::get_license [js]  

Where: 
 js<-carobiner::get_metadata(dataset_id,path,group,major=1,minor = 0)  
}

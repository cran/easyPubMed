\name{getPubmedIds}
\alias{getPubmedIds}
\title{Simple PubMed Search}
\description{Query PubMed in a simple way via the PubMed API eSearch function. Calling this function results in posting the result on the PubMed History Server and allows later access to the desired data via the fetchPubmedData() function.}
\usage{
getPubmedIds(pbmedQuery)
}
\arguments{
  \item{pbmedQuery}{is a character vector and is the String that will be used for querying PubMed.}
}
\details{This function will use the String provided as argument for querying PubMed via the eSearch function of the PubMed API. The Query Term can include one or multiple words, as well as the standard PubMed operators (AND, OR, NOT) and tags (i.e., [AU], [PDAT], [Affiliation], and so on). ESearch will post the UIDs resulting from the search operation onto the History server so that they can be used directly in a subsequent fetchPubmedData() call.}
\value{The function returns a list. The list includes the number of records found on PubMed and the first 20 PubMed IDs (UID) retrieved by the query. The list also includes QueryKey and WebEnv that are required for a subsequent fetchPubmedData() call.}
\references{A more exhaustive description of the package and of this function is available at <http://www.biotechworld.it/bioinf/2016/01/05/querying-pubmed-via-the-easypubmed-package-in-r/> }
\author{Damiano Fantini}
\examples{
##  Search for scientific articles written by Damiano Fantini
##  and print the number of Records to screen.
##  Also print the retrieved UIDs to screen.
##
damiOnPubmed <- getPubmedIds("Damiano Fantini[AU]")
print(damiOnPubmed$Count) # number of Scientific Papers written by Damiano Fantini
print(unlist(damiOnPubmed$IdList))
}
\keyword{ ~PubMed }

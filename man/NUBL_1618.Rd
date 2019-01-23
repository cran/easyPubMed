\name{NUBL_1618}
\alias{NUBL_1618}
\docType{data}
\title{
PubMed Records about Bladder Research from Northwestern University
}
\description{
This dataset includes a collection of 87 PubMed Records of scientific publications about Bladder biology and pathology, published by clinical and research groups from Northwestern University (Chicago, IL), between 2016 and 2018.
}
\usage{data("NUBL_1618")}
\format{
  The dataset is formatted as a list including 2 elements:
 $ records : character vector, including 84 elements. Each element is a PubMed Record downloaded using easyPubMed
 $ query_string: string (character vector of length 1) corresponding to the query used to retrieve the 'NUBL_1618' records
}
\details{
The dataset was obtained using the code as shown at the following URL: 
\url{https://www.data-pulse.com/projects/Rlibs/vignettes/NUBL_1618.html}
}
\references{
\url{https://www.data-pulse.com/dev_site/easypubmed/}
}
\examples{
## Display some contents
data("NUBL_1618")
# display Query String used for collecting the data
print(NUBL_1618$query_string)
# show one PubMed record element from the IL vector
NU_records <- NUBL_1618$records
cat(NU_records[[1]])
# cast PM recort to data.frame
BL_df <- article_to_df(NU_records[6], max_chars = 0)
print(BL_df)
}

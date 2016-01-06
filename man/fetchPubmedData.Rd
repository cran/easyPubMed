\name{fetchPubmedData}
\alias{fetchPubmedData}
\title{Retrieve PubMed Data In XML Format}
\description{Retrieve data from Pubmed following a PubMed search performed via the getPubmedIds() function. Data are downloaded in the XML format and are retrieved in batches of up to 5000 entries.}
\usage{
fetchPubmedData(pubmedIdList, retstart = 0, retmax = 500)
}
\arguments{
  \item{pubmedIdList}{is a list and is the result of a getPubmedIds() call.}
  \item{retstart}{is an integer (>=0) and corresponds to the index of the first UID in the retrieved PubMed Search Result set to be included in the XML output (default=0, corresponding to the first record of the entire set).}
  \item{retmax}{is an integer (>=1) and corresponds to the maximum number of UIDs from the retrieved set to be downloaded.}
}
\details{This function will take the result of a getPubmedIds() call as argument and will download the corresponding data from Entrez via the PubMed API efetch function. The first entry to be retrieved may be adjusted via the retastart parameter (this allows the user to download large batches of PubMed data). The maximum number of entries to be retrieved can also be set adjusting the retmax parameter (0 < retmax < 5000). Retrieved data in the XML format will be downloaded on the fly (no files are saved locally as a result of a fetchPubmedData() call).}
\value{This function returns a XMLInternalDocument-class object. The function output contains all data retrieved from Pubmed in XML format and can be accessed using a XML parser (for example, the XML package). Alternatively, specific records may be extracted using regular expressions.}
\references{A more exhaustive description of the package and of this function is available at <http://www.biotechworld.it/bioinf/2016/01/05/querying-pubmed-via-the-easypubmed-package-in-r/>}
\author{Damiano Fantini}
\examples{
##  Search for scientific articles written by Damiano Fantini
##  and print their title screen.
##
damiOnPubmed <- getPubmedIds("Damiano Fantini[AU]")
damiPapers <- fetchPubmedData(damiOnPubmed)
titles<- unlist(xpathApply(damiPapers, "//ArticleTitle", saveXML))
tPos <- regexpr("<ArticleTitle>.*<\\\\/ArticleTitle>", titles)
titles <- substr(titles, tPos + 14 , tPos + attributes(tPos)$match.length -16)
print(titles)
##
##
##  In the following example, fetchPubmedData() is used in combination with
##  custom retstart and retmax arguments. This shows how to download data
##  from PubMed in batches of the desired size. This approach should be used
##  when downloading a large number of records. The output should be the
##  same as in the first example
##
myQuery <- getPubmedIds("Damiano Fantini[AU]")
myTitles <- c()
pubsNum <- myQuery$Count
myRetstart <- 0
myRetmax <- 4
while (myRetstart < pubsNum){
  tmpPapers <- fetchPubmedData(myQuery, retstart = myRetstart, retmax = myRetmax)  
  tmpTitles <- unlist(xpathApply(tmpPapers, "//ArticleTitle", saveXML))
  tPos <- regexpr("<ArticleTitle>.*<\\\\/ArticleTitle>", tmpTitles)
  tmpTitles <- substr(tmpTitles, tPos + 14 , tPos + attributes(tPos)$match.length -16)
  myTitles <- append(myTitles, tmpTitles)
  myRetstart <- myRetstart + myRetmax 
}
print(myTitles)
}
\keyword{ ~PubMed }

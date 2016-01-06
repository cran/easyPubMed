\name{easyPubMed-package}
\alias{easyPubMed-package}
\alias{easyPubMed}
\docType{package}
\title{Searching and Retrieving Scientific Publication Data from PubMed}
\description{It is a tool that makes querying and downloading data from PubMed easy. Data are retrieved from PubMed in XML format. It offers a simple-to-use interface for the PubMed API and is suitable for retrieving a large number of records. XML data are retrieved on the fly, therefore no files are saved on the local machine by default. }
\details{This package access the PubMed API and searches or downloads data. EasyPubMed makes the process of searching and retrieving Scientific Publication Data from PubMed via R very easy. easyPubMed functions access the PubMed API to search for publications of interest (using query terms provided by the user) and can retrieve corresponding data in XML format. getPubmedIds() queries Entrez and posts the results on the PubMed History server. This server is then used for retrieving data via the fetchPubmedData() function. This procedure is suitable for downloading a large number of records in batches of up to 5000 records per time. More info are available at <http://www.biotechworld.it/bioinf/2016/01/05/querying-pubmed-via-the-easypubmed-package-in-r/>.
}
\author{Damiano Fantini <damiano.fantini@gmail.com>}
\keyword{ package }
\examples{
##  Search for scientific articles written by Damiano Fantini 
##  (via getPubmedIds() function), retrieve data from Entrez 
##  in XML format (via fetchPubmedData() function) and then 
##  print article titles to screen.
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

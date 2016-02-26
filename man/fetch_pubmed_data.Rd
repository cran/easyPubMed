\name{fetch_pubmed_data}
\alias{fetch_pubmed_data}
\title{
Retrieve PubMed Data In XML Format
}
\description{
Retrieve data from PubMed following a search performed via the get_pubmed_ids() function. Data are downloaded in the XML format and are retrieved in batches of up to 5000 records.
}
\usage{
fetch_pubmed_data(pubmed_id_list, retstart = 0, retmax = 500)
}
\arguments{
  \item{pubmed_id_list}{
is a list and is the result of a get_pubmed_ids() call.
}
  \item{retstart}{
is an integer (>=0) and corresponds to the index of the first UID in the retrieved PubMed Search Result set to be included in the XML output (default=0, corresponding to the first record of the entire set).
}
  \item{retmax}{
is an integer (>=1) and corresponds to the maximum number of UIDs from the retrieved set to be downloaded.
}
}
\details{
This function will take the result of a get_pubmed_ids() call as argument and will download the corresponding data from Entrez via the PubMed API efetch function. The first entry to be retrieved may be adjusted via the retastart parameter (this allows the user to download large batches of PubMed data). The maximum number of entries to be retrieved can also be set adjusting the retmax parameter (1 < retmax < 5000). Retrieved data in the XML format will be downloaded on the fly (no files are saved locally as a result of a fetch_pubmed_data() call).
}
\value{
a XMLInternalDocument-class object. The function output contains all data retrieved from Pubmed in XML format and can be accessed using a XML parser (for example, the XML package). Alternatively, specific records may be extracted using regular expressions.
}
\references{
<"http://www.biotechworld.it/bioinf/2016/01/05/querying-pubmed-via-the-easypubmed-package-in-r/">
}
\author{
Damiano Fantini <"damiano.fantini@gmail.com">
}
\examples{
##  Download PubMed Records corresponding to papers written
##  by Damiano Fantini. Data is downloaded in XML format
##  Article Titles are print to screen.
##
dami_on_pubmed <- get_pubmed_ids("Damiano Fantini[AU]")
dami_papers_xml <- fetch_pubmed_data(dami_on_pubmed)
titles <- unlist(xpathApply(dami_papers_xml, "//ArticleTitle", saveXML))
t_pos <- regexpr("<ArticleTitle>.*<\\\/ArticleTitle>", titles)
titles <- substr(titles, t_pos + 14 , t_pos + attributes(t_pos)$match.length -16)
print(titles)
}

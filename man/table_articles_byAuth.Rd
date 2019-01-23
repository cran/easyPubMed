\name{table_articles_byAuth}
\alias{table_articles_byAuth}
\title{
Extract Publication and Affiliation Data from PubMed Records 
}
\description{
Extract Publication Info from PubMed records and cast data into a data.frame where each row corresponds to a different author. It is possible to retrieve data from first authors or last authors only as well as information from all authors of each PubMed record.
}
\usage{
table_articles_byAuth(pubmed_data, 
                      included_authors = "all", 
                      max_chars = 500, 
                      autofill = TRUE, 
                      dest_file = NULL, 
                      getKeywords = TRUE, 
                      encoding = "UTF8")
}
\arguments{
  \item{pubmed_data}{
PubMed Data in XML format: typically, an XML file resulting from a batch_pubmed_download() call or an XML object, result of a fetch_pubmed_data() call.
}
  \item{included_authors}{
Character: c("first", "last", "all"). Only includes information from the first, the last or all authors of a PubMed record.
}
  \item{max_chars}{
Numeric: maximum number of chars to extract from the AbstractText field.
}
  \item{autofill}{
Logical. If TRUE, missing affiliations are imputed according to the available values (from the same article).
}
  \item{dest_file}{
String (character of length 1). Name of the file that will be written for storing the output. If NULL, no file will be saved.
}
  \item{getKeywords}{
Logical. If TRUE, the operation will attempt to extract PubMed record keywords (MESH topics, keywords).
}
  \item{encoding}{The encoding of an input/output connection can be specified by name (for example, "ASCII", or "UTF-8", in the same way as it would be given to the function base::iconv(). See iconv() help page for how to find out more about encodings that can be used on your platform. Here, we recommend using "UTF-8". }
}
\details{
Retrieve publiccation and author information from PubMed data in the form of a data frame. 
}
\value{
Data frame including the following fields: c("article.title","article.abstract", "date.year","date.month","date.day", "journal.abbrv","journal.title", "keywords", "auth.last","auth.fore","auth.address", "auth.email").
}
\references{
\url{https://www.data-pulse.com/dev_site/easypubmed/}
}
\author{
Damiano Fantini <"damiano.fantini@gmail.com">
}
\examples{
\dontrun{
## Cast PubMed record info into a data.frame
## Auto-fill enabled
dami_query <- "Damiano Fantini[AU]"
dami_on_pubmed <- get_pubmed_ids(dami_query)
dami_abstracts_xml <- fetch_pubmed_data(dami_on_pubmed)
xx <- table_articles_byAuth(pubmed_data = dami_abstracts_xml, 
                            included_authors = "first", 
                            max_chars = 100, 
                            autofill = TRUE)[1:2,]
print(xx[1:5, c("pmid", "lastname", "jabbrv")])
#
## Download records first
## Also, auto-fill disabled
dami_query <- "Damiano Fantini[AU]"
curr.file <- batch_pubmed_download(dami_query, dest_file_prefix = "test_bpd_")
xx <- table_articles_byAuth(pubmed_data = curr.file[1], 
                            included_authors = "all", 
                            max_chars = 20, 
                            autofill = FALSE)
print(xx[1:5, c("pmid", "lastname", "jabbrv")])
}
}

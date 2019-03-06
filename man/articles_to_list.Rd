\name{articles_to_list}
\alias{articles_to_list}
\title{
Cast PubMed Data into a List of Articles
}
\description{
Convert an XML object of PubMed records into a list of strings (character vector of length 1) corresponding to individual PubMed articles. PubMed records are identified by a "/PubmedArticle" XML tag. This automatically cast all the content of each PubMed record to a character-class object without removing XML tags.
}
\usage{
articles_to_list(pubmed_data, encoding = "UTF8", simplify = TRUE)
}
\arguments{
  \item{pubmed_data}{
String corresponding to the name of an XML file (typically, the result of a batch_pubmed_download() call). Alternatively, an XML Object, such as the result of a fetch_pubmed_data() call.
}
\item{encoding}{The encoding of an input/output connection can be specified by name (for example, "ASCII", or "UTF-8", in the same way as it would be given to the function base::iconv(). See iconv() help page for how to find out more about encodings that can be used on your platform. Here, we recommend using "UTF-8". }
\item{simplify}{Logical; should the result be simplified to a character vector. If FALSE, results are returned as a list. }
}
\details{
The input is an XML object or an XML file, typically the result of a fetch_pubmed_data() call or a batch_pubmed_download() call. The function returns a list where each element is a different PubMed record.
}
\value{
List or character vector including all the records from the original XML object in text format. Elements in the list are not named and are only accessible via their numeric index.
}
\references{
\url{https://www.data-pulse.com/dev_site/easypubmed/}
}
\author{
Damiano Fantini <"damiano.fantini@gmail.com">
}
\examples{
## Retrieve PubMed data and return a list ot articles
my_query <- "Damiano Fantini[AU]"
my_query <- get_pubmed_ids(pubmed_query_string = my_query)
my_data <- fetch_pubmed_data(my_query, encoding = "ASCII")
listed_articles <- articles_to_list(my_data)
custom_grep(listed_articles[[2]], "ArticleTitle", "char")
\dontrun{
## Download PubMed data and return a list ot articles
dami_query <- "Damiano Fantini[AU] AND 2018[PDAT]"
outfile <- batch_pubmed_download(dami_query, dest_file_prefix = "easyPM_ex001_")
listed_articles <- articles_to_list(pubmed_data = outfile)
custom_grep(listed_articles[[2]], "ArticleTitle", "char")
}
}

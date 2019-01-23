\name{PubMed_stopwords}
\alias{PubMed_stopwords}
\docType{data}
\title{
PubMed Title Stopwords}
\description{
A character vector including a set of stopwords commonly filtered out by web-based PubMed searches
}
\usage{data("PubMed_stopwords")}
\format{ character vector of length 133, including stopwords such as "a", "about", "again", "all", "almost", "also", and so on.
}
\references{
\url{https://www.data-pulse.com/dev_site/easypubmed/}
\url{https://www.ncbi.nlm.nih.gov/books/NBK3827/table/pubmedhelp.T.stopwords/}
}
\examples{
data(PubMed_stopwords)
head(PubMed_stopwords)
}

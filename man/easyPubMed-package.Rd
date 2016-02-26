\name{easyPubMed-package}
\alias{easyPubMed-package}
\alias{easyPubMed}
\docType{package}
\title{easyPubMed-package}
\description{
Query NCBI Entrez and retrieve PubMed records in XML format. PubMed records can be downloaded and saved as XML files. Data integrity is enforced during the download, allowing to retrieve and save very large number of records effortlessly.
}
\author{
Damiano Fantini <"damiano.fantini@gmail.com">
}
\references{
<"http://www.biotechworld.it/bioinf/2016/01/05/querying-pubmed-via-the-easypubmed-package-in-r/">
}
\keyword{ package }
\examples{
dami_on_pubmed <- get_pubmed_ids("Damiano Fantini[AU]")
dami_papers <- fetch_pubmed_data(dami_on_pubmed)
titles <- unlist(xpathApply(dami_papers, "//ArticleTitle", saveXML))
title_pos <- regexpr("<ArticleTitle>.*<\\\/ArticleTitle>", titles)
titles <- substr(titles, title_pos + 14, title_pos + attributes(title_pos)$match.length - 16)
print(titles)
}

\name{get_pubmed_ids_by_fulltitle}
\alias{get_pubmed_ids_by_fulltitle}
\title{
Simple PubMed Record Search by Full-length Title
}
\description{
Query PubMed (Entrez) in a simple way via the PubMed API eSearch function. This function is designed to query PubMed using a full-length publication title as query string. It performs stopword removal from the query sring before querying the PubMed server. Calling this function results in posting the results on the PubMed History Server. This allows later access to the resulting data via the fetch_pubmed_data() function, or other easyPubMed functions.
}
\usage{
get_pubmed_ids_by_fulltitle(fulltitle, field = "[Title]", api_key = NULL)
}
\arguments{
  \item{fulltitle}{String (character vector of length 1) that corresponds to the full-length publication title used for querying PubMed (titles should be used as is, without adding extra filters/tags)}
  \item{field}{String (character vector of length 1) with a tag indicating the PubMed record field where the full-length string (fulltitle) should be searched in. By default, this points to the 'Title' field. This field can be changed (use fields supported by PubMed) as required by the user (for example, to attempt an exact-match query using a specific sentence included in the abstract of a record)
}
  \item{api_key}{String (character vector of length 1): user-specific API key to increase the limit of queries per second. You can obtain your key from NCBI. }
}
\details{
This function will use the String provided as argument for querying PubMed via the eSearch function of the PubMed API. The Query Term should include a full-length publication title, without other PubMed operators (AND, OR, NOT) nor tags (i.e., [AU], [PDAT], [Affiliation], and so on). ESearch will post the UIDs resulting from the search operation onto the History server so that they can be used directly in a subsequent fetchPubmedData() call.
}
\value{
The function returns a list. The list includes the number of records found on PubMed and the first 20 PubMed IDs (UID) retrieved by the query. The list also includes QueryKey and WebEnv that are required for a subsequent fetch_pubmed_data() call.
}
\references{
\url{https://www.data-pulse.com/dev_site/easypubmed/}
\url{https://www.ncbi.nlm.nih.gov/books/NBK3827/#_pubmedhelp_Search_Field_Descriptions_and_}
}
\author{
Damiano Fantini <"damiano.fantini@gmail.com">
}
\examples{
\dontrun{
##  Search for a scientific article matching a full-length title
##
my_query <- "Body mass index and cancer risk among Chinese patients with type 2 diabetes mellitus"
my_field <- "[Title]"
# Full-length title query (designed to query titles)
res <- get_pubmed_ids_by_fulltitle(my_query, field = my_field)
# Num results
print(as.numeric(res$Count))
}
}

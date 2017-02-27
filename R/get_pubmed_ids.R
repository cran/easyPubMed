get_pubmed_ids <-
function (pubmed_query_string) 
{
  myQuery <- as.character(pubmed_query_string)
  myQuery <- gsub(" ", "+", myQuery, fixed = TRUE)
  myPubmedURL <- paste("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?", 
                       "db=pubmed&term=", myQuery, "&usehistory=y", sep = "")
  IDconnect <- url(myPubmedURL, open = "rb")
  idXML <- readLines(IDconnect, warn = FALSE, encoding = "UTF-8")
  idXML <- XML::xmlTreeParse(idXML)
  close.connection(IDconnect)
  myIDlist <- XML::xmlToList(idXML)
  return(myIDlist)
}

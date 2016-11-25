fetch_pubmed_data <-
function (pubmed_id_list, retstart = 0, retmax = 500)
{
  myIDlist <- pubmed_id_list
  if ((!is.list(myIDlist)) | is.na(myIDlist$WebEnv) | is.na(myIDlist$QueryKey) |
      is.na(myIDlist$Count) | !is.integer(as.integer(retstart)) |
      !is.integer(as.integer(retmax))) {
    stop("There is an issue with the PubMed ID list you supplied. Please, call the function again and supply the result of a <get_pubmed_ids()> call as argument. Thank you.")
  }
  else {
    myWebEnv <- myIDlist$WebEnv
    myKey <- myIDlist$QueryKey
    myCount <- as.numeric(as.character(myIDlist$Count))
    myRetstart = as.integer(retstart)
    if (myRetstart < 0) {
      myRetstart = 0
    }
    myRetmax <- as.integer(retmax)
    if (myRetmax > 5000) {
      myRetmax = 5000
    }
    if (myRetmax < 1) {
      myRetmax = 1
    }
    efetch_url = paste("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?",
                       "db=pubmed&WebEnv=", myWebEnv, "&query_key=", myKey,
                       "&retstart=", myRetstart, "&retmax=", myRetmax, "&rettype=null&retmode=xml",
                       sep = "")
    tmpConnect <- url(efetch_url, open = "rb")
    abstrXML <- readLines(tmpConnect, warn = FALSE, encoding = "xml")
    abstrXML <- XML::xmlParse(abstrXML, encoding = "UTF-8")
    close.connection(tmpConnect)
    return(abstrXML)
  }
}

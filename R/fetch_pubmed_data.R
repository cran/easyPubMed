fetch_pubmed_data <-
function (pubmed_id_list, 
                               retstart = 0, 
                               retmax = 500,
                               format = "xml") 
{
  myIDlist <- pubmed_id_list
  if ((!is.list(myIDlist)) | is.na(myIDlist$WebEnv) | is.na(myIDlist$QueryKey) | 
      is.na(myIDlist$Count) | !is.integer(as.integer(retstart)) | 
      !is.integer(as.integer(retmax))) {
    stop("There is an issue with the PubMed ID list you supplied. Please, call the function again and supply the result of a <get_pubmed_ids()> call as argument. Thank you.")
  } else {
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
    if (format[1] %in% c("medline","uilist","abstract","asn.1", "xml")) {
      myFormat <- format[1]
    } else {
      myFormat <- "xml"
    }
    typeMode <- switch(myFormat, 
                       "asn.1" = c("null", "asn.1"),
                       "xml" = c("null", "xml"),
                       "medline" = c("medline", "text"),
                       "uilist" = c("uilist", "text"),
                       "abstract" = c("abstract", "text"))
    efetch_url = paste("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?", 
                       "db=pubmed&WebEnv=", myWebEnv, "&query_key=", myKey, 
                       "&retstart=", myRetstart, "&retmax=", myRetmax, 
                       "&rettype=", typeMode[1],"&retmode=", typeMode[2], 
                       sep = "")
    tmpConnect <- url(efetch_url, open = "rb")
    out.data <- readLines(tmpConnect, warn = FALSE, encoding = "UTF-8")
    if (myFormat == "xml") {
      out.data <- XML::xmlParse(out.data, encoding = "UTF-8")  
    }
    close.connection(tmpConnect)
    return(out.data)
  }
}

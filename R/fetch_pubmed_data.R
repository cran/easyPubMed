fetch_pubmed_data <-
  function (pubmed_id_list,
            retstart = 0,
            retmax = 500,
            format = "xml", 
            encoding = "UTF8") 
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
    # api_key retrieval
    api_key <- pubmed_id_list$APIkey
    if (!is.null(api_key)) {
      efetch_url <- paste(efetch_url, "&api_key=", api_key, sep = "")
    }
    
    # Connect and data retrieval
    tmpConnect <- url(efetch_url, open = "rb", encoding = "UTF8")
    on.exit(close(tmpConnect))
    out.data <- suppressWarnings(readLines(tmpConnect, warn = FALSE, encoding = "UTF8")) 
    
    if (encoding != "UTF8")
      out.data <- base::iconv(out.data, from = "UTF8", to = encoding, sub = ".")

    if (format[1] == "xml") {
      out.data <- paste(out.data, collapse = "")
    }
                          
    return(out.data)
  }
}

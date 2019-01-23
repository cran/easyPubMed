get_pubmed_ids <-
  function (pubmed_query_string, 
            api_key = NULL) 
{
  myQuery <- as.character(pubmed_query_string)
  myQuery <- gsub(" ", "+", myQuery, fixed = TRUE)
  myPubmedURL <- paste("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?", 
                       "db=pubmed&term=", myQuery, "&usehistory=y", sep = "")
  if (!is.null(api_key)) {
    myPubmedURL <- paste(myPubmedURL, "&api_key=", api_key, sep = "")
  }
  
  tryCatch({

    IDconnect <- url(myPubmedURL, open = "rb", encoding = "UTF8")
    idXML <- readLines(IDconnect, warn = FALSE, encoding = "UTF8")
    idXML <- paste(idXML, collapse = "")
    
    # Initialize collector
    myIDlist <- list()
    
    my_tags <- c("Count", "RetMax", "RetStart", 
                 "QueryKey", "WebEnv", "IdList", 
                 "TranslationSet", "QueryTranslation")
    
    # First pass
    for (j in 1:length(my_tags)) {
      ttag <- my_tags[j]
      xx <- custom_grep(idXML, tag = ttag, "char")
      myIDlist[[ttag]] <- xx[1]
    }
    
    # Try to expand IdList
    nutag <- "Id"
    xx <- myIDlist[["IdList"]]
    xx <- custom_grep(xx, "Id", format = "list")
    names(xx) <- rep("Id", length(xx))
    myIDlist[["IdList"]] <- xx
    
    # Try to expand TranslationSet
    xx <- myIDlist[["TranslationSet"]]
    myIDlist[["TranslationSet"]] <- list()
    nutag <- c("From", "To")
    for (z in nutag) {
      yy <- custom_grep(xx, z, format = "char")
      myIDlist[["TranslationSet"]][[z]] <- yy[1]
    }
    
  }, error = function(e) { 
    myIDlist <- NA 
  }, finally =  {
    close(IDconnect)
  })
  
  # Final check!
  if(!is.list(myIDlist)) {
    stop("An error has occurred!")  
  }
  myIDlist[['OriginalQuery']] <- myQuery
  myIDlist[['APIkey']] <- api_key
  return(myIDlist)
}

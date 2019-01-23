get_pubmed_ids_by_fulltitle <- 
  function(fulltitle, field = "[Title]", api_key = NULL) 
{
  out <- get_pubmed_ids(paste("\"", fulltitle, "\"", field, sep = ""), api_key = api_key)
  if (as.numeric(out$Count) > 0) {
    return (out)
  }
  
  stopwords <- easyPubMed::PubMed_stopwords
  keys <- strsplit(fulltitle, split = "[[:space:]]")[[1]]
  keys <- tolower(keys)
  keys <- keys[!keys %in% stopwords]
  Sys.sleep(0.34)
  new_query <- paste(keys, field, sep = "", collapse = " AND ")
  return(get_pubmed_ids(new_query, api_key = api_key))
}
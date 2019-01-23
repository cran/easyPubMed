fetch_all_pubmed_ids <-
  function(pubmed_id_list)
{
  # expected records, set retmax
  exp_num <- as.numeric(pubmed_id_list$Count)
  if (is.numeric(exp_num) && exp_num > 0) {
    my_retmax <- exp_num + 1
  } else {
    my_retmax <- 100000
  }
  
  # query, and then extract IDs
  myPubmedURL <- paste("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?", 
                       "db=pubmed&retmax=", my_retmax, "&term=", pubmed_id_list$OriginalQuery, "&usehistory=n", sep = "")
  IDconnect <- url(myPubmedURL, open = "rb", encoding = "UTF8")
  on.exit(close(IDconnect))
  idXML <- readLines(IDconnect, warn = FALSE, encoding = "UTF8") 

  collect_ids <- list()
  for (i in 1:length(idXML)) {
    if (grepl("^<Id>", idXML[i])) {
      xx <- custom_grep(idXML[i], tag = "Id", format = "char")
      collect_ids[[length(collect_ids) + 1]] <- as.character(xx[1])
    }
  }
  myIDlist <- as.character(do.call(c, collect_ids))
  
  # final check and return
  if(length(myIDlist) != exp_num)
    message(paste("Note that only ", length(myIDlist), " PubMed IDs were retrieved (", 
                  exp_num, " were expected).", sep = ""))
  
  return(myIDlist)
}

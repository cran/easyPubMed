articles_to_list <-
  function(pubmed_data, encoding = "UTF8") 
{
  # check if it is a XMLAbstractDocument or a file
  TMP <- substr(pubmed_data[1], 1, 1000)
  if (grepl("<PubmedArticle", TMP)) {
    
    # it's a string to process
    pubmed_data <- gsub("[[:space:]]{2,}", " ", pubmed_data[1])
    out <- custom_grep(pubmed_data, tag = "PubmedArticle", format = "char")
    out <- paste("<PubmedArticle>", out, "</PubmedArticle>")
  
  } else if (file.exists(pubmed_data)) {
    
    # it's a file
    con1 <- file(pubmed_data[1], encoding = "UTF8")
    on.exit(close(con1))
    myXML <- readLines(con = con1, 
                       n = -1, ok = TRUE, encoding = "UTF8") 
    
    if (encoding != "UTF8")
      myXML <- base::iconv(myXML, from = "UTF8", to = encoding, sub = ".")

    myXML <- paste(myXML, collapse = "")
    myXML <- gsub("[[:space:]]{2,}", " ", myXML)
    
    out <- custom_grep(myXML, tag = "PubmedArticle", format = "char")
    out <- paste("<PubmedArticle>", out, "</PubmedArticle>")
    
  } else {
    message("An error occurred")
    return(NULL)  
  }

  return(out)
}  
    

table_articles_byAuth <-
  function (pubmed_data, 
            included_authors = "all", 
            max_chars = 500, 
            autofill = TRUE, 
            dest_file = NULL, 
            getKeywords = TRUE, 
            encoding = "UTF8") 
{
  if (!included_authors %in% c("all", "first", "last")) 
    stop("Method is not supported!")
  message("Processing PubMed data ", appendLF = FALSE)
  
  paper.data <- articles_to_list(pubmed_data = pubmed_data, encoding = encoding)
  
  expFields <- c("pmid", "doi", "title", "abstract", "year", "month", "day", "jabbrv", 
                 "journal", "keywords", "lastname", "firstname", "address", "email")
  papers.authors.list <- lapply(1:length(paper.data), (function(i) {
    if (length(paper.data) > 50) {
      rep.dot <- as.integer(seq(1, length(paper.data), 
                                length.out = 50))
      if (i %in% rep.dot) 
        message(".", appendLF = FALSE)
    } else {
      message(".", appendLF = FALSE)
    }
    art <- paper.data[[i]]
    out <- tryCatch({article_to_df(pubmedArticle = art, 
                                  autofill = autofill, 
                                  max_chars = max_chars, 
                                  getKeywords = getKeywords, 
                                  getAuthors = TRUE)}, 
                    error = function(e) { NULL })
    
    if (is.null(out)) {
      out <- data.frame(pmid = NA, doi = NA, title = NA, 
                        abstract = NA, year = NA, month = NA, day = NA, 
                        jabbrv = NA, journal = NA, keywords = NA, lastname = NA, firstname = NA, 
                        address = NA, email = NA)
    }
    if (included_authors == "first") {
      out <- out[1, ]
    } else if (included_authors == "last") {
      out <- out[nrow(out), ]
    } 
    
    # Handle missing fields exception
    out2 <- data.frame(rebuild = (1:nrow(out))) 
    for (jj in 1:length(expFields)) {
      if (expFields[jj] %in% names(out)) {
        out2[,expFields[jj]] <- out[,expFields[jj]]
      } else {
        out2[,expFields[jj]] <- NA
      }
    }
    out2[,-1]
  }))
  message(" done!")
  
  #y <- names(papers.authors.list[[1]])
  #kp <- sapply(papers.authors.list, function(x) {sum(! y %in% names(x)) == 0 })
  #class(papers.authors.list[!kp])
  #names(papers.authors.list[!kp][[1]])
  #sum(!kp)
  
  papers.authors.df <- do.call(rbind, papers.authors.list)
  keep.rw <- apply(papers.authors.df, 1, (function(rw) {
    sum(is.na(rw)) < length(rw)
  }))
  papers.authors.df <- papers.authors.df[keep.rw, ]
  if (!is.null(dest_file)) {
    if (class(dest_file) == "character" & length(dest_file) == 1) {
      tryCatch(utils::write.table(papers.authors.df, dest_file, fileEncoding = encoding), 
               error = function(e) {
                 NULL
               })
    }
  }
  return(papers.authors.df)
}

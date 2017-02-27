table_articles_byAuth <-
function(pubmed_data, included_authors = "all", max_chars = 500, autofill = TRUE, dest_file = NULL) {
  if (!included_authors %in% c("all", "first", "last"))
    stop("Method is not supported!")
  #
  message("Processing PubMed data ", appendLF = FALSE)
  paper.data <- articles_to_list(pubmed_data)
  papers.authors.list <- lapply(1:length(paper.data), (function(i){
    if (length(paper.data)>50) {
      rep.dot <- as.integer(seq(1, length(paper.data), length.out = 50))
      if (i %in% rep.dot)
        message(".", appendLF = FALSE)
    } else {
      message(".", appendLF = FALSE) 
    }
    #
    art <- paper.data[[i]]
    out <-  tryCatch(article_to_df(pubmedArticle = art, autofill = autofill, max_chars = max_chars), error = function(e) {NULL})
    if (is.null(out)){
      out <- data.frame(pmid=NA, doi=NA, title=NA, abstract=NA, 
                        year=NA, month=NA, day=NA, jabbrv=NA, 
                        journal=NA, lastname=NA, firstname=NA, 
                        address=NA, email=NA)
    }
    if (included_authors == "first"){
      out <- out[1, ] 
    } else if (included_authors == "last") {
      out <- out[nrow(out), ]
    } else {
      # do nothing
    }
    out   
  }))
  message(" done!")
  papers.authors.df <- do.call(rbind, papers.authors.list)
  keep.rw <- apply(papers.authors.df, 1, (function(rw) {sum(is.na(rw)) < length(rw)}))
  papers.authors.df <- papers.authors.df[keep.rw,]
  #
  if(!is.null(dest_file)){
    if(class(dest_file) == "character" & length(dest_file) == 1){
      tryCatch(utils::write.table(papers.authors.df, dest_file), error = function(e){NULL})
    }
  }
  return(papers.authors.df)
}

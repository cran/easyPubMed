article_to_df <-
function(pubmedArticle, autofill = FALSE, max_chars = 500) {
  #
  options(warn = -1)
  # initial check
  if (class(pubmedArticle) != "character" |
      regexpr("(<PubmedArticle)(.+)(\\/PubmedArticle>)", pubmedArticle) < 0 )
    stop("An error occurred")
  #
  if (!is.numeric(max_chars)) {
    max_chars <- 500
  } else if (max_chars < 0) {
    max_chars <- 0  
  }
  #
  #
  tmp.article <- custom_grep(xml_data = pubmedArticle, tag = "PubmedArticle", format = "char")
  #
  tmp.title <- custom_grep(xml_data = tmp.article, tag = "ArticleTitle", format = "char")
  tmp.abstract <- custom_grep(xml_data = tmp.article, tag = "AbstractText", format = "char")
  if (length(tmp.abstract) > 1){
    tmp.abstract <- paste(tmp.abstract, collapse = " ", sep = " ")
  } else if (length(tmp.abstract) < 1) {
    tmp.abstract <- NA
  }
  #
  my.dateType <- c("DateCompleted",  "DateCreated",  "DateRevised",  "PubDate")
  sel.dateType <-which(sapply(my.dateType, (function(xi) {
    regexpr(xi, tmp.article) > 0
  })))[1]
  #
  tmp.date <- custom_grep(xml_data = tmp.article, tag = my.dateType[sel.dateType], format = "char")
  tmp.date <- sapply(c("Year", "Month", "Day"), (function(tt){
    custom_grep(xml_data = tmp.date, tag = tt, format = "char")   
  }))
  #
  tmp.paperID  <- custom_grep(xml_data = tmp.article, tag = "ArticleIdList", format = "char")
  tmp.paperID <- gsub("[[:space:]]", "", tmp.paperID)
  #
  tmp.PMID <- gsub("^(.*ArticleIdIdType=\\\"pubmed\\\")([[:space:]]|[[:alnum:]]){0,20}>", "", tmp.paperID)
  tmp.PMID <- gsub("<.*$", "", tmp.PMID)
  #
  tmp.DOI <- gsub("^(.*ArticleIdIdType=\\\"doi\\\")([[:space:]]|[[:alnum:]]){0,20}>", "", tmp.paperID)
  tmp.DOI <- gsub("<.*$", "", tmp.DOI)
  #
  tmp.jabbrv  <- custom_grep(xml_data = tmp.article, tag = "ISOAbbreviation", format = "char")
  tmp.journal <- custom_grep(xml_data = tmp.article, tag = "Title", format = "char")
  #
  # vctor with all unique fields extracted o far
  tmp.resout <- c(pmid=tmp.PMID, 
                  doi=tmp.DOI, 
                  title=tmp.title,
                  abstract=substr(tmp.abstract, 0, max_chars),
                  year = as.character(tmp.date[1]),
                  month = as.character(tmp.date[2]),
                  day = as.character(tmp.date[3]),
                  jabbrv=tmp.jabbrv,
                  journal=tmp.journal)
  #
  tmp.authors <- custom_grep(xml_data = tmp.article, tag = "AuthorList", format = "char")
  author.list <- custom_grep(xml_data = tmp.authors, tag = "Author", format = "char")
  final.mat <- t(sapply(author.list, (function(al){
    tmp.lastnm <- custom_grep(xml_data = al, tag = "LastName", format = "char")
    tmp.firstnm <- custom_grep(xml_data = al, tag = "ForeName", format = "char")
    tmp.email <- regexpr("([[:alnum:]]|\\.|\\-\\_){3,200}@([[:alnum:]]|\\.|\\-\\_){3,200}(\\.)([[:alnum:]]){2,6}", al)
    if (tmp.email > 0) {
      tmp.email <- substr(al, tmp.email, tmp.email + attributes(tmp.email)$match.length -1 )
    } else {
      tmp.email <- NA
    }
    #
    if (regexpr("Affiliation", al) > 0) {
      tmp.add <- custom_grep(al, "Affiliation", format = "char")[1]
      tmp.add <- trim_address(tmp.add)
    } else {
      tmp.add <- NA
    }
    c(tmp.resout, 
      lastname=tmp.lastnm, 
      firstname=tmp.firstnm, 
      address=tmp.add, 
      email=tmp.email)
    #
  })))
  rownames(final.mat) <- NULL
  final.mat <- data.frame(final.mat, stringsAsFactors = FALSE)
  #
  if (autofill){
    tmp.address <- final.mat[,"address"]
    na.pos <- is.na(tmp.address)
    if (sum(na.pos) != length(tmp.address)) {
      tmp.list <- lapply(tmp.address, function(x) {x} ) 
      cur.add <-  tmp.list[[(which(!na.pos)[1])]]
      for (i in 1:length(na.pos)){
        if(na.pos[i]){
          tmp.list[[i]] <- cur.add
        } else {
          cur.add <- tmp.list[[i]]
        }
      }
      final.mat[,"address"] <- do.call(c, tmp.list)
    }
  }
  #
  options(warn = 0)
  return(final.mat)
}

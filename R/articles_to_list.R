articles_to_list <-
function(pubmed_data) {
  # check if it is a XMLAbstractDocument or a file
  options(warn = -1)
  if (sum(class(pubmed_data) == "XMLAbstractDocument") > 0) {
    out <- tryCatch(XML::xpathSApply(pubmed_data, "//PubmedArticle", XML::saveXML), error = function(e) { NULL })
  } else if (class(pubmed_data) == "character") {
    out <- tryCatch(XML::xmlParse(pubmed_data), error = function(e) {NULL})    
    if (!is.null(out))
      out <- tryCatch(XML::xpathApply(out, "//PubmedArticle", XML::saveXML), error = function(e) { NULL })    
  } else {
    out <- NULL
  }
  if (is.null(out)) {
    message("An error occurred")
  }
  options(warn = 0)
  return(out)
}

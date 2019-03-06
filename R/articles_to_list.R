#' Cast PubMed Data into a List of Articles
#'
#' Convert an XML object of PubMed records into a list of strings 
#' (character vector of length 1) corresponding to individual PubMed articles. 
#' PubMed records are identified by a "/PubmedArticle" XML tag. This automatically casts 
#' all the content of each PubMed record to a character-class object without removing XML tags.
#' 
#' @usage articles_to_list(pubmed_data, encoding = "UTF8")
#' 
#' @param pubmed_data String corresponding to the name of an XML file 
#' (typically, the result of a batch_pubmed_download() call). Alternatively, 
#' an XML Object, such as the result of a fetch_pubmed_data() call.
#' @param encoding The encoding of an input/output connection can be specified by name 
#' (for example, "ASCII", or "UTF-8", in the same way as it would be given to the function base::iconv(). 
#' See iconv() help page for how to find out more about encodings that can be used on your platform. 
#' Here, we recommend using "UTF-8".
#' 
#' @details The input is an XML object or an XML file, typically the result of a fetch_pubmed_data() 
#' call or a batch_pubmed_download() call. The function returns a list where each element 
#' is a different PubMed record
#' 
#' Character vector listing all the records from the original XML object in text format. Elements in the list are not named and are only accessible via the numeric index.
#' 
#' @author Damiano Fantini <damiano.fantini@@gmail.com>
#' 
#' @references https://www.data-pulse.com/dev_site/easypubmed/
#' 
#' @examples 
#' \dontrun{
#' ## Retrieve PubMed data and return a list ot articles
#' my_query <- "Damiano Fantini[AU]"
#' my_query <- get_pubmed_ids(pubmed_query_string = my_query)
#' my_data <- fetch_pubmed_data(my_query, encoding = "ASCII")
#' listed_articles <- articles_to_list(my_data)
#' custom_grep(listed_articles[[2]], "ArticleTitle", "char")
#' ## Download PubMed data and return a list ot articles
#' dami_query <- "Damiano Fantini[AU] AND 2018[PDAT]"
#' outfile <- batch_pubmed_download(dami_query, dest_file_prefix = "easyPM_ex001_")
#' listed_articles <- articles_to_list(pubmed_data = outfile)
#' custom_grep(listed_articles[[2]], "ArticleTitle", "char")
#' 
#' }
#' 
#' @export
articles_to_list <-
  function(pubmed_data, encoding = "UTF8", simplify = TRUE) 
{
    
  # Define a nested (core) function handling standardized input
  # This assumes a string as a input
    easyPM_exec_art_to_lst <- function(pm_dataa, simply = TRUE) 
    {
      # it's a string to process
      pm_datab <- strsplit(pm_dataa, "<PubmedArticle(>|[[:space:]]+?.*>)")[[1]][-1]
      pm_datab <- sapply(pm_datab, function(x) {
        #trim extra stuff at the end of the record
        if (!grepl("</PubmedArticle>$", x))
          x <- sub("(^.*</PubmedArticle>).*$", "\\1", x) 
        
        # Rebuid XML structure and proceed
        x <- paste("<PubmedArticle>", x)
        gsub("[[:space:]]{2,}", " ", x)}, 
        USE.NAMES = FALSE, simplify = simply)
      
      pm_datab
    }
    
  # Execute f(x)
  # Handle inputs of different type
  # check if it is a XMLAbstractDocument or a file
  TMP <- substr(pubmed_data[1], 1, 1000)
  if (grepl("<PubmedArticle", TMP)) {
    
    # it's a string to process
    out <- easyPM_exec_art_to_lst(pubmed_data[1], simply = simplify)
  
  } else if (file.exists(pubmed_data[1])) {
    
    # it's a file
    con1 <- file(pubmed_data[1], encoding = "UTF8")
    on.exit(close(con1))
    myXML <- readLines(con = con1, 
                       n = -1, ok = TRUE, encoding = "UTF8") 
    
    if (encoding != "UTF8")
      myXML <- base::iconv(myXML, from = "UTF8", to = encoding, sub = ".")

    myXML <- paste(myXML, collapse = "")
    
    # Run as above
    out <- easyPM_exec_art_to_lst(myXML, simply = simplify)
    
  } else {
    message("An error occurred")
    return(NULL)  
  }

  return(out)
}  
    

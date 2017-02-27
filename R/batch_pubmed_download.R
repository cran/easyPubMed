batch_pubmed_download <-
function (pubmed_query_string, 
                                   dest_dir = NULL,
                                   dest_file_prefix = "easyPubMed_data_",
                                   format = "xml",
                                   batch_size = 400, 
                                   res_cn = 1) 
{
  baseDir <- getwd()
  if (!is.null(dest_dir)) {
    setwd(as.character(dest_dir))
  }
  fileName.collector <- list()
  myQuery <- NULL
  while (is.null(myQuery)) {
    myQuery <- tryCatch(easyPubMed::get_pubmed_ids(pubmed_query_string), 
                        error = function(e) NULL)
  }
  pubsNum <- as.numeric(myQuery$Count)
  tmpPapers <- NULL
  myRetstart <- 0
  myRetmax <- batch_size
  j = 1
  expTot <- pubsNum/batch_size
  if (expTot > as.integer(expTot)) {
    expTot <- as.integer(expTot) + 1
  } else {
    expTot <- as.integer(expTot)
  }
  while (myRetstart < pubsNum) {
    if (j < res_cn) {
      message(paste("cycle", j, "/", expTot, "skipped...", 
                    sep = " "))
    } else {
      while (is.null(myQuery) | is.null(tmpPapers)) {
        myQuery <- tryCatch(easyPubMed::get_pubmed_ids(pubmed_query_string), 
                            error = function(e) NULL)
        tmpPapers <- tryCatch(easyPubMed::fetch_pubmed_data(pubmed_id_list = myQuery, 
                                                retstart = myRetstart, 
                                                retmax = myRetmax, 
                                                format = format), 
                              error = function(e) NULL, 
                              finally = print(paste("PubMed data batch", 
                                                    j, "/", 
                                                    expTot, "downloaded...",
                                                    sep = " ")))
        if (is.null(tmpPapers)) {
          message("Data retrieval error. Retrying...")
        }
      }
      totDigits <- nchar(as.character(expTot)) + 1
      myExt <- paste(rep(0, totDigits - nchar(as.character(j))), 
                     collapse = "")
      if (format == "xml") {
        doSaveData <- tryCatch(XML::saveXML(tmpPapers,
                                            paste(dest_file_prefix, myExt, j, ".xml", sep = "")), 
                               error = function(e) "ERROR")
      } else {
        tmp.dest.file <- paste(dest_file_prefix, myExt, j, ".txt", sep = "")
        doSaveData <- tryCatch(write(tmpPapers, tmp.dest.file), 
                               error = function(e) "ERROR")
        if(is.null(doSaveData))
          doSaveData <- tmp.dest.file
      }
      myQuery <- NULL
      tmpPapers <- NULL
      if (doSaveData == "ERROR") {
        myRetstart <- myRetstart - myRetmax
        j <- j - 1
        message("An error occurred... Trying to download data from PubMed again...")
      } else {
        fileName.collector[[1+length(fileName.collector)]] <- doSaveData
      }
    }
    myRetstart <- myRetstart + myRetmax
    j <- j + 1
  }
  setwd(baseDir)
  tryCatch(do.call(c, fileName.collector), error = function(e){NULL})
}

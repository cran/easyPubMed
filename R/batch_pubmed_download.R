batch_pubmed_download <-
  function (pubmed_query_string, 
            dest_dir = NULL,
            dest_file_prefix = "easyPubMed_data_",
            format = "xml",
            api_key = NULL,
            batch_size = 400, 
            res_cn = 1, 
            encoding = "UTF8") 
    
{
  baseDir <- getwd()
  if (!is.null(dest_dir)) {
    setwd(as.character(dest_dir))
  }
  fileName.collector <- list()
  myQuery <- NULL
  my_rtime <- ifelse(is.null(api_key), 0.34, 0.11)
  
  cur_time <- Sys.time()
  while (is.null(myQuery)) {
    diff_time <- my_rtime - (as.numeric(Sys.time() - cur_time))
    if (diff_time > 0) {
      Sys.sleep(diff_time)
    }
    cur_time <- Sys.time()
    myQuery <- tryCatch(get_pubmed_ids(pubmed_query_string, api_key = api_key), 
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
      cur_time <- Sys.time()
      while (is.null(myQuery) | is.null(tmpPapers)) {
        
        diff_time <- my_rtime - (as.numeric(Sys.time() - cur_time))
        if (diff_time > 0) {
          Sys.sleep(diff_time)
        }
        cur_time <- Sys.time()
        
        myQuery <- tryCatch(get_pubmed_ids(pubmed_query_string, api_key = api_key), 
                            error = function(e) NULL)
        
        diff_time <- my_rtime - (as.numeric(Sys.time() - cur_time))
        if (diff_time > 0) {
          Sys.sleep(diff_time)
        }
        cur_time <- Sys.time()
        
        # Force download as XML, but withoud collapsing strings
        if (format[1] == "xml") {
          format <- "batchxml"
        }
        
        tmpPapers <- tryCatch(fetch_pubmed_data(pubmed_id_list = myQuery, 
                                                retstart = myRetstart, 
                                                retmax = myRetmax,
                                                format = format, 
                                                encoding = encoding),
                              
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
  
      tmp.dest.file <- paste(dest_file_prefix, myExt, j, ".txt", sep = "")
      con1 <- file(tmp.dest.file, encoding = encoding)
      doSaveData <- tryCatch(write(tmpPapers, tmp.dest.file), 
                             error = function(e) {"ERROR"}, 
                             finally = {close(con1)})
      if(is.null(doSaveData))
        doSaveData <- tmp.dest.file
      
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

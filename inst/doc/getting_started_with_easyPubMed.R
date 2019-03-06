## ----inst__0001, include = TRUE, echo = TRUE, eval = FALSE---------------
#  install.packages("easyPubMed")

## ----inst___02, include = TRUE, echo = TRUE, eval = FALSE----------------
#  library(easyPubMed)

## ----include = FALSE-----------------------------------------------------
library(easyPubMed)
out.X <- download.file(url = "https://www.data-pulse.com/projects/Rlibs/supporting/easyPM_vignette_suppl.rda", 
                       destfile = "easyPM_vignette_suppl.rda", quiet = TRUE)
load(file = "easyPM_vignette_suppl.rda")
out.A <- NULL 
out.B <- NULL

## ----inst___04, include = TRUE, echo = TRUE, eval = FALSE----------------
#  library(devtools)
#  install_github("dami82/easyPubMed")

## ----message = FALSE, warning = FALSE, eval = FALSE----------------------
#  my_query <- 'Damiano Fantini[AU] AND "2018"[PDAT]'
#  my_entrez_id <- get_pubmed_ids(my_query)
#  my_abstracts_txt <- fetch_pubmed_data(my_entrez_id, format = "abstract")
#  
#  # You may omit this conversion if your system supports UTF8
#  my_abstracts_txt <- iconv(my_abstracts_txt, from = "UTF8", to = "ASCII", sub = ".")

## ----include=FALSE, echo = FALSE, eval = TRUE----------------------------
my_abstracts_txt <- suppl_x[["e_01"]]

## ----message = FALSE, warning = FALSE, eval = TRUE-----------------------
head(my_abstracts_txt)

## ----message = FALSE, warning = FALSE, eval = FALSE----------------------
#  my_abstracts_xml <- fetch_pubmed_data(pubmed_id_list = my_entrez_id)

## ----include=FALSE, echo = FALSE, eval = TRUE----------------------------
my_abstracts_xml <- suppl_x[["e_02"]]

## ----message = FALSE, warning = FALSE, eval = TRUE-----------------------
# You may omit this conversion if your system supports UTF8
my_abstracts_xml <- iconv(my_abstracts_xml, from = "UTF8", to = "ASCII", sub = ".")

class(my_abstracts_xml) 

my_titles <- custom_grep(my_abstracts_xml, "ArticleTitle", "char")

# use gsub to remove the tag, also trim long titles
TTM <- nchar(my_titles) > 75
my_titles[TTM] <- paste(substr(my_titles[TTM], 1, 70), "...", sep = "")

# Print as a data.frame (use kable)
head(my_titles)

## ----message = FALSE, warning = FALSE, eval=FALSE------------------------
#  new_query <- 'Bladder[TIAB] AND Northwestern[AD] AND Chicago[AD] AND "2018"[PDAT]'
#  out.A <- batch_pubmed_download(pubmed_query_string = new_query,
#                                 format = "xml",
#                                 batch_size = 20,
#                                 dest_file_prefix = "easyPM_example",
#                                 encoding = "ASCII")
#  

## ----message = FALSE, warning = FALSE, include = FALSE, echo = FALSE, eval=TRUE----
out.A <- suppl_x[["e_03"]]

## ----message = FALSE, warning = FALSE, eval=TRUE-------------------------
# this variable stores the name of the output files
print(out.A) 

## ----message = FALSE, warning = FALSE, eval=TRUE-------------------------
my_PM_list <- articles_to_list(pubmed_data = my_abstracts_xml)
class(my_PM_list[1])
print(substr(my_PM_list[4], 1, 510))

## ----message = FALSE, warning = FALSE, eval=TRUE-------------------------
curr_PM_record <- my_PM_list[1]
custom_grep(curr_PM_record, tag = "PubDate")

custom_grep(curr_PM_record, tag = "LastName", format = "char")

## ----message = FALSE, warning = FALSE, eval=TRUE-------------------------
# Select a single PubMed record from the internal dataset, NUBL_1618
curr_PM_record <- easyPubMed::NUBL_1618$records[37]
my.df <- article_to_df(curr_PM_record, max_chars = 18)

# Fields extracted from the PubMed record
head(colnames(my.df))

# Trim long strings and then display some content: each row corresponds to one author
my.df$title <- substr(my.df$title, 1, 15)
my.df$address <- substr(my.df$address, 1, 19)
my.df$jabbrv <- substr(my.df$jabbrv, 1, 10)

# Visualize
my.df[,c("pmid", "title", "jabbrv", "firstname", "address")] 

## ----message = FALSE, warning = FALSE, eval=TRUE-------------------------
my.df2 <- article_to_df(curr_PM_record, autofill = TRUE)

# Trim long strings and then display some content: each row corresponds to one author
my.df2$title <- substr(my.df2$title, 1, 15)
my.df2$jabbrv <- substr(my.df2$jabbrv, 1, 10)
my.df2$address <- substr(my.df2$address, 1, 19)

# Visualize
my.df2[,c("pmid", "title", "jabbrv", "firstname", "address")]

## ----message = FALSE, warning = FALSE, eval=TRUE-------------------------
xx <- lapply(my_PM_list, article_to_df, autofill = TRUE, max_chars = 50)
full_df <- do.call(rbind, xx)

full_df[seq(1, nrow(full_df), by = 10), c("pmid", "lastname", "jabbrv")] 

## ----takes_some_time, message = FALSE, warning = FALSE, eval=TRUE--------
new_query <- 'Bladder[TIAB] AND Northwestern[AD] AND Chicago[AD] AND "2018"[PDAT]' 
out.B <- batch_pubmed_download(pubmed_query_string = new_query, 
                               dest_file_prefix = "NUBL_18_", 
                               encoding = "ASCII")

# Retrieve the full name of the XML file downloaded in the previous step
new_PM_file <- out.B[1]
new_PM_df <- table_articles_byAuth(pubmed_data = new_PM_file, 
                                   included_authors = "first", 
                                   max_chars = 0, 
                                   encoding = "ASCII")

# Printing a sample of the resulting data frame
new_PM_df$address <- substr(new_PM_df$address, 1, 28)
new_PM_df$jabbrv <- substr(new_PM_df$jabbrv, 1, 9)
sid <- seq(1, nrow(new_PM_df), by = 10)

new_PM_df[sid, c("pmid", "year", "jabbrv", "lastname", "address")]

## ----takes_some_time2, message = FALSE, warning = FALSE, eval=FALSE------
#  new_query <- 'Bladder[TIAB] AND Northwestern[AD] AND Chicago[AD] AND "2018"[PDAT]'
#  new_query <- get_pubmed_ids(new_query)
#  fetched_data <- fetch_pubmed_data(new_query, encoding = "ASCII")

## ----takes_some_time2biz, include = FALSE, echo = FALSE, message = FALSE, warning = FALSE, eval=TRUE----
fetched_data <- suppl_x[["e_04"]]

## ----takes_some_time2triz, message = FALSE, warning = FALSE, eval=TRUE----
new_PM_df <- table_articles_byAuth(pubmed_data = fetched_data, 
                                   included_authors = "first", 
                                   max_chars = 0, 
                                   encoding = "ASCII")

# Printing a sample of the resulting data frame
new_PM_df$address <- substr(new_PM_df$address, 1, 28)
new_PM_df$jabbrv <- substr(new_PM_df$jabbrv, 1, 9)
sid <- seq(1, nrow(new_PM_df), by = 10)

new_PM_df[sid, c("pmid", "year", "jabbrv", "lastname", "address")] 

## ----message = FALSE, warning = FALSE, eval=TRUE-------------------------
sessionInfo()

## ----include = FALSE-----------------------------------------------------
# cleaning
for (xfile in c(out.A, out.B)) {
   tryCatch(file.remove(xfile), error = function(e){NULL})  
}


## ----include=FALSE, echo=FALSE, results='hide', eval=TRUE---------------------
# Declarations
base_epm_ver <- '3.0'
stab_epm_ver <- '3.1.3'
this_epm_ver <- '3.1.3'



# pre-load libs and data
library(easyPubMed)
data("epm_samples")

# Collect custom f(x)
rebuild_uili <- epm_samples$fx$rebuild_uili
rebuild_li <- epm_samples$fx$rebuild_li
rebuild_df <- epm_samples$fx$rebuild_df
fabricate_epm_obj <- epm_samples$fx$fabricate_epm_obj
slice_epm_obj_to_special <- epm_samples$fx$slice_epm_obj_to_special

# Collect data
blca_2018 <- epm_samples$bladder_cancer_2018
blca_40y <- epm_samples$bladder_cancer_40y

# Fabricate objects (vignette version)
epm <- fabricate_epm_obj(blca_2018$demo_data_03)
epm_xmpl_01 <- slice_epm_obj_to_special(epm, mode = 1)
epm_xmpl_02 <- slice_epm_obj_to_special(epm, mode = 2)
epm_xmpl_03 <- fabricate_epm_obj(blca_2018$demo_data_04)
epm_xmpl_04 <- fabricate_epm_obj(blca_40y$demo_data_01)
epm_xmpl_06 <- fabricate_epm_obj(blca_2018$demo_data_02)
epm_xmpl_07 <- fabricate_epm_obj(blca_2018$demo_data_05)

## ----inst__0001, include = TRUE, echo = TRUE, eval = FALSE--------------------
# install.packages("easyPubMed")

## ----inst___04, include = TRUE, echo = TRUE, eval = FALSE---------------------
# devtools::install_github("dami82/easyPubMed")

## ----eval=FALSE, include=TRUE, echo=TRUE--------------------------------------
# # Load library
# library(easyPubMed)
# 
# # Define Query String
# my_query <- '"bladder cancer"[Ti] AND "2018"[PDAT]'
# 
# # Submit the Query
# epm <- epm_query(my_query)
# 
# # Retrieve Records (xml format)
# epm <- epm_fetch(epm, format = 'xml')
# 
# # Extract Information
# epm <- epm_parse(epm)
# 
# # All results are stored in an easyPubMed object.
# epm

## ----echo=FALSE, include=TRUE, results='markup', eval=TRUE--------------------
epm

## ----include=TRUE, results='markup', eval=TRUE, echo=TRUE---------------------
job_meta <- get_epm_meta(x = epm)
head(job_meta)

## ----results='markup'---------------------------------------------------------
raw_records <- get_epm_raw(epm)

# elements are named after the corresponding PMIDs
head(names(raw_records))
# elements include raw PubMed records
first_record <- raw_records[[1]] 

# Show excerpt (from record #1)
cat(substr(first_record, 1, 1200))

## ----results='markup'---------------------------------------------------------
proc_data <- get_epm_data(epm)

# show an excerpt (first 6 records, selected columns)
slctd_fields <- c('pmid', 'doi', 'jabbrv', 'year', 'month', 'day')
head(proc_data[, slctd_fields])

## -----------------------------------------------------------------------------
# Get PMIDs
all_pmids <- get_epm_uilist(epm)

# Show excerpt
head(all_pmids)

## ----results='markup', message=FALSE, warning=FALSE---------------------------
# Article Title (including new-line chars)
my_title <- "Role of gemcitabine and cisplatin as 
             neoadjuvant chemotherapy in muscle invasive bladder cancer: 
             Experience over the last decade."

# Unpolished title string
cat(my_title)
# Clean the title
my_title <- gsub('[[:space:]]+', ' ', my_title)

# Clean title string
cat(my_title)

## ----results='hide', message=FALSE, warning=FALSE, eval=FALSE, include=TRUE, echo=TRUE----
# # Query and fetch
# epm_xmpl_01 <- epm_query_by_fulltitle(fulltitle = my_title)
# epm_xmpl_01 <- epm_fetch(epm_xmpl_01)
# epm_xmpl_01

## ----results='markup', message=FALSE, warning=FALSE, eval=TRUE, include=TRUE, echo=FALSE----
epm_xmpl_01

## ----results='markup', message=FALSE, warning=FALSE, eval=FALSE, include=TRUE, echo=TRUE----
# my_pmids <- c('31572460', '31511849', '31411998')
# 
# epm_xmpl_02 <- epm_query_by_pmid(pmids = my_pmids)
# epm_xmpl_02 <- epm_fetch(epm_xmpl_02)
# epm_xmpl_02

## ----results='markup', message=FALSE, warning=FALSE, eval=TRUE, include=TRUE, echo=FALSE----
epm_xmpl_02

## ----results='markup', message=FALSE, warning=FALSE, eval=FALSE, include=TRUE, echo=TRUE----
# # Define Query String
# my_query <- '"bladder cancer"[Ti] AND "2018"[PDAT]'
# 
# # Submit the Query
# epm_xmpl_03 <- epm_query(my_query)
# 
# # Retrieve Records (request 'medline' format!)
# epm_xmpl_03 <- epm_fetch(epm_xmpl_03, format = 'medline')
# 
# # Get records
# xmpl_03_raw <- get_epm_raw(epm_xmpl_03)
# 
# # Elements are named after the corresponding PMIDs
# head(names(xmpl_03_raw))

## ----results='markup', message=FALSE, warning=FALSE, eval=TRUE, include=TRUE, echo=FALSE----
xmpl_03_raw <- get_epm_raw(epm_xmpl_03)
head(names(xmpl_03_raw))

## ----results='markup', message=FALSE, warning=FALSE---------------------------
# Elements include raw PubMed records
first_record <- xmpl_03_raw[[1]] 

# Show an Excerpt (record n. 12, first 18 lines)
cat(head(first_record, n=20), sep = '\n')  

## ----eval=FALSE, echo=TRUE, include=TRUE--------------------------------------
# # Define Query String
# blca_query <- '"bladder cancer"[Ti] AND ("1980"[PDAT]:"2020"[PDAT])'
# 
# # Submit the Query
# epm_xmpl_04 <- epm_query(blca_query)
# 
# # Retrieve Records (medline format)
# epm_xmpl_04 <- epm_fetch(epm_xmpl_04)
# 
# # Parse all records
# epm_xmpl_04 <- epm_parse(epm_xmpl_04)
# 
# # Show Object
# epm_xmpl_04

## ----eval=TRUE, echo=FALSE, include=TRUE, results='markup'--------------------
# Show Object
epm_xmpl_04

## ----include=TRUE, echo=TRUE, eval=FALSE--------------------------------------
# # Define Query String
# my_query <- '"bladder cancer"[Ti] AND "2018"[PDAT]'
# 
# # Submit the Query
# epm_xmpl_05 <- epm_query(my_query)
# 
# # Retrieve Records
# epm_xmpl_05 <- epm_fetch(epm_xmpl_05, write_to_file = TRUE)
# 
# # Check if file exists
# dir(pattern = '^easypubmed')

## ----echo=FALSE, include=TRUE, results='markup', eval=TRUE--------------------
print('easypubmed_job_202311201513_batch_01.txt')

## ----eval=FALSE, echo=TRUE, include=TRUE--------------------------------------
# # Import XML records from saved file
# epm_xmpl_06 <- epm_import_xml(x = 'easypubmed_job_202311201513_batch_01.txt')
# 
# # Show Object
# epm_xmpl_06

## ----eval=TRUE, echo=FALSE, include=TRUE, results='markup'--------------------
epm_xmpl_06

## ----eval=FALSE, echo=TRUE, include=TRUE--------------------------------------
# my_query <- '"bladder cancer"[Ti] AND "2018"[PDAT]'
# 
# # Submit the Query
# epm_xmpl_07 <- epm_query(my_query)
# 
# # Retrieve Records
# epm_xmpl_07 <- epm_fetch(epm_xmpl_07)
# 
# # Parse (custom params)
# epm_xmpl_07 <- epm_parse(epm_xmpl_07,
#                          max_authors = 3, compact_output = TRUE,
#                          max_references = 5, ref_id_type = 'pmid')
# 
# # Request parsed data
# epm_data <- get_epm_data(epm_xmpl_07)
# 
# # Columns of interest
# cols_of_int <- c('pmid',  'doi', 'authors', 'jabbrv', 'year', 'references')
# 
# # Show an excerpt
# head(epm_data[, cols_of_int])

## ----eval=TRUE, echo=FALSE, include=TRUE, results='markup'--------------------
epm_data <- get_epm_data(epm_xmpl_07)

# Columns of interest
cols_of_int <- c('pmid',  'doi', 'authors', 'jabbrv', 'year', 'references')

# Show an excerpt
head(epm_data[, cols_of_int])

## ----message = FALSE, warning = FALSE, eval=TRUE------------------------------
sessionInfo()


\name{batch_pubmed_download}
\alias{batch_pubmed_download}
\title{
Download Large Number of PubMed Records in XML Format
}
\description{
Performs a PubMed Query (via the get_pubmed_ids() function), downloads the resulting data (via multiple fetch_pubmed_data() calls) and then saves data in a series of xml files on the local drive. The function is suitable for the download of very large number of records.
}
\usage{
batch_pubmed_download(pubmed_query_string, dest_dir = NULL, batch_size = 400, res_cn = 1)
}
\arguments{
  \item{pubmed_query_string}{
a string (character-vector of length 1): this is the string used for querying PubMed (regular PubMed synthax).
}
  \item{dest_dir}{
a string (character-vector of length 1): this string corresponds to the name of the existing folder where files will be saved. Existing files will be overwritten. If null, current working directory will be used.  
}
  \item{batch_size}{
integer (1 < batch_size < 5000): maximum number of records to be saved in a single xml files. 
}
  \item{res_cn}{
integer (> 0): number of the first batch of records to download. This parameter is only useful to resume an uncomplete download job after a system crash.
}
}
\details{
This function is used for downloading large number of PubMed records as a set of xml files that are saved in the folder specified by the user. This function enforces data integrity. If a batch of downloaded data is corrupted, it is discarded and downloaded again. Each download cycle is monitored until the download job is successfully completed. This function should allow to download a whole copy of PubMed, if desired. The function informs the user about the current progress by constantly printing to console the number of batches still in queue for download. pubmed_query_string accepts regular PubMed synthax. The function will query PubMed multiple times using the same query string. Therefore, it is recommended to use a [EDAT] filter in the query if you want to ensure reproducible results.
}

\references{
<"http://www.biotechworld.it/bioinf/2016/01/05/querying-pubmed-via-the-easypubmed-package-in-r/">
}
\author{
Damiano Fantini <"damiano.fantini@gmail.com">
}
\examples{
##  dummy example
##  query PubMed for scientific articles written by Damiano Fantini
##  and download the resulting records in batches of up to 3 records.
##  Records are saved as xml files in the working directory

batch_pubmed_download('Damiano Fantini[AU]', batch_size = 3)

##  - no run - 
##  real world example
##  download 5581 records from Entrez - executing the code will take some time!
##  the following code will download all PubMed abstract about the gene p53
##  published between june 1st, 2014 and june 1st, 2015
#
#   batch_pubmed_download('P53 AND "2014/6/1"[PDAT] : "2015/06/1"[PDAT]')
#
##  - end of no run -
}

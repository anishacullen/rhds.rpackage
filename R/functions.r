## The format of sample identifiers/barcodes is described here:
## https://docs.gdc.cancer.gov/Encyclopedia/pages/TCGA_Barcode/
##
## Here is a summary:
## e.g. TCGA-3C-AAAU-01A-11D-A41Q-05
##   project TCGA
##   tissue source site 3C
##   participant AAAU
##   sample 01 (01-09 tumor, 10-19 normal, 20-29 controls)
##   vial A
##   portion 11
##   analyte D (as in DNA)
##   plate A41Q
##   analysis center 05
##
## The following function extracts the participant identifier
## from a sample id/barcode.

#' Extract participant ID from TCGA barcode
#' @param id TCGA barcode
#' @return participant ID
#' @export
extract.participant <- function(id) {
  sub("TCGA-[^-]+-([^-]+)-.*", "\\1", id)
}

#' Extract tissue ID from TCGA barcode
#' @param id TCGA barcode
#' @return tissue ID
#' @export
extract.tissue <- function(id) {
  sub("TCGA-[^-]+-[^-]+-([0-9]+)[^-]+-.*", "\\1", id)
}

#' Extract participant ID from TCGA barcode
#' @param tar.file
#' @param exract.gilr
#' @param new.file
#' @param resultsdir
#' @return file details
#' @export
extract.file <- function(tar.file, extract.file, new.file, resultsdir) {
  # get file path to extracted file
  x.file <-
    grep(extract.file,
      utils::untar(tar.file, list = T),
      value = T
    )
    
  # extract the tar file
  cat("Extracting", tar.file, "to", new.file, "\n")
  utils::untar(tar.file, exdir=resultsdir, extras="--no-same-owner")
  x.file = file.path(resultsdir,x.file)

  # move the data to named output
  file.copy(x.file, new.file)

  # remove untared directory
  unlink(dirname(x.file), recursive = TRUE)
}

#' Write a table detailing the required outputs
#' @param x 
#' @param filename 
#' @return table
#' @export
my.write.table <- function(x, filename) {
  cat("saving", basename(filename), "...\n")
  write.table(x, file = filename, row.names = T, col.names = T, sep = "\t")
}
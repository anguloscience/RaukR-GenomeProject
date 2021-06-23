#Install packages and load libraries

renv::install("tidyverse", "tibble")

require(tidyverse, tibble)


# Input VCF data.
# if your dataset is going to be downloaded you can use (example): 
# vcf <- system.file("extdata", "chr22.1000g.vcf.tar.gz", package="ExploreData", mustWork = T)


## tibble
tib_vcf = "chr22.1000g.copy.vcf" %>%
  read.table() %>%
  as_tibble() %>%
  setNames(.,c("chrom", "pos", "new-var", "ref", "alt", "phred", 
               "filter", "format", "GT", paste0(rep("ind",2504),"-", 1:2504)))



#function to split "format" column
# "x" is the name of the column
splitcol<-function(x) {
  f<-strsplit(x,"=")
  setNames(sapply(f,'[',2), sapply(f,'[',1))
}
#Making each row into a vector
fmt<-lapply(strsplit(tib_vcf$format,";"), splitcol)
#new column names based on "format"
col_nam<-unique(unlist(sapply(fmt, names)))
#extract data for all rows from each column
row_dat<-do.call(rbind, lapply(fmt, '[', col_nam))

#put together the new data set (format splitted) with the original dataset
#be careful with the column number, so you introduce this data where you really want to.

vcf_det<-cbind(tib_vcf[,1:7], row_dat[,1:12], tib_vcf[,9:2513])





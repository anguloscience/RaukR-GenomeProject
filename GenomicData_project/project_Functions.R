# This function is to separate the "specific" info from the column
info_extract <- function(data = NULL, info = NULL){
  #split the string
  tmp_a <- data %>% str_split(., pattern = ";") %>% unlist()
  tmp_b <- lapply(tmp_a, FUN = function(x){
    return(x %>% str_split(., pattern = "=") %>% unlist())
  })
  tmp_c <- do.call('rbind', tmp_b) %>% t() %>% as.data.frame()
  colnames(tmp_c) <- tmp_c[1,]
  tmp_c <- tmp_c[-1,]
  return(tmp_c[info])
}
# Usage example
#info_extract(data = chr22vcf$info[15], info = "VT")


# adding a random comment

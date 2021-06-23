#' Print summary of the vcf file
#'
#' This function prints descriptive summary from a vcfR that is created
#' with vcfR2tidy() function from vcfR package
#'
#' The function takes the vcfR file and type of the summary as an input.
#' Possible argumets for an input file are "variant", "structural", "both" and
#' "gt", argument "both" is a default value. Options variant and structural
#' print how many different (structural) variants are possible based on
#' the fixed file data from the tidyvcfR object. The "gt" option calls for
#' another function count.variants() to calculate how many times each variant type
#' occurs in the genotypes among all individuals in the dataset. This counting
#' function returns a list of the counts, which is then used to print the summary
#' numbers of the variants.
#'
#' @section Warning:
#' Function requires tidyvcfR object as an input
#'
#' @param x tidyvcfR object
#' @return Outputs to console.
#' @export



print.summary <-  function(vcf, type = "both"){
  if(type == "variant"){
    print("Different variants:", quote = FALSE)
    M <- as.matrix(sort(table(vcf$fix$VT), decreasing = TRUE))
    colnames(M) <- "Frequency"
    print(M)
  }
  if(type == "structural"){
    print("Different structural variants:", quote = FALSE)
    N <- as.matrix(sort(table(vcf$fix$SVTYPE), decreasing = TRUE))
    colnames(N) <- "Frequency"
    print(N)
  }
  if(type == "both"){
    M <- as.matrix(sort(table(vcf$fix$VT), decreasing = TRUE))
    colnames(M) <- "Frequency"
    N <- as.matrix(sort(table(vcf$fix$SVTYPE), decreasing = TRUE))
    colnames(N) <- "Frequency"
    print("Different variants:", quote = FALSE)
    print(M)
    print("Different structural variants:", quote = FALSE)
    print(N)
  }
  if(type == "gt"){
    x <- count.variants(vcf = vcf)
    print("Total counts of different variants among all individuals in this dataset:", quote = FALSE)
    for (i in 1:length(x)) {
      print(paste0(names(x[i]), "s:", x[[i]] ), quote = FALSE)
    }
  }
}

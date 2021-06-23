#' Counting variant types that occur in the individual genotypes
#'
#' This function is used to calculate how many times each variant type
#' occurs in the genotype among all individuals in the input dataset.
#'
#' The information is saved to a list, which has names for different variant types
#' And then string of the number for counts how many times each variation occurs
#' in the data
#' @section Warning:
#' The function is very slow, big datasets take a lot of time
#'
#' The function requires a tidyvcfR file as an input
#'
#' @param x A number.
#' @return Returns a list object, call function to variable
#' @examples
#' count.variants(data_subset)
#'
#' @export

count.variants <- function(vcf){
  VP <- list() #empty list for collecting variant types and counting them
  gt1 <- vcf$gt[grep(1, vcf$gt$gt_GT),] #separate df for all mutations from individuals
  for (i in 1:nrow(gt1)) {  #for loop to find mutation positions from gt
    row <- which(grepl(gt1$POS[i], vcf$fix$POS)) #find the row number from fix file for POS
    x <- vcf$fix$VT[row] #find the variant type(s)
    for (j in 1:length(x)) { #another loop in case two or more variant types share a position
      if(x[j] %in% names(VP) == TRUE){ #if list() includes the variant, add one
        VP[[x[j]]] <- c(VP[[x[j]]] +1)
      }
      if(x[j] %in% names(VP) == FALSE){ #if the list does not include the variant,
        VP[[x[j]]] <- 1                 #add variant type and one
      }
    }
  }
  return(VP)
}


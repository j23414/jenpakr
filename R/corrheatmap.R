#! /usr/bin/env Rscript

library(magrittr)

#' Converts HI Titer values to generalized mean titer (GMT)
#' @param hi_titers HI Titer values, assumes all titers are above 0
#' @return The GMT titer = log(HI titer/ 10, 2)
#' @export
HI_to_GMT <- function(hi_titers){
  gmt <- log(hi_titers/10, 2)
  return(gmt)
}

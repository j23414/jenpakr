#! /usr/bin/env Rscript

library(tidyverse)
library(magrittr)

# === Functions

#' Format ViPR dates (MM/DD/YYYY) into some form of YYYY-MM-DD, with unknown set to XX
#' @param vc Character vector containing the vipr date such as "01/30/2022" or "01/2022"
#' @param delim Delimiter of date values, which is "/" by default but could also be "-"
#' @return The formatted date in "YYYY-MM-DD" format
#' @export
format_vipr_date <- function(vc, delim="/"){
  # If vc is NA, return early
  if (is.na(vc)){
    return("XXXX-XX-XX")
  }
  if ( vc == "-N/A-") {
    return("XXXX-XX-XX")
  }
  # If vc is delimited
  if (grepl(delim, vc)) {
    vc_temp <- vc %>%
      stringr::str_split(., delim, simplify = T) %>%
      as.vector(.)
    # MM/DD/YYYY
    if (length(vc_temp)==3){
      vc <- vc_temp %>% 
        { c(.[3], .[1], .[2]) } %>%
        paste(., collapse = "-", sep = "")
    }
    # MM/YYYY
    if (length(vc_temp)==2){
      vc <- vc_temp %>% 
        { c(.[2], .[1] , "XX") } %>%
        paste(., collapse = "-", sep = "")
    }
    
    return(vc)
  }
  # YYYY
  if (str_length(vc)==4){
    return(paste(vc, "-XX-XX", sep=""))
  }
  # If not recognized, return original date string
  return(vc)
}

#' Format NCBI dates (YYYY-MM-DD) into some form of YYYY-MM-DD, with unknown set to XX
#' @param vc Character vector containing the ncbi date such as "2022-01-30" or "2022-01"
#' @return The formatted date in "YYYY-MM-DD" format
#' @export
format_ncbi_date <- function(vc){
  # If NA, return early
  if(is.na(vc)){
    return(vc)
  }
  # If only YYYY, add unknown month day
  if(str_length(vc)==4){
    return(paste(vc, "-XX-XX", sep=""))
  }
  # If only YYYY-MM, add unknown day
  if(str_length(vc)==7){
    return(paste(vc, "-XX", sep=""))
  }
  # If not recognized, return original date string
  return(vc)
}

# This is pathogen specific
#guess_name <- function(gb_title){
#  if(grepl("Human orthopneumovirus strain ", gb_title)){
#    new_name = gb_title %>%
#      gsub("Human orthopneumovirus strain ","", .) %>%
#      gsub(",.*","", .) 
#    return(new_name)
#  }
#  if(grepl("Human respiratory syncytial virus A strain ", gb_title)){
#    new_name = gb_title %>%
#      gsub("Human respiratory syncytial virus A strain ", "", .) %>%
#      gsub(",.*", "", .)
#    return(new_name)
#  }
#  if(grepl("Human respiratory syncytial virus B strain ", gb_title)){
#    new_name = gb_title %>%
#      gsub("Human respiratory syncytial virus B strain ", "", .) %>%
#      gsub(",.*", "", .)
#    return(new_name)
#  }
#  return("")
#}
# 
# # TODO: generalize this, read in dictionary haha
# guess_genotype <- function(strain){
#   if(grepl("RSVA", strain) | grepl("RSV_A", strain) | grepl("RSV-A", strain) | grepl("/A/", strain)){
#     return("A")
#   }
#   if(grepl("RSVB", strain) | grepl("RSV_B", strain) | grepl("RSV-B", strain) | grepl("/B/", strain)){
#     return("B")
#   }
#   return("")
# }


#' Read in delimited data
#' Drop any empty columns
#' Remove spaces from column names
#' @param filename Character vector containing path to file
#' @param delim Delimiter of file, tab by default
#' @param type Basic is a basic delimited file, but special handling for ncbi or vipr
#' @return The cleaned dataframe
#' @export
read_delim_file <- function(filename, delim="\t", type="basic", col_names=TRUE) {
  df <- readr::read_delim(
    filename, 
    delim=delim,
    col_names=col_names,
    col_types = cols(.default = "c")                  # convert to characters
  ) %>%
    discard(~all(is.na(.) | . == "" | . == "-N/A-"))  # drop empty cols
  
  isNAtable = df == "-N/A-"
  df[isNAtable] = NA
  
  # Format Column names
  names(df) = names(df) %>% 
    tolower(.) %>% 
    gsub(" ", "_", .)
  
  # VIPR specific
  if(type == "vipr") {
    df = df %>%
      group_by(genbank_accession) %>%
      mutate(
        collection_date = collection_date %>% format_vipr_date(vc=., delim="/"),
        genbank=genbank_accession,
        genotype_vipr=`subtype/genotype_(vipr)`
      ) %>%
      ungroup(.) %>%
      select(-genbank_accession, -`subtype/genotype_(vipr)`, -virus_type, -mol_type) %>%
      select(genbank, collection_date, strain_name, country, everything())
  }
  
  # Fauna specific
  if(type == "fauna") {
    df = df %>%
      mutate(
        genbank=accession,
        accession=NULL,
        virus=NULL,
        db=NULL,
        segment=NULL,
        collection_date=date,
        strain_name=strain,
        date=NULL,
        strain=NULL
        )
  }
  
  return(df)
}


#' Helps harmonize or expand column names, preparing for a summarize merge later
#' @param data Data frame
#' @param cname Vector of strings of column names that need to exist in data frame
#' @return New data frame containing a merge of original columns and cname
#' @export
fncols <- function(data, cname) {
  add <- cname[!cname %in% names(data)]
  if (length(add) != 0) data[add] <- NA
  data
}

#' Merge unique items in column by group, probably used in a summarize function
#' @param vc Vector of values for a particular column
#' @param delim Delimiter between unique values, will split original string into delims, and smash them together again
#' @return New string of unique values, delimited by delim
#' @export
uniqMerge <- function(vc, delim = ",") {
  # Drop NA, drop empty strings, combine together
  vc <- vc %>%
    na.omit(.) %>%
    stringi::stri_remove_empty_na(.) %>%
    unique(.) %>%
    paste(., collapse = delim, sep = "")
  # Split, unique, delim
  # todo: refactor
  if (grepl(delim, vc)) {
    vc <- vc %>%
      stringr::str_split(., delim, simplify = T) %>%
      as.vector(.) %>%
      unique(.) %>%
      paste(., collapse = delim, sep = "")
  }
  return(vc)
}

#' Merge two dataframes
#' @param one_df First data frame to merge, col names will be listed first.
#' @param two_df Second data frame to merge, col names will be harmonized with first. 
#' @return Merged data frame
#' @export
merge_two <- function(one_df, two_df) {
  one = fncols(one_df, names(two_df))
  two = fncols(two_df, names(one_df)) %>%
    select(names(one))
  all = rbind(one, two) %>%
    group_by(genbank) %>%
    summarize_at(., vars(-group_cols()), uniqMerge) %>%
    ungroup(.)
  return(all)
}

# # === Clean Data
# readr::write_delim(keep_df, "vipr_ncbi.tsv", delim="\t")
# writexl::write_xlsx(keep_df, "vipr_ncbi.xlsx")
                 
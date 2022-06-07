#! /usr/bin/env Rscript

library(tidyverse)
library(magrittr)

# # === Inputs
# VIPR_FILE="vipr.tsv"
# NCBI_FILE="ncbi.csv"
# 
# vipr <- readr::read_delim(VIPR_FILE, delim="\t")
# ncbi <- readr::read_delim(NCBI_FILE, delim=",")
# 
# (names(vipr) = gsub(" ","_", names(vipr)))
# (names(ncbi) = gsub(" ","_", names(ncbi)))
# 
# vipr[vipr == "-N/A-"] <- NA

# === Functions

#' Format ViPR dates (MM/DD/YYYY) into some form of YYYY-MM-DD, with unknown set to XX
#' @param vc Character vector containing the vipr date such as "01/30/2022" or "01/2022"
#' @param delim Delimiter of date values, which is "/" by default but could also be "-"
#' @return The formatted date in "YYYY-MM-DD" format
#' @export
format_vipr_date <- function(vc, delim="/"){
  # If vc is NA, return early
  if(is.na(vc)){
    return(vc)
  }
  # If vc is delimited
  if (grepl(delim, vc)) {
    vc_temp <- vc %>%
      stringr::str_split(., delim, simplify = T) %>%
      as.vector(.)
    # MM/DD/YYYY
    if(length(vc_temp)==3){
      vc <- vc_temp %>% 
        { c(.[3], .[1], .[2]) } %>%
        paste(., collapse = "-", sep = "")
    }
    # MM/YYYY
    if(length(vc_temp)==2){
      vc <- vc_temp %>% 
        { c(.[2], .[1] , "XX") } %>%
        paste(., collapse = "-", sep = "")
    }
    
    return(vc)
  }
  # YYYY
  if(str_length(vc)==4){
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

guess_name <- function(gb_title){
  if(grepl("Human orthopneumovirus strain ", gb_title)){
    new_name = gb_title %>%
      gsub("Human orthopneumovirus strain ","", .) %>%
      gsub(",.*","", .) 
    return(new_name)
  }
  if(grepl("Human respiratory syncytial virus A strain ", gb_title)){
    new_name = gb_title %>%
      gsub("Human respiratory syncytial virus A strain ", "", .) %>%
      gsub(",.*", "", .)
    return(new_name)
  }
  if(grepl("Human respiratory syncytial virus B strain ", gb_title)){
    new_name = gb_title %>%
      gsub("Human respiratory syncytial virus B strain ", "", .) %>%
      gsub(",.*", "", .)
    return(new_name)
  }
  return("")
}

# TODO: generalize this, read in dictionary haha
guess_genotype <- function(strain){
  if(grepl("RSVA", strain) | grepl("RSV_A", strain) | grepl("RSV-A", strain) | grepl("/A/", strain)){
    return("A")
  }
  if(grepl("RSVB", strain) | grepl("RSV_B", strain) | grepl("RSV-B", strain) | grepl("/B/", strain)){
    return("B")
  }
  return("")
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

# # === Clean Data
# # ncbi
# cncbi <- ncbi %>% 
#   rename(
#     genbank=Accession,
#     segment=Segment,
#     country=Country,
#     species=Species,
#     length=Length,
#     genotype=Genotype
#   ) %>%
#   group_by(genbank) %>%
#   mutate(
#     Isolate=case_when(!is.na(Isolate) ~ Isolate,
#                       is.na(Isolate) ~ guess_name(GenBank_Title)),
#     strain=Isolate %>% gsub(" ", "_", .),
#     date=Collection_Date %>% format_ncbi_date(.),
#     release_date =  as.POSIXct(Release_Date, format="%Y-%m-%d") %>% as.character(.),
#     host=Host %>% gsub("Homo sapiens","Human", .),
#     genotype=case_when(!is.na(genotype)~ genotype,
#                        is.na(genotype) ~ guess_genotype(strain))
#   )  %>%
#   ungroup(.) %>%
#   select(., -c("Isolate", "Collection_Date","Host", "SRA_Accession", "species", "Molecule_type", "Genus", "Family", "BioSample", "Release_Date","Publications"))
# 
# # names(cncbi)
# #look <- cncbi %>% subset(str_length(strain)<1)
# 
# # vipr
# cvipr <- vipr %>% 
#   rename(
#     genbank=GenBank_Accession,
#     country=Country,
#     genotype=Pango_Genome_Lineage,
#     species=Virus_Species,
#     length=Sequence_Length
#   ) %>%
#   group_by(genbank) %>%
#   mutate(
#     strain=Strain_Name %>% gsub(" ", "_",.),
#     host=Host %>% gsub("Unknown", "", .),
#     date=Collection_Date %>% format_vipr_date(., delim="/"),
#     genotype=case_when(!is.na(genotype)~ genotype,
#                        is.na(genotype) ~ guess_genotype(strain))
#   ) %>%
#   ungroup(.)%>%
#   select(., -c("Strain_Name", "Collection_Date", "Host", "...11", "Mol_Type", "species"))
# 
# #look <- subset(cvipr, str_length(genotype)>0)
# #names(cvipr)
# 
# cncbi = fncols(cncbi, names(cvipr)) 
# cvipr = fncols(cvipr, names(cncbi))
# 
# # ==== Merge
# name_order=c("genbank", "strain",
#              "genotype",
#              "date","release_date", 
#              "host", "GenBank_Host","Isolation_Source", 
#              "country","Geo_Location", "USA",
#              "length","LEN", "Nuc_Completeness","segment",
#              "GenBank_Title","Submitters",
#              "check")
# rest_order = setdiff(names(cvipr), name_order)
# 
# all = rbind(cvipr, cncbi) %>%
#   group_by(genbank) %>%
#   summarize_at(., vars(-group_cols()), uniqMerge) %>%
#   mutate(
#     LEN=as.numeric(length),
#     check=grepl(",",strain)
#   ) %>%
#   select(., c(name_order, rest_order))
# 
# keep_df <- all %>%
#   subset(is.na(LEN) | LEN > 10000)   # filter to length of 10K or longer
# 
# names(keep_df) <- tolower(names(keep_df))
# readr::write_delim(keep_df, "vipr_ncbi.tsv", delim="\t")
# writexl::write_xlsx(keep_df, "vipr_ncbi.xlsx")
                 
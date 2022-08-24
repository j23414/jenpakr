# Collection of plotting functions

HI Titer plots, rename this later. 

```
devtools::install_github("j23414/jenpakr")
```

During development, follow the [R Package Development Protocol](https://raw.githubusercontent.com/rstudio/cheatsheets/main/package-development.pdf).

```
devtools::load_all("~/github/j23414/jenpakr") # Or path to your code base
```

# Expected Input

For the HI plots, the expected input should be an excel file containing HI Titer Values in a table.

|   | Strain1 | Strain2 | Strain3 | Strain4 |
|---|---|---|---|---|
| Strain1 |   |   |   |   |
| Strain2 |   |   |   |   |
| Strain3 |   |   |   |   |


# Merging Datasets

```
library(jenpakr)
library(readxl)
library(tidyverse)
library(magrittr)

# ================= Read in Data to merge

# Normal excel cache
cache_data <- readxl::read_excel("path/to/cache.xlsx")

# Possible special cases
vipr_data <- jenpakr::read_delim_file("path/to/vipr.tsv", type="vipr")
ncbi_data <- jenpakr::read_delim_file("path/to/ncbi.tsv")
fauna_data <- jenpakr::read_delim_file("path/to/fauna.tsv", type="fauna")
clades_data <- jenpakr::read_delim_file("path/to/clades.csv", delim=",")

# ================== Merge files
merged_df <- cache_data %>%
  jenpakr::merge_two(., vipr_data) %>%
  jenpakr::merge_two(., ncbi_data) %>%
  jenpakr::merge_two(., fauna_data) %>%
  jenpakr::merge_two(., clades_data)

# ================== Save new cache
writexl::write_xlsx(merged_df, "new_cache.xlsx")
```
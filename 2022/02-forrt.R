# R script for "Open and Reproducible Research Glossary"
# URL: https://erikgahner.dk/2022/open-and-reproducible-research-glossary/

library("tidyverse")
library("gt")

download.file(url = "https://github.com/forrtproject/forrtproject.github.io/archive/master.zip"
              , destfile = "forrtproject.github.io-master.zip")

unzip(zipfile = "forrtproject.github.io-master.zip")

setwd("forrtproject.github.io-master/content/glossary/")

get_definition <- function(file) {
  
  glossary_file <- jsonlite::read_json(file, simplifyVector = TRUE)
  
  return(data.frame(title = glossary_file$title, definition = glossary_file$definition))
  
}

glossary_files <- list.files()[-1]

glossary_df <- map_df(glossary_files[str_detect(glossary_files, ".md")], get_definition)

glossary_df |> 
  gt() |> 
  cols_label(title = "Title",
             definition = "Definition") |> 
  gtsave("../../../forrt_glossary.htm")
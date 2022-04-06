#' Assemble Markdown Files into a Larger Document
#'
#' @param title The title of the Larger Document.
#' @param author The author of the Larger Document.
#' @param directory The path to the directory containing the Markdown Files.
#'
#' @importFrom magrittr %>%
#' @importFrom stringr str_sub
#' @importFrom stringr str_c
#' @importFrom readr read_file
#' @importFrom rmarkdown render
#' @importFrom rmdformats downcute
#'
#' @return A Larger Document- e.g. a HTML web page- with the content of the Markdown Files.
#' @export
#'
markdown_assemble <- function(title, author, directory){

  yaml_header <- c('---',
                   paste0('title: "', title, '"'),
                   paste0('author: ', noquote(author)),
                   'date: "`r Sys.Date()`"',
                   noquote('output:
rmdformats::downcute:
  highlight: tango
---

```{r setup, include=FALSE}
## Global options
knitr::opts_chunk$set(cache = TRUE)
```'),
                   collapse = "\r\n\r\n")

  file_selection_pattern <- "[[:xdigit:]]*.md"

  output_file_name <- paste0(title, ".Rmd")

  markdown_file_list <- list.files(path = directory,
                                   pattern = file_selection_pattern)
  markdown_file_list <- paste0(directory, markdown_file_list)

  date_order <- str_sub(markdown_file_list, start = -11, end = -4) %>%
    order(decreasing = TRUE)

  markdown_file_list <- markdown_file_list[date_order]
  markdown_files <- lapply(markdown_file_list, read_file) %>% cbind() %>%
    unlist() %>% paste0("\r\n")

  markdown_file <- str_c(c(yaml_header, markdown_files),
                         collapse = "\r\n")

  output_file <- file(description = output_file_name)
  writeLines(markdown_file, output_file)
  close(output_file)

  #options(knitr.duplicate.label = "allow")
  render(input = output_file_name, output_format = "rmdformats::downcute",
         output_dir = directory)

  file.remove(output_file_name)
}

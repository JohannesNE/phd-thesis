library(googledrive)
library(tidyverse)

files <- drive_find("PhD \\d+ -")

files$name |> walk(~drive_download(.x, path = paste0("temp_docs/", .x), type = "txt",
                                   overwrite = TRUE))

docs <- list.files("temp_docs", full.names = TRUE)

lines <- map(docs, read_lines)
map(lines, nchar) |> 
  unlist() |> 
  sum()

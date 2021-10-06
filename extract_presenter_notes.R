library(stringr)
library(purrr)
library(dplyr)


yaml_header <- readLines("presenter_notes_yaml_header.txt")

webinar_lines <- readLines("ipumsr_webinar.Rmd") %>% 
  paste0(collapse = "\n") %>% 
  str_split("\n---\n") %>% 
  .[[1]] %>% 
  str_trim()

webinar_slides <- tibble(raw = webinar_lines)

presenter_notes <- webinar_slides %>% 
  mutate(
    heading = str_match(raw, "((?<!#)# .+?)(?:$|\n)")[, 2], # "(?:...)" makes a "non-capturing group"
    notes = str_split_fixed(raw, "\\?\\?\\?\n+", n = 2)[, 2]
  ) %>% 
  mutate(
    heading = if_else(is.na(heading), "# NO HEADING", heading),
    heading = paste0(heading, " (", row_number(), ")"),
    heading_plus_notes = paste0(heading, "\n\n", notes, "\n\n")
  ) %>% 
  pull(heading_plus_notes) 


writeLines(
  c(yaml_header, "\n", presenter_notes),
  "ipumsr_webinar_presenter_notes.Rmd"
)

rmarkdown::render("ipumsr_webinar_presenter_notes.Rmd")

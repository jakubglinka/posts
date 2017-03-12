

stri_extract_chapter <- function(text) {
  
  stri_extract_first_regex(text, "<h3>[^<]*</h3>") %>%
    stri_replace_all_regex("<h3>|</h3>","") %>%
    stri_last_NA
  
}

stri_extract_chapter_title <- function(text) {
  
  stri_extract_first_regex(text, "<h3>\\d*\\.*\\s*</*i*b*>.*</h3>") %>%
    stri_replace_all_regex("<[^<>]*>","") %>%
    stri_last_NA
  
}


stri_last_NA <- function(text) {
  
  last.code <- ""
  text %>% sapply(function(xx) {
    if (is.na(xx)) {
      last.code
    } else {
      last.code <<- xx
      xx
    }
  })
  
}



library(shiny)
library(tidyverse)
library(stringr)
library(ggplot2)
library(gtools)
library(hunspell)
library(wordcloud)

possible_words <- function(letters_str, sizes){
  
  # Validation
  stopifnot(is.character(letters_str),
            between(str_length(letters_str), 3, 7),
            is.numeric(sizes))
  
  # split up the string into a vector of characters
  letters_vector <- str_split(letters_str, boundary("character"))[[1]]
  
  # compute all of the 3 letter permutations
  perms <- permutations(n = length(letters_vector),
                        r = sizes[1],
                        v = letters_vector,
                        set = FALSE)
  
  # flatten each permutations back into a "word vector"
  word_list <- apply(perms, 1 , str_flatten)
  
  # convert to a tibble date frame, and filter out mispelled words
  words_df <- tibble(word_list = word_list) %>%
    filter(hunspell_check(words = word_list))
  
  # Repeat for words of size 4,5,... letters
  if (length(sizes) > 1) {
    for (i in 2:length(sizes)) {
      size = sizes[i]
      perms <- permutations(n = length(letters_vector),
                            r = sizes[i],
                            v = letters_vector,
                            set = FALSE)
      
      word_list <- apply(perms, 1 , str_flatten)
      
      more_words <- tibble(word_list = word_list) %>%
        filter(hunspell_check(words = word_list))
      words_df <- rbind(words_df, more_words)
    }
  }
  
  # Return only a distinct set of words
  words_df %>% 
    distinct()
}

define_word <- function(word_to_lookup = "cat", lang_arg = "lang=en"){
  
  base_url <- "https://googledictionaryapi.eu-gb.mybluemix.net/?define="
  full_url <- paste0(base_url, word_to_lookup, "&", lang_arg)
  
  reponse <- httr::GET(full_url)
  
  parsed_repsonse <- httr::content(reponse)
  
  meaning <- parsed_repsonse[[1]]$meaning
  
  fields <- meaning %>% names()
  
  
  # print the definitions
  word_def <- paste(fields[1], ":", meaning[[1]][[1]]$definition)
  for (i in fields) {
    for (j in 1:length(meaning[[i]])) {
      next_def <- paste(i, ":", meaning[[i]][[j]]$definition)
      word_def <- paste(word_def, next_def, collapse = "\n")
      # print(paste(i, ":", meaning[[i]][[j]]$definition))
    }
  }
  
  word_def
  
}

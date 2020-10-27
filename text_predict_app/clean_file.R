library(tm)
library(qdap)
conts <- read.csv('data/contractions.csv', header = T)
names(conts) <- c('conts','words')
conts[] <- lapply(conts, as.character)
profs <- read.csv('data/bad_words2.csv', header = T)
profs$word <- as.character(profs$word)
removeProf <- function(x){ multigsub(profs$word, '', x) } # Profanities
removeExtras <- function(x){return( gsub('#[a-zA-Z0-9]+( ?) | 
                                          ( ?)(f|ht)tp(s?)://.*[.][^ ]* |
                                          \"|\'','',x) )} # hashtags, url, apostrophe and quotations
removeConts <- function(x){ multigsub(conts$conts, conts$words, x) } # contractions
removeNoeng <- function(x){return( gsub('\\W+',' ',x) )} # non-english
file_clean <- function(x){
  x <- x %>% removeConts() %>% removeProf() %>% 
    removeExtras() %>% removeNumbers() %>% removeNoeng() %>% stripWhitespace()
  return(x)
}

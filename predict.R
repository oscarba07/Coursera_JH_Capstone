s1 <- Sys.time()
setwd('D:/Oscar/Documents/JH Data Science/Capstone')
library(zip)
flist <- zip_list('Coursera-Swiftkey.zip')[,1]
us.fl <- flist[grep('en_US.*txt',flist)]
us.fl

library(magrittr) # For the %>% operator
library(tibble) # For the tibble data frame
library(stringi) # Loads stri_stats_latex function
file_read <- function(file_path){
  file_name <- substr(file_path,
                      start=regexpr('[.][a-zA-Z]+[.]txt', file_path)[1]+1,
                      stop=regexpr('[.]txt', file_path)[1]-1)
  con <- file(description = file_path, 'rb', encoding = "UTF-8") 
  sauce <- con %>% readLines(encoding = "UTF-8")
  file_size <- object.size(sauce)[1]/2^20
  sauce <- sauce %>% tibble()
  close(con)
  file_lines <- dim(sauce)[1]
  file_words <- stri_stats_latex(sauce$.)['Words']
  m <- data.frame('file'=file_name, 'size.mb'=file_size,'lines'=file_lines, 'words'=file_words)
  rownames(m) <- NULL
  l <- list('sum'=m,'sauce'=sauce)
  return(l)
}

en.data <- lapply(us.fl,file_read)
data.sum <- rbind(en.data[[1]]$sum,en.data[[2]]$sum,en.data[[3]]$sum)
names(en.data) <- data.sum$file
data.sum

library(tm)
library(qdap)
conts <- read.csv('contractions.csv', header = T)
names(conts) <- c('conts','words')
conts[] <- lapply(conts, as.character)
profs <- read.csv('bad_words2.csv', header = T)
profs$word <- as.character(profs$word)
removeProf <- function(x){ multigsub(profs$word, '', x) } # Profanities
removeExtras <- function(x){return( gsub('#[a-zA-Z0-9]+( ?) | 
                                          ( ?)(f|ht)tp(s?)://.*[.][^ ]* |
                                          \"|â|\'','',x) )} # hashtags, url, apostrophe and quotations
removeConts <- function(x){ multigsub(conts$conts, conts$words, x) } # contractions
removeNoeng <- function(x){return( gsub('\\W+',' ',x) )} # non-english
file_clean <- function(x){
  x <- x %>% removeConts() %>% removeProf() %>% 
    removeExtras() %>% removeNumbers() %>% removeNoeng() %>% stripWhitespace()
  return(x)
}

s1 <- Sys.time()
cleaned.data <- lapply(en.data, FUN=function(x){x$sauce$.}) %>% 
  lapply(., FUN=function(x){sample(x,size=0.025*length(x), replace = F)} ) %>% 
  lapply(., FUN=file_clean) %>% lapply(., function(x){enframe(x, value = '.')})

crp <- do.call(rbind, cleaned.data)

rm(en.data, cleaned.data)



library(tidytext)
unig <- unnest_tokens(crp, input=., output=ngram, token='ngrams', n=1, format = 'text', to_lower = T)
big <- unnest_tokens(crp, input=., output=ngram, token='ngrams', n=2, format = 'text', to_lower = T)
trig <- unnest_tokens(crp, input=., output=ngram, token='ngrams', n=3, format = 'text', to_lower = T)
quadg <- unnest_tokens(crp, input=., output=ngram, token='ngrams', n=4, format = 'text', to_lower = T)

library(dplyr)
unig.f <- count(unig, ngram, sort=T)
big.f <- count(big, ngram, sort=T)
trig.f <- count(trig, ngram, sort=T)
quadg.f <- count(quadg, ngram, sort=T)
s2 <- Sys.time()
s2-s1




t <- 'you can do'

s1 <- Sys.time()

t <- file_clean(t) %>% tolower(.)
wrds <- unnest_tokens(enframe(t), output = words, input=value)
n <- length(wrds$words)
if (n==0){
  pr <- 'Please enter text'
} else {
  if (n>=3) {
    t.3 <- paste(wrds$words[(n-2):n], collapse = ' ')
    p <- head(quadg.f[grep(paste('^',t.3,' ',sep=''),quadg.f$ngram),],3)
    if(dim(p)[1]<3) {
      t.2 <- paste(wrds$words[2:n], collapse = ' ')
      p <- rbind(p,head(trig.f[grep(paste('^',t.2,' ',sep=''),trig.f$ngram),],3-dim(p)[1]))
      if(dim(p)[1]<3) {
        t.1 <- wrds$words[n]
        p <- rbind(p,head(big.f[grep(paste('^',t.1,' ',sep=''),big.f$ngram),],3-dim(p)[1]))
        if(dim(p)[1]<3) {
          p <- rbind(p,head(unig.f,3-dim(p)[1]))
        }
      }
    }
  } else {
    if(n==2){
      p <- head(trig.f[grep(paste('^',t,' ',sep=''),trig.f$ngram),],3)
      if(dim(p)[1]<3) {
        t.1 <- wrds$words[n]
        p <- rbind(p,head(big.f[grep(paste('^',t.1,' ',sep=''),big.f$ngram),],3-dim(p)[1]))
        if(dim(p)[1]<3) {
          p <- rbind(p,head(unig.f,3-dim(p)[1]))
        }
      }
    } else{
      if(n==1){
        p <- head(big.f[grep(paste('^',t,' ',sep=''),big.f$ngram),],3)
        if(dim(p)[1]<3) {
          p <- rbind(p,head(unig.f,3-dim(p)[1]))
        }
      }
    }
  }
  pr <- stri_extract_last_words(p$ngram)
}
pr
s2 <- Sys.time()
s2-s1


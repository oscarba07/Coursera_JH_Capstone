library(tidytext)
library(stringi)
library(tibble)
load('grams.Rdata')
t.pred <- function(t){
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
  return(pr)
}

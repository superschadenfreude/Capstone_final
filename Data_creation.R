### ATS

## 2.0 Read the data as lines in one single doc. I already download the zip file 
## from: https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip


EN_blogs <- readLines("en_US.blogs.txt", encoding="UTF-8", skipNul=TRUE)
EN_news <- readLines("en_US.news.txt", encoding="UTF-8", skipNul=TRUE)
EN_twitter <- readLines("en_US.twitter.txt", encoding="UTF-8", skipNul=TRUE)
EN <- list(EN_blogs,EN_news, EN_twitter)

## 3.0 Obtain the sample (because the size of the file I considered 10%)

set.seed(9999)
### Only for dev, I selected 0.001, MUST BE 0.1
S_EN <- mapply(sample, EN, size=(max(lengths(EN))*.1)) 

require(quanteda)
Mydfm <- S_EN %>% 
 tokens(remove_numbers=T, remove_punct=T, remove_symbols=T, remove_hyphens=T, remove_twitter=T, remove_url=T)
##  I removed this section: %>% tokens_remove(stopwords("en"))
## 3.1 Remove files to save memory
rm(EN_blogs, EN_news, EN_twitter,EN)


## 4.0 Create tokens and convert to DFM to DF
require(dplyr)
ngram_1 <- Mydfm %>% tokens_ngrams(n = 1) %>% dfm() %>% colSums() %>% as.data.frame() %>%
   `colnames<-`(c("frecuency")) %>% slice_max(frecuency, n = 7,with_ties = FALSE)
ngram_2 <- Mydfm %>% tokens_ngrams(n = 2) %>% dfm() %>% colSums() %>% as.data.frame() %>%
   `colnames<-`(c("frecuency")) %>% filter (frecuency > 10)
ngram_3 <- Mydfm %>% tokens_ngrams(n = 3) %>% dfm() %>% colSums() %>% as.data.frame() %>%
   `colnames<-`(c("frecuency")) %>% filter (frecuency > 10)
ngram_4 <- Mydfm %>% tokens_ngrams(n = 4) %>% dfm() %>% colSums() %>% as.data.frame() %>%
   `colnames<-`(c("frecuency")) %>% filter (frecuency > 10)

## 5.0 Convert DF to columns
require(tidyr)
require(stringr)
ngram_1$sentence <- rownames(ngram_1)
names(ngram_1)[2] <-c("Word1")
ngram_1 <- ngram_1 %>% slice_max(frecuency, n = 5,with_ties = FALSE)
ngram_2$sentence <- rownames(ngram_2)
ngram_2 <- ngram_2 %>% separate(sentence, c("Word1","Word2"),"_") 
ngram_3$sentence <- rownames(ngram_3)
ngram_3 <- ngram_3 %>% separate(sentence, c("Word1","Word2", "Word3"),"_")
ngram_4$sentence <- rownames(ngram_4)
ngram_4 <- ngram_4 %>% separate(sentence, c("Word1","Word2", "Word3","Word4"),"_") ###ATS

## 6.0 Save n grams for the shinny app
saveRDS(ngram_1, file = "ngram_filter_1.RDS")
saveRDS(ngram_2, file = "ngram_filter_2.RDS")
saveRDS(ngram_3, file = "ngram_filter_3.RDS")
saveRDS(ngram_4, file = "ngram_filter_4.RDS")













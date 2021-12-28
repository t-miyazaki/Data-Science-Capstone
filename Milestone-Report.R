# setup
setwd("~/Desktop/Coursera"); set.seed(0)
library(tidyverse); library(quanteda); library(quanteda.textplots)
library(ngram); library(textclean); library(sentimentr)

# loading_data
processFile <- function(path){
        txts <- scan(path, what = character(), 
                     sep = "\n", blank.lines.skip = TRUE,
                     skipNul = TRUE, quiet = TRUE)
        return(txts)
}

file_paths <- list.files(path = "./Word-Prediction",
                         pattern = "txt",
                         full.names = TRUE)
file_list <- lapply(file_paths, processFile)
file_names <- c("blogs", "news", "twitter")
names(file_list) <- file_names

# file paths
file_paths

# wordcount
wordcountFile <- function(file){
        n <- wordcount(file)
        n_sum <- sum(n, na.rm = TRUE)
        return(n_sum)
}
wordcount_list <- lapply(file_list, wordcountFile)
wordcount_list

# linecount
linecount_list <- lapply(file_list, length)
linecount_list

# 10% sampling and tokenizing
tokenizeFile <- function(files, p = 0.1){
        docs <- unlist(files)
        size <- length(docs) * p
        docs <- sample(docs, size = size)
        docs <- replace_non_ascii(docs)
        corp <- corpus(docs)
        toks <- tokens(corp, remove_punct = TRUE)
        
        # removing bad words
        pwords <- lexicon::profanity_alvarez
        toks <- tokens_remove(toks, pattern = pwords)
        return(toks)
}

toks <- tokenizeFile(file_list, p = 0.1)
toks

# constructing a document-feature matrix
dfmat <- dfm(toks)
topwords <- topfeatures(dfmat, 1000)
df1 <- data.frame(topwords) %>%
        mutate(proportion = topwords/sum(dfmat)) %>%
        mutate(cum_sum = cumsum(proportion))
head(df1); tail(df1) # Top 1000 words cover 70% of all words instances
plot(topwords, main = "Top 1,000 Word Count")
textplot_wordcloud(dfmat, min_count = 1000)

# generating n-grams (n = 2)
toks_ngrams <- tokens_ngrams(toks, n = 2)
dfmat_ngrams <- dfm(toks_ngrams)
topngrams <- topfeatures(dfmat_ngrams, 1000)
df2 <- data.frame(topngrams) %>%
        mutate(proportion = topngrams/sum(dfmat_ngrams)) %>%
        mutate(cum_sum = cumsum(proportion))
head(df2); tail(df2)
plot(topngrams, main = "Top 1,000 Bigram Count")

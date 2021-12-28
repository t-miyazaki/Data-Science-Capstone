set.seed(0)
library(shiny)
library(tidyverse)
library(quanteda)
library(quanteda.textplots)
library(ngram)
library(textclean)
library(sentimentr)
library(data.table)
library(caTools)

# Define server logic required for next word prediction
shinyServer(function(input, output) {
    output$table <- renderTable({
        processFile <- function(path){
            txts <- scan(path, what = character(), 
                         sep = "\n", blank.lines.skip = TRUE,
                         skipNul = TRUE, quiet = TRUE)
            return(txts)
        }
        
        if (!file.exists("data.txt")){
            file_paths <- list.files(pattern = "txt")
            file_list <- lapply(file_paths, processFile)
            file_names <- c("blogs", "news", "twitter")
            names(file_list) <- file_names
            # sampling
            p = 0.001
            docs <- unlist(file_list)
            n <- as.integer(length(docs) * p)
            docs <- sample(docs, size = n)
            write(docs, file = "data.txt")
            rm(file_list)
        }
        
        docs <- processFile("data.txt")
        
        # tokenizing
        tokenizeFile <- function(docs){
            docs <- replace_non_ascii(docs)
            corp <- corpus(docs)
            toks <- tokens(corp, 
                           remove_punct = TRUE,
                           remove_symbols = TRUE,
                           remove_numbers = TRUE)
            
            # removing bad words
            pwords <- lexicon::profanity_alvarez
            toks <- tokens_remove(toks, pattern = pwords)
            return(toks)
        }
        
        # constructing a document-feature matrix of n-grams
        dfmatFile <- function(toks, ng){
            ngrams <- tokens_ngrams(toks, n = ng)
            dfmat <- dfm(ngrams)
            return(dfmat)
        }
        
        toks <- tokenizeFile(docs)
        dfmat1 <- dfmatFile(toks, 1) # dfm of 1-grams
        dfmat2 <- dfmatFile(toks, 2) # dfm of 2-grams
        dfmat3 <- dfmatFile(toks, 3) # dfm of 3-grams
        
        # counting n-gram frequency
        ngramFreq <- function(dfmat){
            freq <- colSums(dfmat)
            return(data.table(term = names(freq), count = freq))
        }
        
        freq1 <- ngramFreq(dfmat1)
        freq2 <- ngramFreq(dfmat2)
        freq3 <- ngramFreq(dfmat3)
        
        # removing uncommon n-grams
        min_count <- 1
        freq1 <- freq1[count > min_count]
        freq2 <- freq2[count > min_count]
        freq3 <- freq3[count > min_count]
        
        # adding columns of (n-1)-grams and the last word
        processFreq <- function(freq){
            split_ngrams <- str_split(freq$term, "_")
            len <- length(split_ngrams)
            ng <- length(split_ngrams[[1]])
            n_1_grams <- vector("list", len)
            last_words <- vector("list", len)
            for (i in 1:len){
                n_1_grams[[i]] <- str_flatten(split_ngrams[[i]][-ng], collapse = " ")
                last_words[[i]] <- split_ngrams[[i]][ng]
            }
            dat <- mutate(freq, 
                          pre.words = n_1_grams,
                          last.word = last_words) %>%
                arrange(-count)
            return(dat)
        }
        
        freq1 <- processFreq(freq1)
        freq2 <- processFreq(freq2)
        freq3 <- processFreq(freq3)
        
        # creating a function to show top predicted words
        predictWordsList <- function(words, num = 10){
            n <- wordcount(words)
            if (n < 2){
                stop("Please enter at least 2 words!")
            }
            split <- str_split(words, pattern = " ")
            pre.words2 <- str_flatten(split[[1]][(n-1):n], collapse = " ")
            pre.words1 <- split[[1]][n]
            
            if (pre.words2 %in% freq3$pre.words){
                dat <- freq3 %>%
                    filter(pre.words == pre.words2) %>%
                    select(pre.words, last.word, count)
            }
            else if (pre.words1 %in% freq2$pre.words){
                dat <- freq2 %>%
                    filter(pre.words == pre.words1) %>%
                    select(pre.words, last.word, count)
            } 
            else {
                dat <- freq1 %>%
                    select(pre.words, last.word, count)
            }
            head(dat, n = num)
        }
        words <- input$words
        predictWordsList(words, n = input$numeric)
    })
})

# Contains functions used by the shinyServer to predict the next word.
# It also loads all required data. It is customized for the English- Spanish, 
# automatic language detecting word predictor
# 
# Created on 2017 by Reynaldo Vazquez for the purpose of developing an English- 
# Spanish, automatic language detecting word predictor
# 
# Permission is granted to copy, use, or modify all or parts of this program and  
# accompanying documents for purposes of research or education, provided this 
# notice and attribution are retained, and changes made are noted.
# 
# This program and accompanying documents are distributed without any warranty,
# expressed or implied. They have not been tested to the degree that would be 
# advisable in important tasks. Any use of this program is entirely at the 
# user's own risk.
# 
################################################################################
################################################################################
# load required data
# English data
wcode   <- read.csv("dictionary.csv", stringsAsFactors = FALSE)
pred1   <- read.csv("pred1.csv")
pred2   <- read.csv("pred2.csv")
pred3   <- read.csv("pred3.csv")
pred4   <- read.csv("pred4.csv")
predC   <- read.csv("pred1c.csv")
chCode  <- read.csv("letter_codes.csv", stringsAsFactors = FALSE)
means   <- read.csv("means.csv", stringsAsFactors = FALSE)
predTables <- c("pred1", "pred2", "pred3", "pred4", "predC") 
# Spanish data
wcode_es   <- read.csv("es_dictionary.csv", stringsAsFactors = FALSE)
pred1_es   <- read.csv("es_pred1.csv")
pred2_es   <- read.csv("es_pred2.csv")
pred3_es   <- read.csv("es_pred3.csv")
pred4_es   <- read.csv("es_pred4.csv")
predC_es   <- read.csv("es_pred1c.csv")
means_es   <- read.csv("es_means.csv", stringsAsFactors = FALSE)
predTables_es <- c("pred1_es", "pred2_es", "pred3_es", "pred4_es", "predC_es")
predCode   <- c("wcode_es", "wcode")
subs       <- read.csv("subs.csv", stringsAsFactors = FALSE)
means <- rbind(means, means_es)
predTables <- c(predTables, predTables_es)
################################################################################
################################################################################
p  <- 8  ## is the number of predictions
back_off <- c("and", "de", "will", "en", "of")  # (handpicked) use when no 
                                                # predictions are found
# initialize global variables
w_index   <- NULL
xn_glob   <- NULL
cw_index  <- NULL
cw_preds  <- NULL
EN        <- TRUE # Sets initial language to English
esPattern <- "á|é|í|ó|ú|ü|ñ"

preprocess.input <- function(x){
    # Preprocesses text. Removes not recognized objects, excessive space and 
    # converts to low caps. Carefully treats Spanish chars to not become codes. 
    # Changes language if a Spanish special char is found. Keeps ' otherwise, 
    # since in EN it is used for current word preds. For next-word preds, '
    # needs to be cleaned out, but done in main fn. 
    x <- tolower(x)
    es <- grepl(esPattern, x, perl = TRUE)
    if (es == TRUE) {
        EN <<- FALSE # a Spanish special char sets the language to EN
        x <- gsub("[!-\\/:-@\\[-`\\{-~]"," ", x, perl=T, useBytes=T)
        x <- iconv(x, to="ASCII//TRANSLIT")
        x <- gsub("[^[:alnum:][:space:]]", "", x, perl = T, useBytes = T)
    } else {
        aphos <- grepl("'", x, perl = TRUE)
        if (aphos == TRUE) {EN <<- TRUE} ## set EN if ' is found
        x <- gsub("[^[:alnum:][:space:]']"," ", x, perl=T, useBytes=T)
        # this works for Spanish and Eng the same, w/o elements in esPattern.
        # no need to change language since it is likely meaningless. Leave the '
    }
    x <- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", x, perl=TRUE, useBytes = T)
}
concatenate.valid <- function(old, toAdd){
    # concatenates vectors, checks for uniqueness and validity. (formerly
    # update index)
    #
    # Args. old: is the "old" existing vector
    #       toAdd: is the vector just found to be concatenated at the end
    new_vector  <- unique(c(old, toAdd))
    new_vector  <- new_vector[!is.na(new_vector)]
}
char.code <- function(ascii_alnum) {
    # Returns numeric code for character. Same for EN and ES
    #
    # Arg. ascii alnum: ONE alnum ascii character. No symbols
    index      <- which(ascii_alnum == chCode[,2])
    char_code  <- chCode[index, 1]}
sub.letter <- function(ascii_alnum) {
    # Returns all versions (accented and unaccented) chars used in Spanish for a 
    # sole lower-case ascii alnum. It requires the ascii-Span char relationship 
    # table
    #
    # Arg. letter: ONE alnum, lower case ascii character. 
    index   <- which(ascii_alnum == subs[,2])
    subbed  <- subs[index, 1]}
word.code <- function(word) {
    # Returns the unique code for a word. Same for EN and ES
    #
    # Arg. word: A string containing ascii lower-case letters and 0-9.
    # No symbols at all, not even '
    word       <- unlist(strsplit(word, "")) 
    word_code  <- as.numeric(unlist(sapply(word, char.code)))
    wgt        <- 1:length(word_code) ## gives a letter a different weight given 
    word_code  <- sum(sqrt(word_code/wgt))}                      ## its position
ph.num.code <- function(coded_words_vec, n){
    # Returns the unique numeric value for a phrase. Same for EN and ES
    #
    # Args. phrase: A vector of coded words returned by word.codes.vector()
    #      n: the number of words in the phrase, used for the weigths
    # Only required for phrases with more than 1 word
    wgths   <- c(1:n) + 0.01 ## gives a word different weight given its position
    phNumCode <- coded_words_vec * wgths
    phNumCode <- sum(phNumCode)
}
integer.code <- function(numCode, den = 6, xp = 10, mn) {
    # Converts the numeric value of a phrase into a valid and unique integer. 
    # Done b/c integers require less resources. Phrases are stored as
    # integers in the prediction tables. means are diff for ES and EN, thus mn 
    # must be adjusted below
    #
    # Args. numCode: the numeric code calculated by ph.num.code()
    #       den, xp: are hand picked parameters, they differ only for the compl-
    #                ementary predictions. 
    #       mn: is the language relative index of the corresponding mean 
    mn <- mn + (5*(!EN))
    mean_n   <- means[(mn),1]
    procCode <- (numCode-mean_n)/mean_n
    procCode <- procCode*(10^(xp))/den
    procCode <- round(procCode)
    intCode  <- suppressWarnings(as.integer(procCode))
}
pred.index   <- function(xn, predn, n, den = 6, xp = 10, lc = 6, mn) {
    # Finds the word indexes for the predictions. predn is language specific,
    # but it is correctly carried from scan.predictions()
    if (n > 1){
        phNumCode <- ph.num.code(xn, n)
    } else {phNumCode <- xn}
    intCode <- integer.code(phNumCode, den = den, xp = xp, mn = mn)
    index   <- which(predn[,1] == intCode)
    w_index <- unlist(predn[index, 2:lc])
}
scan.predictions <- function(xn, n, length_preds){
    # Scans through prediciton tables finding the most possible predictions up 
    # to "p", given the parameters.
    scan_index <- NULL
    while (length_preds < p & n > 0) {
        prednIdx <- n + (5*(!EN))
        predn    <- get(predTables[prednIdx])
        temp_idx <- pred.index(xn = xn, predn = predn, n = n, mn = n)
        scan_index   <- concatenate.valid(scan_index, temp_idx)
        length_preds <<- length_preds + length(scan_index)
        if (n == 1 & length_preds < p){
            pred     <- get(predTables[(4+prednIdx)])
            temp_idx <- pred.index(xn = xn, predn = pred, n = n, 
                                   xp = 9, den = 2, mn = 5, lc = 3)
            scan_index   <- concatenate.valid(scan_index, temp_idx)
            length_preds <<- length_preds + length(scan_index)
        }
        n  <- n -1
        xn <- xn[-1]
    }
    return(scan_index)
}

predict.both <- function(x){
    ## Provides: Suggestions to complete, or correct, the word being typed and
    ## Predicts next word for x. Formerly two separate functions, thus the name. 
    ## 
    ## Arg. x: A phrase of one or more words
    ## 
    ## To predict current word, it separates the last set of chars. 
    ## If there are more than 1 set of chars. It uses all the previous chars to 
    ## the last set to predict what are the words most likely to follow. Then it 
    ## looks for matches among those predicted words, then in a prob ranked
    ## dictionary.
    phrase      <- preprocess.input(x)
    words_orig  <<- unlist(strsplit(phrase, " "))
    wco         <<- length(words_orig)
    words  <<- gsub("'", "", words_orig, perl=T, useBytes = T) # good for EN & ES
    if (wco > 3) {words <- words[(wco-3):wco]}                # next-word preds
    xs  <- sapply(words, word.code)
    xn  <- xs
    wc  <- length(xs)
    n   <- wc
    length_preds  <<- 0
    w_index       <- scan.predictions(xn, n, length_preds)
    retrieveTable <- get(predCode[(EN+1)])
    predictions   <- retrieveTable[w_index,]
    length_1      <- length_preds
    if (length_preds < 6){
        EN <<- !EN
        w_index <- scan.predictions(xn, n, length_preds)
        retrieveTable <- get(predCode[(EN+1)])
        predictions   <- concatenate.valid(predictions, retrieveTable[w_index,])
        length_preds  <<- length(predictions)
        if ((length_preds - length_1) < 3 ){
            EN <<- !EN # switch back if not more than 2 addt'l preds were found
        }
    }
    ## Next: retrive matches for the current word in the dictionary
    current <<- NULL
    grepEn  <<- "^%%X" ## symbols that will not be matched
    grepEs  <<- "^%%X"
    current <<- first.current()
    length_1 <- length(current)
    if (length_1 < 3){
        EN <<- !EN                        # switch the language
        current <- first.current()        # find matches with updated language
        length_2 <- length(current)
        if ((length_2 - length_1) < 2 & length_1 > 1 ) { # if the difference is 
            EN <<- !EN        # small, switch language back but w/ conditions
        }
    }
    ## The following should be done, regardless of length_preds and lang, so it
    ## can be used by the current predictor. It is also used as a back-off 
    ## method for the next word
    if (wco > 1 & identical(xn_glob, xs[-wc]) == FALSE){
        xn_glob  <<- xs[-wc]  ## ignore last word. Make a global var so that it 
                              ## won't need to be recalculated over and over
        cw_index <<- scan.predictions(xn = xn_glob, n = (wc-1), 
                                      length_preds = 0) 
        ## pretend there's no predictions so that it makes a full search.
        ## later on, can adjust p as a parameter so that it does more searches
        cw_preds <<- retrieveTable[cw_index,]
    }
    if (wco > 1){
        predictions  <- concatenate.valid(predictions, cw_preds)
        length_preds <<- length(predictions)
        grepBi <- paste0(grepEn, "|", grepEs)
        toAttach <- grep(grepBi, cw_preds, perl=TRUE, ignore.case = TRUE, 
                          value = TRUE)
        current <- concatenate.valid(toAttach, current)   
    }
    length_1 <- length(current)
    if (length_1 < 4) {
        current <- second.current(current)
        if (length(current) < 3) {
            EN <<- !EN
            current <- second.current(current)
            length_2 <- length(current)
            if ((length_2 - length_1) < 2 & length_1 > 1 ) { # if the difference
                EN <<- !EN  # is small, switch language back but w/conditions
            }
        }
    }
    current <- current[!is.na(current)]
    dup <- which(duplicated(tolower(current)))
    if (length(dup) > 0){current <- current[-dup]} ## this ends search for currt
    if (length_preds < p){ ## rm "s" (plurals) at end of 4 letter or longer wrds
        srem <- gsub("(?<=[[:alnum:]]{4})s", "", words, perl=TRUE,useBytes = T)
        if (identical(srem, words) == FALSE){
            xn <- sapply(srem, word.code)
            n  <- wc
            w_index <- scan.predictions(xn, n, length_preds)
            predictions <- concatenate.valid(predictions, 
                                               retrieveTable[w_index,])
            length_preds <<- length(predictions)
        }
        if (length_preds < p){
            predictions   <- concatenate.valid(predictions, back_off)
        }
    }
    dup <- which(duplicated(c(tolower(predictions), words[wco]), fromLast=TRUE))
    if (length(dup) > 0){predictions <- predictions[-dup]}
    ## this ends the search for next-word predictions
    outputLists <- list("predictions" = predictions, "current" = current)
    return(outputLists)
}
first.current <- function(){
    if (EN == TRUE){ # if lang is EN
        grepEn   <<- words_orig[wco]
        grepEn   <<- paste0("^", grepEn)
        currentn <- grep(grepEn, wcode$word[1:6000], perl = TRUE, 
                         ignore.case = TRUE, value = TRUE)
        current  <- concatenate.valid(current, currentn)
    } else { # if lang is ES
        grepEs  <<- words[wco]
        grepEs  <<- unlist(strsplit(grepEs, "")) 
        grepEs  <<- unlist(sapply(grepEs, sub.letter))
        grepEs  <<- paste(grepEs, collapse = "")
        grepEs  <<- paste0("^", grepEs)
        currentn <- grep(grepEs, wcode_es$word[1:6000], perl = TRUE, 
                         ignore.case = TRUE, value = TRUE)
        current <- concatenate.valid(current, currentn)
    }
    return(current)
}

second.current <- function(current){
    if (EN == TRUE) {
        current <- regex.searches.en(grepEn, current)
    } else {
        current <- regex.searches.es(grepEs, current)
    }
}
regex.searches.en <- function(grepEn, current){
    # Makes regex transformations and searches for new matches in the prediction
    # dictionary.
    # 
    # Args: grepEn: an "unfinised" word, current: the vector of matches so far
    grepEn <- gsub("h", "h*", grepEn, perl=TRUE)   ## mark "h" as error suspect
    grepEn <- gsub("'", "'*", grepEn, perl=TRUE)   ## mark ' as error suspect
    grepEn <- gsub("(?=[^h*^])", "h*", grepEn, perl =TRUE ) ## add an h* before
                                                   ## any char except h and *
    grepEn <- gsub("$", "h*", grepEn, perl =TRUE ) ## add an h* at the end
    grepEn <- gsub("[ck](?=[aou])", "[ck]", grepEn, perl =TRUE ) ## consider c 
                                ## and k for any c or k followed by a, o, or u
    grepEn <- gsub("[csz](?=[eih])", "[csz]", grepEn, perl =TRUE ) ## consider
                           ## c, s and z for any c, s, or z followed by e, i, h
    grepEn <- gsub("[sz](?=[aou])", "[sz]", grepEn, perl =TRUE ) ## consider
                           ## c, s and z for any c, s, or z followed by e or i
    current <- unique(c(current, grep(grepEn, wcode$word, perl = TRUE, 
                                      ignore.case = TRUE, value = TRUE)))
    if (length(current) < 4) {
        grepEn <- gsub("(?=[^'*^\\]cksz])", "'*", grepEn, perl =TRUE )
                    ## add a ' before any char except ', *, and other exceptions
        grepEn <- gsub("(?=[sk][^\\]z])", "'*", grepEn, perl=TRUE)#completes the
                                                            ## above for k and s
        grepEn <- gsub("[bv]", "[bv]", grepEn, perl = TRUE )## consider v<->b
        grepEn <- gsub("[iy]", "[iy]", grepEn, perl = TRUE )## consider i<->y
        current <- unique(c(current, grep(grepEn, wcode$word, perl = TRUE, 
                                          ignore.case = TRUE, value = TRUE)))
    }
    return(current)
}
regex.searches.es <- function(grepEs, current){
    # Makes regex transformations and searches for new matches in the prediction
    # dictionary. Customized for Spanish.
    # 
    # Args: grepEs: an "unfinised" word, current: the vector of matches so far
    grepEs <- gsub("\\[sz\\](?=\\[eé\\])|\\[sz\\](?=\\[iíy\\])", "[szc]", 
            grepEs, perl = TRUE ) ## replace [sz] followed by e or i with [szc]
    grepEs <- gsub("[bv]", "[bv]", grepEs, perl = TRUE )     ## consider v for b
    grepEs <- gsub("k(?=\\[eé\\])|k(?=\\[iíy\\])", "[kq]u*", 
                   grepEs, perl = TRUE )  ## consider qu* for k after e or i
    grepEs <- gsub("[kc](?=\\[aá\\])|[kc](?=\\[oó\\])|[kc](?=\\[uúü\\])|[kc]$", 
                   "[ck]", grepEs, perl = TRUE ) ## consider k for c, and 
    ## viceversa after a, o, u
    current <- unique(c(current, grep(grepEs, wcode_es$word, perl = TRUE, 
                                      ignore.case = TRUE, value = TRUE)))
    if (length(current) < 4){
        grepEs <- gsub("(?<=[^k])q", "[kq]", grepEs, perl = TRUE )    ## k for q
        grepEs <- gsub("\\[hj\\]", "h*", grepEs, perl=TRUE)  ## mark orig "h" 
        ## as error suspect
        grepEs <- gsub("(?=\\[)", "h*", grepEs, perl =TRUE ) ## add an h* before
        # "[" which most likely contains vowels, though can also be nñ or others
        current <- unique(c(current, grep(grepEs, wcode_es$word, perl = TRUE, 
                                          ignore.case = TRUE, value = TRUE)))
    }
    return(current)
}
################################################################################
replace.last <- function(phrase, lastword){
    # It's called when a user selects a current word option. It completes or
    # substitutes the word being typed. It requires the entire phrase and the 
    # word which has been selected.
    # 
    words  <- unlist(strsplit(phrase, " "))
    toRemove <- length(words)
    if (toRemove > 1 & lastword != ""){
        words <- words[-toRemove]
        newPhrase <- paste(paste(words, collapse = " "), lastword)
    } else if (toRemove > 0 & lastword != "") {
        newPhrase <- lastword
    } else {newPhrase <- phrase}
    return(newPhrase)
}
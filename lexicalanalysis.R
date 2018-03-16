# LEXICAL ANALYSIS

# read in bag of words
pos.words <- scan("positive-words.txt", what="character", comment.char=";")
neg.words <- scan("negative-words.txt", what="character", comment.char = ";")

# sentiment function
score.sentiment <- function(tweet_text, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  
  word.list <- str_split(tweet_text, "\\s+")
  pos_matches <- lapply(word.list, function(x) match(x, pos.words))
  neg_matches <- lapply(word.list, function(x) match(x, neg.words))
  
  pospoints <- lapply(pos_matches, function(x) !is.na(x))
  negpoints <- lapply(neg_matches, function(x) !is.na(x))
  
  possum <- unlist(lapply(pospoints, sum))
  negsum <- unlist(lapply(negpoints, sum))
  
  score <- possum - negsum
  
  vals <- as.data.frame(cbind(score, possum, negsum))
  
  return(vals)
}

# check if word is in list
# neg.words[neg.words == "idiot"]

# add new words to the bags
pos <- c(pos.words, "help")
neg <- c(neg.words, "wtf", "misogynistic", "asshole", "fuck", "misogynist", "fucked")

# use the score function
scores <- score.sentiment(df$text, pos, neg, .progress="text")

# combine original df with scores
df_analysis <- cbind(scores, df)
# CLEANING DATA, REMOVING DUPLICATES

# list to df
df <- twListToDF(tweets)

# extract text
twitter.text <- df$text

cleantext <- function(twitter.text)
{
  # emojis removed
  twitter.text <- sapply(twitter.text, function(row) iconv(row, "latin1", "ASCII", sub=""))
  
  # remove unprintable characters
  twitter.text <- gsub("[^[:print:]]", "", twitter.text)
  
  # remove URLs
  twitter.text <- gsub("http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+", "", twitter.text)
  
  # remove truncated text
  twitter.text <- gsub("[^[:space:]]*…$", "", twitter.text)
  
  # remove special characters and "#" to keep word for analysis
  twitter.text <- gsub("\\*|\\.*|#|\\\\|\"|\n|- CNET|\\||-|/r/|/|:", "", twitter.text)
  
  # remove @___ handles
  twitter.text <- gsub("@[a-zA-Z0-9_]* ", "", twitter.text)
  
  # remove puncuation, decimal #s, new line
  twitter.text <- gsub('[[:punct:]]', "", twitter.text)
  twitter.text <- gsub('[[:cntrl:]]', "", twitter.text)
  twitter.text <- gsub('\\d+', "", twitter.text)
  twitter.text <- gsub('\n', "", twitter.text) 
  
  # space at start and end of text
  twitter.text <- gsub("^ *| *$", "", twitter.text)
  
  # convert to lowercase
  twitter.text <- tolower(twitter.text)
  
  return(twitter.text)
}

# put text back into data frame
df$text <- twitter.text

# use function to clean text
df$text <- cleantext(df$text)

# remove duplicates
library(dplyr)
df <- df[!duplicated(df$text), ]

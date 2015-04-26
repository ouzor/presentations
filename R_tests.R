# R tests for TTY slides

library("dplyr")
library("ggplot2")

## Read data from the spreadsheet #######

# Following http://blog.revolutionanalytics.com/2014/06/reading-data-from-the-new-version-of-google-spreadsheets.html

# Sourcing gists does not work with two files...
# library("devtools")
# source_gist("https://gist.github.com/andrie/a9ff69a274963c70b72f#file-readgooglesheet-r")
# source_gist("https://gist.github.com/andrie/a9ff69a274963c70b72f#file-cleangoogletable-r")

source("read_google_spreadsheet.R")
gdoc.url <- "https://docs.google.com/spreadsheets/d/19DWN0n1GBOLRpttgt9oXhObs58XxlXVW7FO72t-82TQ/pubhtml"

temp <- readGoogleSheet(gdoc.url)
survey.res <- cleanGoogleTable(temp, table=1)

# # Alternative, mentioned in comments
# gdoc.key <- "19DWN0n1GBOLRpttgt9oXhObs58XxlXVW7FO72t-82TQ"
# require(RCurl)
# url <- paste0("https://docs.google.com/spreadsheets/d/",gdoc.key,"/export?format=csv&id=",gdoc.key)
# # url <- paste0("https://docs.google.com/spreadsheets/d/",gdoc.key,"/export?format=csv")
# # url <- paste0("https://docs.google.com/spreadsheets/d/",gdoc.key,"/export?format=csv&id=KEY")
# myCsv <- getURL(url,.opts=list(ssl.verifypeer=FALSE))
# test <- read.csv(textConnection(myCsv))


## Wordcloud of data science #########

library("wordcloud")
library("tm")

# Parse keywords
keywords <- survey.res[,3] %>%
  tolower %>%
  gsub(";", ",", x=.) %>%
  strsplit(split=",") %>%
  unlist %>%
  trim %>%
  table
  
# Word cloud
png("wordcloud1.png")
set.seed(123)
wordcloud(words=names(keywords), freq=keywords, scale=c(4,0.7), min.freq = 1,
          random.order=FALSE, random.color=FALSE, rot.per=0.0, 
          use.r.layout=FALSE, colors=brewer.pal(6, "Dark2"), fixed.asp=FALSE)
dev.off()

## Distribution of number of lecture participants #######

guesses <- as.numeric(survey.res[,4])
qplot(guesses)



# The input text file was created with a simple 'select all' on page
# https://www.slush.org/companies/whos-coming/
slush14.raw <- scan("../../files/data/slush2014_companies.txt",
                    what="character", sep="\n", skip=40, n=6620)
# Extract descriptions and process a bit
desc <- slush14.raw[(which(slush14.raw=="+") -1)[-1]]
desc <- tolower(gsub("[[:punct:]]", "", desc))



# Create the word cloud
wordcloud(desc, max.words=80, scale=c(7,1), 
          random.order=FALSE, random.color=FALSE, rot.per=0.0, 
          use.r.layout=FALSE, colors=brewer.pal(6, "Dark2"), fixed.asp=FALSE)








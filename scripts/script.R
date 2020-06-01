#Ladowanie bibliotek
library(tm)

#zmiana katalogu roboczego
workDir <- "C:\\Users\\mwmat\\Desktop\\PJN-Projekt28"
setwd(workDir)

#lokalizacja katalogow funkcjonalnych
inputDir <- ".\\data"
outputDir <- ".\\results"
dir.create(outputDir, showWarnings = FALSE)

#utworzenie korpusu dokumentow
corpusDir <- paste(
  inputDir,
  "tematy - oryginal",
  sep = "\\"
)
corpus <- VCorpus(
  DirSource(
    corpusDir,
    pattern = "*.txt",
    encoding = "UTF-8"
  ),
  readerControl = list(
    language = "pl_PL"
  )
)

#wstepne przetwarzanie
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, content_transformer(tolower))
stoplistFile <- paste(
  inputDir,
  "stopwords_pl.txt",
  sep = "\\"
)
stoplist <- readLines(stoplistFile, encoding = "UTF-8")
corpus <- tm_map(corpus, removeWords, stoplist)
corpus <- tm_map(corpus, stripWhitespace)

#usuniecie z tekstow em dash i 3/4
removeChar <- content_transformer(function(text,pattern) gsub(pattern, "", text))
corpus <- tm_map(corpus, removeChar, intToUtf8(8722))
corpus <- tm_map(corpus, removeChar, intToUtf8(190))

#usuniecie rozszerzen z nazw dokumentow
cutExtensions <- function(document, extension) {
  meta(document, "id") <- gsub(paste("\\.", extension, "$", sep=""),"", meta(document, "id"))
  return(document)
}
corpus <- tm_map(corpus, cutExtensions, "txt")

#usuniecie podzialu na akapity z tekstu
pasteParagraphs <- content_transformer(function(text, char) paste(text, collapse = char))
corpus <- tm_map(corpus, pasteParagraphs, " ")

#lematyzacja
polish <- dictionary(lang="pl_PL")
lemmantize <- function(text) {
  simpleText <- str_trim(as.character(text))
  vectorizedText <- str_split(simpleText, pattern = " ")
  lemmatizedText <- hunspell_stem(vectorizedText[[1]], dict = polish)
  for (i in 1:length(lemmatizedText)) {
    if (length(lemmatizedText[[i]]) == 0) lemmatizedText[i] <- vectorizedText[[1]][i]
    if (length(lemmatizedText[[i]]) > 1) lemmatizedText[i] <- lemmatizedText[[i]][1]
  }
  newText <- paste(lemmatizedText, collapse = " ")
  return(newText)
}
corpus <- tm_map(corpus, content_transformer(lemmantize))

#eksport korpousu dokumentów wstępnie potrzetworzonych
preprocessedDir <- paste(
  inputDir,
  "tematy - przetworzone",
  sep = "\\"
)
dir.create(preprocessedDir)
writeCorpus(corpus, path = preprocessedDir)
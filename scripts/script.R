#Ladowanie bibliotek
library(tm)
library(hunspell)
library(stringr)

#zmiana katalogu roboczego
workDir <- "C:\\Users\\mwmat\\Desktop\\PJN-Projekt28"
setwd(workDir)

#lokalizacja katalogow funkcjonalnych
inputDir <- ".\\data"
outputDir <- ".\\results"
workspaceDir <- ".\\workspaces"
dir.create(outputDir, showWarnings = FALSE)
dir.create(workspaceDir, showWarnings = FALSE)

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

#eksport korpousu dokumentow wstepnie potrzetworzonych
preprocessedDir <- paste(
  inputDir,
  "tematy - przetworzone",
  sep = "\\"
)
dir.create(preprocessedDir)
writeCorpus(corpus, path = preprocessedDir)

####################################################################################################################################

#utworzenie korpusu dokumentów przetworzonych
corpusDir <- paste(
  inputDir,
  "tematy - przetworzone",
  sep = "\\"
)
corpus <- VCorpus(
  DirSource(
    corpusDir,
    pattern = "*.txt",
    encoding = "windows-1250"
  ),
  readerControl = list(
    language = "pl_PL"
  )
)

#usuniêcie rozszerzeñ z nazw dokumentów przetworzonych
cutExtensions <- function(document, extension) {
  meta(document, "id") <- gsub(paste("\\.",extension,"$", sep=""),"", meta(document, "id"))
  return(document)
}
corpus <- tm_map(corpus, cutExtensions, "txt")

#tworzenie macierzy czêstoœci
tdmTfAll <- TermDocumentMatrix(corpus)
tdmTfidfAll <- TermDocumentMatrix(
  corpus, 
  control = list(
    weighting = weightTfIdf
  )
)
tdmTfBounds <- TermDocumentMatrix(
  corpus, 
  control = list(
    bounds = list(
      global = c(2,16)
    )
  )
)
tdmTfidfBounds <- TermDocumentMatrix(
  corpus, 
  control = list(
    weighting = weightTfIdf,
    bounds = list(
      global = c(2,16)
    )
  )
)
dtmTfAll <- DocumentTermMatrix(corpus)
dtmTfidfAll <- DocumentTermMatrix(
  corpus, 
  control = list(
    weighting = weightTfIdf
  )
)
dtmTfidfBounds <- DocumentTermMatrix(
  corpus, 
  control = list(
    weighting = weightTfIdf,
    bounds = list(
      global = c(2,16)
    )
  )
)

#konwersja macierzy rzadkich do macierzy klasycznych
tdmTfAllMatrix <- as.matrix(tdmTfAll)
tdmTfidfAllMatrix <- as.matrix(tdmTfidfAll)
tdmTfBoundsMatrix <- as.matrix(tdmTfBounds)
tdmTfidfBoundsMatrix <- as.matrix(tdmTfidfBounds)
dtmTfAllMatrix <- as.matrix(dtmTfAll)
dtmTfidfAllMatrix <- as.matrix(dtmTfidfAll)
dtmTfidfBoundsMatrix <- as.matrix(dtmTfidfBounds)

#eksport macirzy czêstoœci do pliku
matrixFile <- paste(
  outputDir,
  "tdmTfAll.csv",
  sep = "\\"
)
write.table(tdmTfAllMatrix, file = matrixFile, sep = ";", dec = ",", col.names = NA)

##################################################################################################

#lokalizacja katalogu ze skryptami
scriptsDir <- ".\\scripts"

#analiza g³ównych sk³adowych
pca <- prcomp(dtmTfidfBounds)

#przygotowanie danych do wkresu
legend <- paste(paste("d", 1:19, sep = ""), rownames(dtmTfidfBounds), sep = ": ")
x <- pca$x[,1]
y <- pca$x[,2]

#wykres dokumentów w przestrzeni dwuwymiarowej
options(scipen = 5)
plot(
  x,
  y,
  col = "orange",
  main = "Analiza g³ównych sk³adowych",
  xlab = "PC1",
  ylab = "PC2",
  xlim = c(-0.16,0.16),
  #ylim = c(,)
)
text(
  x,
  y, 
  paste("d", 1:19, sep = ""),
  col = "orange",
  pos = 4
)
legend(
  "bottom",
  legend,
  cex = 0.6,
  text.col = "orange"
)

#eksport wykresu do pliku .png
plotFile <- paste(
  outputDir,
  "pca.png",
  sep = "\\"
)
png(filename = plotFile)
options(scipen = 5)
plot(
  x,
  y,
  col = "orange",
  main = "Analiza g³ównych sk³adowych",
  xlab = "PC1",
  ylab = "PC2"
)
text(
  x,
  y, 
  paste("d", 1:19, sep = ""),
  col = "orange",
  pos = 4
)
legend(
  "bottom",
  legend,
  cex = 0.6,
  text.col = "orange"
)
dev.off()
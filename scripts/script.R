#Ladowanie bibliotek
library(tm)
library(hunspell)
library(stringr)
library(lsa)
library(topicmodels)
library(wordcloud)
library(proxy)
library(dendextend)
library(corrplot)
library(flexclust)

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
      global = c(1,20)
    )
  )
)
tdmTfidfBounds <- TermDocumentMatrix(
  corpus, 
  control = list(
    weighting = weightTfIdf,
    bounds = list(
      global = c(1,20)
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
      global = c(1,20)
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

################################################################################################ lsa.R
#analiza ukrytych wymiarów semantycznych (dekompozycja wg. wartoœci osobliwych)
lsa <- lsa(tdmTfidfBoundsMatrix)
lsa$tk #odpowiednik macierzy U, wspó³rzêdne wyrazów
lsa$dk #odpowiednik macierzy V, wspó³rzêdne dokumentów
lsa$sk #odpowiednik macierzy D, znaczenie sk³adowych

#przygotowanie danych do wykresu
coordTerms <- lsa$tk%*%diag(lsa$sk)
coorDocs <- lsa$dk%*%diag(lsa$sk)
terms <- c("") #tu trzeba s³owa klucze
#(harry", "czarodziej", "dumbledore", "hermiona", "ron", "komnata", "powiedzieæ", "chcieæ", "dowiadywaæ", "albus", "syriusz", "lupin", "umbridge", "edmund", "kaspian", "³ucja", "czarownica", "piotr", "zuzanna", "aslana", "narnii", "baron", "dziecko", "wyspa", "bell", "edward", "wampir", "jacob"
termsImportance <- diag(lsa$tk%*%diag(lsa$sk)%*%t(diag(lsa$sk))%*%t(lsa$tk))
importantTerms <- names(tail(sort(termsImportance),25))
coordTerms <- coordTerms[terms,]
#coordTerms <- coordTerms[importantTerms,]
legend <- paste(paste("d", 1:19, sep = ""), rownames(coorDocs), sep = "<-")
x1 <- coorDocs[,1]
y1 <- coorDocs[,2]
x2 <- coordTerms[,1]
y2 <- coordTerms[,2]

#wykres dokumentów i wybranych s³ów w przestrzeni dwuwymiatowej
options(scipen = 5)
plot(
  x1, 
  y1, 
  xlim = c(-0.2,0.05),
  #ylim = c(,),
  pch = 1, 
  col = "orange"
)
points(
  x2, 
  y2, 
  pch = 2, 
  col = "brown"
)
text(
  x1, 
  y1, 
  paste("d", 1:19, sep = ""), 
  col = "orange",
  pos = 4
)
text(
  x2, 
  y2, 
  rownames(coordTerms), 
  col = "brown",
  pos = 4
)
legend("bottomleft", legend, cex = 0.7, text.col = "orange")

#eksport wykresu do pliku .png
plotFile <- paste(
  outputDir, 
  "lsa.png",
  sep = "\\"
)
png(file = plotFile)
options(scipen = 5)
plot(
  x1, 
  y1, 
  xlim = c(-0.2,0.05),
  #ylim = c(,),
  pch = 1, 
  col = "orange"
)
points(
  x2, 
  y2, 
  pch = 2, 
  col = "brown"
)
text(
  x1, 
  y1, 
  paste("d", 1:19, sep = ""), 
  col = "orange",
  pos = 4
)
text(
  x2, 
  y2, 
  rownames(coordTerms), 
  col = "brown",
  pos = 4
)
legend("bottomleft", legend, cex = 0.5, text.col = "orange")
dev.off()

############################################################################################################## Ida.R
#analiza ukrytej alokacji Dirichlet'a
nTerms <- ncol(dtmTfAll)
nTopics <- 4
lda <- LDA(
  dtmTfAll,
  k = nTopics,
  method = "Gibbs",
  control = list(
    burnin = 2000,
    thin = 100,
    iter = 3000
  )
)
perplexity <- perplexity(lda, dtmTfAll)
results <- posterior(lda)

par(mai = c(1,2,1,1))
#prezentacja tematów
topic1 <- head(sort(results$terms[1,], decreasing = T),20)
barplot(
  rev(topic1),
  horiz = T,
  las = 1, 
  main = "Temat 1",
  xlab = "Prawdopodobieñstwo",
  col = "orange"
)

topic2 <- head(sort(results$terms[2,], decreasing = T),20)
barplot(
  rev(topic2),
  horiz = T,
  las = 1, 
  main = "Temat 2",
  xlab = "Prawdopodobieñstwo",
  col = "violet"
)

topic3 <- head(sort(results$terms[3,], decreasing = T),20)
barplot(
  rev(topic3),
  horiz = T,
  las = 1, 
  main = "Temat 3",
  xlab = "Prawdopodobieñstwo",
  col = "turquoise"
)

topic4 <- head(sort(results$terms[4,], decreasing = T),20)
barplot(
  rev(topic4),
  horiz = T,
  las = 1, 
  main = "Temat 4",
  xlab = "Prawdopodobieñstwo",
  col = "lightgreen"
)

#prezentacja dokumentów
document1 <- results$topics[1,]
barplot(
  rev(document1),
  horiz = T,
  las = 1, 
  main = rownames(results$topics)[1],
  xlab = "Prawdopodobieñstwo",
  col = "violet"
)

document4 <- results$topics[4,]
barplot(
  rev(document4),
  horiz = T,
  las = 1, 
  main = rownames(results$topics)[4],
  xlab = "Prawdopodobieñstwo",
  col = "lightgreen"
)

document11 <- results$topics[11,]
barplot(
  rev(document11),
  horiz = T,
  las = 1, 
  main = rownames(results$topics)[11],
  xlab = "Prawdopodobieñstwo",
  col = "turquoise"
)

document19 <- results$topics[19,]
barplot(
  rev(document19),
  horiz = T,
  las = 1, 
  main = rownames(results$topics)[19],
  xlab = "Prawdopodobieñstwo",
  col = "orange"
)

#################################################################################### keywords.R
#dla pierwszego dokumentu
##wagi tf jako miara wa¿noœci s³ów
keywordsTf1 <- head(sort(dtmTfAllMatrix[1,], decreasing = T))
keywordsTf1

##wagi tfidf jako miara wa¿noœci s³ów
keywordsTfidf1 <- head(sort(dtmTfidfAllMatrix[1,], decreasing = T))
keywordsTfidf1

##lda jako miara wa¿noœci s³ów
termsImportance1 <- c(results$topics[1,]%*%results$terms)
names(termsImportance1) <- colnames(results$terms)
keywordsLda1 <- head(sort(termsImportance1, decreasing = T))
keywordsLda1

##chmura tagów
par(mai = c(0,0,0,0))
wordcloud(corpus[16], max.words = 200, colors = brewer.pal(8,"PuOr"))

##################################################################################### clustering.R
#analiza skupieñ dokumentów
##hierarchiczna
#parametry matedy:
# 1. macierz czêstoœci:
# a. waga (weighting)
# b. zakres zmiennych (bounds)
# 2. miara odleg³oœci (euclideam, jaccard, cosine)
# 3. sposób wyznaczania odleg³oœci pomiêdzy skupieniami (single, complete, ward.D2)

par(mai = c(1,2,1,1))
###eksperyment 1
dist1 <- dist(dtmTfAllMatrix, method = "euclidean")
hclust1 <- hclust(dist1, method = "ward.D2")
plot(hclust1)
barplot(
  hclust1$height, 
  names.arg = 18:1, 
  col = "orange"
)
nClusters1 = 5
clusters1 <- cutree(hclust1, k = nClusters1)
clustersMatrix1 <- matrix(0, 19, nClusters1)
rownames(clustersMatrix1) <- names(clusters1)
for (i in 1:19) {
  clustersMatrix1[i,clusters1[i]] <- 1
}
corrplot(clustersMatrix1)
dendrogram1 <- as.dendrogram(hclust1)
coloredDendrogram1 <- color_branches(dendrogram1, h = 100)
plot(coloredDendrogram1)

###eksperyment 2
dist2 <- dist(dtmTfidfBoundsMatrix, method = "cosine")
hclust2 <- hclust(dist2, method = "ward.D2")
plot(hclust2)
barplot(
  hclust2$height, 
  names.arg = 18:1, 
  col = "orange"
)
nClusters2 = 3
clusters2 <- cutree(hclust2, k = nClusters2)
clustersMatrix2 <- matrix(0, 19, nClusters2)
rownames(clustersMatrix2) <- names(clusters2)
for (i in 1:19) {
  clustersMatrix2[i,clusters2[i]] <- 1
}
corrplot(clustersMatrix2)
dendrogram2 <- as.dendrogram(hclust2)
coloredDendrogram2 <- color_branches(dendrogram2, h = 1.5)
plot(coloredDendrogram2)

###porównanie wyników eksperymentów
Bk_plot(
  dendrogram1,
  dendrogram2,
  add_E = F,
  rejection_line_asymptotic = F,
  main = "Index Fawlks'a Mallows'a",
  ylab = "Index Fawlks'a Mallows'a"
)

##niehierarchiczna (k-œrednich)
#parametry matedy:
# 1. macierz czêstoœci:
# a. waga (weighting)
# b. zakres zmiennych (bounds)
# 2. zak³¹dana liczba klas

###eksperyment 3
nClusters3 <- 3
kmeans3 <- kmeans(dtmTfidfBounds, centers = nClusters3)
clustersMatrix3 <- matrix(0, 19, nClusters3)
rownames(clustersMatrix3) <- names(kmeans3$cluster)
for (i in 1:19) {
  clustersMatrix3[i,kmeans3$cluster[i]] <- 1
}
corrplot(clustersMatrix3)

pattern <- c(1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3)

##porównanie wyników klasyfikacji
randEx1Ex3 <- randIndex(clusters1, kmeans3$cluster, F)
randEx1Ex2 <- randIndex(clusters1, clusters2, F)

randEx1Pattern <- randIndex(clusters1, pattern, F)
randEx2Pattern <- randIndex(clusters2, pattern, F)

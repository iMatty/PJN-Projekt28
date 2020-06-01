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
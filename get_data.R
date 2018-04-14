library(xlsx)

colclass <- rep('character', times=4)

muts_amps <- read.xlsx("data/TCGA-and-HPA_DATA-MYC-BRCA1-BRCA2.xlsx", 
                       sheetName = 'TCGA samples_mutations',
                       colClasses=colclass)

colclass <- c(
  c('character', 'numeric'), 
  rep('character', times=10), 
  rep('numeric', times=), 
  rep('character', times=2))

brca1 <- read.xlsx("data/TCGA-and-HPA_DATA-MYC-BRCA1-BRCA2.xlsx", 
                   sheetName = 'BRCA1',
                   colClasses=colclass)

brca2 <- read.xlsx("data/TCGA-and-HPA_DATA-MYC-BRCA1-BRCA2.xlsx", 
                   sheetName = 'BRCA1',
                   colClasses=colclass)

colclass <- c('character', 'character', 'numeric')

char_check <- function(x){
  if(identical(x, character(0))){
    return(NA_character_)
  }  else {
    return(x)
  }
}

hpa_myc <- read.xlsx("data/TCGA-and-HPA_DATA-MYC-BRCA1-BRCA2.xlsx", 
                     sheetName = 'HPA_MYC',
                     colClasses=colclass)

hpa_brca1 <- read.xlsx("data/TCGA-and-HPA_DATA-MYC-BRCA1-BRCA2.xlsx", 
                       sheetName = 'HPA_BRCA1', 
                       colClasses=colclass)

hpa_brca2 <- read.xlsx("data/TCGA-and-HPA_DATA-MYC-BRCA1-BRCA2.xlsx", 
                       sheetName = 'HPA_BRCA2',
                       colClasses=colclass)

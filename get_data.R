library(xlsx)
library(stringr)
library(data.table)

colclass <- c(
  c('character', 'numeric'), 
  rep('character', times = 6), 
  rep('numeric', times = 1), 
  rep('character', times = 2),
  rep('character', times = 1),
  rep('character', times = 1))

clinical <- read.xlsx("data/2010-09-11380C-Table_S1.2.xlsx", 
                      sheetName = 'KeyclinicalDAta',
                      colClasses = colclass,
                      stringsAsFactors=FALSE)

colclass <- rep('character', times = 4)

muts_amps <- read.xlsx("data/TCGA-and-HPA_DATA-MYC-BRCA1-BRCA2.xlsx", 
                       sheetName = 'TCGA samples_mutations',
                       colClasses = colclass,
                       stringsAsFactors=FALSE)

na_none <- function (x) {
  x[is.na(x)] <- 'NONE'
  return(x)
}

muts_amps <- as.data.frame(apply(muts_amps, 2, na_none))

colclass <- c(
  c('character', 'numeric'), 
  rep('character', times = 10), 
  rep('numeric', times = 2), 
  rep('character', times = 2))

brca1 <- read.xlsx("data/TCGA-and-HPA_DATA-MYC-BRCA1-BRCA2.xlsx", 
                   sheetName = 'BRCA1',
                   colClasses = colclass,
                   stringsAsFactors=FALSE)

brca2 <- read.xlsx("data/TCGA-and-HPA_DATA-MYC-BRCA1-BRCA2.xlsx", 
                   sheetName = 'BRCA2',
                   colClasses = colclass,
                   stringsAsFactors=FALSE)

colclass <- c('character', 'character', 'numeric')

char_check <- function(x){
  if (identical(x, character(0))) {
    return(NA_character_)
  }  else {
    return(x)
  }
}

subs <- function(x){
  v <- strsplit(x[1], ',')[[1]]
  year <- str_detect(v, 'year')
  sex <- str_detect(v, 'male')
  race <- !as.logical(year + sex)
  new_list <- c(
    age = trimws(gsub("([0-9]+).*$", "\\1", char_check(v[year]))),
    sex = trimws(char_check(v[sex])), 
    race = trimws(char_check(v[race])))
  return(new_list)
}

merge_new_cols <- function(df){
  temp_l <- lapply(as.vector(df$Description), subs)
  indata <- as.data.frame(do.call(rbind, temp_l))
  indata$age <- as.numeric(as.character(indata$age))
  indata$sex <- as.character(indata$sex)
  indata$race <- as.character(indata$race)
  df$Description <- NULL
  new_df <- cbind(df, indata)
  return(new_df)
}

hpa_myc <- read.xlsx("data/TCGA-and-HPA_DATA-MYC-BRCA1-BRCA2.xlsx", 
                     sheetName = 'HPA_MYC',
                     colClasses = colclass,
                     stringsAsFactors=FALSE)

# GET DEMOGRAPHICS
hpa_myc <- merge_new_cols(hpa_myc)
demographics <- hpa_myc[,c('Sample.ID', 'age', 'sex', 'race')]

hpa_myc <- hpa_myc[,c('Sample.ID', 'FPKM')]
names(hpa_myc)[2] <- 'fpkm_myc'

hpa_brca1 <- read.xlsx("data/TCGA-and-HPA_DATA-MYC-BRCA1-BRCA2.xlsx",
                       sheetName = 'HPA_BRCA1',
                       colClasses = colclass)
hpa_brca1 <- hpa_brca1[,c('Sample', 'FPKM')]
names(hpa_brca1)[2] <- 'fpkm_brca1'

hpa_brca2 <- read.xlsx("data/TCGA-and-HPA_DATA-MYC-BRCA1-BRCA2.xlsx",
                       sheetName = 'HPA_BRCA2',
                       colClasses = colclass)
hpa_brca2 <- hpa_brca2[,c('Sample', 'FPKM')]
names(hpa_brca2)[2] <- 'fpkm_brca2'

# Homogenize sample_id key to merge data
set_sample_id <- function(x){
  y <- append(c('sample_id'), names(x)[2:length(x)])
  colnames(x) <- y
  return(x)
}

# Fix sample IDs by removing last letter (unrequired) from selected datasets
fix_sample_ids <- function(s_id){
  return(
    substr(s_id, 1, nchar(s_id) - 1)
  )
}

df_list <- list(
  muts_amps, brca1, brca2, demographics, clinical) # pack dfs
df_list <- lapply(df_list, setDT) # conver dfs to data tables
df_list <- lapply(df_list, set_sample_id) # apply change 'sample_id' t all dts

# FIX SAMPLE IDS BY REMOVING LETTER TERMINATION
range_fix <- seq(4,7,1)
range_fix <- seq(4,4,1)
for (i in range_fix) {
  df_list[[i]]$sample_id <- sapply(
    as.vector(df_list[[i]]$sample_id),
    fix_sample_ids)
}

# CATEGORIZE INTO NUMERIC-BOOLEAN VECTOR MYC AND BRCA DATA
categorize <- function(x, myc = F){
  if (as.logical(myc)) {
    if (x == 'NONE') {
      return('NOT_AMP')
    } else {
      if (str_detect(x, 'AMP')){
        return('AMP')
      } else {
        return('NOT_AMP')
      }
    }
  } else {
    if (str_detect(x, 'MUT:')){
      return('MUTATED')
    } else {
      return('NOT_MUTATED')
    }
  }
}

vars_fix <- list(
  c(2, 'myc_cat', T),
  c(3, 'brca1_cat', F),
  c(4, 'brca2_cat', F)
)

for (v in vars_fix) {
  df_list[[1]][, v[[2]]] <- sapply(
    df_list[[1]][, as.integer(v[[1]]), with = F][[1]], 
    categorize,
    myc = v[[3]])
}

rm(list = setdiff(ls(), "df_list"))

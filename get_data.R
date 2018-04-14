library(xlsx)
library(stringr)
library(data.table)


colclass <- rep('character', times = 4)

muts_amps <- read.xlsx("data/TCGA-and-HPA_DATA-MYC-BRCA1-BRCA2.xlsx", 
                       sheetName = 'TCGA samples_mutations',
                       colClasses = colclass)

colclass <- c(
  c('character', 'numeric'), 
  rep('character', times = 10), 
  rep('numeric', times = ), 
  rep('character', times = 2))

brca1 <- read.xlsx("data/TCGA-and-HPA_DATA-MYC-BRCA1-BRCA2.xlsx", 
                   sheetName = 'BRCA1',
                   colClasses = colclass)

brca2 <- read.xlsx("data/TCGA-and-HPA_DATA-MYC-BRCA1-BRCA2.xlsx", 
                   sheetName = 'BRCA1',
                   colClasses = colclass)

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
    year = gsub("([0-9]+).*$", "\\1", char_check(v[year])),
    sex = char_check(v[sex]), 
    race = char_check(v[race]))
  return(new_list)
}

merge_new_cols <- function(df){
  temp_l <- lapply(as.vector(df$Description), subs)
  indata <- as.data.frame(do.call(rbind, temp_l))
  df$Description <- NULL
  new_df <- merge(df, indata, by = NULL)
  return(new_df)
}

hpa_myc <- read.xlsx("data/TCGA-and-HPA_DATA-MYC-BRCA1-BRCA2.xlsx", 
                     sheetName = 'HPA_MYC',
                     colClasses = colclass)
hpa_myc <- merge_new_cols(hpa_myc)

# hpa_brca1 <- read.xlsx("data/TCGA-and-HPA_DATA-MYC-BRCA1-BRCA2.xlsx", 
#                        sheetName = 'HPA_BRCA1', 
#                        colClasses = colclass)
# hpa_brca1 <- merge_new_cols(hpa_brca1)
# 
# hpa_brca2 <- read.xlsx("data/TCGA-and-HPA_DATA-MYC-BRCA1-BRCA2.xlsx", 
#                        sheetName = 'HPA_BRCA2',
#                        colClasses = colclass)
# hpa_brca2 <- merge_new_cols(hpa_brca2)

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

df_list <- list(muts_amps, brca1, brca2, hpa_myc, hpa_brca1, hpa_brca2) # pack dfs
df_list <- list(muts_amps, brca1, brca2, hpa_myc) # pack dfs
df_list <- lapply(df_list, setDT) # conver dfs to data tables
df_list <- lapply(df_list, set_sample_id) # apply change 'sample_id' t all dts

df_list[[4]]$sample_id <-sapply(as.vector(df_list[[4]]$sample_id), fix_sample_ids)
df_list[[5]]$sample_id <-sapply(as.vector(df_list[[5]]$sample_id), fix_sample_ids)
df_list[[5]]$sample_id <-sapply(as.vector(df_list[[6]]$sample_id), fix_sample_ids)

full_data <- join_all(df_list, by='sample_id', type='left', match = "all")

library(xlsx)


mdacc_clinical <- read.xlsx("data/HGSC_DB.xlsx", 
                   sheetName = 'CLINICAL_NGS_CONC',
                   stringsAsFactors=FALSE)

mdacc_somatic <- read.xlsx("data/HGSC_DB.xlsx", 
                   sheetName = 'SOMATIC MUTATIONS',
                   stringsAsFactors=FALSE)

mdacc_germline <- read.xlsx("data/HGSC_DB.xlsx", 
                   sheetName = 'GERMLINE MUTATIONS',
                   stringsAsFactors=FALSE)

mdacc_myc <- read.xlsx("data/HGSC_DB.xlsx", 
                   sheetName = 'MYC COPY NUM_FINAL',
                   stringsAsFactors=FALSE)

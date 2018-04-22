library(psych)
library(dplyr)

# GET DATA 
source('get_data.R') # get tidy data via script
mut_vs_amp <- df_list[[1]]
muts_brca1 <- df_list[[2]]
muts_brca2 <- df_list[[3]]
demog <- df_list[[4]]
clinical <- df_list[[5]]
rm("df_list") # Remove df_list obtained from source('get_data.R')

df <- left_join(mut_vs_amp, demog, by = 'sample_id')
df$sample_id <- sapply(df$sample_id, function(x) {substr(x, 1, nchar(x) - 3)})

intersect(df$sample_id, clinical$sample_id)

df2 <- left_join(df, clinical, by = 'sample_id')
typeof(df$sample_id)
typeof(clinical$sample_id)

df$age_dicot <- cut(
  df$age, breaks = c(-Inf, 60, +Inf), 
  labels = c('<=60','>60'))
df$myc_cat <- as.factor(df$myc_cat)
df$brca1_cat <- as.factor(df$brca1_cat)
df$brca2_cat <- as.factor(df$brca2_cat)
df$sex <- NULL
df$race <- NULL

names(df)
numerical <- c("age")
categorical <- c("sample_id", "MYC", "BRCA1", "BRCA2",
                 "myc_cat", "brca1_cat", "brca2_cat", "age_dicot")
df[numerical] <- sapply(df[numerical], function(x) {as.numeric(unlist(x))}) 
sapply(df[numerical], typeof)
df[ categorical] <- sapply(df[categorical], factor) 
sapply(df[categorical], typeof)


# DATA EXPLORATION AND DESCRIPTIVE STATS

head(df[,c(3,6,4,7)])
head(df[numerical])
head(df[categorical])

summary(df[numerical])
describe(df[,numerical])

categ_data_summary <- function(in_col){
  counts <- as.matrix(table(in_col))
  proportions <- as.matrix(round(table(in_col)/length(in_col), 2))
  rmatch <- match(rownames(counts), rownames(proportions) )
  cat_data <- as.data.frame(cbind( counts, proportions[rmatch,] ))
  colnames(cat_data) <- c('counts', 'proportions')
  return(cat_data)
}
categorical_summary <- c("MYC", "myc_cat", "brca1_cat", "brca2_cat", "age_dicot")
cat_data <- lapply(df[categorical_summary], categ_data_summary)

# RMD vars
n_total_patients <- dim(df)[1]
n_tcga_patients <- dim(df)[1]
per_tcga_patients <- length(mut_vs_amp$sample_id)
n_brca1_mut <- sum(as.numeric(df$brca1_cat))
per_brca1_mut <- round(100*(n_brca1_mut/n_total_patients),1)
n_brca2_mut <- sum(as.numeric(df$brca2_cat))
per_brca2_mut <- round(100*(n_brca2_mut/n_total_patients),1)
n_myc_amp <-  sum(as.numeric(df$myc_cat))
per_myc_amp <- round(100*(n_myc_amp/n_total_patients),1)

f_df <- df[complete.cases(df), ]


# Models
model <- glm(brca1_cat ~ myc_cat, data = f_df, 
             family=binomial(link='logit'))

model <- glm(brca1_cat ~ myc_cat + age_dicot, data = f_df, 
             family=binomial(link='logit'))

model <- glm(brca2_cat ~ myc_cat, data = f_df, 
             family=binomial(link='logit'))

model <- glm(brca2_cat ~ myc_cat + age_dicot, data = f_df, 
             family=binomial(link='logit'))

summary(model)


require(rms)

mod1b <- lrm(brca1_cat ~ myc_cat + age_dicot, data = df, na.action=na.delete)

fisher.test(mut_vs_amp$brca1_cat, mut_vs_amp$myc_cat)
fisher.test(mut_vs_amp$myc_cat, mut_vs_amp$brca2_cat)

table(df$myc_cat, df$brca1_cat)

mantelhaen.test(table(df$myc_cat, df$brca1_cat, df$age_dicot))

table(df$brca1_cat, predict > 0.3)

library(ROCR)
ROCRpred <- prediction(predict, df$brca1_cat)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))

library(ggplot2)
ggplot(df, aes(x=myc_cat, y=brca1_cat)) + geom_point() + geom_jitter() +
  stat_smooth(method="glm", family="binomial", se=FALSE)


table(muts_amps$brca3_cat , muts_amps$myc_cat )

df <- left_join(df, muts_brca1[,c('sample_id', 'MS')], by = 'sample_id')


data <- read.csv('private_data/tma_myc_tidy.csv')
exp_data <- data[,c('myc_ihc_bin', 'brca_status')]
exp_data[exp_data==""] <- NA
exp_data <- na.omit(exp_data)

tab <- table(exp_data$myc_ihc_bin, exp_data$brca_status,  exclude = '')
tab

chisq.test(tab) 
fisher.test(tab)

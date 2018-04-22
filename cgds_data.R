library(cgdsr)


mycgds = CGDS("http://www.cbioportal.org/public-portal/")

cancerstudies = getCancerStudies(mycgds)

study_name <- 'Ovarian Serous Cystadenocarcinoma (TCGA, Nature 2011)'
study_number <- sapply(
  cancerstudies, function (x) {grep(study_name, x, fixed = TRUE)})$name
mycancerstudy <- cancerstudies$cancer_study_id[study_number]
caselist = getCaseLists(mycgds, mycancerstudy)
names(caselist)
caselist$case_list_description
caselist$case_list_id

mycaselist <- caselist$case_list_id[18]

mycaselist<-mycaselist$case_ids[1]
mygeneticprofile = getGeneticProfiles(mycgds, mycancerstudy)
mygeneticprofile = getGeneticProfiles(mycgds, mycancerstudy)[1,1]
mygeneticprofile = getGeneticProfiles(mycgds, mycancerstudy)[2,1]
mygeneticprofile = getGeneticProfiles(mycgds, mycancerstudy)[3,1]
mygeneticprofile = getGeneticProfiles(mycgds, mycancerstudy)[4,1]
mygeneticprofile$genetic_profile_id
mygeneticprofile$genetic_alteration_type
mygeneticprofile$genetic_profile_description
names(mygeneticprofile)
mygeneticprofile$genetic_profile_id
mycaselist$case_ids
mygeneticprofile$genetic_alteration_type
names(mycaselist)
names(mygeneticprofile)
# Get data slices for a specified list of genes, genetic profile and case list
gen_pro <- getProfileData(mycgds,c('MYC'), mygeneticprofile,mycaselist)
dim(gen_pro)
gen_pro
getMutationData(mycgds, mycaselist, mygeneticprofile, c('BRCA1','BRCA2', 'MYC'))

# Get clinical data for the case list
myclinicaldata = getClinicalData(mycgds, mycaselist)

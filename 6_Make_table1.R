## Author: Gry Poulsen
## Date: 29-06-23
## Description: Create table with number of each measurements and individuals


computeN<-function(clinfea){
path_CD<-paste0(
  "F:/Projekter/FSEID00001565/Data/Subprojects/Marie_IBD_clinical_biomarker/",clinfea,"_CD_onlyGP.txt")
test_df_CD <- read.csv(path_CD, sep=" ")
test_df_CD <- test_df_CD %>% mutate(IBD = fct_relevel(IBD, "Control", "CD"))
N_CD<-test_df_CD %>% group_by(IBD) %>% summarise(nobs=n(),nindi=n_distinct(lbnr))

path_UC<-paste0(
  "F:/Projekter/FSEID00001565/Data/Subprojects/Marie_IBD_clinical_biomarker/",clinfea,"_UC_onlyGP.txt")
test_df_UC <- read.csv(path_UC, sep=" ")
test_df_UC <- test_df_UC %>% mutate(IBD = fct_relevel(IBD, "Control", "UC"))
N_UC<-test_df_UC %>% group_by(IBD) %>% summarise(nobs=n(),nindi=n_distinct(lbnr))

return(cbind(clinfea,N_CD[1,],N_CD[2,],N_UC[1,],N_UC[2,]))
}

tab1_out<-rbind(
computeN("CRP"),
computeN("F_cal"),
computeN("Leukocytes"),
computeN("Neutrophilocytes2"),
computeN("Lymphocytes"),
computeN("Monocytes"),
computeN("Eosinophiles1"),
computeN("Basophiles"),
computeN("Thrombocytes"),
computeN("Hemoglobin"),
computeN("Iron"),
computeN("Folate"),
computeN("VitB12"),
computeN("Vitamin D1"),
computeN("ALAT"),
computeN("Albumin1"),
computeN("Bilirubin"))

# Not included
#computeN("ASAT"),
#computeN("ESR2"),


write.csv(
  data.frame(
  tab1_out$clinfea,
  CD_controls=paste0(tab1_out[,3],"/ ",tab1_out[,4]),
  CD_patients=paste0(tab1_out[,6],"/ ",tab1_out[,7]),
  UC_controls=paste0(tab1_out[,9],"/ ",tab1_out[,10]),
  UC_patients=paste0(tab1_out[,12],"/ ",tab1_out[,13])),
  file="F:/Projekter/FSEID00001565/Data/Subprojects/Marie_IBD_clinical_biomarker/results/table1.csv")


### To address reviewer comment: Number of observations 1+ years before diagnosis

computeN_1yplus<-function(clinfea){
  path_CD<-paste0(
    "F:/Projekter/FSEID00001565/Data/Subprojects/Marie_IBD_clinical_biomarker/",clinfea,"_CD_onlyGP.txt")
  test_df_CD <- read.csv(path_CD, sep=" ") 
  test_df_CD <- test_df_CD %>% filter(sampling_to_diag>=365) %>% mutate(IBD = fct_relevel(IBD, "Control", "CD"))
  N_CD<-test_df_CD %>% group_by(IBD) %>% summarise(nobs=n(),nindi=n_distinct(lbnr))
  
  path_UC<-paste0(
    "F:/Projekter/FSEID00001565/Data/Subprojects/Marie_IBD_clinical_biomarker/",clinfea,"_UC_onlyGP.txt")
  test_df_UC <- read.csv(path_UC, sep=" ")
  test_df_UC <- test_df_UC %>% filter(sampling_to_diag>=365) %>% mutate(IBD = fct_relevel(IBD, "Control", "UC"))
  N_UC<-test_df_UC %>% group_by(IBD) %>% summarise(nobs=n(),nindi=n_distinct(lbnr))
  
  return(cbind(clinfea,N_CD[1,],N_CD[2,],N_UC[1,],N_UC[2,]))
}

tab1_out_1yplus<-rbind(
  computeN_1yplus("CRP"),
  computeN_1yplus("F_cal"),
  computeN_1yplus("Leukocytes"),
  computeN_1yplus("Neutrophilocytes2"),
  computeN_1yplus("Lymphocytes"),
  computeN_1yplus("Monocytes"),
  computeN_1yplus("Eosinophiles1"),
  computeN_1yplus("Basophiles"),
  computeN_1yplus("Thrombocytes"),
  computeN_1yplus("Hemoglobin"),
  computeN_1yplus("Iron"),
  computeN_1yplus("Folate"),
  computeN_1yplus("VitB12"),
  computeN_1yplus("Vitamin D1"),
  computeN_1yplus("ALAT"),
  computeN_1yplus("Albumin1"),
  computeN_1yplus("Bilirubin"))

# Not included
#computeN("ASAT"),
#computeN("ESR2"),


write.csv(
  data.frame(
    tab1_out_1yplus$clinfea,
    CD_controls=paste0(tab1_out_1yplus[,3],"/ ",tab1_out_1yplus[,4]),
    CD_patients=paste0(tab1_out_1yplus[,6],"/ ",tab1_out_1yplus[,7]),
    UC_controls=paste0(tab1_out_1yplus[,9],"/ ",tab1_out_1yplus[,10]),
    UC_patients=paste0(tab1_out_1yplus[,12],"/ ",tab1_out_1yplus[,13])),
  file="F:/Projekter/FSEID00001565/Data/Subprojects/Marie_IBD_clinical_biomarker/results/extra_table1_1yplus.csv")




### To address reviewer comment: Median number of observations

compute_medianN<-function(clinfea){
  path_CD<-paste0(
    "F:/Projekter/FSEID00001565/Data/Subprojects/Marie_IBD_clinical_biomarker/",clinfea,"_CD_onlyGP.txt")
  test_df_CD <- read.csv(path_CD, sep=" ") 
  test_df_CD <- test_df_CD %>%  mutate(IBD = fct_relevel(IBD, "Control", "CD"))
  N_CD<-test_df_CD %>% group_by(IBD, lbnr) %>% summarise(n_obs=n(),nindi=n_distinct(lbnr))
  median_N_CD<-N_CD %>% group_by(IBD) %>% summarise(median_n_obs=median(n_obs),mean_n_obs=mean(n_obs))

  path_UC<-paste0(
    "F:/Projekter/FSEID00001565/Data/Subprojects/Marie_IBD_clinical_biomarker/",clinfea,"_UC_onlyGP.txt")
  test_df_UC <- read.csv(path_UC, sep=" ")
  test_df_UC <- test_df_UC %>%  mutate(IBD = fct_relevel(IBD, "Control", "UC"))
  N_UC<-test_df_UC %>% group_by(IBD, lbnr) %>%  summarise(n_obs=n(),nindi=n_distinct(lbnr))
  median_N_UC<-N_UC %>% group_by(IBD) %>% summarise(median_n_obs=median(n_obs),mean_n_obs=mean(n_obs))
  
  return(cbind(clinfea,median_N_CD[1,],median_N_CD[2,],median_N_UC[1,],median_N_UC[2,]))
}

tab1_out_median<-rbind(
  compute_medianN("CRP"),
  compute_medianN("F_cal"),
  compute_medianN("Leukocytes"),
  compute_medianN("Neutrophilocytes2"),
  compute_medianN("Lymphocytes"),
  compute_medianN("Monocytes"),
  compute_medianN("Eosinophiles1"),
  compute_medianN("Basophiles"),
  compute_medianN("Thrombocytes"),
  compute_medianN("Hemoglobin"),
  compute_medianN("Iron"),
  compute_medianN("Folate"),
  compute_medianN("VitB12"),
  compute_medianN("Vitamin D1"),
  compute_medianN("ALAT"),
  compute_medianN("Albumin1"),
  compute_medianN("Bilirubin"))

# Not included
#computeN("ASAT"),
#computeN("ESR2"),


write.csv(
  data.frame(
    tab1_out_median$clinfea,
    CD_controls_median_n = round(tab1_out_median[,3],2),
    CD_controls_mean_n   = round(tab1_out_median[,4],2),
    CD_patients_median   = round(tab1_out_median[,6],2),
    CD_patients_mean     = round(tab1_out_median[,7],2),
    UC_controls_median   = round(tab1_out_median[,9],2),
    UC_controls_mean     = round(tab1_out_median[,10],2),
    UC_patients_median   = round(tab1_out_median[,12],2),
    UC_patients_mean     = round(tab1_out_median[,13],2) ),
  file="F:/Projekter/FSEID00001565/Data/Subprojects/Marie_IBD_clinical_biomarker/results/extra_table_median_n_measurements.csv")


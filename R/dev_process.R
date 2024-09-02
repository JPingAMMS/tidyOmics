# library(devtools)
# library(dplyr)

# use_r("utils")
# use_r("utils_plot")

# #------- data make & save --------
# # Path
# path_dir_input = "G:/matrix_AMS2018_2019/data_rmoutlier"  # Harddisk: 高原①
# path_dir_pheno = "E:/pj/gy习服.多组学/phenotypes/1.clean"
#
# # Read-in
# # Phenotypes
# pheno = readr::read_csv(paste0(path_dir_pheno,"/AMS2018_2019_pheno.csv")) %>%
#   mutate(group = substr(uniqueID,1,1))
# pheno$group = factor(pheno$group, levels=c("A","B","C"), labels = c("Base","Day1","Day90"))
#
# # Omics
# omic2 = read.csv(paste0(path_dir_input,"/AMS2018_2019_omic2_imp_outlier_combat_norm.csv"))
# omic3 = read.csv(paste0(path_dir_input,"/AMS2018_2019_omic3_imp_outlier_combat_norm.csv"))
# omic4 = read.csv(paste0(path_dir_input,"/AMS2018_2019_omic4_imp_outlier_combat_norm.csv"))
# omic5 = read.csv(paste0(path_dir_input,"/AMS2018_2019_omic5_tidymass_norm.csv"),
#                                        check.names = FALSE)
# # check
# head(omic2)[,1:5]
# head(omic3)[,1:5]
# head(omic4)[,1:5]
# head(omic5)[,1:5]
#
# # save RData
# save(pheno, file = "data/pheno_mat.RData")
# save(omic2, omic3, omic4, omic5,
#          file = "data/omics_mat.RData")

# load(file = "data/pheno_mat.RData")
# load(file = "data/omics_mat.RData")

load(file = "E:/pj/r_package_publish/tidyOmics_data/omics_mat.RData")
load(file = "E:/pj/r_package_publish/tidyOmics_data/pheno_mat.RData")

# 写man
# document()


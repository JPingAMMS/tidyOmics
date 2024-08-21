#' Title
#'
#' @param omic_df
#' @param pheno_df
#' @param group
#' @param values
#'
#' @return
#' @export
#'
#' @examples
omic_subset = function(omic_df,
                       pheno_df,
                       group,
                       values){
  library(dplyr)
  sample_in_group = pheno_df %>% filter(group %in% values) %>% pull(uniqueID)
  omic = omic_df %>% select(symbol, sample_in_group)
  return(omic)
}

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



get_entrezid = function(dat){

  if("symbol" %in% colnames(dat)){
    if(is.numeric(dat[,"symbol"])){
      dat = dat %>% tibble::column_to_rownames("symbol")
    }
  } else{warning("The df must have a column called 'symbol'!")}

  #表达数据,行为基因名,列为样本
  symbol = dat[,"symbol"]
  gene = bitr(unique(symbol), fromType = "SYMBOL",
              toType = c( "ENTREZID"),
              OrgDb = org.Hs.eg.db)
  dat_entrezid = left_join(gene, dat, by=c("SYMBOL"="symbol")) %>%
    dplyr::select(-SYMBOL) %>%
    dplyr::rename(symbol=ENTREZID)

  return(dat_entrezid)
}



#' Title
#'
#' @param dat
#'
#' @return
#' @import org.Hs.eg.db
#' @export
#'
#' @examples
get_symbol = function(dat){

  if(substr(dat[1,1] ,1,4) == "ENSG"){
    # dat = dat %>% tibble::column_to_rownames("symbol")
    cat(paste0("The first column of the df is 'ENSG' ids!\n And df[1,1] is ",dat[1,1]))
  } else{warning("The first column of the df must be 'ENSG' ids!")}

  #表达数据,行为基因名,列为样本
  library(org.Hs.eg.db)
  symbol = dat %>% pull(symbol)
  gene = bitr(unique(symbol),
              fromType = "ENSEMBL",
              toType = "SYMBOL",
              OrgDb = org.Hs.eg.db)
  dat_symbol = left_join(gene, dat, by=c("ENSEMBL"="symbol")) %>%
    dplyr::select(-ENSEMBL) %>%
    dplyr::rename(symbol=SYMBOL)

  return(dat_symbol)
}

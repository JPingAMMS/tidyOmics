
library(dplyr)

#' plot_group_box
#'
#' @param omic_df
#' @param pheno_df
#' @param gene
#' @param group
#' @param test
#'
#' @return
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr left_join
#' @importFrom tibble column_to_rownames
#' @importFrom tibble rownames_to_column
#' @importFrom ggpubr ggboxplot
#' @export
#'
#' @examples
plot_group_box = function(omic_df,
                    pheno_df,
                    gene = "ACVR1B",
                    groups = "group",
                    comparisons = list(c("Base", "Day1"),c("Day1", "Day90"),c("Base","Day90")),
                    test="ttest"){

  # omic_df = omic3
  # pheno_df = pheno
  # gene = "ACVR1B"
  # groups = "AMSstate"
  # comparisons= list(c("Healthy","AMS"))

  # omic
  if(is.na(gene)){
    stop("Set which gene want to plot as y-axis!")
  }

  if(is.na(group)){
    stop("Set which group want to plot as x-axis!")
  }

  # omic_df = omic3
  omic = omic_df %>% filter(symbol == gene)
  omic_mat = omic %>%
    tibble::column_to_rownames(var="symbol") %>%
    t() %>% as.data.frame() %>%
    tibble::rownames_to_column(var="uniqueID")


  # pheno_df = pheno
  pheno_grp = pheno_df %>% select(uniqueID, groups)

  omic_pheno = omic_mat %>%
    dplyr::left_join(pheno_grp, by=c("uniqueID"="uniqueID")) %>%
    na.omit()

  library(ggpubr)
  my_comparisons <- comparisons
  p <- ggboxplot(omic_pheno, x = groups, y = gene,
                 color = groups,
                 outlier.shape = NA,
                 add = "jitter"
                 ) +
    stat_compare_means(comparisons=my_comparisons,
                       method = "t.test",
                       aes(label = paste0("p = ", after_stat(p.format))))+
    labs(x=NULL)

  return(p)
}


#' plot_cor_line
#'
#' @param var1_df
#' @param var2_df
#' @param method
#'
#' @return
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr left_join
#' @importFrom dplyr rename
#' @importFrom tibble column_to_rownames
#' @importFrom tibble rownames_to_column
#' @importFrom ggpubr ggscatter
#' @export
#'
#' @examples
plot_cor_line = function(var1_df,
                         var2_df,
                         method="pearson"){

  # omic1 and omic2 must have a shared column named `uniqueID`
  # The other columns are treated as x and y in the association analysis.

  # # test
  # var1_df = omic2 %>% filter(symbol == "MCM2")
  # var2_df = omic3 %>% filter(symbol == "HIF1A")
  # var2_df = pheno %>% select(uniqueID, DBP)



  if(any( "uniqueID" %in% names(var2_df))){
    warning("You may want to test associations with pheno!")
    var2_in_omic2 = var2_df %>%
      rename(SampleID="uniqueID")
  } else {
    var2_in_omic2 = var2_df %>%
      column_to_rownames(var="symbol") %>%
      t() %>% as.data.frame() %>%
      rownames_to_column(var="SampleID")
  }


  var1_in_omic1 = var1_df %>%
    column_to_rownames(var="symbol") %>%
    t() %>% as.data.frame() %>%
    rownames_to_column(var="SampleID")

  var1 = names(var1_in_omic1)[2]
  var2 = names(var2_in_omic2)[2]

  if(var1==var2){
    warning("var1 and var2 are the same var!")
  }


  omic = var1_in_omic1 %>%
    left_join(var2_in_omic2, by=c("SampleID"="SampleID")) %>%
    na.omit()

  p = ggscatter(omic, x=var1, y = var2,
            size = 2, # 点的颜色与大小
            add = "reg.line",  # 添加回归线
            add.params = list(color = "red", fill = "lightgray"), # 回归线的颜色设置为红色，区间颜色设置为灰色
            conf.int = TRUE, # 添加回归线的置信区间
            cor.coef = TRUE, # 添加相关系数
            cor.coeff.args = list(method = method), #选择Pearson相关
            xlab = var1,
            ylab = var2)
    # scale_x_continuous(breaks =  seq(0,1.6,by=0.4), limits = c(0, 1.6))
  return(p)
}


#' plot_cor_heatmap_pheno
#'
#' @param method
#' @param omic_df
#' @param pheno_df
#' @param gene_int
#' @param pheno_int
#'
#' @return
#' @importFrom dplyr %>%
#' @export
#'
#' @examples
plot_cor_heatmap_pheno = function(omic_df,
                               pheno_df,
                               gene_int = NA,
                               pheno_int = NA,
                               # subset =
                               method="pearson"){

  # params
  if(is.na(gene_int)){
    gene_list <- omic_df$symbol
  } else {
    gene_list <- gene_int
  }

  if(is.na(pheno_int)){
    pheno_list <- c("Age", "height", "weight", "BMI", "LLQ",
                    "WBC", "LYMPH#", "LYMPH%", "MONO#", "MONO%",
                    "BASO#","BASO%","EO#", "EO%", "NEUT#", "NEUT%", "MID%", "MID#",
                    "HCT", "HGB",  "MCH", "RBC", "RDW-CV", "RDW-SD", "MCHC", "MCV", "MPV",
                    "PCT", "PDW", "P-LCR", "PLT",
                    "PEF", "FEV1", "FVC", "FEF25", "FEF50", "FEF75", "FEV1%VCMAX", "VCMAX",
                    "DBP", "SBP", "HeartRatio", "SpO2",
                    "grip", # "gripL", "gripR",
                    "ALT", "AST", "AST_ALT", "ALP", "GGT",
                    "TP", "GLB", "ALB", "Alb_Glb_ratio", "IBIL", "DBIL", "TBIL",
                    "TBA", "GLU", "CREA", "UREA", "UA", "K", "Na", "CL")
  } else {
    pheno_list <- pheno_int
  }

  # data
  # omic_df = omic3
  # pheno = pheno_df

  omic = omic_df %>%
    filter(symbol %in% gene_list) %>%
    tibble::column_to_rownames(var="symbol")
  omic_mat = omic %>%
    t() %>% as.data.frame() %>%
    tibble::rownames_to_column(var="SampleID")

  omic_pheno = pheno %>% dplyr::left_join(omic_mat, by=c("uniqueID"="SampleID"))

  # calc cor & p
  library(ggcorrplot)
  library(ggthemes)
  library(psych)

  data_gene <- omic_pheno[,c(gene_list)]
  data_pheno <- omic_pheno[,c(pheno_list)]

  cor <- corr.test(data_pheno,data_gene, method = method,adjust = "none",ci = F)
  cmt <- cor$r
  pmt <- cor$p.adj

  # plot
  p <- ggcorrplot(cmt, method = "square",
                  outline.color = "white",
                  ggtheme = theme_bw(),
                  colors = c("#839EDB", "white", "#FF8D8D"),
                  lab = T, lab_size=2,
                  p.mat = pmt, sig.level = 0.06, insig= "blank",
                  pch.col = "red", pch.cex = 3, tl.cex = 12)

  return(p)
}


#
plot_cor_heatmap_pheno = function(var1,
                                  var2,
                                  method="pearson"){

}

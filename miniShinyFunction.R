library(ggpubr)
library(ggsci)
library(haven)
library(htmlTable)
library(PMCMRplus)
library(readxl)
library(rstatix)
# library(shiny)
# library(shinyBS)
# library(shinyjs)
library(showtext) # Chinese text presentation in plots
library(tidyverse)
library(plotly)

#### Define color ####
color_lvl_1 <- "#54278f" # Dark purple for variable type, first variable level and example
color_lvl_2 <- "#2c7fb8" # Blue for categorical variable level, second variable level and step
color_lvl_3 <- "#de2d26" # Red for abnormality and attention
color_lvl_4 <- "#31a354" # Green for normality and normal status
color_lvl_5 <- "#FF8C00" # Orange for choice selection and third variable level


#### General function ####
##### Importfile function #####
importFile_func <- function(inputFile){
  infile <- inputFile # Extract infile path
  req(infile) # infile path should be True
  if (grepl(".csv", infile[1])){
    importFile <- read_csv(infile$datapath)
  }
  if (grepl(".xlsx", infile[1])){
    importFile <- read_excel(infile$datapath)
  }
  if (grepl(".xls", infile[1])){
    importFile <- read_excel(infile$datapath)
  }
  if (grepl(".sas7bdat", infile[1])){
    importFile <- read_sas(infile$datapath)
  }
  if (grepl(".dta", infile[1])){
    importFile <- read_dta(infile$datapath)
  }
  varName <- names(importFile)
  return(list(importFile,varName))
}

##### Dataframe format conversion #####
long2wide_func <- function(ds, namesFrom, valuesFrom, namesGlue=FALSE){
  if (namesGlue){
    ng <- paste0("cvt_{",namesFrom,"}")
  }
  else {
    ng <- NULL
  }
  
  ds %>% 
    pivot_wider(
      names_from = !!sym(namesFrom),
      values_from = !!sym(valuesFrom),
      names_glue = ng
    )
}

wide2long_func <- function(ds, colsName){
  ds %>% 
    pivot_longer(
      cols = all_of(colsName),
      names_to = "timepoint",
      values_to = "value"
    ) 
}


#### Tab:rawData Function ####
##### Normality test analysis function #####
normTestAna_func <- function(ds, var, group){
  if (group == "(无)"){
    ds_nest <- ds %>%
      select(all_of(var)) %>%
      drop_na() %>%
      nest_by() # Nest a full tibble
  }
  else {
    ds_nest <- ds %>%
      select(all_of(c(var,group))) %>%
      drop_na() %>%
      nest_by(!!sym(group)) # Nest tibbles by subgroups of "group"
  }
  normTest_ds <- ds_nest %>%
    mutate(
      sample.size = dim(data)[1],
      t_model = ifelse(
        sample.size>50,
        map(data, ~ks.test(x=.x, y="pnorm", mean=mean(.x), sd=sd(.x))),
        ifelse(
          sample.size>=3,
          map(data, ~shapiro.test(.x)),
          NA
        )
      ),
      statistic.value = ifelse(is.null(t_model), NA, t_model$statistic),
      p.value = ifelse(is.null(t_model), NA, t_model$p.value),
      normality.test.method = ifelse(is.null(t_model), "样本不足", t_model$method)
    ) %>%
    select(-data,-t_model)
  return(normTest_ds)
}

##### Normality test report function #####
normTestFeedback_func <- function(dsList, num_col, cat_col){
  if (cat_col != "(无)"){
    fb_num <- ""
    for (i in seq(num_col)){
      ds_num <- dsList[[i]] # Normality test for each numeric variable
      cat_lvl <- ds_num[[cat_col]] # Retrieve categorical levels
      p_num <- ds_num[["p.value"]] # Retrieve p values
      fb_cat <- ""
      for (j in seq(cat_lvl)){
        if (is.na(p_num[j])){
          p_eval <- paste0("<font color=\"",color_lvl_3,"\"><b>因样本量不足，无法计算</b></font>")
        }
        else if (!is.na(p_num[j]) & p_num[j]<=0.05){
          p_eval <- paste0("<font color=\"",color_lvl_3,"\"><b>不服从</b></font>正态分布")
        }
        else {
          p_eval <- paste0("<font color=\"",color_lvl_4,"\"><b>服从</b></font>正态分布")
        }
        fb_cat <- paste0(
          fb_cat,
          "&emsp;在分组变量<font color=\"",color_lvl_2,"\"><b>", cat_col, 
          "</b></font>为\"<b>", cat_lvl[j], "</b>\"的水平上的数据", 
          p_eval, " (P=", signif(p_num[j],3),")。<br>"
        )
      }
      fb_num <- paste0(
        fb_num,
        "对于连续变量<font color=\"",color_lvl_1,"\"><b>", num_col[i],"</b></font>而言：<br>",
        fb_cat,
        "<br>"
      )
    }
  }
  else{
    fb_num <- ""
    for (i in seq(num_col)){
      ds_num <- dsList[[i]]
      p_num <- ds_num[["p.value"]]
      if (is.na(p_num)){
        p_eval <- paste0("<font color=\"",color_lvl_3,"\"><b>因样本量不足，无法计算</b></font>")
      }
      else if (!is.na(p_num) & p_num<=0.05){
        p_eval <- paste0("<font color=\"",color_lvl_3,"\"><b>不服从</b></font>正态分布")
      }
      else {
        p_eval <- paste0("<font color=\"",color_lvl_4,"\"><b>服从</b></font>正态分布")
      }
      fb_num <- paste0(
        fb_num,
        "对于连续变量<font color=\"",color_lvl_1,"\"><b>", num_col[i],
        "</b></font>而言：其数据", p_eval, " (P=", signif(p_num,3),")。<br><br>"
      )
    }
  }
  return(fb_num)
}


#### Tab: groupCompare ####
##### Failed normality check feedback #####
# For normality feedback after normTestAna_func()
# There should be only one element as the group variable
normTestFail_func <- function(ds, tp, group=NULL){
  if (is.null(group)){
    paste0(
      "变量<font color=\"",color_lvl_1,"\"><b>", tp,
      "</b></font>在<font color=\"",color_lvl_2,"\"><b>", ds[[tp]], 
      "</b></font>水平上的数据<font color=\"",color_lvl_3,"\"><b>不服从</b></font>正态分布，正态性检验的P值为", 
      signif(as.numeric(ds[["p.value"]],3)), "。<br>"
    )
  } else {
    paste0(
      "在变量<font color=\"",color_lvl_1,"\"><b>", tp,
      "</b></font>的<font color=\"",color_lvl_2,"\"><b>", ds[[tp]], 
      "</b></font>水平联合变量<font color=\"",color_lvl_1,"\"><b>", group,
      "</b></font>的<font color=\"",color_lvl_2,"\"><b>", ds[[group]], 
      "水平上的数据<font color=\"",color_lvl_3,"\"><b>不服从</b></font>正态分布，正态性检验的P值为", 
      signif(as.numeric(ds[["p.value"]],3)), "。<br>"
    )
  }
}

##### Posthoc test feedback #####
posthoc_func <- function(ds, tp, group=NULL){
  if (is.null(group)){
    if (ds[["p.adj"]]<=0.05){
      paste0(
        "&emsp;在变量<font color=\"",color_lvl_1,"\"><b>", tp,
        "</b></font>的<font color=\"",color_lvl_2,"\"><b>", ds[["group1"]], 
        "</b></font>水平与<font color=\"",color_lvl_2,"\"><b>", ds[["group2"]],
        "</b></font>水平之间<font color=\"",color_lvl_3,"\"><b>存在</b></font>显著差异（P=", 
        signif(as.numeric(ds[["p.adj"]]),3), "）。<br>"
      )
    } else{
      paste0(
        "&emsp;在变量<font color=\"",color_lvl_1,"\"><b>", tp,
        "</b></font>的<font color=\"",color_lvl_2,"\"><b>", ds[["group1"]], 
        "</b></font>水平与<font color=\"",color_lvl_2,"\"><b>", ds[["group2"]],
        "</b></font>水平之间<font color=\"",color_lvl_4,"\"><b>不存在</b></font>显著差异。<br>"
      )
    }
  } else{
    if (ds[["p.adj"]]<=0.05){
      paste0(
        "&emsp;在变量<font color=\"",color_lvl_1,"\"><b>", group,
        "</b></font>的<font color=\"",color_lvl_2,"\"><b>", ds[[group]],
        "</b></font>水平上，变量<font color=\"",color_lvl_1,"\"><b>", tp,
        "</b></font>的<font color=\"",color_lvl_2,"\"><b>", ds[["group1"]], 
        "</b></font>水平与<font color=\"",color_lvl_2,"\"><b>", ds[["group2"]],
        "</b></font>水平之间<font color=\"",color_lvl_3,"\"><b>存在</b></font>显著差异（P=", 
        signif(as.numeric(ds[["p.adj"]]),3), "）。<br>"
      )
    } else{
      paste0(
        "&emsp;在变量<font color=\"",color_lvl_1,"\"><b>", group,
        "</b></font>的<font color=\"",color_lvl_2,"\"><b>", ds[[group]],
        "</b></font>水平上，变量<font color=\"",color_lvl_1,"\"><b>", tp,
        "</b></font>的<font color=\"",color_lvl_2,"\"><b>", ds[["group1"]], 
        "</b></font>水平与<font color=\"",color_lvl_2,"\"><b>", ds[["group2"]],
        "</b></font>水平之间<font color=\"",color_lvl_4,"\"><b>不存在</b></font>显著差异。<br>"
      )
    }
  }
}

##### Friedman test feedback #####
friedmanRlt_func <- function(ds, tp, group=NULL){
  if (is.null(group)){
    if (ds[["p"]]<=0.05){
      paste0(
        "&emsp;在变量<font color=\"",color_lvl_1,"\"><b>", tp,
        "</b></font>的不同水平之间<font color=\"",color_lvl_3,"\"><b>存在</b></font>显著差异（P=", 
        signif(as.numeric(ds[["p"]]),3), "）。<br>"
      )
    } else{
      paste0(
        "&emsp;在变量<font color=\"",color_lvl_1,"\"><b>", tp,
        "</b></font>的不同水平之间<font color=\"",color_lvl_4,"\"><b>不存在</b></font>显著差异。<br>"
      )
    }
  } else{
    if (ds[["p"]]<=0.05){
      paste0(
        "&emsp;在变量<font color=\"",color_lvl_1,"\"><b>", group,
        "</b></font>的<font color=\"",color_lvl_2,"\"><b>", ds[[group]],
        "</b></font>水平上，变量<font color=\"",color_lvl_1,"\"><b>", tp,
        "</b></font>的不同水平之间<font color=\"",color_lvl_3,"\"><b>存在</b></font>显著差异（P=", 
        signif(as.numeric(ds[["p"]]),3), "）。<br>"
      )
    } else{
      paste0(
        "&emsp;在变量<font color=\"",color_lvl_1,"\"><b>", group,
        "</b></font>的<font color=\"",color_lvl_2,"\"><b>", ds[[group]],
        "</b></font>水平上，变量<font color=\"",color_lvl_1,"\"><b>", tp,
        "</b></font>的不同水平之间<font color=\"",color_lvl_4,"\"><b>不存在</b></font>显著差异。<br>"
      )
    }
  }
}

##### Friedman posthoc (Nemenyi test) feedback #####
posthocFriedman_func <- function(prlt, tp){
  p.rownames <- dimnames(prlt)[[1]]
  p.colnames <- dimnames(prlt)[[2]]
  p_feedback <- ""
  for (i in seq(p.rownames)){
    for (j in seq(p.colnames)){
      p.val <- prlt[i,j]
      if (!is.na(p.val) & p.val<=0.05){
        p_feedback <- paste0(
          p_feedback,
          "&emsp;在变量<font color=\"",color_lvl_1,"\"><b>", tp,
          "</b></font>的<font color=\"",color_lvl_2,"\"><b>", p.rownames[i], 
          "</b></font>与<font color=\"",color_lvl_2,"\"><b>", p.rownames[j],
          "</b></font>水平之间<font color=\"",color_lvl_3,"\"><b>存在</b></font>显著差异（P=", 
          signif(as.numeric(p.val),3), "）。<br>"
        )
      } else if (!is.na(p.val) & p.val>0.05){
        p_feedback <- paste0(
          p_feedback,
          "&emsp;在变量<font color=\"",color_lvl_1,"\"><b>", tp,
          "</b></font>的<font color=\"",color_lvl_2,"\"><b>", p.rownames[i], 
          "</b></font>与<font color=\"",color_lvl_2,"\"><b>", p.rownames[j],
          "</b></font>水平之间<font color=\"",color_lvl_4,"\"><b>不存在</b></font>显著差异。<br>"
        )
      } else {
        p_feedback <- p_feedback
      }
    }
  }
  return(p_feedback)
}

##### Friedman test group feedback #####
posthocFriedmanGroup_func <- function(ds, value, tp, group){
  if (ds[["p"]]<=0.05){
    test_posthocInter <- frdAllPairsNemenyiTest(
      data = ds %>% filter(!!sym(group)==ds[[group]]),
      eval(parse(text=paste0("formula = ",value,"~",tp,"|",id)))
    )
    test_posthocInter_eval <- paste0(
      "在变量<font color=\"",color_lvl_1,"\"><b>", group, "</b></font>的<font color=\"",color_lvl_2,"\"><b>", ds[[group]], 
      "</b></font>水平上",
      posthocFriedman_func(ds=test_posthocInter[["p.value"]], tp=tp)
    )
  }
  else {
    test_posthocInter_eval <- paste0(
      "在变量<font color=\"",color_lvl_1,"\"><b>", group, "</b></font>的<font color=\"",color_lvl_2,"\"><b>", ds[[group]], 
      "</b></font>水平上变量<font color=\"",color_lvl_1,"\"><b>", tp, 
      "</b></font>的各水平之间<font color=\"",color_lvl_4,"\"><b>不存在</b></font>显著差异。<br>"
    )
  }
  feedback_list <- list(test_posthocInter, test_posthocInter_eval)
  names(feedback_list) <- c(paste0("Post-hoc analysis: ", group," at level ", ds[[group]]), "feedback")
  return(feedback_list)
}

##### Concatenate feedback string vector into a single vector #####
feedback_concat <- function(str_vec){
  feedback_string <- ""
  for (i in str_vec){
    feedback_string <- paste0(feedback_string,i)
  }
  return(feedback_string)
}


##### Single sample ttest #####
single_ttest_func <- function(ds, value, mu){
  
  # Get single ttest result
  test_rlt <- ds %>% t_test(eval(parse(text=paste0(value,"~1"))), mu=mu)
  
  # Evaluate p-value
  test_p <- test_rlt$p
  if (test_p<=0.05 & test_rlt$statistic<=0){
    test_rpt <- paste0(
      "单样本t检验结果显示，变量<font color=\"",color_lvl_1,"\"><b>", value,
      "</b></font>的均值<font color=\"",color_lvl_3,"\"><b>显著低于</b></font><font color=\"",
      color_lvl_1,"\"><b>目标值</b></font>",mu,
      "（P=",
      signif(test_p,3),
      "）。"
    )
  } else if (test_p<=0.05 & test_rlt$statistic>0){
    test_rpt <- paste0(
      "单样本t检验结果显示，变量<font color=\"",color_lvl_1,"\"><b>", value,
      "</b></font>的均值<font color=\"",color_lvl_3,"\"><b>显著高于</b></font><font color=\"",
      color_lvl_1,"\"><b>目标值</b></font>",mu,
      "（P=",
      signif(test_p,3),
      "）。"
    )
  } else {
    test_rpt <- paste0(
      "单样本t检验结果显示，变量<font color=\"",color_lvl_1,"\"><b>", value,
      "</b></font>的均值与<font color=\"",color_lvl_1,"\"><b>目标值</b></font>",mu,
      "相比<font color=\"",color_lvl_4,"\"><b>不存在</b></font>显著差异（P=",
      signif(test_p,3),
      "）。"
    )
  }
  
  rlt_list <- list(test_rpt, test_rlt)
  names(rlt_list) <- c("test_report", "test_result")
  return(rlt_list)
}


##### Paired ttest function #####
paired_ttest_func <- function(ds, tp, value){
  
  # Convert df from long to wide format for difference normality test
  ds_wide <- long2wide_func(ds, namesFrom=tp, valuesFrom=value)
  var1 <- unique(ds[[tp]])[1]
  var2 <- unique(ds[[tp]])[2]
  
  delta_ds <- ds_wide %>%
    mutate(delta = eval(parse(text=paste0(var2,"-",var1)))) %>%
    select(delta) %>%
    nest_by() %>%
    mutate(
      sample.size = dim(data)[1],
      median = map(data, ~median(.x))$delta,
      mean = map(data, ~mean(.x))$delta,
      t_model = ifelse(
        sample.size>50,
        map(data, ~ks.test(x=.x, y="pnorm", mean=mean(.x), sd=sd(.x))),
        ifelse(
          sample.size>=3,
          map(data, ~shapiro.test(.x)),
          NA
        )
      ),
      statistic.value = ifelse(is.null(t_model), NA, t_model$statistic),
      p.value = ifelse(is.null(t_model), NA, t_model$p.value),
      normality.test.method = ifelse(is.null(t_model), "样本不足", t_model$method)
    ) %>%
    select(-data,-t_model)
  
  # Evaluate median
  if (delta_ds$median<0){
    delta_median_eval <- paste0(
      "由于差值(",var2,"-",var1,")的中位数为",signif(delta_ds$median,3),
      "，因此<font color=\"",color_lvl_1,"\"><b>",var2,
      "</b></font>的值显著低于<font color=\"",color_lvl_1,"\"><b>",var1,"</b></font>的值。<br>"
    )
  }
  else if (delta_ds$median>0){
    delta_median_eval <- paste0(
      "由于差值(",var2,"-",var1,")的中位数为",signif(delta_ds$median,3),
      "，因此<font color=\"",color_lvl_1,"\"><b>",var2,
      "</b></font>的值显著高于<font color=\"",color_lvl_1,"\"><b>",var1,"</b></font>的值。<br>"
    )
  }
  
  # Evaluate mean
  if (delta_ds$mean<0){
    delta_mean_eval <- paste0(
      "由于差值(",var2,"-",var1,")的均值为",signif(delta_ds$mean,3),
      "，因此<font color=\"",color_lvl_1,"\"><b>",var2,
      "</b></font>的值显著低于<font color=\"",color_lvl_1,"\"><b>",var1,"</b></font>的值。<br>"
    )
  }
  else if (delta_ds$mean>0){
    delta_mean_eval <- paste0(
      "由于差值(",var2,"-",var1,")的均值为",signif(delta_ds$mean,3),
      "，因此<font color=\"",color_lvl_1,"\"><b>",var2,
      "</b></font>的值显著高于<font color=\"",color_lvl_1,"\"><b>",var1,"</b></font>的值。<br>"
    )
  }
  
  # Evaluate delta p-value
  delta_p <- delta_ds[["p.value"]]
  if (is.na(delta_p)){
    delta_p_eval <- paste0("<font color=\"",color_lvl_3,"\"><b>样本量不足，无法计算</b></font>。")
    test_rlt <- NULL
  }
  else if (!is.na(delta_p) & delta_p<=0.05){
    delta_p_eval <- paste0(
      "两次测试的差值<font color=\"",color_lvl_3,"\"><b>不服从</b></font>正态分布（P=",
      signif(delta_p,3),
      "）。<br>选用<b>Wilcoxon符号秩检验</b>进行数据分析。<br>"
    )
    test_rlt <- ds %>% wilcox_test(eval(parse(text=paste0(value,"~",tp))), paired=TRUE)
  }
  else {
    delta_p_eval <- paste0(
      "两次测试的差值<font color=\"",color_lvl_4,"\"><b>服从</b></font>正态分布（P=",
      signif(delta_p,3),
      "）。<br>选用<b>配对t检验</b>进行数据分析。<br>"
    )
    test_rlt <- ds %>% t_test(eval(parse(text=paste0(value,"~",tp))), paired=TRUE)
  }
  
  # Evaluate test p-value
  if (is.null(test_rlt)){
    test_p_eval <- NULL
  }
  else {
    test_p <- test_rlt$p
    if (test_p<=0.05){
      test_p_eval <- paste0(
        "两个测试值之间<font color=\"",color_lvl_3,"\"><b>存在</b></font>显著差异（P=",
        signif(test_p,3),
        "）。"
      )
      if (delta_p<=0.05){test_p_eval <- paste0(test_p_eval, delta_median_eval)}
      else {test_p_eval <- paste0(test_p_eval, delta_mean_eval)}
    }
    else {
      test_p_eval <- paste0(
        "两个测试值之间<font color=\"",color_lvl_4,"\"><b>不存在</b></font>显著差异（P=",
        signif(test_p,3),
        "）。"
      )
    }
  }
  
  test_rpt <- paste0(delta_p_eval, test_p_eval)
  
  # Prepare for significant bar plot
  sig_plot <- test_rlt %>%
    mutate(p.adj.signif = case_when(
      p<=0.01 ~ "**",
      p<=0.05&p>0.01 ~ "**",
      TRUE ~"ns")
    ) %>%
    add_xy_position(x=tp) %>%
    mutate(
      p.format = p_format(
        p,
        accuracy=0.001,
        leading.zero=FALSE
      )
    )
  
  test_rlt_list <- list(delta_ds, test_rlt)
  names(test_rlt_list) <- c("Normality test of difference", "Paired test summary")
  
  rlt_list <- list(test_rpt, test_rlt_list, sig_plot)
  names(rlt_list) <- c("test_report", "test_result", "sig_plot")
  return(rlt_list)
}

##### Repeated measures ANOVA #####
rep_anova_func <- function(ds, id, tp, value, group=NULL){
  # Normality check within groups
  if (is.null(group)){
    normTest_ds <- normTestAna_func(ds=ds, var=value, group=tp)
  } else {
    normTest_ds <- normTestAna_func(ds=ds, var=value, group=c(tp,group))
  }
  
  # Retrieve significant normality
  normTestSig_ds <- normTest_ds %>% filter(p.value<=0.05 | is.na(p.value))
  
  if (dim(normTestSig_ds)[1]==0){
    # Normality check passed
    normTest_eval <- paste0(
      "正态性检验显示，变量<font color=\"",color_lvl_2,"\"><b>", value,
      "</b></font>在各水平的正态性检验<font color=\"",color_lvl_4,"\"><b>已通过</b></font>。<br>继续进行重复测量方差分析。<br>"
    )
    # Anova test
    test_model <- anova_test(
      data = ds,
      dv = value,
      wid = id,
      within = timepoint,
      between = group,
      type = 3,
      detailed = TRUE
    )
    test_model$ANOVA <- test_model$ANOVA %>% mutate(eta_sqr=SSn/(SSn+SSd))
    
    if (dim(test_model$ANOVA)[1]==dim(test_model$`Mauchly's Test for Sphericity`)[1]+1){
      # This shows a one-way repeated ANOVA
      test_betweenP <- NULL
      test_interP <- NULL
      test_withinF <- test_model$ANOVA$F[2]
      test_withinEtaSqr <- test_model$ANOVA$eta_sqr[2]
      
      # Sphericity test: within group
      if (test_model$`Mauchly's Test for Sphericity`$p<0.05){
        if (test_model$`Sphericity Corrections`$GGe<0.75){
          test_withinP <- test_model$`Sphericity Corrections`$`p[GG]`
          test_withinDFn <- unlist(strsplit(test_model$`Sphericity Corrections`$`DF[GG]`, ", "))[1]
          test_withinDFd <- unlist(strsplit(test_model$`Sphericity Corrections`$`DF[GG]`, ", "))[2]
        } else{
          test_withinP <- test_model$`Sphericity Corrections`$`p[HF]`
          test_withinDFn <- unlist(strsplit(test_model$`Sphericity Corrections`$`DF[HF]`, ", "))[1]
          test_withinDFd <- unlist(strsplit(test_model$`Sphericity Corrections`$`DF[HF]`, ", "))[2]
        }
      } else{
        test_withinP <- test_model$ANOVA$P[2]
        test_withinDFn <- test_model$ANOVA$DFn[2]
        test_withinDFd <- test_model$ANOVA$DFd[2]
      }
    } else{
      # This shows a two-way repeated ANOVA
      test_betweenP <- test_model$ANOVA$P[2]
      test_betweenDFn <- test_model$ANOVA$DFn[2]
      test_betweenDFd <- test_model$ANOVA$DFd[2]
      test_betweenF <- test_model$ANOVA$F[2]
      test_withinF <- test_model$ANOVA$F[3]
      test_interF <- test_model$ANOVA$F[4]
      test_betweenEtaSqr <- test_model$ANOVA$eta_sqr[2]
      test_withinEtaSqr <- test_model$ANOVA$eta_sqr[3]
      test_interEtaSqr <- test_model$ANOVA$eta_sqr[4]
      # Sphericity test: within group 
      if (test_model$`Mauchly's Test for Sphericity`$p[1]<0.05){
        if (test_model$`Sphericity Corrections`$GGe[1]<0.75){
          test_withinP <- test_model$`Sphericity Corrections`$`p[GG]`[1]
          test_withinDFn <- unlist(strsplit(test_model$`Sphericity Corrections`$`DF[GG]`[1], ", "))[1]
          test_withinDFd <- unlist(strsplit(test_model$`Sphericity Corrections`$`DF[GG]`[1], ", "))[2]
        } else{
          test_withinP <- test_model$`Sphericity Corrections`$`p[HF]`[1]
          test_withinDFn <- unlist(strsplit(test_model$`Sphericity Corrections`$`DF[HF]`[1], ", "))[1]
          test_withinDFd <- unlist(strsplit(test_model$`Sphericity Corrections`$`DF[HF]`[1], ", "))[2]
        }
      } else{
        test_withinP <- test_model$ANOVA$P[3]
        test_withinDFn <- test_model$ANOVA$DFn[3]
        test_withinDFd <- test_model$ANOVA$DFd[3]
      }
      # Sphericity test: between group
      if (test_model$`Mauchly's Test for Sphericity`$p[2]<0.05){
        if (test_model$`Sphericity Corrections`$GGe[2]<0.75){
          test_interP <- test_model$`Sphericity Corrections`$`p[GG]`[2]
          test_interDFn <- unlist(strsplit(test_model$`Sphericity Corrections`$`DF[GG]`[2], ", "))[1]
          test_interDFd <- unlist(strsplit(test_model$`Sphericity Corrections`$`DF[GG]`[2], ", "))[2]
        } else{
          test_interP <- test_model$`Sphericity Corrections`$`p[HF]`[2]
          test_interDFn <- unlist(strsplit(test_model$`Sphericity Corrections`$`DF[HF]`[2], ", "))[1]
          test_interDFd <- unlist(strsplit(test_model$`Sphericity Corrections`$`DF[HF]`[2], ", "))[2]
        }
      } else{
        test_interP <- test_model$ANOVA$P[4]
        test_interDFn <- test_model$ANOVA$DFn[4]
        test_interDFd <- test_model$ANOVA$DFd[4]
      }
    }
    
    # Between subject results feedback
    if (is.null(test_betweenP)){
      test_betweenP_eval <- NULL
    } else if (test_betweenP<=0.05){
      test_betweenP_eval <- paste0(
        "组间分析结果显示，变量<font color=\"",color_lvl_2,"\"><b>", value,
        "</b></font>在组间变量<font color=\"",color_lvl_2,"\"><b>", group,
        "</b></font>的不同水平之间<font color=\"",color_lvl_3,"\"><b>存在</b></font>显著性差异（F<sub>(", 
        test_betweenDFn, ", ", test_betweenDFd,
        ")</sub>=",test_betweenF,", P=", signif(as.numeric(test_betweenP),3),
        ", &eta;<sup>2</sup><sub>p</sub>=", signif(as.numeric(test_betweenEtaSqr),3),
        "）。<br>"
      )
      
      # Posthoc test
      test_posthocBetween <- emmeans_test(
        data = ds,
        eval(parse(text=paste0("formula=",value,"~",group)))
      )
      test_posthocBetweenSig_eval <- apply(test_posthocBetween, MARGIN=1, FUN=posthoc_func, tp=group)
      test_posthocBetween_eval <- paste0(
        "组间分析的事后检验(T-test, bonferroni方法进行P值校正)显示，变量<font color=\"",color_lvl_2,"\"><b>", value, "</b></font>的数值：<br>",
        feedback_concat(test_posthocBetweenSig_eval)
      )
    } else{
      test_betweenP_eval <- paste0(
        "组间分析结果显示，变量<font color=\"",color_lvl_2,"\"><b>", value,
        "</b></font>在组内变量<font color=\"",color_lvl_2,"\"><b>", group,
        "</b></font>的不同水平之间<font color=\"",color_lvl_4,"\"><b>不存在</b></font>显著性差异。<br>"
      )
      test_posthocBetween <- NULL
      test_posthocBetween_eval <- "不需进行事后检验。<br>"
    }
    
    # Within subject results feedback
    if (test_withinP<=0.05){
      test_withinP_eval <- paste0(
        "组内分析结果显示，变量<font color=\"",color_lvl_2,"\"><b>", value,
        "</b></font>在组内变量<font color=\"",color_lvl_2,"\"><b>", tp,
        "</b></font>的不同水平之间<font color=\"",color_lvl_3,"\"><b>存在</b></font>显著性差异（F<sub>(", 
        test_withinDFn, ", ", test_withinDFd,
        ")</sub>=",test_withinF,", P=", signif(as.numeric(test_withinP),3),
        ", &eta;<sup>2</sup><sub>p</sub>=", signif(as.numeric(test_withinEtaSqr),3),
        "）。<br>"
      )
      
      # Posthoc test
      test_posthocWithin <- emmeans_test(
        data = ds,
        eval(parse(text=paste0("formula=",value,"~",tp)))
      )
      test_posthocWithinSig_eval <- apply(test_posthocWithin, MARGIN=1, FUN=posthoc_func, tp=tp)
      test_posthocWithin_eval <- paste0(
        "组内分析的事后检验(T-test, bonferroni方法进行P值校正)显示，变量<font color=\"",color_lvl_2,"\"><b>", value, "</b></font>的数值：<br>",
        feedback_concat(test_posthocWithinSig_eval)
      )
    } else{
      test_withinP_eval <- paste0(
        "组内分析结果显示，变量<font color=\"",color_lvl_2,"\"><b>", value,
        "</b></font>在组内变量<font color=\"",color_lvl_2,"\"><b>", tp,
        "</b></font>的不同水平之间<font color=\"",color_lvl_4,"\"><b>不存在</b></font>显著性差异。<br>"
      )
      test_posthocWithin <- NULL
      test_posthocWithin_eval <- "不需进行事后检验。<br>"
    }
    
    # Interaction results feedback
    if (is.null(test_interP)){
      test_interP_eval <- NULL
    } else if (test_interP<=0.05){
      test_interP_eval <- paste0(
        "交互效应的分析结果显示，变量<font color=\"",color_lvl_2,"\"><b>", tp,
        "</b></font>与变量<font color=\"",color_lvl_2,"\"><b>", group,
        "</b></font>之间<font color=\"",color_lvl_3,"\"><b>存在</b></font>交互效应（F<sub>(", 
        test_interDFn, ", ", test_interDFd,
        ")</sub>=",test_interF,", P=", signif(as.numeric(test_interP),3),
        ", &eta;<sup>2</sup><sub>p</sub>=", signif(as.numeric(test_interEtaSqr),3),
        "）。<br>"
      )
      
      # Posthoc test
      test_posthocInterBetween <- emmeans_test(
        data = ds %>% group_by(!!sym(group)),
        eval(parse(text=paste0("formula=",value,"~",tp)))
      )
      test_posthocInterSig_evalBetween <- apply(test_posthocInterBetween, MARGIN=1, FUN=posthoc_func, tp=tp, group=group)
      
      test_posthocInterWithin <- emmeans_test(
        data = ds %>% group_by(!!sym(tp)),
        eval(parse(text=paste0("formula=",value,"~",group)))
      )
      test_posthocInterSig_evalWithin <- apply(test_posthocInterWithin, MARGIN=1, FUN=posthoc_func, tp=group, group=tp)
      test_posthocInter_eval <- paste0(
        "交互效应的事后检验(T-test, bonferroni方法进行P值校正)显示，变量<font color=\"",color_lvl_2,"\"><b>", value, "</b></font>的数值：<br>",
        feedback_concat(test_posthocInterSig_evalBetween),
        feedback_concat(test_posthocInterSig_evalWithin)
      )
    } else{
      test_interP_eval <- paste0(
        "交互效应的分析结果显示，变量<font color=\"",color_lvl_2,"\"><b>", tp,
        "</b></font>与变量<font color=\"",color_lvl_2,"\"><b>", group,
        "</b></font>之间<font color=\"",color_lvl_4,"\"><b>不存在</b></font>交互效应。<br>"
      )
      test_posthocInterBetween <- NULL
      test_posthocInterWithin <- NULL
      test_posthocInter_eval <- "不需进行事后检验。<br>"
    }
  } else {
    # Normality check failed
    normTestSig_eval <- apply(normTestSig_ds, MARGIN=1, FUN=normTestFail_func, tp=tp, group=group)
    normTest_eval <- paste0(
      "正态性检验显示，变量<font color=\"",color_lvl_2,"\"><b>", value,
      "</b></font>在各水平的正态性检验<font color=\"",color_lvl_3,"\"><b>未全部通过</b></font>。<br>",
      feedback_concat(normTestSig_eval),
      "将进行Friedman检验。"
    )
    
    # Within suject results feedback
    test_modelWithin <- friedman_test(
      data = ds,
      eval(parse(text=paste0("formula=",value,"~",tp,"|",id)))
    )
    test_withinP_eval <- paste0(
      "组内分析结果显示，变量<font color=\"",color_lvl_2,"\"><b>", value, "</b></font>的数值：<br>",
      feedback_concat(apply(test_modelWithin, MARGIN=1, FUN=friedmanRlt_func, tp=tp))
    )
    
    # Posthoc test
    if (test_modelWithin$p<=0.05){
      test_posthocWithin <- frdAllPairsNemenyiTest(
        data = ds,
        eval(parse(text=paste0("formula = ",value,"~",tp,"|",id)))
      )
      test_posthocWithin_eval <- paste0(
        "组内分析的事后检验(Nemenyi test, one-step方法进行P值校正)显示，变量<font color=\"",color_lvl_2,"\"><b>", value, "</b></font>的数值：<br>",
        posthocFriedman_func(ds=test_posthocWithin[["p.value"]], tp=tp)
      )
    } else {
      test_posthocWithin <- NULL
      test_posthocWithin_eval <- "不需进行事后检验。<br>"
    }
    
    if (group!="NULL"){
      # Between subject results feedback
      test_modelBetween <- friedman_test(
        data = ds,
        eval(parse(text=paste0("formula=",value,"~",group,"|",id)))
      )
      test_betweenP_eval <- paste0(
        "组间分析结果显示，变量<font color=\"",color_lvl_2,"\"><b>", value, "</b></font>的数值：<br>",
        feedback_concat(apply(test_modelBetween, MARGIN=1, FUN=friedmanRlt_func, tp=group))
      )
      
      # Posthoc test
      if (test_modelBetween$p<=0.05){
        test_posthocBetween <- frdAllPairsNemenyiTest(
          data = ds,
          eval(parse(text=paste0("formula = ",value,"~",group,"|",id)))
        )
        test_posthocBetween_eval <- paste0(
          "组间分析的事后检验(Nemenyi test, one-step方法进行P值校正)显示，变量<font color=\"",color_lvl_2,"\"><b>", value, "</b></font>的数值：<br>",
          posthocFriedman_func(ds=test_posthocBetween[["p.value"]], tp=group)
        )
      } else {
        test_posthocBetween <- NULL
        test_posthocBetween_eval <- "不需进行事后检验。<br>"
      }
      
      # Interaction feedback
      test_modelInterBetween <- friedman_test(
        data = ds %>% group_by(!!sym(group)),
        eval(parse(text=paste0("formula=",value,"~",tp,"|",id)))
      )
      test_modelInterWithin <- friedman_test(
        data = ds %>% group_by(!!sym(tp)),
        eval(parse(text=paste0("formula=",value,"~",group,"|",id)))
      )
      
      test_interP_eval <- paste0(
        "交互效应结果显示，变量<font color=\"",color_lvl_2,"\"><b>", value, "</b></font>的数值：<br>",
        feedback_concat(apply(test_modelInterBetween, MARGIN=1, FUN=friedmanRlt_func, tp=tp, group=group)),
        feedback_concat(apply(test_modelInterWithin, MARGIN=1, FUN=friedmanRlt_func, tp=group, group=tp))
      )
      
      # Posthoc test: interaction between groups
      test_modelInterBetween_evalList <- apply(test_modelInterBetween, MARGIN=1, FUN=posthocFriedmanGroup_func, value=value, tp=tp, group=group)
      
      test_posthocInterBetween <- list()
      for (i in seq(test_modelInterBetween_evalList)){
        name_prev <- names(test_modelInterBetween)
        name_add <- names(test_modelInterBetween_evalList[[i]])[1]
        test_posthocInterBetween <- append(test_posthocInterBetween,list(test_modelInterBetween_evalList[[i]][[1]]))
        names(test_posthocInterBetween) <- c(name_prev, name_add)
      }
      
      test_modelInterBetween_eval <- ""
      for (i in seq(test_modelInterBetween_evalList)){
        test_modelInterBetween_eval <- paste0(test_modelInterBetween_eval,test_modelInterBetween_evalList[[i]][[2]])
      }
      
      # Posthoc test: interaction within groups
      test_modelInterWithin_evalList <- apply(test_modelInterWithin, MARGIN=1, FUN=posthocFriedmanGroup_func, value=value, tp=group, group=tp)
      
      test_posthocInterWithin <- list()
      for (i in seq(test_modelInterWithin_evalList)){
        name_prev <- names(test_posthocInterWithin)
        name_add <- names(test_modelInterWithin_evalList[[i]])[1]
        test_posthocInterWithin <- append(test_posthocInterWithin,list(test_modelInterWithin_evalList[[i]][[1]]))
        names(test_posthocInterWithin) <- c(name_prev, name_add)
      }
      
      test_modelInterWithin_eval <- ""
      for (i in seq(test_modelInterWithin_evalList)){
        test_modelInterWithin_eval <- paste0(test_modelInterWithin_eval,test_modelInterWithin_evalList[[i]][[2]])
      }
      
      test_posthocInter_eval <- paste0(
        "交互效应的事后检验(Nemenyi test, one-step方法进行P值校正)显示，变量<font color=\"",color_lvl_2,"\"><b>", value, "</b></font>的数值：<br>",
        test_modelInterBetween_eval,
        test_modelInterWithin_eval
      )
    } else {
      test_modelBetween <- NULL
      test_betweenP_eval <- NULL
      test_posthocBetween <- NULL
      test_posthocBetween_eval <- NULL
      
      test_modelInterBetween <- NULL
      test_modelInterWithin <- NULL
      test_interP_eval <- NULL
      test_posthocInterBetween <- NULL
      test_posthocInterWithin <- NULL
      test_posthocInter_eval <- NULL
    }
    
    test_model <- list(
      test_modelBetween, 
      test_modelWithin, 
      test_modelInterBetween, 
      test_modelInterWithin
    )
    
    names(test_model) <- c(
      "Friedman test: between group test", 
      "Friedman test: within group test", 
      "Friedman test: interaction test by between group factor", 
      "Friedman test: interaction test by within group factor"
    )
  }
  
  # Result reports
  test_rpt <- paste0(
    normTest_eval,
    test_betweenP_eval,
    test_posthocBetween_eval,
    test_withinP_eval,
    test_posthocWithin_eval,
    test_interP_eval,
    test_posthocInter_eval,
  )
  
  # Analysis summary
  test_modelPosthoc <- list(
    summary(test_posthocBetween),
    summary(test_posthocWithin),
    summary(test_posthocInterBetween),
    summary(test_posthocInterWithin)
  )
  
  names(test_modelPosthoc) <- c(
    "Post-hoc: between group factor",
    "Post-hoc: within group factor",
    "Post-hoc: interaction test by between group factor", 
    "Post-hoc: interaction test by within group factor"
  )
  
  test_rlt_list <- list(normTest_ds,test_model,test_modelPosthoc)
  names(test_rlt_list) <- c("Normality test","Repeated-measure test summary", "Post-hoc tests")
  
  rlt_list <- list(test_rpt, test_rlt_list, sig_plot)
  names(rlt_list) <- c("test_report", "test_result", "sig_plot")
  return(rlt_list) 
}

##### Plot function with single group #####
plotTypeSingle_func <- function(ds, yVar, plotType, errorBar=NULL, orientation=NULL){
  if (plotType == "boxplot"){
    p <- ggboxplot(
      data=ds, y=yVar, outlier.shape=NA, 
      bxp.errorbar=TRUE, bxp.errorbar.width=0.3,
      orientation=ifelse(orientation, "horizontal", "vertical")
    )
  } else if (plotType=="barplot"){
    ds_summary <- ds %>% 
      summarise(mean=mean(!!sym(yVar)), sd=sd(!!sym(yVar)))
    
    errorBar_params <- switch (
      errorBar,
      error_0 = list(add="mean"), # No errorbar
      error_1 = list(add="mean_sd"), # Full errorbar
      error_2 = list(add="mean_sd", error.plot = "upper_errorbar") # Half errorbar
    )
    
    p <- do.call(
      ggbarplot,
      c(
        list(data=ds, y=yVar),
        errorBar_params
      )
    )
    
    # Set x-axis to 0 if the means are negative
    if (any(ds_summary$mean<0)){
      p <- p + 
        geom_hline(yintercept=0) +
        scale_y_continuous(
          expand = expansion(mult = c(0.05, 0.1))
        )
    } else {
      p <- p + 
        scale_y_continuous(
          expand = expansion(mult = c(0, 0.1))
        )
    } 
  }
  else {p <- ggscatter(data=ds, y=yVar)} # Scatter pplot
  
  p <- p + theme_bw()
  return(p)
}

##### Plot function with multiple groups #####
plotTypeMulti_func <- function(ds, xVar, yVar, grpVar, plotType, errorBar=NULL, orientation=NULL){
  
  if (is.character(ds[[xVar]])){ # Check if x is in the type of character
    
    unique_var <- unique(ds[[xVar]])
    ds_summary <- ds %>% 
      rowwise() %>% 
      mutate_at(c(xVar),function(x){which(unique_var==x)}) # Convert x into numeric values
  }
  
  if ((grpVar=="NULL")|(xVar==grpVar)){
    ds_summary <- ds %>% 
      group_by(!!sym(xVar)) %>% 
      summarise(mean=mean(!!sym(yVar)), sd=sd(!!sym(yVar))) %>% 
      ungroup()
  } else {
    ds_summary <- ds %>% 
      group_by(!!sym(xVar),!!sym(grpVar)) %>% 
      summarise(mean=mean(!!sym(yVar)), sd=sd(!!sym(yVar))) %>% 
      ungroup()
  }
  
  if (plotType == "boxplot"){
    boxplot_params <- list(
      data=ds, x=xVar, y=yVar, outlier.shape=NA, 
      bxp.errorbar=TRUE, bxp.errorbar.width=0.3,
      orientation=ifelse(orientation, "horizontal", "vertical")
    )
    if (grpVar!="NULL"){
      boxplot_params$color <- grpVar
    }
    p <- do.call(
      ggboxplot,
      boxplot_params
    )
  } else if (plotType=="barplot"){
    errorBar_params <- switch (
      errorBar,
      error_0 = list(add="mean"), # No errorbar
      error_1 = list(add="mean_sd"), # Full errorbar
      error_2 = list(add="mean_sd", error.plot = "upper_errorbar") # Half errorbar
    )
    if (grpVar!="NULL"){
      errorBar_params$fill <- grpVar
    }
    p <- do.call(
      ggbarplot,
      c(
        list(data=ds, x=xVar, y=yVar),
        errorBar_params
      )
    )
    
    # Set x-axis to 0 if the means are negative
    if (any(ds_summary$mean<0)){
      p <- p + 
        geom_hline(yintercept=0) +
        scale_y_continuous(
          expand = expansion(mult = c(0.05, 0.1))
        )
    } else {
      p <- p + 
        scale_y_continuous(
          expand = expansion(mult = c(0, 0.1))
        )
    }
  } else {
    if (xVar==grpVar){grpVar <- "steelblue"}
    errorBar_params <- switch (
      errorBar,
      error_0 = list(add="mean"), # No errorbar
      error_1 = list(add="mean_sd"), # Full errorbar
      error_2 = list(add="mean_sd", error.plot = "upper_errorbar") # Half errorbar
    )
    if (grpVar!="NULL"){
      errorBar_params$color <- grpVar
    }
    p <- do.call(
      ggline,
      c(
        list(data=ds, x=xVar, y=yVar),
        errorBar_params
      )
    )
  }
  
  p <- p + theme_bw()
  return(p)
}

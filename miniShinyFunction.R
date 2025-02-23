library(ggpubr)
library(ggsci)
library(haven)
library(htmlTable)
library(PMCMRplus)
library(readxl)
library(rstatix)
library(shiny)
library(shinyBS)
library(shinyjs)
library(showtext) # Chinese text presentation in plots
library(tidyverse)

#### Define color ####
color_lvl_1 <- "#54278f" # Dark purple for variable type, first variable level and example
color_lvl_2 <- "#2c7fb8" # Blue for categorical variable level, second variable level and step
color_lvl_3 <- "#de2d26" # Red for abnormality and attention
color_lvl_4 <- "#31a354" # Green for normality and normal status
color_lvl_5 <- "#FF8C00" # Orange for choice selection and third variable level


##########################
#### General function ####
##########################
#### Importfile function ####
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


##############################
#### Tab:rawData Function ####
##############################
#### Normality test analysis function ####
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

#### Normality test report function ####
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


##############################
##### Tab: groupCompare ######
##############################
#### Failed normality check feedback ####
# For normality feedback after normTestAna_func()
# There should be only one element as the group variable
normTestFail_func <- function(ds, tp, group=NULL){
  if (is.null(group)){
    paste0(
      "变量<font color=\"",color_lvl_2,"\"><b>", tp,
      "</b></font>在<font color=\"",color_lvl_1,"\"><b>", ds[[tp]], 
      "</b></font>水平上的数据<font color=\"",color_lvl_3,"\"><b>不服从</b></font>正态分布，正态性检验的P值为", 
      signif(as.numeric(ds[["p.value"]],3)), "。<br>"
    )
  } else {
    paste0(
      "在变量<font color=\"",color_lvl_2,"\"><b>", tp,
      "</b></font>的<font color=\"",color_lvl_1,"\"><b>", ds[[tp]], 
      "</b></font>水平联合变量<font color=\"",color_lvl_2,"\"><b>", group,
      "</b></font>的<font color=\"",color_lvl_1,"\"><b>", ds[[group]], 
      "水平上的数据<font color=\"",color_lvl_3,"\"><b>不服从</b></font>正态分布，正态性检验的P值为", 
      signif(as.numeric(ds[["p.value"]],3)), "。<br>"
    )
  }
}

#### Posthoc test feedback ####
posthoc_func <- function(ds, tp, group=NULL){
  if (is.null(group)){
    if (ds[["p.adj"]]<=0.05){
      paste0(
        "&emsp;在变量<font color=\"",color_lvl_2,"\"><b>", tp,
        "</b></font>的<font color=\"",color_lvl_1,"\"><b>", ds[["group1"]], 
        "</b></font>水平与<font color=\"",color_lvl_1,"\"><b>", ds[["group2"]],
        "</b></font>水平之间<font color=\"",color_lvl_3,"\"><b>存在</b></font>显著差异（P=", 
        signif(as.numeric(ds[["p.adj"]]),3), "）。<br>"
      )
    } else{
      paste0(
        "&emsp;在变量<font color=\"",color_lvl_2,"\"><b>", tp,
        "</b></font>的<font color=\"",color_lvl_1,"\"><b>", ds[["group1"]], 
        "</b></font>水平与<font color=\"",color_lvl_1,"\"><b>", ds[["group2"]],
        "</b></font>水平之间<font color=\"",color_lvl_4,"\"><b>不存在</b></font>显著差异。<br>"
      )
    }
  } else{
    if (ds[["p.adj"]]<=0.05){
      paste0(
        "&emsp;在变量<font color=\"",color_lvl_2,"\"><b>", group,
        "</b></font>的<font color=\"",color_lvl_1,"\"><b>", ds[[group]],
        "</b></font>水平上，变量<font color=\"",color_lvl_2,"\"><b>", tp,
        "</b></font>的<font color=\"",color_lvl_1,"\"><b>", ds[["group1"]], 
        "</b></font>水平与<font color=\"",color_lvl_1,"\"><b>", ds[["group2"]],
        "</b></font>水平之间<font color=\"",color_lvl_3,"\"><b>存在</b></font>显著差异（P=", 
        signif(as.numeric(ds[["p.adj"]]),3), "）。<br>"
      )
    } else{
      paste0(
        "&emsp;在变量<font color=\"",color_lvl_2,"\"><b>", group,
        "</b></font>的<font color=\"",color_lvl_1,"\"><b>", ds[[group]],
        "</b></font>水平上，变量<font color=\"",color_lvl_2,"\"><b>", tp,
        "</b></font>的<font color=\"",color_lvl_1,"\"><b>", ds[["group1"]], 
        "</b></font>水平与<font color=\"",color_lvl_1,"\"><b>", ds[["group2"]],
        "</b></font>水平之间<font color=\"",color_lvl_4,"\"><b>不存在</b></font>显著差异。<br>"
      )
    }
  }
}

#### Friedman test feedback ####
friedmanRlt_func <- function(ds, tp, group=NULL){
  if (is.null(group)){
    if (ds[["p"]]<=0.05){
      paste0(
        "&emsp;在变量<font color=\"",color_lvl_2,"\"><b>", tp,
        "</b></font>的不同水平之间<font color=\"",color_lvl_3,"\"><b>存在</b></font>显著差异（P=", 
        signif(as.numeric(ds[["p"]]),3), "）。<br>"
      )
    } else{
      paste0(
        "&emsp;在变量<font color=\"",color_lvl_2,"\"><b>", tp,
        "</b></font>的不同水平之间<font color=\"",color_lvl_4,"\"><b>不存在</b></font>显著差异。<br>"
      )
    }
  } else{
    if (ds[["p"]]<=0.05){
      paste0(
        "&emsp;在变量<font color=\"",color_lvl_2,"\"><b>", group,
        "</b></font>的<font color=\"",color_lvl_1,"\"><b>", ds[[group]],
        "</b></font>水平上，变量<font color=\"",color_lvl_2,"\"><b>", tp,
        "</b></font>的不同水平之间<font color=\"",color_lvl_3,"\"><b>存在</b></font>显著差异（P=", 
        signif(as.numeric(ds[["p"]]),3), "）。<br>"
      )
    } else{
      paste0(
        "&emsp;在变量<font color=\"",color_lvl_2,"\"><b>", group,
        "</b></font>的<font color=\"",color_lvl_1,"\"><b>", ds[[group]],
        "</b></font>水平上，变量<font color=\"",color_lvl_2,"\"><b>", tp,
        "</b></font>的不同水平之间<font color=\"",color_lvl_4,"\"><b>不存在</b></font>显著差异。<br>"
      )
    }
  }
}

#### Friedman posthoc (Nemenyi test) feedback ####
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
          "&emsp;在变量<font color=\"",color_lvl_2,"\"><b>", tp,
          "</b></font>的<font color=\"",color_lvl_1,"\"><b>", p.rownames[i], 
          "</b></font>与<font color=\"",color_lvl_1,"\"><b>", p.rownames[j],
          "</b></font>水平之间<font color=\"",color_lvl_3,"\"><b>存在</b></font>显著差异（P=", 
          signif(as.numeric(p.val),3), "）。<br>"
        )
      } else if (!is.na(p.val) & p.val>0.05){
        p_feedback <- paste0(
          p_feedback,
          "&emsp;在变量<font color=\"",color_lvl_2,"\"><b>", tp,
          "</b></font>的<font color=\"",color_lvl_1,"\"><b>", p.rownames[i], 
          "</b></font>与<font color=\"",color_lvl_1,"\"><b>", p.rownames[j],
          "</b></font>水平之间<font color=\"",color_lvl_4,"\"><b>不存在</b></font>显著差异。<br>"
        )
      } else {
        p_feedback <- p_feedback
      }
    }
  }
  return(p_feedback)
}

#### Friedman test group feedback ####
posthocFriedmanGroup_func <- function(ds, value, tp, group){
  if (ds[["p"]]<=0.05){
    test_posthocInter <- frdAllPairsNemenyiTest(
      data = ds %>% filter(!!sym(group)==ds[[group]]),
      eval(parse(text=paste0("formula = ",value,"~",tp,"|",id)))
    )
    test_posthocInter_eval <- paste0(
      "在变量<font color=\"",color_lvl_2,"\"><b>", group, "</b></font>的<font color=\"",color_lvl_1,"\"><b>", ds[[group]], 
      "</b></font>水平上",
      posthocFriedman_func(ds=test_posthocInter[["p.value"]], tp=tp)
    )
  }
  else {
    test_posthocInter_eval <- paste0(
      "在变量<font color=\"",color_lvl_2,"\"><b>", group, "</b></font>的<font color=\"",color_lvl_1,"\"><b>", ds[[group]], 
      "</b></font>水平上变量<font color=\"",color_lvl_2,"\"><b>", tp, 
      "</b></font>的各水平之间<font color=\"",color_lvl_4,"\"><b>不存在</b></font>显著差异。<br>"
    )
  }
  feedback_list <- list(test_posthocInter, test_posthocInter_eval)
  names(feedback_list) <- c(paste0("Post-hoc analysis: ", group," at level ", ds[[group]]), "feedback")
  return(feedback_list)
}

#### Concatenate feedback string vector into a single vector ####
feedback_concat <- function(str_vec){
  feedback_string <- ""
  for (i in str_vec){
    feedback_string <- paste0(feedback_string,i)
  }
  return(feedback_string)
}
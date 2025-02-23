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
source("miniShinyFunction.R", local=TRUE) # Load customised library: miniShinyFunction.R

showtext_auto() #Chinese text presentation in plots

#### Define color ####
color_lvl_1 <- "#54278f" # Dark purple for variable type, first variable level and example
color_lvl_2 <- "#2c7fb8" # Blue for categorical variable level, second variable level and step
color_lvl_3 <- "#de2d26" # Red for abnormality and attention
color_lvl_4 <- "#31a354" # Green for normality and normal status
color_lvl_5 <- "#FF8C00" # Orange for choice selection and third variable level

#### Dataframe format ####
htmlFormat_g1_repeat0 <- htmlTable(
  matrix(
    c(1,15,
      2,20,
      3,25,
      "...","...",
      "n",15),
    ncol=2, byrow = TRUE
  ),
  header =  c("ID","Value"),
  caption = markdown(
    paste0(
      "将进行**独立样本t检验**；<br>
      在<font color=\"",color_lvl_2,"\"><b>步骤2</b></font>的**选择统计方法**一栏中选择<font color=\"",
      color_lvl_5,"\"><b>独立样本t检验</b></font>；<br>
      数据文件需按照下面的格式准备："
    )
  ),
  tfoot = markdown(
    "**ID**为受试者编号，**Value**为测试值；<br>"
  ),
  collapse = "separate_shiny"
)

htmlFormat_g1_repeat1 <- htmlTable(
  matrix(
    c(1,"T0",15,
      1,"T1",20,
      2,"T0",25,
      2,"T1",22,
      "...","...","...",
      "n","T0",10,
      "n","T1",15),
    ncol=3, byrow = TRUE
  ),
  header =  c("ID","Timepoint","Value"),
  caption = markdown(
    "将进行**配对t检验**（参数检验）或**配对样本Wilcoxon符号秩检验**（非参数检验）；<br>
    在<font color=#31a354><b>步骤2</b></font>中选择**配对检验**；<br>
    数据文件需按照下面的长数据格式准备："
  ),
  tfoot = markdown(
    "**ID**为受试者编号，**Timepoint**为测试时间点，**Value**为每个时间点对应的测试值；<br>
    若数据不为上述格式，可在左侧***数据处理***一栏中进行数据格式转化"
  ),
  collapse = "separate_shiny"
)

htmlFormat_g1_repeat2 <- htmlTable(
  matrix(
    c(1,"T0",15,
      1,"T1",20,
      1,"...",19,
      1,"Tn",16,
      2,"T0",25,
      2,"T1",22,
      2,"...",18,
      2,"Tn",20,
      "...","...","...",
      "n","T0",10,
      "n","T1",15,
      "n","...",12,
      "n","Tn",17),
    ncol=3, byrow = TRUE
  ),
  header =  c("ID","Timepoint","Value"),
  caption = markdown(
    "将进行**单因素重复测量方差分析**（参数检验）或**Friedman检验**（非参数检验）；<br>
    在<font color=#31a354><b>步骤2</b></font>中选择**方差分析**；<br>
    数据文件需按照下面的长数据格式准备："
  ),
  tfoot = markdown(
    "**ID**为受试者编号，**Timepoint**为测试时间点，**Value**为每个时间点对应的测试值；<br>
    若数据不为上述格式，可在左侧***数据处理***一栏中进行数据格式转化"
  ),
  collapse = "separate_shiny"
)

#### Dataframe format conversion ####
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

#### Paired ttest function ####
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

#### repeated measures ANOVA ####
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

#### Plot type function ####
plotType_func <- function(ds, xVar, yVar, grpVar, plotType, ytickNum, errorBar=NULL){
  
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
    if (grpVar=="NULL"){
      p <- ggboxplot(
        data=ds, x=xVar, y=yVar, outlier.shape=NA, 
        bxp.errorbar=TRUE, bxp.errorbar.width=0.3
      ) 
    }
    else {
      p <- ggboxplot(
        data=ds, x=xVar, y=yVar, color=grpVar, 
        outlier.shape=NA, bxp.errorbar=TRUE, bxp.errorbar.width=0.3
      ) 
    }
    
    p <- p +
      scale_y_continuous(
        n.breaks = ytickNum, 
        expand = expansion(mult = c(0.05, 0.1))
      )
  }
  
  if (plotType=="barplot"){
    if (grpVar=="NULL"){
      if (errorBar=="error_0"){
        p <- ggbarplot(data=ds, x=xVar, y=yVar, add="mean") 
      } else if (errorBar=="error_1"){ # Full errorbar
        p <- ggbarplot(data=ds, x=xVar, y=yVar, add="mean_sd") 
      } else if (errorBar=="error_2"){ # Half errorbar
        p <- ggbarplot(data=ds, x=xVar, y=yVar, add="mean_sd", error.plot = "upper_errorbar") 
      }
    }
    else{
      if (errorBar=="error_0"){
        p <- ggbarplot(data=ds, x=xVar, y=yVar, fill=grpVar, add="mean") 
      } else if (errorBar=="error_1"){ # Full errorbar
        p <- ggbarplot(data=ds, x=xVar, y=yVar, fill=grpVar, add="mean_sd") 
      } else if (errorBar=="error_2"){ # Half errorbar
        p <- ggbarplot(data=ds, x=xVar, y=yVar, fill=grpVar, color=grpVar, add="mean_sd") 
      }
    }
    
    # Set x-axis to 0 if the means are negative
    if (any(ds_summary$mean<0)){
      p <- p + geom_hline(yintercept=0) +
        scale_y_continuous(
          n.breaks = ytickNum, 
          expand = expansion(mult = c(0.05, 0.1))
        )
    } else {
      p <- p +
        scale_y_continuous(
          n.breaks = ytickNum, 
          expand = expansion(mult = c(0, 0.1))
        )
    }
  }
  
  if (plotType == "lineplot"){
    if (grpVar=="NULL"){
      if (errorBar=="error_0"){
        p <- ggline(data=ds, x=xVar, y=yVar, add="mean") 
      } else if (errorBar=="error_1"){ # Full errorbar
        p <- ggline(data=ds, x=xVar, y=yVar, add="mean_sd") 
      } else if (errorBar=="error_2"){ # Half errorbar
        p <- ggline(
          data=ds, x=xVar, y=yVar, 
          add="mean_sd", error.plot = "upper_errorbar"
        ) 
      }
    }
    else {
      if (xVar==grpVar){grpVar <- "steelblue"}
      if (errorBar=="error_0"){
        p <- ggline(data=ds, x=xVar, y=yVar, color=grpVar, add="mean") 
      } else if (errorBar=="error_1"){ # Full errorbar
        p <- ggline(data=ds, x=xVar, y=yVar, color=grpVar, add="mean_sd") 
      } else if (errorBar=="error_2"){ # Half errorbar
        p <- ggline(
          data=ds, x=xVar, y=yVar, 
          color=grpVar, add="mean_sd",
          error.plot = "upper_errorbar"
        ) 
      }
    }
    p <- p +
      scale_y_continuous(
        n.breaks = ytickNum, 
        expand = expansion(mult = c(0.05, 0.1))
      )
  }
  
  p <- p + theme_bw()
  return(p)
}

#### server function ####
function(input, output, session) {
  
  #### Tab: rawData ####
  #### Step 1: Import dataset ####
  run_rawData_step1_importFile <- reactive({importFile_func(input$rawData_step1_importFile)})  
  
  # Update full variables for data input
  observe({
    updatePickerInput(
      session = session,
      "rawData_step1_contSelect",
      choices = run_rawData_step1_importFile()[[2]]
    )
  })
  
  # Update categorical variables for data input
  observe({
    updatePickerInput(
      session = session,
      "rawData_step1_catSelect",
      choices = run_rawData_step1_importFile()[[2]]
    )
  })
  
  run_rawData_step1_outFileInfo <- eventReactive(
    input$rawData_step1_cfmRun,
    {
      validate(
        need(
          length(intersect(input$rawData_step1_contSelect,input$rawData_step1_catSelect))==0, 
          paste0(
            "选取的连续变量与分类变量中包含重复变量：",
            paste(intersect(input$rawData_step1_contSelect,input$rawData_step1_catSelect), collapse="，"),
            "。需要更正！"
          )
        ),
        need(
          length(input$rawData_step1_contSelect)>0 | length(input$rawData_step1_catSelect)>0,
          paste("需选取至少一个连续变量或分类变量！")
        )
      )
      
      rawData_ds <- run_rawData_step1_importFile()[[1]] %>% 
        select(all_of(c(input$rawData_step1_contSelect,input$rawData_step1_catSelect))) %>% 
        mutate(
          across(all_of(input$rawData_step1_contSelect), as.numeric),
          across(all_of(input$rawData_step1_catSelect), as.factor)
        )
      
      # Miss record feedback
      miss_record_count <- sum(!complete.cases(rawData_ds))
      if (miss_record_count == 0){
        outInfo <- "无缺失记录。"
      } 
      else {
        miss_col_count <- colSums(is.na(rawData_ds))
        outInfo <- paste(
          "其中，变量<em>",
          paste(names(miss_col_count)[miss_col_count>0], collapse="、"),
          "</em>包含<b>",
          paste(miss_col_count[miss_col_count>0], collapse="、"),
          "</b>条缺失记录。"
        )
      }
      
      # Activate rawData_normTestCatVar button in Step 2 and 3
      if(!is.null(input$rawData_step1_catSelect)){
        shinyjs::enable(id="rawData_step2_normTestCatVar")
        shinyjs::enable(id="rawData_step3_desCatVar")
        updatePickerInput(
          session = session,
          "rawData_step2_normTestCatVar",
          choices = c("(无)", input$rawData_step1_catSelect)
        )
        updatePickerInput(
          session = session,
          "rawData_step3_desCatVar",
          choices = c("(无)", input$rawData_step1_catSelect)
        )
      } 
      else {
        shinyjs::disable(id="rawData_step2_normTestCatVar")
        shinyjs::reset(id="rawData_step2_normTestCatVar")
        shinyjs::disable(id="rawData_step3_desCatVar")
        shinyjs::reset(id="rawData_step3_desCatVar")
      }
      
      if(!is.null(input$rawData_step1_contSelect)){
        shinyjs::enable(id="rawData_step2_cfmRun")
        shinyjs::enable(id="rawData_step2_normTestContVar")
        updatePickerInput(
          session = session,
          "rawData_step2_normTestContVar",
          choices = input$rawData_step1_contSelect
        )
      } 
      else {
        shinyjs::disable(id="rawData_step2_cfmRun")
        shinyjs::disable(id="rawData_step2_normTestContVar")
        shinyjs::reset(id="rawData_step2_normTestContVar")
        shinyjs::disable(id="rawData_step2_normTestCatVar")
      }
      
      # Output information
      list(rawData_ds,outInfo)
    }
  )
  
  output$rawData_step1_outFileInfo <- renderText({
    paste(
      "共导入<b>", dim(run_rawData_step1_outFileInfo()[[1]])[1], "</b>条记录数据。",
      run_rawData_step1_outFileInfo()[[2]]
    )
  })
  
  #### Step 2: Normality test ####
  rawData_normTestDs <- eventReactive(
    input$rawData_step2_cfmRun,
    {
      req(input$rawData_step2_normTestContVar)
      
      if (input$rawData_step2_normTestCatVar == "(无)"){
        normTest_ds <- run_rawData_step1_outFileInfo()[[1]] %>% 
          select(all_of(c(input$rawData_step2_normTestContVar)))
      }
      else{
        normTest_ds <- run_rawData_step1_outFileInfo()[[1]] %>% 
          select(all_of(c(input$rawData_step2_normTestContVar,input$rawData_step2_normTestCatVar)))
      }
      
      return(normTest_ds)
    }
  )
  
  # Normality test: return a list of Normality test results by continuous variables
  run_rawData_step2_normTestRlt <- eventReactive(
    input$rawData_step2_cfmRun,
    {
      normAnaList <- list()
      ds_tmp <- rawData_normTestDs()
      group_tmp <- input$rawData_step2_normTestCatVar
      for (i in seq(length(input$rawData_step2_normTestContVar))){
        var_tmp <- input$rawData_step2_normTestContVar[i]
        normAnaList[[i]] <- normTestAna_func(
          ds = ds_tmp, 
          var = var_tmp,
          group = group_tmp
        )
      }
      names(normAnaList) <- input$rawData_step2_normTestContVar
      normAnaList
    }
  )
  
  output$rawData_step2_normTestRlt <- renderPrint({run_rawData_step2_normTestRlt()})
  
  # Normality test report
  run_rawData_step2_normTestRpt <- eventReactive(
    input$rawData_step2_cfmRun,
    {
      normTestFeedback_func( 
        dsList = run_rawData_step2_normTestRlt(),
        num_col = input$rawData_step2_normTestContVar,
        cat_col = input$rawData_step2_normTestCatVar
      )
    }
  )
  
  output$rawData_step2_normTestRpt <- renderText({run_rawData_step2_normTestRpt()})
  
  # Normality histogram
  observeEvent(
    input$rawData_step2_cfmRun,
    {
      updatePickerInput(
        session = session,
        "rawData_step2_normTestHistVar",
        choices = input$rawData_step2_normTestContVar,
        selected = input$rawData_step2_normTestContVar[1]
      )
    }
  )
  
  normTestGroupVar <- eventReactive(
    input$rawData_step2_cfmRun,
    {
      if (input$rawData_step2_normTestCatVar == "(无)"){"NULL"}
      else {input$rawData_step2_normTestCatVar}
    }
  )
  
  run_rawData_step2_normTestHist <- reactive({
    req(rawData_normTestDs())
    req(normTestGroupVar())
    
    rawData_histPlot <- ggplot(
      data = rawData_normTestDs(), 
      aes_string(x=input$rawData_step2_normTestHistVar, fill=normTestGroupVar())) +  
      geom_histogram(
        aes(y = ..density..), 
        alpha = 0.7, 
        bins=input$rawData_step2_normTestHistBin, 
        position="identity", 
        color="black", 
        size=0.4
      )+ 
      geom_density(alpha=0.6, size=0.3)+
      labs(x = input$rawData_step2_normTestHistVar,  
           y = "概率密度",  
           title = paste0("变量",input$rawData_step2_normTestHistVar,"分布直方图"))
      ggplotly(rawData_histPlot)
  })
  
  output$rawData_step2_normTestHist <- renderPlotly({run_rawData_step2_normTestHist()})
  
  #### Step 3: Descriptive table ####
  run_rawData_step3_desTable <- eventReactive(
    input$rawData_step3_cfmRun,
    {
      req(run_rawData_step1_outFileInfo())
      
      if (input$rawData_step3_desCatVar != "(无)"){
        # Generate parental category line
        catLength <- length(levels(run_rawData_step1_outFileInfo()[[1]][[input$rawData_step3_desCatVar]]))
        catStat <- NULL
        for (i in seq(catLength)){
          statName <- paste0("stat_", i)
          catStat <- append(catStat, statName)
        }
        table_summary <- run_rawData_step1_outFileInfo()[[1]] %>% 
          tbl_summary(
            by = input$rawData_step3_desCatVar,
            type = all_continuous()~"continuous2",
            statistic = all_continuous()~c("{mean} ± {sd}", "{median} ({p25}, {p75})", "{min}, {max}"),
            digits = list(all_continuous()~input$rawData_step3_desDigit),
            missing_text = "缺失"
          ) %>% 
          add_overall() %>%
          modify_spanning_header(
            catStat ~ paste0("**", input$rawData_step3_desCatVar, "**")
          )  %>% 
          add_p(
            pvalue_fun = function(x) {
              if_else(
                is.na(x), 
                "未能计算",
                if_else(x<0.001, "<0.001", format(round(x,3),scientific=FALSE))
              )
            }
          ) %>% 
          separate_p_footnotes()
      } else {
        table_summary <- run_rawData_step1_outFileInfo()[[1]] %>% 
          tbl_summary(
            type = all_continuous()~"continuous2",
            statistic = all_continuous()~c("{mean} ± {sd}", "{median} ({p25}, {p75})", "{min}, {max}"),
            digits = list(all_continuous()~input$rawData_step3_desDigit),
            missing_text = "缺失"
          )
      }
      
      table_summary %>%         
        modify_header(
          label ~ "**变量**"
        ) %>% 
        modify_caption(
          "**数据描述表**"
        ) %>% 
        bold_labels() %>% 
        as_gt()
    }
  )
  
  output$rawData_step3_desTable = render_gt({run_rawData_step3_desTable()})
  
  output$rawData_step3_desTableDownload = downloadHandler(
    filename = function(){
      paste("SummaryTable-", Sys.Date(), ".docx", sep = "")
    },
    content = function(file){
      run_rawData_step3_desTable() %>% 
        gt::gtsave(filename = file)
    }
  )
  
  #### Tab: groupCompare ####
  #### Subtab 1, step 1: single group test selection ####
  run_groupCompare_g1_step1_tblFormat <- eventReactive(
    input$groupCompare_g1_step1_cfmRun,
    {
      if (input$groupCompare_g1_step1_repeat == "repeat0"){
        htmlFormat_g1_repeat0
      }
      else if (input$groupCompare_g1_step1_repeat == "repeat1"){
        htmlFormat_g1_repeat1
      }
      else {
        htmlFormat_g1_repeat2
      }
    }
  )
  
  output$groupCompare_g1_step1_tblFormat <- renderUI({run_groupCompare_g1_step1_tblFormat()})
  
  #### Subtab 1, step 2: Import dataset ####
  run_groupCompare_g1_step2_importFile <- reactive({importFile_func(input$groupCompare_g1_step2_importFile)})
  
  # Update id variable for data input
  observe({
    updatePickerInput(
      session = session,
      "groupCompare_g1_step2_id",
      choices = run_groupCompare_g1_step2_importFile()[[2]],
      selected = run_groupCompare_g1_step2_importFile()[[2]][[1]]
    )
  })
  
  # Update timepoint variable for data input
  observe({
    updatePickerInput(
      session = session,
      "groupCompare_g1_step2_tp",
      choices = run_groupCompare_g1_step2_importFile()[[2]],
      selected = run_groupCompare_g1_step2_importFile()[[2]][[2]]
    )
  })
  
  # Update value variable for data input
  output$groupCompare_g1_step2_valueUI <- renderUI({
    req(run_groupCompare_g1_step2_importFile())
    if (input$groupCompare_g1_step2_method == "配对检验"){
      fluidRow(
        width = 10,
        pickerInput(
          inputId = "groupCompare_g1_step2_value",
          label = tags$span(
            "选择测试值变量",
            tags$span(icon("exclamation-circle")) %>%
              add_prompt(
                message = "对应示例中的Value",
                position = "right"
              )
          ),
          choices = run_groupCompare_g1_step2_importFile()[[2]],
          selected = run_groupCompare_g1_step2_importFile()[[2]][[3]],
          options = pickerOptions("title" = "尚未选择")
        )
      )
    }
    else if (input$groupCompare_g1_step2_method == "方差分析"){
      fluidRow(
        width = 10,
        pickerInput(
          inputId = "groupCompare_g1_step2_value",
          label = tags$span(
            "选择测试值变量",
            tags$span(icon("exclamation-circle")) %>%
              add_prompt(
                message = "对应示例中的Value",
                position = "right"
              )
          ),
          choices = run_groupCompare_g1_step2_importFile()[[2]],
          selected = run_groupCompare_g1_step2_importFile()[[2]][[3]],
          options = pickerOptions(
            "actionsBox" = TRUE, 
            "title" = "尚未选择",
            "selectAllText" = "全部选择",
            "deselectAllText" = "全部清除"
          ),
          multiple = TRUE
        )
      )
    }
  })
  
  # Process compareGroup_g1_ds
  compareGroup_g1_ds <- eventReactive(
    input$groupCompare_g1_step2_cfmRun,
    {
      req(input$groupCompare_g1_step2_id)
      req(input$groupCompare_g1_step2_tp)
      req(input$groupCompare_g1_step2_value)
      
      groupCompare_ds <- run_groupCompare_g1_step2_importFile()[[1]] %>% 
        select(all_of(c(input$groupCompare_g1_step2_id, input$groupCompare_g1_step2_tp, input$groupCompare_g1_step2_value))) %>% 
        drop_na()

      return(groupCompare_ds)
    }
  )
  
  run_groupCompare_g1_step2_repeat1_test <- reactive({
    if (input$groupCompare_g1_step2_method=="配对检验"){
      paired_ttest_func(
        ds = compareGroup_g1_ds(),
        tp = input$groupCompare_g1_step2_tp,
        value = input$groupCompare_g1_step2_value
      )
    }
  })
  
  #### groupCompare_g1_step2_rpt ####
  output$groupCompare_g1_step2_rpt <- renderText({
    run_groupCompare_g1_step2_repeat1_test()[["test_report"]]
  })
  
  #### groupCompare_g1_step2_rlt ####
  output$groupCompare_g1_step2_rlt <- renderPrint({
    run_groupCompare_g1_step2_repeat1_test()[["test_result"]]
  })
  
  #### groupCompare_g1_step2_plot ####
  # Update UI
  output$groupCompare_g1_step2_plotUI <- renderUI({
    if (input$groupCompare_g1_step2_plotType=="boxplot"){
      fluidRow(
        width = 12,
        awesomeCheckbox(
          inputId = "groupCompare_g1_step2_plotJitter",
          label = "散点抖动"
        )
      )
    }
    else {
      fluidRow(
        width = 12,
        pickerInput(
          inputId = "groupCompare_g1_step2_plotError",
          label = "误差线呈现",
          choices = c("不显示"="error_0","显示完整误差线"="error_1","显示一半误差线"="error_2")
        )
      )
    }
  })
  
  # Plot
  run_groupCompare_g1_step2_plot <- eventReactive(
    input$groupCompare_g1_step2_plotRun,
    {
      req(compareGroup_g1_ds())
      req(run_groupCompare_g1_step2_repeat1_test())
      
      if (input$groupCompare_g1_step2_plotTheme=="NULL"){grpVar <- "NULL"} 
      else {grpVar <- input$groupCompare_g1_step2_tp}
      
      # Plot type specific adjustment
      if (input$groupCompare_g1_step2_plotType=="boxplot"){
        p <- plotType_func(
          ds = compareGroup_g1_ds(), 
          xVar = input$groupCompare_g1_step2_tp, 
          yVar = input$groupCompare_g1_step2_value, 
          grpVar = grpVar, 
          ytickNum = input$groupCompare_g1_step2_plotYtickNum,
          plotType = input$groupCompare_g1_step2_plotType
        )
      } else {
        p <- plotType_func(
          ds = compareGroup_g1_ds(), 
          xVar = input$groupCompare_g1_step2_tp, 
          yVar = input$groupCompare_g1_step2_value, 
          grpVar = grpVar, 
          ytickNum = input$groupCompare_g1_step2_plotYtickNum,
          plotType = input$groupCompare_g1_step2_plotType,
          errorBar = input$groupCompare_g1_step2_plotError
        ) 
      }
        
      if (input$groupCompare_g1_step2_plotType=="boxplot"){
        # Add jitter
        if (input$groupCompare_g1_step2_plotJitter){
          set.seed(21) # To obtain the same jitter
          p <- p +
            geom_jitter(
              position = position_jitter(width = 0.3, height = 0.1), 
              size = 3, alpha = 0.3, stroke = NA, show.legend = FALSE
            )
        }
      }

      # Add theme
      if (input$groupCompare_g1_step2_plotTheme %in% c("Set1","Set2","Set3","Dark2","Paired")){
        p <- p + 
          scale_fill_brewer(palette = input$groupCompare_g1_step2_plotTheme) +
          scale_color_brewer(palette = input$groupCompare_g1_step2_plotTheme)
      } else if (input$groupCompare_g1_step2_plotTheme!="NULL"){
        p <- p + 
          eval(parse(text=paste0("scale_fill_",input$groupCompare_g1_step2_plotTheme,"()"))) +
          eval(parse(text=paste0("scale_color_",input$groupCompare_g1_step2_plotTheme,"()")))
      }

      # Legend position
      if (input$groupCompare_g1_step2_plotLegend=="legend_1"){
        p <- p + theme(legend.position = c(0.01, 0.99), legend.justification = c(0,1))
      } else if (input$groupCompare_g1_step2_plotLegend=="legend_2"){
        p <- p + theme(legend.position = c(0.01, 0.01), legend.justification = c(0,0))
      } else if (input$groupCompare_g1_step2_plotLegend=="legend_3"){
        p <- p + theme(legend.position = c(0.99, 0.99), legend.justification = c(1,1))
      } else if (input$groupCompare_g1_step2_plotLegend=="legend_4"){
        p <- p + theme(legend.position = c(0.99, 0.01), legend.justification = c(1,0))
      } else if (input$groupCompare_g1_step2_plotLegend=="legend_5"){
        p <- p + theme(legend.position = c(0.01, 0.99), legend.justification = c(1,0))
      } else {
        p <- p + theme(legend.position=input$groupCompare_g1_step2_plotLegend)
      }
      
      # Add significance
      if (input$groupCompare_g1_step2_plotSignif!="NULL"){
        p <- p + 
          stat_pvalue_manual(
            data = run_groupCompare_g1_step2_repeat1_test()[["sig_plot"]], 
            label = paste0("{",input$groupCompare_g1_step2_plotSignif,"}"),
            size = 4.8,
            hide.ns = TRUE
          )
      }
      
      p <- p + 
        labs(
          title = input$groupCompare_g1_step2_plotTitle,
          x = input$groupCompare_g1_step2_plotXlabel,
          y = input$groupCompare_g1_step2_plotYlabel,
          fill = input$groupCompare_g1_step2_plotLegendLabel,
          color = input$groupCompare_g1_step2_plotLegendLabel
        ) +
        theme(
          panel.grid=element_blank(),
          plot.title= element_text(
            hjust = input$groupCompare_g1_step2_plotTitlePosition,
            size = input$groupCompare_g1_step2_plotTitleFontSize
          ),
          axis.text = element_text(size=input$groupCompare_g1_step2_plotAxisFontSize),
          axis.title = element_text(size=input$groupCompare_g1_step2_plotAxisFontSize)
        )
      
      return(p)
    }
  )
  
  output$groupCompare_g1_step2_plot <- renderPlot({run_groupCompare_g1_step2_plot()})
  output$groupCompare_g1_step2_plotDownload_png = downloadHandler(
    filename = function(){
      paste("SingleGroupPlot", Sys.Date(), ".png", sep = "")
    },
    content = function(file){
      ggsave(
        file, 
        plot = run_groupCompare_g1_step2_plot(), 
        device = "png",
        dpi = input$groupCompare_g1_step2_plotDownload_dpi,
        width = input$groupCompare_g1_step2_plotDownload_width,
        height = input$groupCompare_g1_step2_plotDownload_ht,
        units = input$groupCompare_g1_step2_plotDownload_unit
      )
    }
  )
  
  #### Subtab 2, step 1: two groups test selection ####
  output$groupCompare_g2_step1_test <- renderPrint({
    paste0("Your selection is", input$groupCompare_g2_step1_repeat)
  })
  
  #### Tab: dataProcess ####
  #### Tool 1: Customize missing value ####
  # Import dataset
  run_dataProcess_missVal_importFile <- reactive({importFile_func(input$dataProcess_missVal_importFile)})
  
  # Update variable for input
  observe({
    updatePickerInput(
      session = session,
      "dataProcess_missVal_varSelect",
      choices = run_dataProcess_missVal_importFile()[[2]]
    )
  })
  
  # Process customized missing value
  run_dataProcess_missVal_tbl <- eventReactive(
    input$dataProcess_missVal_cfmRun,
    {
      validate(
        need(
          input$dataProcess_missVal_value,
          "未定义缺失值！"
        )
      )
      
      req(input$dataProcess_missVal_value)
      req(input$dataProcess_missVal_varSelect)

      miss_val <- strsplit(input$dataProcess_missVal_value, ",")[[1]]
      missVal_ds <- run_dataProcess_missVal_importFile()[[1]] %>% 
        select(all_of(c(input$dataProcess_missVal_varSelect))) %>%
        mutate(
          across(all_of(c(input$dataProcess_missVal_varSelect)), ~replace(.x, .x %in% miss_val, NA))
        )
      return(missVal_ds)
    }
  )
  
  run_dataProcess_missVal_tbl_msg <- eventReactive(
    input$dataProcess_missVal_cfmRun,
    {
      markdown("在生成表格后，还可对表格中的变量进行筛选；完成处理后，点击`Excel`或者`CSV`按钮保存为对应格式的文件。")
    }
  )
  
  output$dataProcess_missVal_tbl_msg <- renderText({run_dataProcess_missVal_tbl_msg()})
  
  output$dataProcess_missVal_tbl <- renderDT({
    run_dataProcess_missVal_tbl() %>% 
      datatable(
        filter = "top",
        extensions = "Buttons",
        rownames= FALSE, # Remove rownames when exporting data
        options = list(
          autoWidth=TRUE,
          dom = "Bftrip",
          scrollX = TRUE,
          buttons = list(
            list(extend = 'excel', title = NULL), # Remove "Exported data" title
            "csv")
        )
      )
  }, server = FALSE) # Output the whole records instead of those presented
  
  #### Tool 2: Format conversion ####
  # Import dataset
  run_dataProcess_dataFormat_importFile <- reactive({importFile_func(input$dataProcess_dataFormat_importFile)})
  
  # Update variable for input
  observe({
    updatePickerInput(
      session = session,
      "dataProcess_dataFormat_varSelect",
      choices = run_dataProcess_dataFormat_importFile()[[2]]
    )
  })
  
  # Update UI
  output$dataProcess_dataFormat_typeUI <- renderUI({
    req(input$dataProcess_dataFormat_varSelect)
    if (input$dataProcess_dataFormat_type=="long2wide"){
      fluidRow(
        column(
          width = 12,
          pickerInput(
            inputId = "dataProcess_dataFormat_namesFrom",
            label = tags$span(
              "选择要进行拓展的变量",
              tags$span(icon("exclamation-circle")) %>%
                add_prompt(
                  message = "对应长数据格式示例中的Timepoint",
                  size = "medium",
                  position = "right"
                )
            ),
            choices = input$dataProcess_dataFormat_varSelect,
            selected = input$dataProcess_dataFormat_varSelect[1]
          ),
          
          pickerInput(
            inputId = "dataProcess_dataFormat_valuesFrom",
            label = tags$span(
              "选择目标变量值",
              tags$span(icon("exclamation-circle")) %>%
                add_prompt(
                  message = "对应长数据格式示例中的Value",
                  size = "medium",
                  position = "right"
                )
            ),
            choices = input$dataProcess_dataFormat_varSelect,
            selected = input$dataProcess_dataFormat_varSelect[1]
          )
        )
      )
    }
    else {
      fluidRow(
        column(
          width = 12,
          pickerInput(
            inputId = "dataProcess_dataFormat_colsName",
            label = tags$span(
              "选择要进行合并的变量",
              tags$span(icon("exclamation-circle")) %>%
                add_prompt(
                  message = "对应宽数据格式示例中的T1,T2,...,Tn",
                  size = "medium",
                  position = "right"
                )
            ),
            choices = input$dataProcess_dataFormat_varSelect,
            options = pickerOptions(
              "actionsBox" = TRUE,
              "title" = "尚未选择",
              "selectAllText" = "全部选择",
              "deselectAllText" = "全部清除"
            ),
            multiple = TRUE
          )
        )
      )
    }
  })
  
  # Run dataProcess
  run_dataProcess_dataFormat_tbl <- eventReactive(
    input$dataProcess_dataFormat_cfmRun,
    {
      req(run_dataProcess_dataFormat_importFile())
      req(input$dataProcess_dataFormat_varSelect)
      
      dataProcess_ds_tmp <- run_dataProcess_dataFormat_importFile()[[1]] %>% 
        select(all_of(input$dataProcess_dataFormat_varSelect))
      
      if (input$dataProcess_dataFormat_type=="long2wide"){
        req(input$dataProcess_dataFormat_namesFrom)
        req(input$dataProcess_dataFormat_valuesFrom)
        dataProcess_ds <- long2wide_func(
          ds = dataProcess_ds_tmp, 
          namesFrom = input$dataProcess_dataFormat_namesFrom, 
          valuesFrom = input$dataProcess_dataFormat_valuesFrom
        )
      }
      else {
        req(input$dataProcess_dataFormat_colsName)
        dataProcess_ds <- wide2long_func(
          ds = dataProcess_ds_tmp, 
          colsName = input$dataProcess_dataFormat_colsName
        )
      }
      return(dataProcess_ds)
    }
  )
  
  run_dataProcess_dataFormat_tbl_msg <- eventReactive(
    input$dataProcess_dataFormat_cfmRun,
    {
      markdown("在生成表格后，还可对表格中的变量进行筛选；完成处理后，点击`Excel`或者`CSV`按钮保存为对应格式的文件。")
    }
  )
  
  output$dataProcess_dataFormat_tbl_msg <- renderText({run_dataProcess_dataFormat_tbl_msg()})
  
  output$dataProcess_dataFormat_tbl <- renderDT({
    run_dataProcess_dataFormat_tbl() %>% 
      datatable(
        filter = "top",
        extensions = "Buttons",
        rownames= FALSE, # Remove rownames when exporting data
        options = list(
          autoWidth=TRUE,
          dom = "Bftrip",
          scrollX = TRUE,
          buttons = list(
            list(extend = 'excel', title = NULL), # Remove "Exported data" title
            "csv")
        )
      )
  }, server=FALSE)
}
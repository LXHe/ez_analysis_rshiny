library(shiny)
library(readxl)
library(tidyverse)
library(haven)
library(shinyBS)
library(shinyjs)

#### Normality test analysis function ####
normTestAna_func <- function(var, group, ds){
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
 ds_final <- ds_nest %>%
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
 return(ds_final)
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
          p_eval <- "<font color=\"#de2d26\"><b>因样本量不足，无法计算</b></font>"
        }
        else if (!is.na(p_num[j]) & p_num[j]<=0.05){
          p_eval <- "<font color=\"#de2d26\"><b>不服从正态分布</b></font>"
        }
        else {
          p_eval <- "<font color=\"#31a354\"><b>服从正态分布</b></font>"
        }
        fb_cat <- paste0(
          fb_cat,
          "&emsp;在分组变量<font color=\"#2c7fb8\"><b>", cat_col, "</b></font>为\"<b>", cat_lvl[j], "</b>\"的水平上的数据", p_eval, " (P=", round(p_num[j],4),")。<br>"
        )
      }
      fb_num <- paste0(
        fb_num,
        "对于连续变量<font color=\"#54278f\"><b>", num_col[i],"</b></font>而言：<br>",
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
        p_eval <- "<font color=\"#de2d26\"><b>因样本量不足，无法计算</b></font>"
      }
      else if (!is.na(p_num) & p_num<=0.05){
        p_eval <- "<font color=\"#de2d26\"><b>不服从正态分布</b></font>"
      }
      else {
        p_eval <- "<font color=\"#31a354\"><b>服从正态分布</b></font>"
      }
      fb_num <- paste0(
        fb_num,
        "对于连续变量<font color=\"#54278f\"><b>", num_col[i],"</b></font>而言：其数据", p_eval, " (P=", round(p_num,4),")。<br><br>"
      )
    }
  }
  return(fb_num)
}

#### Dataframe format ####
htmlFormat_g1_repeat1 <- htmlTable(
  matrix(
    c(1,2,"...","n",
      10,14,"...",13,
      20,22,"...",6),
    ncol=3, byrow = FALSE
  ),
  header =  c("ID","T0","T1"),
  caption = markdown("将进行**配对T检验**（参数检验）或**Wilcoxon符号秩检验**（非参数检验）；数据文件需按照下面的格式准备："),
  tfoot = markdown("**ID**为受试者编号；**T0**和**T1**分别对应第一次和第二次测试值"),
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
  caption = markdown("将进行**重复测量方差分析**（参数检验）或**Friedman检验**（非参数检验）；数据文件需按照下面的格式准备："),
  tfoot = markdown("**ID**为受试者编号；**Timepoint**为测试时间点；**Value**为每个时间点对应的测试值"),
  collapse = "separate_shiny"
)

#### server function ####
function(input, output, session) {
  
  #### Tab: rawData ####
  #### Step 1: Import dataset ####
  run_rawData_importFile <- reactive({
    infile <- input$rawData_importFile # Extract infile path
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
    importFile
  })  
  
  # Present variable names of uploaded file
  rawData_varList <- reactive({names(run_rawData_importFile())})
  
  # Update full variables for data input
  observe({
    updatePickerInput(
      session = session,
      "rawData_contSelect",
      choices = rawData_varList()
    )
  })
  
  # Update categorical variables for data input
  observe({
    updatePickerInput(
      session = session,
      "rawData_catSelect",
      choices = rawData_varList()
    )
  })
  
  # Activate customised missing value button
  observe({
    if(input$rawData_missValueCheck){
      shinyjs::enable(id="rawData_missValue")
    } 
    else{
      shinyjs::disable(id="rawData_missValue")
      updateTextInput(
        session = session,
        inputId = "rawData_missValue",
        value = ""
      )
    }
  })
  
  run_rawData_outFileInfo <- eventReactive(
    input$rawData_cfmRun_step1,
    {
      validate(
        need(
          length(intersect(input$rawData_contSelect,input$rawData_catSelect))==0, 
          paste0(
            "选取的连续变量与分类变量中包含重复变量：",
            paste(intersect(input$rawData_contSelect,input$rawData_catSelect), collapse="，"),
            "。需要更正！"
          )
        ),
        need(
          length(input$rawData_contSelect)>0 | length(input$rawData_catSelect)>0,
          paste("需选取至少一个连续变量或分类变量！")
        )
      )
      
      # Create dataset for rawData and customise missValue if defined
      if (input$rawData_missValueCheck){
        miss_val <- strsplit(input$rawData_missValue, ",")[[1]]
        rawData_ds <- run_rawData_importFile() %>% 
          select(all_of(c(input$rawData_contSelect,input$rawData_catSelect))) %>%
          mutate(
            across(all_of(c(input$rawData_contSelect,input$rawData_catSelect)), ~replace(.x, .x %in% miss_val, NA)),
            across(all_of(input$rawData_contSelect), as.numeric),
            across(all_of(input$rawData_catSelect), as.factor)
          )
      }
      else {
        rawData_ds <- run_rawData_importFile() %>% 
          select(all_of(c(input$rawData_contSelect,input$rawData_catSelect))) %>% 
          mutate(
            across(all_of(input$rawData_contSelect), as.numeric),
            across(all_of(input$rawData_catSelect), as.factor)
          )
      }
      
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
      if(!is.null(input$rawData_catSelect)){
        shinyjs::enable(id="rawData_normTestCatVar")
        shinyjs::enable(id="rawData_desCatVar")
        updatePickerInput(
          session = session,
          "rawData_normTestCatVar",
          choices = c("(无)", input$rawData_catSelect)
        )
        updatePickerInput(
          session = session,
          "rawData_desCatVar",
          choices = c("(无)", input$rawData_catSelect)
        )
      } 
      else {
        shinyjs::disable(id="rawData_normTestCatVar")
        shinyjs::reset(id="rawData_normTestCatVar")
        shinyjs::disable(id="rawData_desCatVar")
        shinyjs::reset(id="rawData_desCatVar")
      }
      
      if(!is.null(input$rawData_contSelect)){
        shinyjs::enable(id="rawData_cfmRun_step2")
        shinyjs::enable(id="rawData_normTestContVar")
        
        updatePickerInput(
          session = session,
          "rawData_normTestContVar",
          choices = input$rawData_contSelect
        )
      } 
      else {
        shinyjs::disable(id="rawData_cfmRun_step2")
        
        shinyjs::disable(id="rawData_normTestContVar")
        shinyjs::reset(id="rawData_normTestContVar")
        
        shinyjs::disable(id="rawData_normTestCatVar")
      }
      
      # Output information
      list(rawData_ds,outInfo)
    }
  )
  
  output$rawData_outFileInfo <- renderText({
    paste(
      "共导入<b>", dim(run_rawData_outFileInfo()[[1]])[1], "</b>条记录数据。",
      run_rawData_outFileInfo()[[2]]
    )
  })
  
  #### Step 2: Normality test ####
  rawData_normTestDs <- eventReactive(
    input$rawData_cfmRun_step2,
    {
      req(input$rawData_normTestContVar)
      
      if (input$rawData_normTestCatVar == "(无)"){
        normTest_ds <- run_rawData_outFileInfo()[[1]] %>% 
          select(all_of(c(input$rawData_normTestContVar)))
      }
      else{
        normTest_ds <- run_rawData_outFileInfo()[[1]] %>% 
          select(all_of(c(input$rawData_normTestContVar,input$rawData_normTestCatVar)))
      }
      
      normTest_ds
    }
  )
  
  # Normality test: return a list of Normality test results by continuous variables
  run_rawData_normTestRlt <- eventReactive(
    input$rawData_cfmRun_step2,
    {
      normAnaList <- list()
      ds_tmp <- rawData_normTestDs()
      group_tmp <- input$rawData_normTestCatVar
      for (i in seq(length(input$rawData_normTestContVar))){
        var_tmp <- input$rawData_normTestContVar[i]
        normAnaList[[i]] <- normTestAna_func(
          var = var_tmp,
          group = group_tmp,
          ds = ds_tmp
        )
      }
      names(normAnaList) <- input$rawData_normTestContVar
      normAnaList
    }
  )
  
  output$rawData_normTestRlt <- renderPrint({run_rawData_normTestRlt()})
  
  # Normality test report
  run_rawData_normTestRpt <- eventReactive(
    input$rawData_cfmRun_step2,
    {
      normTestFeedback_func( 
        dsList = run_rawData_normTestRlt(),
        num_col = input$rawData_normTestContVar,
        cat_col = input$rawData_normTestCatVar
      )
    }
  )
  
  output$rawData_normTestRpt <- renderText({run_rawData_normTestRpt()})
  
  # Normality histogram
  observeEvent(
    input$rawData_cfmRun_step2,
    {
      updatePickerInput(
        session = session,
        "rawData_normTestHist_var",
        choices = input$rawData_normTestContVar,
        selected = input$rawData_normTestContVar[1]
      )
    }
  )
  
  normTestGroupVar <- eventReactive(
    input$rawData_cfmRun_step2,
    {
      if (input$rawData_normTestCatVar == "(无)"){"NULL"}
      else {input$rawData_normTestCatVar}
    }
  )
  
  run_rawData_normTestHist <- reactive({
    req(rawData_normTestDs())
    req(normTestGroupVar())
    
    rawData_histPlot <- ggplot(
      data = rawData_normTestDs(), 
      aes_string(x=input$rawData_normTestHist_var, fill=normTestGroupVar())) +  
      geom_histogram(
        aes(y = ..density..), 
        alpha = 0.7, 
        bins=input$rawData_normTestHist_bin, 
        position="identity", 
        color="black", 
        size=0.4
      )+ 
      geom_density(alpha=0.6, size=0.3)+
      labs(x = input$rawData_normTestHist_var,  
           y = "概率密度",  
           title = paste0("变量",input$rawData_normTestHist_var,"分布直方图"))
      ggplotly(rawData_histPlot)
  })
  
  output$rawData_normTestHist <- renderPlotly({run_rawData_normTestHist()})
  
  #### Step 3: Descriptive table ####
  run_rawData_desTable <- eventReactive(
    input$rawData_cfmRun_step3,
    {
      req(run_rawData_outFileInfo())
      
      if (input$rawData_desCatVar != "(无)"){
        # Generate parental category line
        catLength <- length(levels(run_rawData_outFileInfo()[[1]][[input$rawData_desCatVar]]))
        catStat <- NULL
        for (i in seq(catLength)){
          statName <- paste0("stat_", i)
          catStat <- append(catStat, statName)
        }
        table_summary <- run_rawData_outFileInfo()[[1]] %>% 
          tbl_summary(
            by = input$rawData_desCatVar,
            type = all_continuous()~"continuous2",
            statistic = all_continuous()~c("{mean} ± {sd}", "{median} ({p25}, {p75})", "{min}, {max}"),
            digits = list(all_continuous()~input$rawData_desDigit),
            missing_text = "缺失"
          ) %>% 
          add_overall() %>%
          modify_spanning_header(
            catStat ~ paste0("**", input$rawData_desCatVar, "**")
          )  %>% 
          add_p(
            pvalue_fun = function(x) {
              if_else(
                is.na(x), 
                "未能计算",
                if_else(x<0.001, "<0.001", format(round(x,3),scientific = F))
              )
            }
          ) %>% 
          separate_p_footnotes()
      } else {
        table_summary <- run_rawData_outFileInfo()[[1]] %>% 
          tbl_summary(
            type = all_continuous()~"continuous2",
            statistic = all_continuous()~c("{mean} ± {sd}", "{median} ({p25}, {p75})", "{min}, {max}"),
            digits = list(all_continuous()~input$rawData_desDigit),
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
  
  output$rawData_desTable = render_gt({
    run_rawData_desTable()
  })
  
  output$download_rawData_desTable_docx = downloadHandler(
    filename = function(){
      paste("SummaryTable-", Sys.Date(), ".docx", sep = "")
    },
    content = function(file){
      run_rawData_desTable() %>% 
        gt::gtsave(filename = file)
    }
  )
  
  #### Tab: groupCompare ####
  #### Subtab 1, step 1: 1 group test selection ####
  run_groupCompare_g1_tblFormat <- eventReactive(
    input$groupCompare_g1_cfmRun_step1,
    {
      if (input$groupCompare_g1_repeat == "repeat1"){
        htmlFormat_g1_repeat1
      }
      else if (input$groupCompare_g1_repeat == "repeat2"){
        htmlFormat_g1_repeat2
      }
    }
  )
  
  output$groupCompare_g1_tblFormat <- renderUI({run_groupCompare_g1_tblFormat()})
  
  #### Subtab 2, step 1: 2 group test selection ####
  output$groupCompare_g2_test <- renderPrint({
    paste0("Your selection is", input$groupCompare_g2_repeat)
  })
}
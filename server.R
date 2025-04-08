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
      "将进行**单样本t检验**；<br>
      在<font color=\"",color_lvl_2,"\"><b>步骤2</b></font>的***选择统计方法***选项中选择<font color=\"",
      color_lvl_5,"\"><b>单样本t检验</b></font>；<br>
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
    paste0(
      "将进行**配对t检验**（参数检验）或**配对样本Wilcoxon符号秩检验**（非参数检验）；<br>
      在<font color=\"",color_lvl_2,"\"><b>步骤2</b></font>的***选择统计方法***选项中选择<font color=\"",
      color_lvl_5,"\"><b>配对检验</b></font>；<br>
      数据文件需按照下面的长数据格式准备："
    )
  ),
  tfoot = markdown(
    "**ID**为受试者编号，**Timepoint**为测试时间点，**Value**为每个时间点对应的测试值；<br>
    若数据不为上述格式，可在左侧`数据处理`一栏中进行数据格式转化"
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
    paste0(
      "将进行**单因素重复测量方差分析**（参数检验）或**Friedman检验**（非参数检验）；<br>
      在<font color=\"",color_lvl_2,"\"><b>步骤2</b></font>的***选择统计方法***选项中选择<font color=\"",
      color_lvl_5,"\"><b>方差分析</b></font>；<br>
      数据文件需按照下面的长数据格式准备："
    )
  ),
  tfoot = markdown(
    "**ID**为受试者编号，**Timepoint**为测试时间点，**Value**为每个时间点对应的测试值；<br>
    若数据不为上述格式，可在左侧`数据处理`一栏中进行数据格式转化"
  ),
  collapse = "separate_shiny"
)



#### server function ####
function(input, output, session) {
  
  ##### Tab: rawData #####
  ###### Step 1: Import dataset ######
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
  
  ###### Step 2: Normality test ######
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
  
  ###### Step 3: Descriptive table ######
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
  
  ##### Tab: groupCompare #####
  ###### Subtab 1, step 1: single group test selection ######
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
  
  ###### Subtab 1, step 2: Import dataset ######
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
  
  # Update value variable for data input
  observe({
    updatePickerInput(
      session = session,
      "groupCompare_g1_step2_value",
      choices = run_groupCompare_g1_step2_importFile()[[2]],
      selected = run_groupCompare_g1_step2_importFile()[[2]][[2]]
    )
  })
  
  # Update timepoint variable for data input
  output$groupCompare_g1_step2_tpUI <- renderUI({
    req(run_groupCompare_g1_step2_importFile())
    if (input$groupCompare_g1_step2_method == "单样本t检验"){
      fluidRow(
        column(
          width = 10,
          numericInputIcon(
            inputId = "groupCompare_g1_step2_mu",
            label = tags$span(
              "输入想要比较的目标值",
              tags$span(icon("exclamation-circle")) %>%
                add_prompt(
                  message = "对应示例场景1中的全国平均身高",
                  position = "left"
                )
            ),
            value = 1.7
          )
        )
      )
    }
    else if (input$groupCompare_g1_step2_method == "配对检验"){
      fluidRow(
        column(
          width = 10,
          pickerInput(
            inputId = "groupCompare_g1_step2_tp",
            label = tags$span(
              "选择测试时间点变量",
              tags$span(icon("exclamation-circle")) %>%
                add_prompt(
                  message = "对应示例中的Timepoint",
                  position = "right"
                )
            ),
            choices = run_groupCompare_g1_step2_importFile()[[2]],
            selected = run_groupCompare_g1_step2_importFile()[[2]][[3]],
            options = pickerOptions("title" = "尚未选择")
          )
        )
      )
    }
    else if (input$groupCompare_g1_step2_method == "方差分析"){
      fluidRow(
        column(
          width = 10,
          pickerInput(
            inputId = "groupCompare_g1_step2_tp",
            label = tags$span(
              "选择测试时间点变量",
              tags$span(icon("exclamation-circle")) %>%
                add_prompt(
                  message = "对应示例中的Timepoint",
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
      )
    }
  })
  
  # Process compareGroup_g1_ds
  compareGroup_g1_ds <- eventReactive(
    input$groupCompare_g1_step2_cfmRun,
    {
      if (input$groupCompare_g1_step2_method=="单样本t检验"){
        req(input$groupCompare_g1_step2_id)
        req(input$groupCompare_g1_step2_value)
        req(input$groupCompare_g1_step2_mu)
        
        groupCompare_ds <- run_groupCompare_g1_step2_importFile()[[1]] %>% 
          select(all_of(c(input$groupCompare_g1_step2_id, input$groupCompare_g1_step2_value))) %>% 
          drop_na()
      } else {
        req(input$groupCompare_g1_step2_id)
        req(input$groupCompare_g1_step2_tp)
        req(input$groupCompare_g1_step2_value)
        
        groupCompare_ds <- run_groupCompare_g1_step2_importFile()[[1]] %>% 
          select(all_of(c(input$groupCompare_g1_step2_id, input$groupCompare_g1_step2_tp, input$groupCompare_g1_step2_value))) %>% 
          drop_na()
      }
      return(groupCompare_ds)
    }
  )
  
  run_groupCompare_g1_step2_repeat1_test <- eventReactive(
    input$groupCompare_g1_step2_cfmRun,
    {
      if (input$groupCompare_g1_step2_method=="单样本t检验"){
        ana_rlt <- single_ttest_func(
          ds = compareGroup_g1_ds(),
          value = input$groupCompare_g1_step2_value,
          mu = input$groupCompare_g1_step2_mu
        )
      }
      else if(input$groupCompare_g1_step2_method=="配对检验"){
        ana_rlt <- paired_ttest_func(
          ds = compareGroup_g1_ds(),
          tp = input$groupCompare_g1_step2_tp,
          value = input$groupCompare_g1_step2_value
        )
      }
      return(ana_rlt)
    }
  )
  
  ####### groupCompare_g1_step2_rpt #######
  output$groupCompare_g1_step2_rpt <- renderText({
    run_groupCompare_g1_step2_repeat1_test()[["test_report"]]
  })
  
  ####### groupCompare_g1_step2_rlt #######
  output$groupCompare_g1_step2_rlt <- renderPrint({
    run_groupCompare_g1_step2_repeat1_test()[["test_result"]]
  })
  
  ####### groupCompare_g1_step2_plot #######
  # Update UI
  output$groupCompare_g1_step2_plotSubUI <- renderUI({
    if (input$groupCompare_g1_step2_plotType=="boxplot"){
      fluidRow(
        width = 12,
        awesomeCheckbox(
          inputId = "groupCompare_g1_step2_plotJitter",
          label = "散点抖动"
        ),
        awesomeCheckbox(
          inputId = "groupCompare_g1_step2_plotOrientation",
          label = "水平呈现"
        )
      )
    }
    else {
      fluidRow(
        width = 12,
        pickerInput(
          inputId = "groupCompare_g1_step2_plotError",
          label = "误差线设置",
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
      if (input$groupCompare_g1_step2_method=="单样本t检验"){
        p <- plotTypeSingle_func(
          ds = compareGroup_g1_ds(), 
          yVar = input$groupCompare_g1_step2_value, 
          ytickNum = input$groupCompare_g1_step2_plotYtickNum,
          plotType = input$groupCompare_g1_step2_plotType,
          errorBar = input$groupCompare_g1_step2_plotError,
          orientation = input$groupCompare_g1_step2_plotOrientation
        )
      }
      else {
        req(run_groupCompare_g1_step2_repeat1_test())
        
        if (input$groupCompare_g1_step2_plotTheme=="NULL"){grpVar <- "NULL"} 
        else {grpVar <- input$groupCompare_g1_step2_tp}
        
        p <- plotTypeMulti_func(
          ds = compareGroup_g1_ds(), 
          xVar = input$groupCompare_g1_step2_tp, 
          yVar = input$groupCompare_g1_step2_value, 
          grpVar = grpVar, 
          ytickNum = input$groupCompare_g1_step2_plotYtickNum,
          plotType = input$groupCompare_g1_step2_plotType,
          errorBar = input$groupCompare_g1_step2_plotError,
          orientation = input$groupCompare_g1_step2_plotOrientation
        ) 
          
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
      
      return(p)
    }
  )
  
  output$groupCompare_g1_step2_plot <- renderPlot({run_groupCompare_g1_step2_plot()})
  output$groupCompare_g1_step2_plotDownload <- downloadHandler(
    filename = function(){
      paste("SingleGroupPlot", Sys.Date(), ".", input$groupCompare_g1_step2_plotDownload_format, sep = "")
    },
    content = function(file) {
      # Process dynamic parameters
      params <- switch(
        input$groupCompare_g1_step2_plotDownload_format,
        png = list(device = "png"),
        jpeg = list(device = "jpeg", quality = input$groupCompare_g1_step2_plotDownload_quality),
        pdf = list(device = cairo_pdf)
      )
      # Save the plot
      do.call(
        ggsave, 
        c(list(
            filename = file,
            plot = run_groupCompare_g1_step2_plot(),
            dpi = input$groupCompare_g1_step2_plotDownload_dpi,
            width = input$groupCompare_g1_step2_plotDownload_width,
            height = input$groupCompare_g1_step2_plotDownload_ht,
            units = input$groupCompare_g1_step2_plotDownload_unit,
            limitsize=FALSE # Allow large size
          ),
          params
        )
      )
    }
  )
  
  ###### Subtab 2, step 1: two groups test selection ######
  output$groupCompare_g2_step1_test <- renderPrint({
    paste0("Your selection is", input$groupCompare_g2_step1_repeat)
  })
  
  ##### Tab: dataProcess #####
  ###### Tool 1: Customize missing value ######
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
  
  ###### Tool 2: Format conversion ######
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
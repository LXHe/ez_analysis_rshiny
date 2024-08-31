library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(gtsummary)
library(gt)
library(DT)
library(shinyWidgets)
library(shinyjs)
library(shinyBS)
library(prompter)
library(tidyverse)
library(plotly)

# Header setting
header <- dashboardHeader(
  title = "运动与健康大数据处理平台",
  titleWidth = "22%" # width in percentage
)

# Sidebar setting
sidebar <- dashboardSidebar(
  width = "22%",
  collapsed = TRUE,
  sidebarMenu(
    id = "sidebar",
    # First tab: Raw data
    menuItem("数据描述", tabName="rawData", icon=icon("magnifying-glass-chart")),
    # Second tab: Summary data
    menuItem(
      "统计分析", 
      icon=icon("file-contract"),
      menuSubItem("Descriptive", tabName="descriptiveTable"),
      menuSubItem("Analysis", tabName="analysisTable")
    ),
    # Third tab: Plot
    menuItem("数据可视化", tabName="sumPlot", icon=icon("chart-pie")),
    # Fourth tab: About
    menuItem("About", tabName="introduction", icon=icon("book-reader"))
  )
)

# Body setting
body <- dashboardBody(
  
  # Change validation need message color
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "index.css")
  ),
  
  use_prompt(),
  useShinyjs(),
  
  # Define hover message
  bsTooltip(
    id = "rawData_cfmRun_step1",
    title = "每次设定完成后，请点击此按钮更新数据"
  ),
  bsTooltip(
    id = "rawData_cfmRun_step2",
    title = "每次设定完成后，请点击此按钮运行本步骤"
  ),
  
  # Main dashboard of Table
  tabItem(
    tabName = "rawData",
    shinydashboardPlus::box(
      style = "margin: 0px",
      title = "步骤1：数据导入与变量设置",
      status = "primary",
      width = NULL,
      solidHeader = TRUE,
      collapsible = TRUE,
      label = actionBttn(
        inputId = "rawData_cfmRun_step1",
        label = tags$span(
          tags$span(icon("circle-play")),
          tags$span("运行本步骤", style="font-size: 13px")
        ),
        size = "s",
        color = "primary"
      ),
      
      fluidRow(
        column(
          width = 3,
          fileInput(
            inputId = "rawData_importFile", 
            label = tags$span(
              "数据导入",
              tags$span(icon("exclamation-circle")) %>%
                add_prompt(
                  message = "支持csv,excel,SAS和stata文件",
                  position = "right"
                )
            ),
            accept = c(".csv", ".xlsx", ".xls", ".sas7bdat", ".dta"),
            buttonLabel = "选择文件...",
            placeholder = "尚未选择"
          )
        ),
        column(
          width = 3,
          pickerInput(
            inputId = "rawData_contSelect",
            label = tags$span(
              "选取连续变量",
              tags$span(icon("exclamation-circle")) %>%
                add_prompt(
                  message = "连续变量指数据是连续的数值（比如身高、体重）；若无，则不用选择",
                  size = "medium",
                  position = "right"
                )
            ),
            choices = NULL,
            options = pickerOptions(
              "actionsBox" = TRUE, 
              "title" = "尚未选择",
              "selectAllText" = "全部选择",
              "deselectAllText" = "全部清除"
            ),
            multiple = TRUE
          )
        ),
        column(
          width = 3,
          pickerInput(
            inputId = "rawData_catSelect",
            label = tags$span(
              "选取分类变量",
              tags$span(icon("exclamation-circle")) %>%
                add_prompt(
                  message = "分类变量指数据是分类信息（比如组别、性别、程度）；若无，则不用选择",
                  size = "medium",
                  position = "right"
                )
            ),
            choices = NULL,
            options = pickerOptions(
              "actionsBox" = TRUE, 
              "title" = "尚未选择",
              "selectAllText" = "全部选择",
              "deselectAllText" = "全部清除"
            ),
            multiple = TRUE
          )
        ),
        column(
          width = 3,
          div(
            style = "display: inline-block;vertical-align: top;position: relative;top: -10px;",
            checkboxInput(
              inputId = "rawData_missValueCheck", 
              label = tags$span(
                HTML("<span style=\"font-family: 'times'; font-size: 14px; font-weight: bold\">自定义缺失值</span>"),
                tags$span(icon("exclamation-circle")) %>%
                  add_prompt(
                    message = "若有多种缺失值，则需用英文逗号将其分开。例如：无,999,-",
                    size = "medium",
                    position = "right"
                  )
              )
            )
          ),
          div(
            style = "display: inline-block;vertical-align: top;position: relative;top: -20px;",
            textInputIcon(
              inputId = "rawData_missValue",
              label = NULL,
              value = NULL,
              icon = NULL
            )
          )
        )
      ),
      htmlOutput("rawData_outFileInfo")
    ),
    
    shinydashboardPlus::box(
      style = "margin: 0px",
      title = "步骤2：正态性检验（仅限于选择了连续变量的情况）",
      status = "primary",
      width = NULL,
      solidHeader = TRUE,
      collapsible = TRUE,
      label = actionBttn(
        inputId = "rawData_cfmRun_step2",
        label = tags$span(
          tags$span(icon("circle-play")),
          tags$span("运行本步骤", style="font-size: 13px")
        ),
        size = "s",
        color = "primary"
      ),
      
      tags$p("正态性检验通常使用下列两种方式："),
      tags$p("1. 公式法：当样本量小于50时，使用",strong("Shapiro-Wilk"),"检验；", "反之，使用",strong("Kolmogorov–Smirnov"),"检验。当P值大于0.05时，说明数据服从正态分布。公式法对数据正态性的判定较为严苛。"),
      tags$p("2. 图像法：当样本量较大时，也可以通过",strong("直方图"),"和",strong("密度曲线"),"判断数据的分布情况。当数据呈现",strong("中间高"),"、",strong("两边低"),"且",strong("左右基本对称"),"的情况时，可大致认为数据服从正态分布。"),
      tags$p("若进行组间比较，需要按照组别，对组内数据分别进行正态性检验。"),
      
      fluidRow(
        column(
          width = 4,
          pickerInput(
            inputId = "rawData_normTestContVar",
            label = tags$span(
              "选取需要进行正态性检验的变量",
              tags$span(icon("exclamation-circle")) %>%
                add_prompt(
                  message = "基于步骤1中选取的连续变量进行选择；至少需要选择1个",
                  size = "medium",
                  position = "right"
                )
            ),
            choices = NULL,
            options = pickerOptions(
              "actionsBox" = TRUE, 
              "title" = "尚未选择",
              "selectAllText" = "全部选择",
              "deselectAllText" = "全部清除"
            ),
            multiple = TRUE
          )
        ),
        column(
          width = 4,
          pickerInput(
            inputId = "rawData_normTestCatVar",
            label = tags$span(
              "选取分组变量",
              tags$span(icon("exclamation-circle")) %>%
                add_prompt(
                  message = "基于步骤1中选取的分类变量进行选择；若不进行分组检验，则选(无)",
                  size = "medium",
                  position = "right"
                )
            ),
            choices = c("(无)")
          )
        )        
      ),
      
      fluidRow(
        tabBox(
          id = "anaTab",
          title = NULL,
          width = 12,
          tabPanel(
            title = "分析报告",
            icon = icon("list-ul"),
            htmlOutput("rawData_normTestRpt")
          ),
          tabPanel(
            title = "统计结果",
            icon = icon("clipboard", class="fa-solid fa-clipboard"),
            verbatimTextOutput("rawData_normTestRlt")
          ),
          tabPanel(
            title = "数据分布图",
            icon = icon("chart-simple"),
            plotlyOutput("rawData_normTestHist")
          )
        )
      )
    )
  )
)

# UI summary
ui <- shinydashboardPlus::dashboardPage(
  options = list(sidebarExpandOnHover = TRUE),
  header, 
  sidebar,
  body
)
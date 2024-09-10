library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(gtsummary)
library(gt)
library(DT)
library(shinyWidgets)
library(shinyjs)
library(shinyBS)
library(shinybusy)
library(tidyverse)
library(plotly)
library(prompter)


#### Header setting ####
header <- dashboardHeader(
  # title = "运动与健康大数据处理平台",
  title = NULL,
  titleWidth = "22%" # width in percentage
)

#### Sidebar setting ####
sidebar <- dashboardSidebar(
  width = "22%",
  collapsed = TRUE,
  sidebarMenu(
    id = "sidebar",
    # First tab: Raw data with data description
    menuItem("数据描述", tabName="rawData", icon=icon("magnifying-glass-chart")),
    # Second tab: statistics
    menuItem(
      "统计分析", 
      icon=icon("file-contract"),
      menuSubItem("组间比较", tabName="groupCompare"),
      menuSubItem("回归分析", tabName="regAnalysis")
    ),
    # Third tab: Data visualization
    menuItem("数据可视化", tabName="dataVisual", icon=icon("chart-pie")),
    # Fourth tab: About us
    menuItem("关于我们", tabName="introduction", icon=icon("book-reader"))
  )
)

#### Body setting ####
body <- dashboardBody(
  
  #### Change validation need message color ####
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "index.css")
  ),
  
  #### Activate shinyjs (library:shinyjs) ####
  useShinyjs(),
  
  #### Define busy indicator (library:shinybusy) ####
  add_busy_spinner(spin = "trinity-rings", color = "#3182bd", timeout = 1000),
  
  #### Define hover message (library:shinyBS) ####
  bsTooltip(
    id = "rawData_cfmRun_step1",
    title = "每次设定完成后，请点击此按钮更新数据"
  ),
  bsTooltip(
    id = c("rawData_cfmRun_step2","rawData_cfmRun_step3","groupCompare_g1_cfmRun_step1"),
    title = "每次设定完成后，请点击此按钮运行本步骤"
  ),
  
  #### Main dashboard of Table ####
  tabItems(
    #### Tab:rawData ####
    tabItem(
      tabName = "rawData",
      #### Step 1 ####
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
            tags$span("运行", style="font-size: 13px")
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
      
      #### Step 2 ####
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
            tags$span("运行", style="font-size: 13px")
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
              fluidRow(
                column(
                  width = 4,
                  pickerInput(
                    inputId = "rawData_normTestHist_var",
                    label = "选择变量",
                    options = pickerOptions("title" = "尚未选择"),
                    choices = NULL
                  )
                ),
                column(
                  width = 4,
                  sliderInput(
                    inputId = "rawData_normTestHist_bin",
                    label = "调整直条数",
                    min = 1,
                    max = 50,
                    value = 20
                  )
                )
              ),
              plotlyOutput("rawData_normTestHist")
            )
          )
        )
      ),
      
      #### Step 3 ####
      shinydashboardPlus::box(
        style = "margin: 0px",
        title = "步骤3：描述性表格",
        status = "primary",
        width = NULL,
        solidHeader = TRUE,
        collapsible = TRUE,
        label = actionBttn(
          inputId = "rawData_cfmRun_step3",
          label = tags$span(
            tags$span(icon("circle-play")),
            tags$span("运行", style="font-size: 13px")
          ),
          size = "s",
          color = "primary"
        ),
        
        tags$p("此部分将对步骤1中选定的变量进行描述性统计。"),
        tags$p("对连续变量而言："),
        tags$p("1.", strong("符合正态分布"), "的数据将以",strong("均值 ± 标准差"), "的形式呈现并选用", strong("参数检验方法"), "进行组间比较。"),
        tags$p("2.", strong("不符合正态分布"), "的数据将以",strong("中位数[下四分位数,上四分位数]"), "的形式呈现并选用", strong("非参数检验方法"), "进行组间比较。"),
        tags$p("对分类变量而言："),
        tags$p("1.数据将以", strong("频数(百分数)"), "的形式呈现。"),        
        tags$p("2. 样本总数", strong("不低于40"), "且", strong("各组期望频数不低于5"), "的数据将选用", strong("卡方检验"), "进行组间比较；反之，选用", strong("Fisher确切概率法。")),
        fluidRow(
          column(
            width = 3,
            pickerInput(
              inputId = "rawData_desCatVar",
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
          ),
          column(
            width = 3,
            numericInput(
              inputId = "rawData_desDigit",
              label = "调整小数点位数",
              value = 2,
              min = 0,
              max = 3
            )
          )
        ),
        
        fluidRow(
          column(
            width = 12,
            tableOutput("rawData_desTable"),
            downloadBttn("download_rawData_desTable_docx", label = "保存为word文件", size = "xs")
          )
        )
      )
    ),
    
    #### Tab:groupCompare ####
    tabItem(
      tabName = "groupCompare",
      fluidRow(
        tabBox(
          width = 12,
          tabPanel(
            title = "单组样本",
            
            shinydashboardPlus::box(
              style = "margin: 0px",
              title = "步骤1：数据准备",
              status = "primary",
              width = NULL,
              solidHeader = TRUE,
              collapsible = TRUE,
              label = actionBttn(
                inputId = "groupCompare_g1_cfmRun_step1",
                label = tags$span(
                  tags$span(icon("circle-play")),
                  tags$span("运行", style="font-size: 13px")
                ),
                size = "s",
                color = "primary"
              ),
              
              HTML(
                "<p>此分析仅适用于<font color=\"#de2d26\"><b>单个组</b></font>中<font color=\"#de2d26\"><b>重复测试</b></font>的<font color=\"#de2d26\"><b>连续变量</b></font>数据。
                目的在于比较这些重复测试的数据之间是否有差异。<br>例如：在早、中、晚三个时间点测试一批短跑运动员的100米成绩，比较这三个时间点的成绩是否有差别。</p>"
              ),

              fluidRow(
                column(
                  width = 3,
                  selectInput(
                    inputId = "groupCompare_g1_repeat",
                    label = "数据重复测试情况",
                    choices = c(
                      "重复测试一次" = "repeat1",
                      "重复测试两次及以上" = "repeat2"
                    ),
                    selected = "重复测试一次"
                  )
                ),
                column(
                  width = 9,
                  htmlOutput("groupCompare_g1_tblFormat")
                )
              )
            )
          ),
          tabPanel(
            title = "两组样本",
            #### Step 1 ####
            shinydashboardPlus::box(
              style = "margin: 0px",
              title = "步骤1：统计方法确定",
              status = "primary",
              width = NULL,
              solidHeader = TRUE,
              collapsible = TRUE,
              label = actionBttn(
                inputId = "groupCompare_cfmRun_step1",
                label = tags$span(
                  tags$span(icon("circle-play")),
                  tags$span("运行", style="font-size: 13px")
                ),
                size = "s",
                color = "primary"
              ),
              
              fluidRow(
                column(
                  width = 3,
                  pickerInput(
                    inputId = "groupCompare_g2_repeat",
                    label = "数据重复测试情况",
                    choices = c(
                      "无重复测试" = "repeat0",
                      "重复测试1次" = "repeat1",
                      "重复测试2次及以上" = "repeat2"
                    ),
                    selected = "无重复测试"
                  )
                ),
                textOutput("groupCompare_g2_test")
              )
            ),
            
            #### Step 2 ####
            shinydashboardPlus::box(
              style = "margin: 0px",
              title = "步骤2：数据导入与变量设置",
              status = "primary",
              width = NULL,
              solidHeader = TRUE,
              collapsible = TRUE,
              label = actionBttn(
                inputId = "groupCompare_cfmRun_step2",
                label = tags$span(
                  tags$span(icon("circle-play")),
                  tags$span("运行", style="font-size: 13px")
                ),
                size = "s",
                color = "primary"
              ),
              
              fluidRow(
                column(
                  width = 3,
                  fileInput(
                    inputId = "groupCompare_importFile", 
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
                )
              )
            )
          ),
          tabPanel(
            title = "三组及以上样本"
          )
        )
      )
    ),
    
    #### Tab:regAnalysis ####
    tabItem(
      tabName = "regAnalysis"
    )
  )
)

#### UI summary ####
ui <- shinydashboardPlus::dashboardPage(
  options = list(sidebarExpandOnHover = TRUE),
  header, 
  sidebar,
  body
)
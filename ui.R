library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(gtsummary)
library(gt)
library(htmlTable)
library(DT)
library(shinyWidgets)
library(shinyjs)
library(shinyBS)
library(shinybusy)
library(tidyverse)
library(plotly)
library(prompter)

#### Define color ####
color_lvl_1 <- "#54278f" # Dark purple for variable type, first variable level and example
color_lvl_2 <- "#2c7fb8" # Blue for categorical variable level, second variable level and step
color_lvl_3 <- "#de2d26" # Red for abnormality and attention
color_lvl_4 <- "#31a354" # Green for normality and normal status
color_lvl_5 <- "#FF8C00" # Orange for choice selection and third variable level

#### Header setting ####
header <- dashboardHeader(
  # title = "运动与健康数据分析平台",
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
      menuSubItem("数据比较", tabName="groupCompare"),
      menuSubItem("回归分析", tabName="regAnalysis")
    ),
    # Third tab: Data visualization
    menuItem("数据处理", tabName="dataProcess", icon=icon("chart-pie")),
    # Fourth tab: About us
    menuItem("关于我们", tabName="about", icon=icon("book-reader"))
  )
)

#### Body setting ####
body <- dashboardBody(
  
  ##### Change validation need message color #####
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "index.css")
  ),
  
  ##### Activate shinyjs (library:shinyjs) #####
  useShinyjs(),
  
  ##### Define busy indicator (library:shinybusy) #####
  add_busy_spinner(spin = "trinity-rings", color = "#3182bd", timeout = 1000),
  
  ##### Define hover message (library:shinyBS) #####
  bsTooltip(
    id = c(
      "rawData_step1_cfmRun","rawData_step2_cfmRun","rawData_step3_cfmRun",
      "groupCompare_g1_step1_cfmRun","groupCompare_g1_step2_cfmRun",
      "dataProcess_missVal_cfmRun","dataProcess_dataFormat_cfmRun"
    ),
    title = "每次设定完成后，请点击此按钮运行本步骤"
  ),
  
  bsTooltip(
    id = c(
      "groupCompare_g1_step2_plotRun"
    ),
    title = "每次设置完毕后，请点击此按钮更新",
    placement = "top"
  ),
  ##### Activate circle hover message (library:promter) #####
  use_prompt(),

  ##### Main dashboard of Table #####
  tabItems(
    ###### Tab:rawData ######
    tabItem(
      tabName = "rawData",
      ####### Step 1 #######
      shinydashboardPlus::box(
        style = "margin: 0px",
        title = "步骤1：数据导入与变量设置",
        status = "primary",
        width = NULL,
        solidHeader = TRUE,
        collapsible = TRUE,
        label = actionBttn(
          inputId = "rawData_step1_cfmRun",
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
              inputId = "rawData_step1_importFile", 
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
              inputId = "rawData_step1_contSelect",
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
              inputId = "rawData_step1_catSelect",
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
          )
        ),
        markdown("***注意***：此步骤仅将数据中的空值作为缺失值；若要自定义缺失值，可在左侧`数据处理`一栏中处理。"),
        htmlOutput("rawData_step1_outFileInfo")
      ),
      
      ####### Step 2 #######
      shinydashboardPlus::box(
        style = "margin: 0px",
        title = "步骤2：正态性检验（仅限于选择了连续变量的情况）",
        status = "primary",
        width = NULL,
        solidHeader = TRUE,
        collapsible = TRUE,
        label = actionBttn(
          inputId = "rawData_step2_cfmRun",
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
              inputId = "rawData_step2_normTestContVar",
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
              inputId = "rawData_step2_normTestCatVar",
              label = tags$span(
                "选取分组变量",
                tags$span(icon("exclamation-circle")) %>%
                  add_prompt(
                    message = "基于步骤1中选取的分类变量进行选择；若不进行分组检验，则选(无)",
                    size = "medium",
                    position = "right"
                  )
              ),
              options = pickerOptions("title" = "尚未选择"),
              choices = NULL
            )
          )        
        ),
        
        fluidRow(
          tabBox(
            width = 12,
            tabPanel(
              title = "分析报告",
              icon = icon("list-ul"),
              htmlOutput("rawData_step2_normTestRpt")
            ),
            tabPanel(
              title = "统计结果",
              icon = icon("clipboard", class="fa-solid fa-clipboard"),
              verbatimTextOutput("rawData_step2_normTestRlt")
            ),
            tabPanel(
              title = "数据分布图",
              icon = icon("chart-simple"),
              fluidRow(
                column(
                  width = 4,
                  pickerInput(
                    inputId = "rawData_step2_normTestHistVar",
                    label = "选择变量",
                    options = pickerOptions("title" = "尚未选择"),
                    choices = NULL
                  )
                ),
                column(
                  width = 4,
                  sliderInput(
                    inputId = "rawData_step2_normTestHistBin",
                    label = "调整直条数",
                    min = 1,
                    max = 50,
                    value = 20
                  )
                )
              ),
              plotlyOutput("rawData_step2_normTestHist")
            )
          )
        )
      ),
      
      ####### Step 3 #######
      shinydashboardPlus::box(
        style = "margin: 0px",
        title = "步骤3：描述性表格",
        status = "primary",
        width = NULL,
        solidHeader = TRUE,
        collapsible = TRUE,
        label = actionBttn(
          inputId = "rawData_step3_cfmRun",
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
              inputId = "rawData_step3_desCatVar",
              label = tags$span(
                "选取分组变量",
                tags$span(icon("exclamation-circle")) %>%
                  add_prompt(
                    message = "基于步骤1中选取的分类变量进行选择；若不进行分组检验，则选(无)",
                    size = "medium",
                    position = "right"
                  )
              ),
              options = pickerOptions("title" = "尚未选择"),
              choices = NULL
            )
          ),
          column(
            width = 3,
            numericInput(
              inputId = "rawData_step3_desDigit",
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
            tableOutput("rawData_step3_desTable"),
            downloadBttn("rawData_step3_desTableDownload", label = "保存为word文件", size = "xs")
          )
        )
      )
    ),
    
    ###### Tab:groupCompare ######
    tabItem(
      tabName = "groupCompare",
      fluidRow(
        tabBox(
          width = 12,
          ####### Subtab: single group #######
          tabPanel(
            title = "单组样本",
            ######## Step 1 ########
            shinydashboardPlus::box(
              style = "margin: 0px",
              title = "步骤1：确定统计方法及数据格式",
              status = "primary",
              width = NULL,
              solidHeader = TRUE,
              collapsible = TRUE,
              label = actionBttn(
                inputId = "groupCompare_g1_step1_cfmRun",
                label = tags$span(
                  tags$span(icon("circle-play")),
                  tags$span("运行", style="font-size: 13px")
                ),
                size = "s",
                color = "primary"
              ),
              
              HTML(
                paste0(
                  "<p>
                    此分析仅适用于<font color=\"",color_lvl_3,"\"><b>只有一个分组</b></font>的<font color=\"",color_lvl_3,"\"><b>连续型</b></font>数据。<br>
                    <font color=\"",color_lvl_1,"\"><b>示例场景1</b></font>：对某大学某专业一个班的男同学进行了身高测量，现打算分析该班男同学的身高与全国男性的平均身高（1.7米）相比是否有差别。在<em><b>数据重复测试次数</b></em>选项中选择<font color=\"",color_lvl_5,"\"><b>无重复测试</font></b>选项。<br>
                    <font color=\"",color_lvl_1,"\"><b>示例场景2</b></font>：一批铅球运动员接受了3个月的力量训练，现打算分析力量训练前后他们的铅球成绩是否有差别。在<em><b>数据重复测试次数</b></em>选项中选择<font color=\"",color_lvl_5,"\"><b>重复测试一次</font></b>选项。<br>
                    <font color=\"",color_lvl_1,"\"><b>示例场景3</b></font>：在早、中、晚三个时间点测试一批短跑运动员的100米成绩，比较这三个时间点的成绩是否有差别。在<em><b>数据重复测试次数</b></em>选项中选择<font color=\"",color_lvl_5,"\"><b>重复测试两次及以上</font></b>选项。
                  </p>"
                )
              ),

              fluidRow(
                column(
                  width = 3,
                  pickerInput(
                    inputId = "groupCompare_g1_step1_repeat",
                    label = "数据重复测试次数",
                    choices = c(
                      "无重复测试" = "repeat0",
                      "重复测试一次" = "repeat1",
                      "重复测试两次及以上" = "repeat2"
                    ),
                    selected = "无重复测试"
                  )
                ),
                column(
                  width = 9,
                  htmlOutput("groupCompare_g1_step1_tblFormat")
                )
              )
            ),
            
            ######## Step 2 ########
            shinydashboardPlus::box(
              style = "margin: 0px",
              title = "步骤2：数据分析",
              status = "primary",
              width = NULL,
              solidHeader = TRUE,
              collapsible = TRUE,
              label = actionBttn(
                inputId = "groupCompare_g1_step2_cfmRun",
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
                    inputId = "groupCompare_g1_step2_importFile", 
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
                  width = 2,
                  pickerInput(
                    inputId = "groupCompare_g1_step2_method",
                    label = "选择统计方法",
                    choices = c(
                      "单样本t检验",
                      "配对检验",
                      "方差分析"
                    ),
                    selected = "单样本t检验"
                  )
                ),
                column(
                  width = 7,
                  fluidRow(
                    column(
                      width = 4,
                      pickerInput(
                        inputId = "groupCompare_g1_step2_id",
                        label = tags$span(
                          "选择编号变量",
                          tags$span(icon("exclamation-circle")) %>%
                            add_prompt(
                              message = "对应示例中的ID",
                              position = "right"
                            )
                        ),
                        choices = NULL,
                        selected = NULL,
                        options = pickerOptions("title" = "尚未选择")
                      )
                    ),
                    column(
                      width = 4,
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
                        choices = NULL,
                        selected = NULL,
                        options = pickerOptions("title" = "尚未选择")
                      )
                    ),
                    column(
                      width = 4,
                      uiOutput("groupCompare_g1_step2_tpUI")
                    )
                  )
                )
              ),
              markdown("***注意***：此步骤仅将数据中的空值作为缺失值；若要自定义缺失值，可在左侧`数据处理`一栏中处理。"),
              fluidRow(
                tabBox(
                  width = 12,
                  tabPanel(
                    title = "分析报告",
                    icon = icon("list-ul"),
                    htmlOutput("groupCompare_g1_step2_rpt")
                  ),
                  tabPanel(
                    title = "统计结果",
                    icon = icon("clipboard", class="fa-solid fa-clipboard"),
                    verbatimTextOutput("groupCompare_g1_step2_rlt")
                  ),
                  tabPanel(
                    title = "数据分布图",
                    icon = icon("chart-simple"),
                    fluidRow(
                      column(
                        width = 2,
                        pickerInput(
                          inputId = "groupCompare_g1_step2_plotType",
                          label = "请选择作图类型",
                          choices = c("箱式图"="boxplot","柱状图"="barplot","折线图"="lineplot"),
                          selected = "boxplot"
                        )
                      ),
                      column(
                        width = 2,
                        pickerInput(
                          inputId = "groupCompare_g1_step2_plotTheme",
                          label = "请选择作图风格",
                          choices = c(
                            "无风格"="NULL", "风格1"="Set1", "风格2"="Set2", "浅色风格"="Set3",  "深色风格"="Dark2", "配对风格"="Paired",
                            "灰度风格"="grey","BMJ"="bmj", "Frontiers"="frontiers", "JAMA"="jama", "JCO"="jco",
                            "Lancet"="lancet", "NEJM"="nejm", "NPG"="npg", "UCSCGB"="ucscgb"
                          ),
                          selected = "风格1"
                        )
                      ),
                      column(
                        width = 2,
                        pickerInput(
                          inputId = "groupCompare_g1_step2_plotSignif",
                          label = "显著性设置",
                          choices = c("不显示"="NULL","显示P值"="p.format","显示星号"="p.adj.signif"),
                          selected = "NULL"
                        )
                      ),
                      column(
                        width = 2,
                        uiOutput("groupCompare_g1_step2_plotSubUI")
                      ),
                      column(
                        width = 4,
                        fluidRow(
                          column(
                            width = 4,
                            dropdown(
                              inputId = "groupCompare_g1_step2_plotConfig",
                              label = "作图界面设置",
                              style = "jelly",
                              right = TRUE,
                              icon = icon("gear"),
                              status = "primary",
                              size = "s",
                              width = "400%",
                              fluidRow(
                                column(
                                  width = 4,
                                  textInput(
                                    inputId = "groupCompare_g1_step2_plotTitle",
                                    label = tags$span(
                                      "设置作图标题",
                                      tags$span(icon("exclamation-circle")) %>%
                                        add_prompt(
                                          message = "若不设定，则不用输入",
                                          position = "right"
                                        )
                                    )
                                  )
                                ),
                                column(
                                  width = 4,
                                  pickerInput(
                                    inputId = "groupCompare_g1_step2_plotTitlePosition",
                                    label = "设置标题位置",
                                    choices = c("居左"=0,"居中"=0.5,"居右"=1),
                                    selected = 0.5
                                  )
                                ),
                                column(
                                  width = 4,
                                  sliderInput(
                                    inputId = "groupCompare_g1_step2_plotTitleFontSize",
                                    label = "调节标题字体大小",
                                    min = 5,
                                    max = 25,
                                    value = 15,
                                    step = 1
                                  )
                                )
                              ),
                              fluidRow(
                                column(
                                  width = 6,
                                  textInput(
                                    inputId = "groupCompare_g1_step2_plotXlabel",
                                    label = tags$span(
                                      "设置X轴名称",
                                      tags$span(icon("exclamation-circle")) %>%
                                        add_prompt(
                                          message = "若不设定，则不用输入",
                                          position = "right"
                                        )
                                    )
                                  )
                                ),
                                column(
                                  width = 6,
                                  textInput(
                                    inputId = "groupCompare_g1_step2_plotYlabel",
                                    label = tags$span(
                                      "设置Y轴名称",
                                      tags$span(icon("exclamation-circle")) %>%
                                        add_prompt(
                                          message = "若不设定，则不用输入",
                                          position = "right"
                                        )
                                    )
                                  )
                                )
                              ),
                              fluidRow(
                                column(
                                  width = 6,
                                  sliderInput(
                                    inputId = "groupCompare_g1_step2_plotYtickNum",
                                    label = "设置Y轴标尺数目",
                                    min = 3,
                                    max = 10,
                                    value = 5,
                                    step = 1
                                  )
                                ),
                                column(
                                  width = 6,
                                  sliderInput(
                                    inputId = "groupCompare_g1_step2_plotAxisFontSize",
                                    label = "调节坐标轴字体大小",
                                    min = 5,
                                    max = 25,
                                    value = 10,
                                    step = 1
                                  )
                                )
                              ),
                              fluidRow(
                                column(
                                  width = 6,
                                  pickerInput(
                                    inputId = "groupCompare_g1_step2_plotLegend",
                                    label = "图例呈现",
                                    choices = c(
                                      "不显示"="none","图中左上角"="legend_1","图中左下角"="legend_2","图中右上角"="legend_3","图中右下角"="legend_4",
                                      "图外左侧"="left","图外上方"="top","图外右侧"="right","图外下方"="bottom"
                                    )
                                  )
                                ),
                                column(
                                  width = 6,
                                  textInput(
                                    inputId = "groupCompare_g1_step2_plotLegendLabel",
                                    label = tags$span(
                                      "设置图例标题",
                                      tags$span(icon("exclamation-circle")) %>%
                                        add_prompt(
                                          message = "若不设定，则不用输入",
                                          position = "right"
                                        )
                                    )
                                  )
                                )
                              )
                            )
                          ),
                          column(
                            width = 4,
                            dropdown(
                              inputId = "groupCompare_g1_step2_plotDownload2",
                              label = "下载作图",
                              style = "jelly",
                              right = TRUE,
                              icon = icon("download"),
                              status = "primary",
                              size = "s",
                              width = "130%",
                              pickerInput(
                                inputId = "groupCompare_g1_step2_plotDownload_unit",
                                label = "选择单位",
                                choices = c("in","mm","px"),
                                selected = "in"
                              ),
                              numericInputIcon(
                                inputId = "groupCompare_g1_step2_plotDownload_width",
                                label = "宽度",
                                value = 10
                              ),
                              numericInputIcon(
                                inputId = "groupCompare_g1_step2_plotDownload_ht",
                                label = "高度",
                                value = 8
                              ),
                              numericInputIcon(
                                inputId = "groupCompare_g1_step2_plotDownload_dpi",
                                label = "DPI",
                                value = 300
                              ),
                              selectInput(
                                "groupCompare_g1_step2_plotDownload_format", 
                                "选择图片格式：",
                                choices = c("PNG" = "png", "PDF" = "pdf", "JPEG" = "jpeg"),
                                selected = "png"
                              ),
                              conditionalPanel(
                                condition = "input.groupCompare_g1_step2_plotDownload_format == 'jpeg'",
                                sliderInput(
                                  "groupCompare_g1_step2_plotDownload_quality", 
                                  "JPEG质量 (0-100):", 
                                  min = 1, max = 100, value = 90)
                                ),
                              downloadBttn("groupCompare_g1_step2_plotDownload", label = "保存图片", size = "xs")
                            )
                          ),
                          column(
                            width = 4,
                            actionBttn(
                              inputId = "groupCompare_g1_step2_plotRun",
                              label = "开始作图",
                              style = "jelly",
                              size = "s",
                              color = "primary",
                              icon = icon("play")
                            )
                          )
                        )

                      )
                    ),
                    fluidRow(
                      column(
                        width = 12,
                        plotOutput("groupCompare_g1_step2_plot")
                      )
                    )
                  )
                )
              )
            )
          ),
          
          ####### Subtab: two groups #######
          tabPanel(
            title = "两组样本",
            ######## Step 1 ########
            shinydashboardPlus::box(
              style = "margin: 0px",
              title = "步骤1：确定统计方法及数据格式",
              status = "primary",
              width = NULL,
              solidHeader = TRUE,
              collapsible = TRUE,
              label = actionBttn(
                inputId = "groupCompare_g2_step1_cfmRun",
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
                    inputId = "groupCompare_g2_step1_repeat",
                    label = "数据重复测试次数",
                    choices = c(
                      "无重复测试" = "repeat0",
                      "重复测试一次" = "repeat1",
                      "重复测试两次及以上" = "repeat2"
                    ),
                    selected = "无重复测试"
                  )
                ),
                textOutput("groupCompare_g2_step1_test")
              )
            ),
            
            ######## Step 2 ########
            shinydashboardPlus::box(
              style = "margin: 0px",
              title = "步骤2：数据导入与变量设置",
              status = "primary",
              width = NULL,
              solidHeader = TRUE,
              collapsible = TRUE,
              label = actionBttn(
                inputId = "groupCompare_g2_step2_cfmRun",
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
                    inputId = "groupCompare_g2_step2_importFile", 
                    label = tags$span(
                      "数据导入",
                      tags$span(icon("exclamation-circle")) %>%
                        add_prompt(
                          message = "支持csv,excel,SAS和stata文件；若要自定义缺失值，可在左侧数据处理一栏中处理",
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
          
          ####### Subtab: three and above groups #######
          tabPanel(
            title = "三组及以上样本"
          )
        )
      )
    ),
    
    ###### Tab:regAnalysis ######
    tabItem(
      tabName = "regAnalysis"
    ),
    
    ###### Tab:dataProcess ######
    tabItem(
      tabName = "dataProcess",
      ####### Customise missing value #######
      shinydashboardPlus::box(
        style = "margin: 0px",
        title = "自定义缺失值",
        status = "primary",
        width = NULL,
        solidHeader = TRUE,
        collapsible = TRUE,
        label = actionBttn(
          inputId = "dataProcess_missVal_cfmRun",
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
              inputId = "dataProcess_missVal_importFile", 
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
              inputId = "dataProcess_missVal_varSelect",
              label = tags$span(
                "选取变量",
                tags$span(icon("exclamation-circle")) %>%
                  add_prompt(
                    message = "自定义缺失值仅会在所选取的变量中进行；最终生成的数据仅会包含所选取的变量",
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
            textInputIcon(
              inputId = "dataProcess_missVal_value",
              label = tags$span(
                "自定义缺失值",
                tags$span(icon("exclamation-circle")) %>%
                  add_prompt(
                    message = "若有多种缺失值，则需用英文逗号将其分开。例如：无,999,-,--",
                    size = "medium",
                    position = "right"
                  )
              ),
              value = NULL,
              icon = NULL
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            htmlOutput("dataProcess_missVal_tbl_msg"),
            DTOutput("dataProcess_missVal_tbl")
          )
        )
      ),
    
      ####### Dataset format conversion #######
      shinydashboardPlus::box(
        style = "margin: 0px",
        title = markdown("数据格式转换：长数据&harr;宽数据"),
        status = "primary",
        width = NULL,
        solidHeader = TRUE,
        collapsible = TRUE,
        label = actionBttn(
          inputId = "dataProcess_dataFormat_cfmRun",
          label = tags$span(
            tags$span(icon("circle-play")),
            tags$span("运行", style="font-size: 13px")
          ),
          size = "s",
          color = "primary"
        ),

        fluidRow(
          column(
            width = 4,
            fluidRow(
              column(
                width = 6,
                htmlTable(
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
                  header =  c("ID","&emsp;Timepoint&emsp;","Value"),
                  caption = markdown("长数据格式："),
                  collapse = "separate_shiny"
                )
              ),
              column(
                width = 6,
                htmlTable(
                  matrix(
                    c(1,12,15,"...",20,
                      2,14,13,"...",14,
                      "...","...","...","...","...",
                      "n",15,18,"...",25),
                    ncol=5, byrow = TRUE
                  ),
                  header =  c("ID","&emsp;T1","&emsp;T2&emsp;","...","&emsp;Tn"),
                  caption = markdown("宽数据格式："),
                  collapse = "separate_shiny"
                )
              )
            )
          ),
          
          column(
            width = 8,
            fluidRow(
              column(
                width = 6,
                fileInput(
                  inputId = "dataProcess_dataFormat_importFile",
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
                ),
                pickerInput(
                  inputId = "dataProcess_dataFormat_varSelect",
                  label = tags$span(
                    "选取变量",
                    tags$span(icon("exclamation-circle")) %>%
                      add_prompt(
                        message = "最终生成的数据仅会包含所选取的变量",
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
                ),
                pickerInput(
                  inputId = "dataProcess_dataFormat_type",
                  label = "选择数据转换类型",
                  choices = c("长数据 => 宽数据"="long2wide", "宽数据 => 长数据"="wide2long"),
                  selected = "long2wide"
                )
              ),
              column(
                width = 6,
                uiOutput("dataProcess_dataFormat_typeUI")
              )
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            htmlOutput("dataProcess_dataFormat_tbl_msg"),
            DTOutput("dataProcess_dataFormat_tbl")
          )
        )
      )
    ),
    
    ###### Tab:about ######
    tabItem(
      tabName = "about"
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
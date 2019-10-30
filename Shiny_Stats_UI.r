library(shiny)
library(lattice)
library(shinydashboard)
library(plotly)
library(devtools)
library(readxl)
library(tidyverse)
library(survival)
library(DT)
library(survminer)
library(caret)
library(randomForest)
library(dplyr)
myData <- USArrests


#Side bar
sidebar<-dashboardSidebar(
  sidebarMenu(
    # Search Engine --- to be improved
    sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                      label = "Search..."),
    # A brief summary on Shiny Stats; links to important/useful websites
    menuItem(text = "Synopsis", tabName = 'Synopsis', icon = icon("dashboard"),
             menuSubItem(text = "GitHub", icon = icon("file-code-0"),
                         href = "https://github.com/ShenSeanChen/UCLChangeMakers
                                -StatisticalVisualisation-RShiny"),
             menuSubItem(text = "ShinyDashboard", icon = icon("file-code-0"),
                         href = "https://rstudio.github.io/shinydashboard/index.html")),
    
    # Visualisation on Statistics 
    menuItem(text = 'Probability Distribution', icon = icon("bar-chart-o"),
             menuSubItem(text = 'Central Limit Theorem', tabName = 'CLT'),
             menuSubItem(text = 'Quantile Plots and Skewness', tabName = 'QQ')
             ),
    
    menuItem(text = 'Normal Linear Regression', icon = icon("chart-line"),
             menuSubItem(text='DataTable, LM and ANOVA', tabName='MLR', icon = icon("table"))),
    
    menuItem(text = "Generalised Linear Model",  icon = icon("chart-line"),
             menuSubItem(text='Generalised Linear Modelling (2D)',tabName='glm'),
             menuSubItem(text='High-Dimensional GLM',tabName='multi-G')),
    
    menuItem(text = 'Non-parametric Regression', icon = icon("chart-line"), 
             menuSubItem(text = 'Survival Analysis', tabName = 'Surv')),
    
    # menuItem(text = 'Stochastic Process', icon = icon("random"),
    #          menuSubItem(text = "Markov Process"), tabName = 'Markov'),
    
    menuItem(text = 'Machine Learning', icon = icon("chart-line"),
              menuSubItem(text='Titanic data',tabName='ti_data'),
              menuSubItem(text='RandomForest',tabName='RandomForest'))
  )
)


#Body
body <- dashboardBody(

tabItems(
  
###############################################################################
# Sysnopsis
###############################################################################
tabItem(tabName = "Synopsis",
          h2("UCLChangeMakers-StatisticalVisualisation-RShiny")),
    
###############################################################################
# Probability Distribution and Central Limit Theorem 
###############################################################################
tabItem(tabName = "CLT",
       fluidRow(box(width=4,
                    #Pupulation size
                    sliderInput("pop_size",
                                label = "Population Size",
                                value = 2000, min = 1000, max = 100000),
                    
                    #Enter sample size
                    textInput("smpl_size",
                              label = "Enter Sample Size",
                              value = 50),
                    
                    #Distribution
                    selectInput("dist", label = "Distribution ",
                                choices = c("Normal" = "norm", "Uniform" = "unif", "Exponential"="exp",
                                            "Binomial"="binom", "Negative binomial"="nbinom", "Poission"="pois", 
                                            "Geometric"="geom", "Hypergeometric"="hyper", "Chi-squared"="chisq", 
                                            "Student's t"="t", "Beta"="beta", "Gamma"="gamma"), 
                                selected = "norm"),
                    conditionalPanel(
                      condition = "input.dist == 'norm'",
                      textInput("miu", label = "Mean", value = 0),
                      textInput("sigma", label = "Standard Deviation", value = 1)
                    ),
                    conditionalPanel(
                      condition = "input.dist == 'unif'",
                      textInput("a", "Minimum value", value = 0),
                      textInput("b", "Maximum value", value = 1)
                    ),
                    conditionalPanel(
                      condition = "input.dist == 'exp'",
                      textInput("lambda", "Rate", value = 1)
                    ),
                    conditionalPanel(
                      condition = "input.dist == 'binom'",
                      textInput("p", "Probability", value = 0.5),
                      textInput("n", "Number of Trials", value = 1)
                    ),
                    conditionalPanel(
                      condition = "input.dist == 'nbinom'",
                      textInput("p2", "Probability", value = 0.5),
                      textInput("r", "Number of Failures", value = 1)
                    ),
                    conditionalPanel(
                      condition = "input.dist == 'pois'",
                      textInput("lambda2", "Rate", value = 1)
                    ),
                    conditionalPanel(
                      condition = "input.dist == 'geom'",
                      textInput("p3", "Probability", value=0.5)
                    ),
                    conditionalPanel(
                      condition = "input.dist == 'hyper'",
                      textInput("M", "Number of Success States in Population", value=10),
                      textInput("N", "Population Size",value=20),
                      textInput("K", "Number of Draws", value=5)
                    ),
                    conditionalPanel(
                      condition = "input.dist == 'chisq'",
                      textInput("df", "Degrees of Freedom", value=1)
                    ),
                    conditionalPanel(
                      condition = "input.dist == 't'",
                      textInput("df2", "Degrees of Freedom", value=1)
                    ),
                    conditionalPanel(
                      condition = "input.dist == 'beta'",
                      textInput("Alpha", "First Shape", value=0.5),
                      textInput("Beta", "Second Shape", value=0.5)
                    ),
                    conditionalPanel(
                      condition = "input.dist == 'gamma'",
                      textInput("k", "Shape", value=0.5),
                      textInput("Theta", "Scale", value=0.5)
                    ),
                    
                    #Sampling iteration
                    sliderInput("smpl_iterate",
                                label = "Sampling Iteration",
                                value = 5, min = 1, max = 1000)),       
                
                box(width=8,tabsetPanel(type="tabs",
                                        #Plot tab
                                        tabPanel("Plot",
                                                 plotOutput("plot_pop"), #plotting histogram and density plot for population
                                                 plotOutput("plot_smpl_mean")), #plotting histogram and density plot for sample
                                        #Data tab
                                        tabPanel("Data", 
                                                 h4("Population"), #heading for the population summary and descriptive statistics
                                                 verbatimTextOutput("pop_summary"), #rendering population summary statistics
                                                 verbatimTextOutput("pop_structure"), 
                                                 h4("Sample"), #heading for the sample mean summary and descriptive statistics
                                                 verbatimTextOutput("smpl_mean_summary"), #rendering sample summary statistics
                                                 verbatimTextOutput("smpl_mean_structure"))
                ))
       )),

###############################################################################
# Quantile Plots and Skewness
###############################################################################
tabItem(tabName = "QQ",
        # header
        fluidRow(
          box(h4("Quantile Plots and Skewness"), 
             #file Input
             selectInput("type",
                         "What distribution would you like?",
                         choices = c("Light-tailed", 
                                     "Heavy-tailed", 
                                     "Normal", 
                                     "Negatively skewed", 
                                     "Positively skewed")),
             actionButton("update",
                          "Produce new sample!", 
                          width = "87.5%")),
             #  p("This applet aims to make QQ plots more intuitive by comparing 
             # sample quantiles against the corresponding normal quantiles in various ways.",
             #  align = "center")) 
          fluidRow(
            box(plotOutput("QQplots")))
        )),  

###############################################################################
# Import data and Run MLR with CheckboxGroup
###############################################################################
tabItem(tabName = "MLR",
       # header
       fluidRow(box(h4("General linear model"), 
                #file Input
                fileInput("file1","choose excel file"),
                radioButtons("fileType_Input",
                             label = h4("Choose File type"),
                             choices = list(".csv/txt" = 1, ".xlsx" = 2),
                             selected = 1,
                             inline = TRUE),
                selectInput("selectInput",
                            "Checkbox group input for response variable:",
                            c("label 1" = "option1",
                              "label 2" = "option2")),
                checkboxGroupInput("inCheckboxGroup",
                                   "Checkbox group input for explanatory variable:",
                                   c("label 1" = "option1",
                                     "label 2" = "option2")),
                uiOutput("choose_columns")),
                
              box(tabsetPanel(type="tabs",
                            tabPanel("data",tableOutput("contents") ),
                            tabPanel("model summary",verbatimTextOutput("text1")),
                            tabPanel("anova",verbatimTextOutput("text2")))) 
        )),           
         
###############################################################################
# Build glm models and predict values based on that
###############################################################################
tabItem(
  tabName = "glm",
  fluidRow(box(selectInput("x", label = "Choose an explanatory variable (x)",
                           choices = colnames(myData), selected = colnames(myData[1])),
               selectInput("y", label = "Choose a response varialbe (y)",
                           choices = colnames(myData), selected = colnames(myData[2])),
               selectInput("family",
                           label = "Choose a family to run glm",
                           choices = c('poisson', 
                                       "gaussian",
                                       "Gamma", 
                                       "inverse.gaussian",
                                       "quasi",
                                       "quasibinomial",
                                       "quasipoisson",
                                       "binomial"
                           ),
                           selected = "poisson"),
               selectInput("link",
                           label = "Change link function",
                           choices = c("Canonical Link",
                                       "()",
                                       "(link='log')",
                                       "(link='probit')",
                                       "(link='cauchit')", 
                                       "(link='cloglog')",
                                       "(link='identity')",
                                       "(link='logit')",
                                       "(link='sqrt')",
                                       "(link='1/mu^2')",
                                       "(link='inverse')"),
                           selected = "Canonical Link"),
               
               selectInput("t_x",
                           label = "Choose a certain transformation for the explanatory variable",
                           choices = c("None", 
                                       "log",
                                       "sqrt",
                                       "square",
                                       "third power"
                           ), 
                           selected = "log")),
           box(verbatimTextOutput("summary_glm1"))),
  
  fluidRow(box(plotOutput('plot_glm1')),
           box(plotOutput('diagnostic_plot1'))
  )),

###############################################################################
# Build higher dimensional glm models and predict values based on that 
###############################################################################
tabItem(tabName = 'multi-G',
        fluidRow(box(selectInput("family2",
                                 label = "Choose a family to run glm",
                                 choices = c('poisson', 
                                             "gaussian",
                                             "Gamma", 
                                             "inverse.gaussian",
                                             "quasi",
                                             "quasibinomial",
                                             "quasipoisson",
                                             "binomial"
                                 ),
                                 selected = "poisson"),
                     selectInput("link2",
                                 label = "Change link function",
                                 choices = c("Canonical Link",
                                             "()",
                                             "(link='log')",
                                             "(link='probit')",
                                             "(link='cauchit')", 
                                             "(link='cloglog')",
                                             "(link='identity')",
                                             "(link='logit')",
                                             "(link='sqrt')",
                                             "(link='1/mu^2')",
                                             "(link='inverse')"),
                                 selected = "Canonical Link")),
                 box(plotOutput("diagnostic_plot2"))),
        verbatimTextOutput("summary_glm2")
),

###############################################################################
# Survival Analysis
###############################################################################
tabItem(tabName = 'Surv',
        h1('Survival Analysis'),
        selectInput('Dataset', 'Select a dataset',c('lung','cgd','pbc')),
        #  selectInput('Covariates','Select a covariate', )
        dataTableOutput('DataSetTable'),
        fluidRow(
          box(title = 'DataSet info',verbatimTextOutput('DataSetInfo'),collapsible = TRUE, width = 12),
          box(title = 'DataSet Summary',verbatimTextOutput("summary"),collapsible = TRUE, width = 12)
          
        ),
        
        fluidRow(box(title = 'plot',plotOutput('DataSetPlot1',
                                               width = "500px", height = "500px")),
                 box('select Covariate',selectInput('covariate', 'Select a covariate', 'placeholder')) )
        
        
),

###############################################################################
# Markov Process
###############################################################################
# tabItem(tabName = 'Markov',
#         fluidRow(
#           titlePanel(title = "Discrete Time Markov Process"),
#           
#           box(h4("Setting up transition matrix"),
#                          numericInput("dimension", "Enter the number of states:", 
#                                       value = Init_States, min = Min_States, max = Max_States),
#                          helpText("You can choose between",Min_States,"and",Max_States, "states"),
#                          uiOutput("matrix"),
#                          h5("First Passage Probability"), 
#                          uiOutput("select"),
#                          numericInput("step", "Enter the number of steps: ", min = 1, value = 1),
#                          submitButton("Submit"),
#                          width = 5),
#             box(type = "tab",
#                           tabPanel("State Space Diagram", plotOutput("trans_plot")),
#                           tabPanel("Summary", verbatimTextOutput("summary")),
#                           tabPanel("First Passage Prob", verbatimTextOutput("pas_prob")))
#               
#             )
#           )

###############################################################################
# RandomForest
###############################################################################
  tabItem(
    tabName = "ti_data",
    selectInput(
      inputId='sex',label='Gender',
      choices=c('male','female'),multiple=TRUE,
      selectize=TRUE
    ),
    selectInput(
      inputId='class',label='Ticket Class',
      choices=c(1,2,3),multiple=TRUE,
      selectize=TRUE
    ),
    selectInput(
      inputId='survived',label='Survival',
      choices=c(0,1),multiple=TRUE,
      selectize=TRUE
    ),
    selectInput(
      inputId='age_group',label='Age group',
      choices=c("Age.00_12", "Age.13_17","Age.18_59","Age.60+"),multiple=TRUE,
      selectize=TRUE
    ),
    dataTableOutput(outputId='table')
    
  ),
  tabItem(tabName = "RandomForest",
          
          sliderInput(inputId = 'ntree', 
                      label = 'number of trees', 
                      min =1, max =1000, 
                      value =100),
          sliderInput(inputId = 'mtry', 
                      label = 'Number of variables considered in choosing each split', 
                      min =1, max =6, 
                      value =3),
          sliderInput(inputId = 'node', 
                      label = 'Minimum number of samples in node', 
                      min =1, max =100, 
                      value =10),
          plotOutput(outputId = 'rf'),
          textOutput(outputId ='error1' ),textOutput(outputId ='error2' ),
          textOutput(outputId ='error3' ),textOutput(outputId ='error4' ),
          verbatimTextOutput(outputId ='rfoutcome'))
  


) # End of TabItems
) # End of Dashbody


ui <- ShinyUI(dashboardPage(header = dashboardHeader(title = "Shiny Stats"),
                    sidebar = sidebar,
                    body = body,
                    skin = "purple"
)


)

# Define UI for application that plots random distributions

#get About panel
tabPanelAbout <- source("about.r")$value


shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("NADA and Substitution Methods - Simulator"),
  
  # Sidebar with a slider input for number of observations
  sidebarPanel(
    
    selectInput("distribution", "Distribution Type:", c("Log-Normal", "Gamma", "Normal")),
    # This UI limits the reactivity to only this button and changing the distribution
    actionButton("myValue1", "Select Distribution and RUN Sim"),
    numericInput("obs",
                 "Number of observations:",
                 min = 0,
                 max = 100,
                 value = 50),
    
    # IF LN this is the input values
    conditionalPanel(
      condition="input.distribution == 'Log-Normal'",
      numericInput("log_sd",
                   "SD of Logs:",
                   min=0.1,
                   max=2.5,
                   value=1,
                   step=0.5
      ),
      numericInput("log_mu",
                   "Mean of Logs:",
                   min=0.0,
                   max=10,
                   value=0.5,
                   step=0.5
      )),
    
    # IF GAMMA this is the input values
    conditionalPanel(
      condition="input.distribution == 'Gamma'",
      numericInput("shape",
                   "Shape:",
                   min=0.1,
                   max=4,
                   value=1,
                   step=0.5
      ),
      numericInput("scale",
                   "Scale:",
                   min=1,
                   max=200,
                   value=100,
                   step=10
      )),
    # IF NORMAL this is the input values
    conditionalPanel(
      condition="input.distribution == 'Normal'",
      numericInput("sd",
                   "Std.Dev:",
                   min=0.1,
                   max=20,
                   value=1,
                   step=0.5
      ),
      
      numericInput("mu",
                   "Mu:",
                   min=-10,
                   max=10,
                   value=10,
                   step=1
      )),
    
    # This ends the conditional input, we now input our censoring information
    numericInput("cenRate1",
                 "Censoring Quantile 1:",
                 min=0,
                 max=1,
                 value=0.5,
                 step=0.1
    ),
    
    # NOTE the weights are a relative weighting
    
    sliderInput("cenWeight1",
                "Weight of censoring value 1:",
                min = 0,
                max = 100,
                value = 33
    ),
    
    numericInput("cenRate2",
                 "Censoring quantile 2:",
                 min=0,
                 max=1,
                 value= 0.1,
                 step=0.1
    ),
    
    sliderInput("cenWeight2",
                "Weight of censoring quantile 2:",
                min = 0,
                max = 100,
                value = 33
    ),
    
    numericInput("cenRate3",
                 "Censoring quantile 3:",
                 min=0,
                 max=1,
                 value=0.1
    ),
    
    
    sliderInput("cenWeight3",
                "Weight of censoring value 3:",
                min=0,
                max=100,
                value= 33
    ),
    
    numericInput("simNum",
                 "Number of Simulated Samples:",
                 min=1,
                 max=1000,
                 value=100,
                 step=100
    ),
    
    # this plots reactively the desnity plot for the proposed distribution and will be reactive to changes in theta values
    plotOutput("distGraph")
  ),
  
  # This is output of the simulation
  mainPanel(
    
    # we create tabs - plots, summary, RMSE, Bias, and Censored Statistical Summary
    tabsetPanel(
      tabPanel("Plots",
               plotOutput("Meangraph"),
               plotOutput("SDgraph")
      ),
      
      tabPanel("Summary",
               verbatimTextOutput("summary"),
               h3("Mean Values"),
               tableOutput("Meanview"),
               h3("Standard Deviation Values"),
               tableOutput("SDview")
      ),
      
      tabPanel("RMSE Plot",
               plotOutput("RMSEplot.mean"),
               plotOutput("RMSEplot.sd")
      ),
      
      tabPanel("Bias Plot",
               plotOutput("Bplot.mean"),
               plotOutput("Bplot.sd")
      ),
      
      tabPanel("Censored Statistics Summary",
               h2("Summary Statistics"),
               tableOutput("censum.all"),
               h2("Censoring Limits"),
               tableOutput("limits.all")
               ),
      tabPanelAbout(), id = "allPanels")
      
    )
    
    
  )
)
# Define UI for application that plots random distributions

#get About panel
tabPanelAbout <- function(){
  tabPanel("About this Application",
           p(style="text-align:justify",'This online application was developed to simulate substitution methods for left-censored data as compared to NADA'),
           p(style="text-align:justify",'However, because the application has been developed using the ',a("Shiny", href="http://www.rstudio.com/shiny/", target="_blank"),'package and ',a("R software", href="http://r-project.org/", target="_blank"),' 
             the mathematical extensibility is virtually unlimited. We hope that you find this online tool useful for your purposes ! This is free and unencumbered software released into the public domain.

            Anyone is free to copy, modify, publish, use, compile, sell, or
            distribute this software, either in source code form or as a compiled
            binary, for any purpose, commercial or non-commercial, and by any
            means.
            
            In jurisdictions that recognize copyright laws, the author or authors
            of this software dedicate any and all copyright interest in the
            software to the public domain. We make this dedication for the benefit
            of the public at large and to the detriment of our heirs and
            successors. We intend this dedication to be an overt act of
            relinquishment in perpetuity of all present and future rights to this
            software under copyright law.
            
            THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
            EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
            MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
            IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
            OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
            ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
            OTHER DEALINGS IN THE SOFTWARE.
            
            For more information, please refer to <http://unlicense.org/>'),
           br(),
           
           HTML('<div style="clear: left;"><img src="Hubona-small-jpeg.jpg" alt="" style="float: left; margin-right:5px" /></div>'),
           strong('Author and Application Developer'),
           p('James T Durant',br(),
             'Industrial Hygienist | R Enthusiast',br()
           ),
           br(),
           
           div(class="row-fluid",
               # div(class="span3",strong('Other app versions'),
               #      p(HTML('<ul>'),
               #       HTML('<li>'),a("Version 1", href="XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX", target="_blank"),HTML('</li>'),
               #        HTML('<li>'),a("Version 2", href="XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX", target="_blank"),HTML('</li>'),
               #        HTML('<li>'),a("Version 3", href="XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX", target="_blank"),HTML('</li>'),
               #        HTML('</ul>')),
               #      strong('Code'),
               #       p('Source code available at',
               #         a('GitHub', href='https://github.com/ua-snap/shiny-apps/tree/master/RVdistsExampleAppV4')),
               #       br()
               #   ),
               
               
               div(class="span3",
                   strong('Reference Documentation'),
                   p(HTML('<ul>'),
                     HTML('<li>'),a("shiny package in R", href="http://www.rstudio.com/shiny/", target="_blank"),HTML('</li>'),
                     HTML('<li>'),a('The R Project', href="http://www.r-project.org/", target="_blank"),HTML('</li>'),
                     HTML('</ul>'))
               )
           )
  )
}


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
                value = 100
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
                value = 0
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
                value= 0
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
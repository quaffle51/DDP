library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    title = 'Coursera DDP Project',
    withMathJax(),
   
    # Application title
    #titlePanel("Five standard distributions")
    # Sidebar with a slider input for the number of df
    headerPanel("Five continuous univariate distributions."),
    sidebarLayout(
        sidebarPanel(
            
            radioButtons("dist", "Distribution type:",
                         c("The chi-squared distribution" = "chi2",
                           "The t distribution"           = "tee",
                           "The F distribution"           = "F",
                           "The gamma distribution"       = "gamma",
                           "The beta distribution"        = "beta"
                         )
            ),
            conditionalPanel(
                br(),
                condition = "input.dist == 'chi2'",
                sliderInput("nu1",HTML("Number of degrees of freedom, \\(\\nu\\):"),min = 0,max = 20,value = 5,animate=TRUE)
            ),
            
            conditionalPanel(
                br(),
                condition = "input.dist == 'tee'",
                sliderInput("nu2","Number of degrees of freedom:",min = 0,max = 30,value = 5,animate=TRUE)
            ),
            
            conditionalPanel(
                br(),
                condition = "input.dist == 'F'",
                sliderInput("nu3","Number of degrees of freedom 1:",min = 0,max = 10,value = 2,animate=TRUE),
                sliderInput("nu4","Number of degrees of freedom 2:",min = 0,max = 10,value = 2,animate=TRUE)
            ),
            
            conditionalPanel(
                br(),
                condition = "input.dist == 'gamma'",
                sliderInput("a","a:",min = 0,max = 10,value = 0.5,step=0.1,animate=TRUE),
                sliderInput("b","b:",min = 0,max = 10,value = 0.5,step=0.1,animate=TRUE)
            ),
            
            conditionalPanel(
                br(),
                condition = "input.dist == 'beta'",
                sliderInput("a1","a:",min = 0,max = 7,value = 0.5,step=0.1,animate=TRUE),
                sliderInput("b1","b:",min = 0,max = 7,value = 0.5,step=0.1,animate=TRUE)
            )
        ),
        
        
        
        # Show a plot of the generated distribution
        mainPanel(
            h3(textOutput("Some text here")),
            p('Here you can display one of five continuous univariate distributions which aside from the normal distribution 
               are arguably the most important continuous univariate distributions.'),
            p('These are the chi-squared, t, F, gamma and beta distributions.'),
            p('You can experiment to see how the density function changes as you change 
              the parameter or parameters of the distribution using the sliders.  Note: you can make the sliders move automatically by clicking the grey arrow head to the right of the slider box.'),
            p(),
            p("The expression for the density function of the respective distribution appears under the 'Summary' tab."),
            p(),
            p("A table of the values used in plotting the density function appears under the 'Table' tab."),
            tabsetPanel(
                tabPanel("Plot",    plotOutput("distPlot")), 
                tabPanel("Summary", uiOutput("summary")), 
                tabPanel("Table",   tableOutput("table"))
            )
            
        )
    )
))
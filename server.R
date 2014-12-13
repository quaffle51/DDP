library(shiny)
shinyServer(
    function(input, output) {
        ans  <- reactive(
                switch(input$dist,
                 chi2 = 	{
                     x <- seq(0,40,0.1)
                     f <- function(x,nu) (1/(2^(nu/2)*gamma(nu/2)))*x^(nu/2-1)*exp(-x/2)
                     df <- data.frame(x,f(x,input$nu1))
                     names(df) <- c("x","f")
                     txt <-  "The chi-squared distribution, denoted by \\(\\chi^2(\\nu)\\), 
                              is a distribution for a random variable that only takes positive values. 
                              It has pdf, \\(f_\\chi(x)\\), is expressed as:

                              $$
                               f_{\\chi}(x) = \\left[\\frac{1}{2^{\\left(\\frac{\\nu}{2}\\right)}}\\Gamma\\left(\\frac{\\nu}{2}\\right)
                               \\right]
                               \\times 
                               x^{\\left(\\frac{\\nu}{2}-1\\right)}\\exp\\left(-\\frac{x}{2}\\right)
                               \\quad \\text{on } x>0,\\\\
                               \\text{where } \\Gamma(.) \\text{ is the Gamma function.}
                              $$

                              The number of degrees of freedom, \\(\\nu\\), is positive and is typically an integer."
                        
                     
                     return(list(df,txt,"Chi-squared Distribution"))
                 },
                 tee = 	{
                     x <- seq(-5,5,0.1)
                     f <- function(x, nu) gamma((nu+1)/2)/((sqrt(nu*pi))*gamma(nu/2))*(1+(x^2/nu))^(-(nu+1)/2)
                     df <- data.frame(x,f(x,input$nu2))
                     names(df) <- c("x","f")
                     txt <- "Student’s \\(t\\) distribution, or the \\(t\\) distribution for short, 
                     denoted \\(t(\\nu)\\), is a distribution for a random variable that can take 
                     any value on the whole real line. Its pdf is, 
                     \\(f_{t}(x)\\), is expressed as:
                     
                     $$
                        f_{t}(x) = 
                        \\left[
                        \\frac{ \\Gamma \\left( \\frac{\\nu+1}{2} \\right) \\Gamma\\left(\\frac{\\nu}{2} \\right)}{\\sqrt{\\nu\\pi}}
                        \\right]
                        \\times
                        \\left(1 + \\frac{x^{2}}{\\nu} \\right)^{-(\\frac{\\nu + 1}{2})} \\quad \\text{on } \\mathbb{R},\\\\
                        \\text{where } \\Gamma(.) \\text{ is the Gamma function.}
                     $$
                     The number of degrees of freedom, \\(\\nu\\), takes only positive values which are usually integers.
                     "
                     return(list(df,txt,"t-Distribution"))
                 },
                 F   =   {
                     x <- seq(0,6,0.02)
                     k <- function(x,nu1,nu2) (gamma((nu1+nu2)/2))/(gamma(nu1/2)*gamma(nu2/2)*(nu1/nu2)^(nu1/2))
                     f <- function(x,nu1,nu2) k(x,nu1,nu2)*x^((nu1-2)/2)*(1+(nu1/nu2)*x)^(-(nu1+nu2)/2)
                     df <- data.frame(x,f(x,input$nu3,input$nu4))
                     names(df) <- c("x","f")
                     txt <- "The F distribution, written \\(F(\\nu_1,\\nu_2)\\), 
                     is, like the chi-squared distribution, a distribution for a random variable that takes only 
                     positive values. 
                     The expression for its pdf, \\(f_F(x)\\), is:
                     $$
                       f_F(x) = 
                       \\left[
                       \\frac
                       {
                        \\Gamma\\left(\\frac{\\nu_1+\\nu_2}{2}\\right)
                       }
                       {
                        \\Gamma\\left(\\frac{\\nu_1}{2}\\right)
                        \\Gamma\\left(\\frac{\\nu_2}{2}\\right)
                       }
                       \\left(\\frac{\\nu_1}{\\nu_2}\\right)^\\left(\\frac{\\nu_1}{2}\\right)
                       \\right]
                       \\times
                       x^\\left(\\frac{\\nu_1}{2}-1\\right)
                       \\left(1 + \\frac{\\nu_1}{\\nu_2}x\\right)^\\left(-\\frac{\\nu_1+\\nu_2}{2}\\right)
                       \\quad \\text{on } x>0,\\\\
                       \\text{where } \\Gamma(.) \\text{ is the Gamma function.}
                     $$
                     Now there are two positive parameters for the degrees-of-freedom, 
                     \\(\\nu_1\\) and \\(\\nu_2\\),typically each of these parameters is an integer. 
                     "
                     return(list(df,txt,"F Distribution"))
                 },
                 gamma = {
                     x <- seq(0,10,0.01)
                     k <- function(a,b) (b^a)/gamma(a)
                     f <- function(x,a,b) k(a,b) * x^(a-1) * exp(-b*x)
                     df <- data.frame(x,f(x,input$a,input$b))
                     names(df) <- c("x","f")
                     txt <- "The gamma distribution, written Gamma(a,b), is a distribution  a 
                             random variable that takes only positive values. It has pdf, 
                             \\(f_{\\gamma}(x)\\), is expressed as:
                    $$
                    f_{\\gamma}(x) =
                    \\left[
                    \\frac{b^a}{\\Gamma(a)}    
                    \\right]
                    \\times
                    x^{a-1}\\exp (-bx)
                    \\quad \\text{on } x>0,\\\\
                    \\text{where } \\Gamma(.) \\text{ is the Gamma function.}
                    $$
                    There are two parameters, \\(a\\) and \\(b\\), each of which takes a positive real value       
                     "
                     return(list(df,txt,"gamma Distributution"))
                 },
                 beta =  {
                     x <- seq(0,1,0.01)
                     B <- function(a,b) gamma(a)*gamma(b)/gamma(a+b)
                     k <- function(a,b) 1/B(a,b)
                     f <- function(x,a,b) k(a,b) * x^(a-1) * (1-x)^(b-1)
                     df <- data.frame(x,f(x,input$a1,input$b1))
                     names(df) <- c("x","f")
                     txt <- "
                     The beta distribution, written 
                     \\(B(a, b)\\), 
                     is a distribution for a 
                     random variable that takes values between zero and one. 
                     It has pdf, \\(f_\\beta(x)\\), is expressed as:
                     $$
                       f_\\beta(x)=
                       \\left[
                       \\frac{\\Gamma(a+b)}{\\Gamma(a)\\Gamma(b)}
                       \\right]
                       \\times
                       x^{(a-1)}(1-x)^{(b-1)}
                       \\quad \\text{on } 0<x<1,\\\\
                       \\text{where } \\Gamma(.) \\text{ is the Gamma function.}
                     $$
                     There are two parameters, \\(a\\) and \\(b\\), each of which takes a positive real value.
                     "
                     return(list(df,txt,"beta Distribution"))
                 }
        )
            
            
    )

        output$distPlot <- renderPlot({
            if (!is.nan(ans()[[1]]$f[1])) {
                plot(ans()[[1]]$x,ans()[[1]]$f,type="l",col="red",xlab="x",ylab="f(x)", main=ans()[[3]])
            }
        })
        #output$summary <- renderText({ans()[[2]]})
        output$table   <- renderTable({ans()[[1]]})
        output$summary <- renderUI({withMathJax(ans()[[2]])})
    }
)

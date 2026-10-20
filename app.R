library(shiny)

# 1. UI Section (Paste content from your old ui.R here)
ui <- fluidPage(navbarPage("Confidence Intervals",
                           tabPanel("Mean: Known Sigma",
                                    sidebarLayout(sidebarPanel(
                                      numericInput("xbar", 
                                                   paste0("Sample Mean (", "\u0058\u0304",")"),
                                                   value = 0),
                                      numericInput("sig", 
                                                   paste0("Population Standard Deviation (", "\u3C3", ")"), 
                                                   value = 1),
                                      numericInput("n", 
                                                   "Sample Size (n)", 
                                                   value = 30),
                                      numericInput("cl", 
                                                   paste0("Confidence Level (1 - ", "\u03B1", ")"), 
                                                   value = 0.90,
                                                   min = 0,
                                                   max = 1, 
                                                   step = 0.05),
                                      submitButton("Update View", icon("refresh"))
                                    ),
                                    mainPanel(fluidRow(
                                      column(6,plotOutput(outputId="CI_ks")),  
                                      column(6,tableOutput(outputId = "tableks")))))),
                           tabPanel("Mean: Unknown Sigma",
                                    sidebarLayout(sidebarPanel(
                                      numericInput("xbar2", 
                                                   paste0("Sample Mean (", "\u0058\u0304",")"),
                                                   value = 0),
                                      numericInput("sig2", 
                                                   "Sample Standard Deviation (S)", 
                                                   value = 1),
                                      numericInput("n2", 
                                                   "Sample Size (n)", 
                                                   value = 15),
                                      numericInput("cl2", 
                                                   paste0("Confidence Level (1 - ", "\u03B1", ")"), 
                                                   value = 0.90,
                                                   min = 0,
                                                   max = 1, 
                                                   step = 0.05),
                                      submitButton("Update View", icon("refresh"))
                                    ),
                                    mainPanel(fluidRow(
                                      column(6,plotOutput(outputId="CI_us")),  
                                      column(6,tableOutput(outputId="tableus")))))),
                           tabPanel("Proportion",
                                    sidebarLayout(sidebarPanel(
                                      numericInput("p", 
                                                   "Sample Proportion (p)", 
                                                   value = 0.3),
                                      numericInput("n3", 
                                                   "Sample Size (n)", 
                                                   value = 30),
                                      numericInput("cl3", 
                                                   paste0("Confidence Level (1 - ", "\u03B1", ")"), 
                                                   value = 0.90,
                                                   min = 0,
                                                   max = 1, 
                                                   step = 0.05),
                                      submitButton("Update View", icon("refresh"))
                                    ),
                                    mainPanel(fluidRow(
                                      column(6,plotOutput(outputId="CI_p")),  
                                      column(6,tableOutput(outputId = "tablep")))))),
))

  
  # 2. Server Section (Paste content from your old server.R here)
  server <- function(input, output, session) {
    
    
    output$CI_ks <- renderPlot({
      
      # generate bins based on input$bins from ui.R
      z <- qnorm((1-input$cl)/2, lower.tail = FALSE)
      se <- input$sig/sqrt(input$n)
      lb <- input$xbar-z*se
      ub <- input$xbar+z*se
      xb <- seq(-4,4,length=100)*se + input$xbar
      hxb <- dnorm(xb,input$xbar,se)
      plot(xb, hxb, type="l", yaxt="n", bty="n", xlab="", ylab="",
           main=paste0(input$cl*100, "% Confidence Interval for the Mean"), sub=paste0(round(lb,4), " < ", "\u03BC", " < ", round(ub,4)))
      xgb=seq(lb,ub,length=100)
      yb=dnorm(xgb, input$xbar, se)
      polygon(c(lb,xgb,ub),c(0,yb,0),col="steelblue")
      abline(v=input$xbar, lwd=2)
      abline(v=lb, lwd=1)
      abline(v=ub, lwd=1)
    })
    
    output$tableks <- renderTable({
      zk <- qnorm((1-input$cl)/2, lower.tail = FALSE)
      sek <- input$sig/sqrt(input$n)
      lbk <- input$xbar-zk*sek
      ubk <- input$xbar+zk*sek
      Measures <- c(paste("Sample Mean, ", "\u0058\u0304"), paste("Population Standard Deviation, ", "\u3C3"), "Standard Error", "Sample Size, n", paste("Alpha, ", "\u03B1"), paste0("Confidence Level, 1 - ", "\u03B1"), "Z Critical Value", "Lower Bound", "Upper Bound")
      Values <- c(input$xbar, input$sig, round(sek,4), input$n, round((1-input$cl), 2), input$cl, round(zk,4), round(lbk,5), round(ubk,5))
      cbind(Measures,Values)
      
    })
    
    output$CI_us <- renderPlot({
      
      # generate bins based on input$bins from ui.R
      t <- qt((1-input$cl2)/2, df = (input$n2-1), lower.tail = FALSE)
      se2 <- input$sig2/sqrt(input$n2)
      lb2 <- input$xbar2-t*se2
      ub2 <- input$xbar2+t*se2
      xb2 <- seq(-4,4,length=100)
      hxb2 <- dt(xb2,df = (input$n2-1))
      plot(xb2, hxb2, type="l", xaxt="n", yaxt="n", bty="n", xlab="", ylab="",
           main=paste0(input$cl2*100, "% Confidence Interval for the Mean"), sub=paste0(round(lb2,4), " < ", "\u03BC", " < ",round(ub2,4)))
      tx=seq(-t, t,length=100)
      ty=dt(tx,(input$n2-1))
      polygon(c(-t,tx,t),c(0,ty,0),col="steelblue", border="steelblue", lwd=2)
      abline(v=0, lwd=2)
      abline(v=-t, lwd=1)
      abline(v=t, lwd=1)
      axis(1, -t, round(lb2,2))
      axis(1, 0, input$xbar2)
      axis(1, t, round(ub2,2))
    })
    
    output$tableus <- renderTable({
      t2 <- qt((1-input$cl2)/2, df = input$n2-1, lower.tail = FALSE)
      seu <- input$sig2/sqrt(input$n2)
      lbu <- input$xbar2-t2*seu
      ubu <- input$xbar2+t2*seu
      Measuresu <- c(paste("Sample Mean, ", "\u0058\u0304"), "Sample Standard Deviation, S", "Standard Error", "Sample Size, n", "Degrees of Freedom, n - 1", paste("Alpha, ", "\u03B1"), paste0("Confidence Level, 1 - ", "\u03B1"), "t Critical Value", "Lower Bound", "Upper Bound")
      Valuesu <- c(input$xbar2, input$sig2, round(seu,4), input$n2, input$n2-1, round((1-input$cl2), 2), input$cl2,  round(t2,4),  round(lbu,5), round(ubu,5))
      cbind(Measures = Measuresu,Values = Valuesu)
    })
    
    output$CI_p <- renderPlot({
      
      # generate bins based on input$bins from ui.R
      zp <- qnorm((1-input$cl3)/2, lower.tail = FALSE)
      sep <- sqrt((input$p*(1-input$p))/input$n3)
      lbp <- input$p-zp*sep
      ubp <- input$p+zp*sep
      xbp <- seq(-4,4,length=100)*sep + input$p
      hxbp <- dnorm(xbp,input$p,sep)
      plot(xbp, hxbp, type="l", yaxt="n", bty="n", xlab="", ylab="",
           main=paste0(input$cl3*100, "% Confidence Interval for the Proportion"), sub = paste0(round(lbp,4), " < ",  "\u03C0",  " < ", round(ubp,4)))
      xgbp=seq(lbp,ubp,length=100)
      ybp=dnorm(xgbp, input$p, sep)
      polygon(c(lbp,xgbp,ubp),c(0,ybp,0),col="steelblue")
      abline(v=input$p, col="black", lwd=2)
      abline(v=lbp, lwd=1)
      abline(v=ubp, lwd=1)
    })
    
    output$tablep <- renderTable({
      zp1 <- qnorm((1-input$cl3)/2, lower.tail = FALSE)
      sep1 <- sqrt((input$p*(1-input$p))/input$n3)
      lbp1 <- input$p-zp1*sep1
      ubp1 <- input$p+zp1*sep1
      Measuresp <- c("Sample Proportion, p", "Sample Size, n", "np", "n(1 - p)", paste("Alpha, ", "\u03B1"), paste0("Confidence Level, 1 - ", "\u03B1"), "Z Critical Value", "Lower Bound", "Upper Bound")
      Valuesp <- c(input$p, input$n3, round((input$n3 *input$p),2), round((input$n3*(1-input$p)),2), round((1-input$cl3), 2), input$cl3, round(zp1,4), round(lbp1,5), round(ubp1,5))
      cbind(Measures = Measuresp, Values = Valuesp)
      
    })
    
  }


    
    # 3. App Launch (This binds the UI and Server together)
    shinyApp(ui = ui, server = server)
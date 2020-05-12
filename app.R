#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(MASS)
library(car)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Boston Data Analisis"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            checkboxGroupInput("preditors",
                               "Choose your preditors",
                               choices = c("crim" = 1, "zn" = 2, "indus" = 3, "chas" = 4, "nox" = 5, "rm" = 6, "age" = 7, "dis" = 8, "rad" = 9, "tax" = 10, "ptratio" = 11, "black" = 12, "lstat" = 13, "I_rm_2_" = 15, "rm_lstat" = 16),
                               selected = c("crim" = 1, "zn" = 2, "indus" = 3, "chas" = 4, "nox" = 5, "rm" = 6, "age" = 7, "dis" = 8, "rad" = 9, "tax" = 10, "ptratio" = 11, "black" = 12, "lstat" = 13, "I_rm_2_" = 15, "rm_lstat" = 16)
            ),
            numericInput("removeRowNum",
                         "Choose how many rows to remove (up to 10)",
                         value = 0,
                         min = 0,
                         max = 10
            ),
            conditionalPanel(condition = "input.removeRowNum > 0", 
                             numericInput("RRow1",
                                         "Remove row number 1",
                                         min = 1,
                                         max = 507,
                                         value = 507)),
            conditionalPanel(condition = "input.removeRowNum > 1", 
                             numericInput("RRow2",
                                          "Remove row number 2",
                                          min = 1,
                                          max = 507,
                                          value = 507)),
            conditionalPanel(condition = "input.removeRowNum > 2", 
                             numericInput("RRow3",
                                          "Remove row number 3",
                                          min = 1,
                                          max = 507,
                                          value = 507)),
            conditionalPanel(condition = "input.removeRowNum > 3", 
                             numericInput("RRow4",
                                          "Remove row number 4",
                                          min = 1,
                                          max = 507,
                                          value = 507)),
            conditionalPanel(condition = "input.removeRowNum > 4", 
                             numericInput("RRow5",
                                          "Remove row number 5",
                                          min = 1,
                                          max = 507,
                                          value = 507)),
            conditionalPanel(condition = "input.removeRowNum > 5", 
                             numericInput("RRow6",
                                          "Remove row number 6",
                                          min = 1,
                                          max = 507,
                                          value = 507)),
            conditionalPanel(condition = "input.removeRowNum > 6", 
                             numericInput("RRow7",
                                          "Remove row number 7",
                                          min = 1,
                                          max = 507,
                                          value = 507)),
            conditionalPanel(condition = "input.removeRowNum > 7", 
                             numericInput("RRow8",
                                          "Remove row number 8",
                                          min = 1,
                                          max = 507,
                                          value = 507)),
            conditionalPanel(condition = "input.removeRowNum > 8", 
                             numericInput("RRow9",
                                          "Remove row number 9",
                                          min = 1,
                                          max = 507,
                                          value = 507)),
            conditionalPanel(condition = "input.removeRowNum > 9", 
                             numericInput("RRow10",
                                          "Remove row number 10",
                                          min = 1,
                                          max = 507,
                                          value = 507))
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("summary", verbatimTextOutput("summary")),
                tabPanel("vif", verbatimTextOutput("vifs")),
                tabPanel("residual", plotOutput("residue")),
                tabPanel("outliers", verbatimTextOutput("myoutliers"))
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    my_data <- data.frame("I_rm_2_" = Boston$rm^2, "rm_lstat" = Boston$rm * Boston$lstat)
    full_Boston <- cbind(Boston, my_data)
    #if (reactive(input$removeRow > 0)) (
    #    lm.fit <- reactive(lm(medv ~ input$preditors, data = Boston[-c(input$removeRow), ]))
    #) else (
        lm.fit <- reactive({
            my_Boston <- full_Boston[-c(input$RRow1, input$RRow2, input$RRow3, input$RRow4, input$RRow5, input$RRow6, input$RRow7, input$RRow8, input$RRow9, input$RRow10), ]
            attach(my_Boston)
            xnam <- paste0(colnames(my_Boston[as.double(input$preditors)]))
            fmla <- as.formula(paste("medv", "~", paste(xnam, collapse = " + ")))
            nfmla <- paste("lm(formula = medv", "~", paste(xnam, collapse = " + "), ",", "data = Boston)")
            model1 <- lm(fmla)
            model1[["call"]] <- nfmla
            model1
        })
    #)
    output$summary <- renderPrint({
        summary(lm.fit())
    })
    
    output$vifs <- renderPrint({
        vif(lm.fit())
    })

    output$residue <- renderPlot({
        par(mfrow = c(2, 2))
        plot(lm.fit())
    })
    
    output$myoutliers <- renderPrint({
        Boston[c(input$RRow1, input$RRow2, input$RRow3, input$RRow4, input$RRow5, input$RRow6, input$RRow7, input$RRow8, input$RRow9, input$RRow10), ]
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(DT)
source("www/grids.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  
  titlePanel(
    div(class = "title", 
        tags$img(
          src = "Umfa_Logo2.png",
          alt = "Logo for UMFA",
          width = 300 ,
          height = 75
        ),
        "UMFA Salary Calculator")
  ),
  
  fluidRow(column(4,
                  selectInput("rank", "Current Rank", 
                              choices = c("Professor", "Associate Professor",
                                          "Assistant Professor", "Lecturer",
                                          "Senior Instructor", "Instructor II",
                                          "Instructor I", "Librarian",
                                          "Associate Librarian", "Assistant Librarian",
                                          "General Librarian"),
                              selected = "Professor"
                  )
  ),
  
  column(4,
         numericInput("salary", "Current Base Salary", value = 75000
         )
  ),
  
  column(4,
         selectInput("year", "Year of Increase", 
                     choices = c(2022, 2023),
                     selected = 2023
                     
         )
  )
  
  ),
 
  fluidRow(
  column(4,
         radioButtons("promotion", "Are You Receiving a Promotion?", 
                      choices = c("Yes", "No"),
                      selected = "No")
  ),
  
  column(4,
         div(class = "button", 
                  # Calculate Salary Button
                  actionButton("calculate", "Calculate New Salary")),
  ),
  
  column(4,
         div(class = "button", 
             # Render Explanation Button
             actionButton("explain", "Explain Salary Calculations")),
  )
  ),
  
  fluidRow(
  
  column(12,
        div(class = "summary", textOutput("summary"))
  )
  ),
  
  fluidRow(
  div(class = "explanation",
    span(style = "text-align: center; font-weight: bold",
         textOutput("explainPreamble")),
    p(textOutput("baseExplain")),
    p(textOutput("promotionExplain")),
    p(textOutput("scaleExplain")),
    p(textOutput("specialExplain")),
    p(textOutput("incrementExplain")),
    p(textOutput("minimumExplain")),
    p(textOutput("promotionIncrementExplain"))
    )
  ),
  
  fluidRow(
  column(4,
         tags$div(class = "button", 
                  # 2021 table button
                  actionButton("table2021", "Show 2021 Salary Grid"))
  
         ),
  column(4,
         tags$div(class = "button", 
                  # 2022 table button
                  actionButton("table2022", "Show 2022 Salary Grid"))
         ),
  
  column(4,
         tags$div(class = "button", 
                  # 2023 table button
                  actionButton("table2023", "Show 2023 Salary Grid"))
  )
  ),
  
  fluidRow(
    column(12, align = "center",
    div(class = "grid",
        DT::dataTableOutput("grids")
    )
    )
  )
)
  



# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #Create grid of values
  dt <- reactive({
    if(input$year == 2022){grid2022DF}
    else{grid2023DF}
  })
  
  # Create grid for previous year
  dtPrevious <- reactive({
    if(input$year == 2022){grid2021DF}
    else{grid2022DF}
  })
  
  # Calculate scale increase
  increase <- reactive({
    if(input$year == 2022){
      1.0225
    } else{
      1.0225
    }
  })
  
  # Calculate end rank
  endrank <- reactive({
    if(input$promotion == "Yes"){ 
      tempdt <- dt()
      tempdt[which(tempdt$rank == input$rank)-1, 1]}
    else{input$rank}
  })
  
  ### Calculate new salary
  
  # Bring promotions to floor minimum of new rank
  salaryBase <- reactive({
    if(input$promotion == "Yes"){
      tempdt<- dtPrevious()
      tempEndRank <- endrank()
      max(input$salary, 
              tempdt$floor[which(tempdt$rank==tempEndRank)])
    }
    else{input$salary}
  })
  
  # Calculate rank maximum
  
  rankMax <- reactive({
    tempdt <- dt()
    tempEndRank <- as.character(endrank())
    tempdt$maximum[which(tempdt$rank == tempEndRank)]
  })
  
  # Calculate rank minimum
  
  rankMin <- reactive({
    tempdt <- dt()
    tempEndRank <- as.character(endrank())
    tempdt$floor[which(tempdt$rank == tempEndRank)]
  })
  
  # Calculate rank minimum pre-April 1
  
  rankMinInitial <- reactive({
    tempdt <- dtPrevious()
    tempEndRank <- as.character(endrank())
    tempdt$floor[which(tempdt$rank == tempEndRank)]
  })
  
  # Calculate increment
  
  increment <- reactive({
    tempdt <- dt()
    tempEndRank <- as.character(endrank())
    tempdt$increment[which(tempdt$rank == tempEndRank)]
  })
  
  # Apply scale increase
  A <- reactive({
    salaryBase()*increase()
  })
  
  
  
  # Check for professor adjustment
  B <- reactive({
    if(input$rank == "Professor"){
      tempdt <- dt()
      A() + min(3000, tempdt$maximum[which(tempdt$rank == "Professor")]- A())
    }
    else{A()}
  })

  # Apply increment up to maximum
  C <- reactive({
    tempdt <- dt()
    tempEndRank <- as.character(endrank())
    B <- as.numeric(B())
    B + min(tempdt$increment[which(tempdt$rank == tempEndRank)],
            tempdt$maximum[which(tempdt$rank == tempEndRank)] - B)
  })

  # Bring salary to rank minimum if applicable
  D <- reactive({
    tempdt <- dt()
    tempEndRank <- endrank()
      max(C(), tempdt$floor[which(tempdt$rank == tempEndRank)])
  })

  # Apply promotion increment
  E <- reactive({
    tempdt <- dt()
    tempEndRank <- endrank()
    if(input$promotion == "Yes"){
      D() + tempdt$increment[which(tempdt$rank == tempEndRank)]
    }
    else{D()}
  })
  
  result <- eventReactive(input$calculate, {
    format(round(E(), 2), nsmall = 2)
  })
  
  # Text output of result
  
  #observeEvent(input$calculate, {
    output$summary <-  renderText({
      paste("As of April 1", input$year, "you will have a rank of", endrank(), "and have a base salary of", result())
      #paste(input$rank, result())
    }) |>
      bindEvent(input$calculate)
    
    output$explainPreamble <- renderText({
      paste("Your salary was calculated as follows:")
    }) |>
      bindEvent((input$explain))
    
    output$baseExplain <- renderText({
      paste("You starting base salary was", input$salary)
    }) |>
      bindEvent(input$explain)
    
    
      output$promotionExplain <- renderText({
        if(input$promotion == "Yes"){
        paste("After bringing your salary to the minimum of your current salary", input$salary,"and your new rank's minimum prior to April 1 of", rankMinInitial(), "your base salary is", salaryBase(), ".")
        }
        else{}
      }) |>
        bindEvent(input$explain)
      
      output$scaleExplain <- renderText({
        paste("After applying a scale increase of", increase(), "your base salary is", format(round(A(), 2), nsmall = 2), ".")
      })|>
        bindEvent(input$explain)
      
      output$specialExplain <- renderText({
        if(input$rank == "Professor"){
        paste("After applying the special adjustment of $3000 for Professors, ensuring that you do not exceed your rank maximum of", rankMax(), "your base salary is", format(round(B(), 2), nsmall = 2), ".")
        }
        else{}
      }) |>
        bindEvent(input$explain)
      
      output$incrementExplain <- renderText({
        paste("After applying your increment of", increment(), "and ensuring you do not exceed your rank maximum of", rankMax(), "your base salary is",format(round(C(), 2), nsmall = 2), ".")
      }) |>
        bindEvent(input$explain)
      
      output$minimumExplain <- renderText({
        if(input$promotion == "Yes"){
        paste("After checking that your salary is not less than your new rank minimum of", rankMin(), "your base salary is", format(round(D(), 2), nsmall = 2), ".")
        }
        else{
          paste("After checking that your salary is not less than your rank minimum of", rankMin(), "your base salary is", format(round(D(), 2), nsmall = 2), ".")
        }
      }) |>
        bindEvent(input$explain)
      
      output$promotionIncrementExplain <- renderText({
        if(input$promotion == "Yes"){
          paste("After applying your bonus promotion increment of", increment(), " (which may cause you to exceed your rank max of", rankMax(), ") your new base salary is", format(round(E(), 2), nsmall = 2),".")
        }
      }) |>
        bindEvent(input$explain)
    
    
    observeEvent(input$table2021, {
      output$grids <- DT::renderDataTable(grid2021)
    })
    
    observeEvent(input$table2022, {
      output$grids <- DT::renderDataTable(grid2022)
    })

    observeEvent(input$table2023, {
      output$grids <- DT::renderDataTable(grid2023)
    })

    
    #})
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

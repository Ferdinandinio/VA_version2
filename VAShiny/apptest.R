library(shiny)


ui <- fluidPage(
  
  radioButtons(inputId = "Indicator", label="Select Attribute", choiceNames = c("Density of Studies", "Density of Organizations", "Ratio of Studies and Organizations"), choiceValues = c("DoS", "DoO", "Ratio")),# 
  
  
  
  
  radioButtons(inputId = "TempRes", label="Select time frame", choiceNames = c("Total", "Pre-1990", "1990-1999", "2000-2009", "2010-2020"), choiceValues = c("Total", "b1990", "b2000", "b2010", "b2020")), #

  
  
  
)

#https://stackoom.com/en/question/3gvf5


server <- function(input, output, session) {
  
  cchoices <- reactive(paste0(input$Indicator, input$TempRes))
  
  # 
  # 
  # #update choices based on input
  # observeEvent( input$TempRes, 
  #   updateRadioButtons(session, inputId = "Indicator",
  #                      choiceNames = c("Density of Studies", "Density of Organizations", "Ratio of Studies and Organizations"), choiceValues = choiceValues[[input$TempRes]], selected = if(is.na(value())==F)print(value()))
  # )

  # observeEvent( c(input$TempRes, input$Indicator),
  #               updateRadioButtons(session, inputId = "Indicator", selected=value())
  # )
  
  
  # observeEvent(input$Indicator, {print(input$Indicator)})
  # observeEvent(c(input$Indicator, input$TempRes), {print(match(input$Indicator, choiceValues[[input$TempRes]]))})
  observeEvent(c(input$Indicator, input$TempRes), {print(value())})
               # if(is.na(value())==F)print(value())
  # choiceValues[[input$TempRes]][match(input$Indicator, choiceValues[[input$TempRes]])]
  # observeEvent(input$TempRes, {print(match(input$Indicator, names(choiceValues[[input$TempRes]])))})
  # outVar = reactive({
  #   mydata = get(input$TempRes)
  #   names(mydata)
  # })
  # 
  # observeEvent({ input$TempRes,
  #   updateSelectInput(session, inputId = "Indicator",
  #                     choices = paste0(iNames,outVar()))
  # })
  
  
}



shinyApp(ui = ui, server = server)

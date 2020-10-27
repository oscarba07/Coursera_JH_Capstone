#library(shiny)
library(shinythemes)
source('text_pred.R')
source('clean_file.R')

ui <- fluidPage(theme = shinytheme('cerulean'),

    titlePanel("Data Science Specialization: Capstone Project"),
    
    sidebarLayout(
        sidebarPanel(
            textInput(inputId = 't', label='Enter text', value=''),
            uiOutput('Ops')
            #submitButton(text='Update')
        ),
        mainPanel(
            tabsetPanel(
                tabPanel('Prediction',
                         h2(textOutput('text')
                                , textOutput('pred')
                            )
                ),
                tabPanel('Documentation',
                         h2('Objective'),
                         p('This is the final project for the Data Science Specialization 
                  by Johns Hopkins University through Coursera.'),
                         p('The objective of this project is to develop a text prediction app. To do this, 
            an n-gram and Katz back-off model was used. Given a text, the model looks for the last 3 words
            and shows the words that complete the most frequent 4-grams. If no 4-grams are found, then the
            model looks for the last 2 words in the most frequent 3-grams. The process is repeated if necessary 
            until the most frequent uni-grams are showed.'),
                         p('The model used 2.5% of the data provided by the Course and can be found here:',
                           a('https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip.'), 
                           'A descriptive analysis of the data was performed and can be found here:',
                           a('https://rpubs.com/oscarba07/DSS_milestone')),
                         h2('Usage'),
                         p('To use this app, enter the text in the text box and wait a few seconds for the app 
                         to predict the next word. Then, choose or write the correct word.'),
                         h2('Setbacks'),
                         p('The prediction model takes a few seconds (4~25) to run but it takes just a couple
                           to recognize that the input text has changed. Which is why, it sometimes runs 
                           before finishing entering the complete text. This is not an app for a slow typer. This issue
                           could have been solved using a submit button but I considered that a dynamic model
                           was better for the purpose of this app. The size of the data used for the
                           model hinders its usage in small devices. Additionally, it uses a maximum of 3 words to
                           predict the next one, not being able to handle texts\' long relationships. This app doesn\'t
                           handle typos or spelling errors either. Finally, punctuations are not considered in this model.
                           This model could definitely be improved.')
                )    
            )
        )
    )
)

server <- function(input, output) {
    ops <- reactive({t.pred(input$t)})
    output$Ops <- renderUI({
        radioButtons(inputId = 'Ops',label='Choose option',choices = ops())
    })
    output$text <- renderText({ paste(input$t, input$Ops, sep=' ')})
}

# Run the application 
shinyApp(ui = ui, server = server)

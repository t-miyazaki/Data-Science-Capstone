library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Next Word Prediction"),

    # Sidebar with a text input
    sidebarLayout(
        sidebarPanel(
            h4("How to use: "),
            h4("1) Enter some texts (at least 2 words)"),
            h4("2) Select number of predictions"),
            h4("3) Click 'Submit' button"),
            h4("4) You can see the next word prediction results"),
            textInput("words", "Enter some texts:", value = ""),
            numericInput("numeric", 
                         "How many prediction results should be shown?",
                         value = 10, min = 1, max = 50, step = 1),
            submitButton("Submit")
        ),

        # Show a table of next word prediction results
        mainPanel(
            h3("Top Predicted Words:"),
            h4("- pre.words: Text input before the predicted word"),
            h4("- last.word: Predicted word"),
            h4("- count: Number of the word frequency in the training data"),
            tableOutput("table")
        )
    )
))

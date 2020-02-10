library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Wordscapes Solver"),
    
    # Sidebar with a text-input for input letters
    sidebarLayout(
        sidebarPanel(
            HTML('In the popular mobile game "Wordscapes", players are are tasked 
            with filling in a clue-less crossword puzzle, by making words from a 
               wheel of 4 to 7 letters. <br> <br> This app can be used to
               help stumped gamers find the words they are missing. <br> <br>'),
            textInput(inputId = 'letters',
                      label = "Enter Between 4 and 7 letters",
                      value = "stumped"),
            textOutput("word_count"),
            hr(),
            HTML('*English words are validate using the <code> hunspell </code> package')
        ),
        
        # Show the table of words
        mainPanel(
            fluidRow(
                column(width = 4,tableOutput("words_table3")),
                column(width = 4,tableOutput("words_table4")),
                column(width = 4,
                       tableOutput("words_table5"),
                       tableOutput("words_table6"),
                       tableOutput("words_table7")
                )
            )
        )
    )
))

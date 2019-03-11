library(shiny)
library(tidyverse)
library(stringr)
library(ggplot2)
library(gtools)
library(hunspell)
library(wordcloud)

possible_words <- function(letters_str, sizes){
    
    # split up the string into a vector of characters
    letters_vector <- str_split(letters_str, boundary("character"))[[1]]
    
    # compute all of the 3 letter permutations
    perms <- permutations(n = length(letters_vector),
                          r = sizes[1],
                          v = letters_vector,
                          set = FALSE)
    
    # flatten each permutations back into a "word vector"
    word_list <- apply(perms, 1 , str_flatten)
    
    # convert to a tibble date frame, and filter out mispelled words
    words_df <- tibble(word_list = word_list) %>%
        filter(hunspell_check(words = word_list))
    
    # Repeat for words of size 4,5,... letters
    if (length(sizes) > 1) {
        for (i in 2:length(sizes)) {
            size = sizes[i]
            perms <- permutations(n = length(letters_vector),
                                  r = sizes[i],
                                  v = letters_vector,
                                  set = FALSE)
            
            word_list <- apply(perms, 1 , str_flatten)
            
            more_words <- tibble(word_list = word_list) %>%
                filter(hunspell_check(words = word_list))
            words_df <- rbind(words_df, more_words)
        }
    }
    
    # Return only a distinct set of words
    words_df %>% 
        distinct()
}

# Define UI for application that draws a histogram
ui <- fluidPage(
    
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
                      value = "girfgoe"),
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
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # Convert letters to lower case, and truncate at 7 letters. (n! gets large 
    # quick!)
    validated_letters <- reactive({
        str_to_lower(input$letters) %>%
            str_trunc(7, ellipsis = '')  %>%
            str_squish()
    })
    
    # Build the list of possible words
    word_list <- reactive({
        letters <- validated_letters()
        max_length <- str_length(letters)
        possible_words(letters, 3:max_length)
    })
    
    # Count how many words are possible
    n_words <- reactive({
        nrow(word_list())
    })
    
    # Render the words table
    output$words_table3 <- renderTable({
        wl <- word_list() %>%
            filter(str_count(word_list) == 3) %>%
            rename(`3 Letter Words` = word_list)
        validate(
            need(str_length(input$letters) >=4, ' '),
            need(nrow(wl) > 0, '')
        )
        wl
    })
    
    output$words_table4 <- renderTable({
        wl <- word_list() %>%
            filter(str_count(word_list) == 4) %>%
            rename(`4 Letter Words` = word_list)
        validate(
            need(str_length(input$letters) >=4, ' '),
            need(nrow(wl) > 0, '')
        )
        wl
    })
    
    output$words_table5 <- renderTable({
        wl <- word_list() %>%
            filter(str_count(word_list) == 5) %>%
            rename(`5 Letter Words` = word_list)
        validate(
            need(str_length(input$letters) >=5, ' '),
            need(nrow(wl) > 0, '')
        )
        wl
    })
    
    output$words_table6 <- renderTable({
        wl <- word_list() %>%
            filter(str_count(word_list) == 6) %>%
            rename(`6 Letter Words` = word_list)
        validate(
            need(str_length(input$letters) >=6, ' '),
            need(nrow(wl) > 0, '')
        )
        wl
    })
    
    output$words_table7 <- renderTable({
        wl <- word_list() %>%
            filter(str_count(word_list) == 7) %>%
            rename(`7 Letter Words` = word_list)
        validate(
            need(str_length(input$letters) >=7, ' '),
            need(nrow(wl) > 0, '')
        )
        wl
    })
    
    # Render the word count
    output$word_count <- renderText({
        validate(
            need(str_length(input$letters) >=4, 'Please enter at least 4 letters')
        )
        paste("There are", n_words(), "words.")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

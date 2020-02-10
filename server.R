library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
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
    
})

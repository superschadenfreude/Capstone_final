require(dplyr)
require(quanteda)
require(stringr)
require(shiny)

ngram_1 <- readRDS("ngram_filter_1.RDS")
ngram_2 <- readRDS("ngram_filter_2.RDS")
ngram_3 <- readRDS("ngram_filter_3.RDS")
ngram_4 <- readRDS("ngram_filter_4.RDS") 
 

filter_print <- function(user_w1, user_w2, user_w3 ) {
  if (any(ngram_4$Word3 == user_w1 & ngram_4$Word2 == user_w2 & ngram_4$Word1 == user_w3)== TRUE) {
    res_4 <- ngram_4 %>% filter(Word3==user_w1 & Word2==user_w2 & Word1==user_w3 ) %>%
      slice_max(frecuency, n = 1,with_ties = FALSE)
    print (res_4$Word4 )
  }else if (any(ngram_3$Word2 == user_w1 & ngram_3$Word1 == user_w2)== TRUE) {
    res_3 <- ngram_3 %>% filter(Word2==user_w1 & Word1==user_w2 ) %>%
      slice_max(frecuency, n = 1,with_ties = FALSE)
    print (res_3$Word3 )
  }else  if (any(ngram_2$Word1 == user_w1)==TRUE){
    res_2 <- ngram_2 %>% filter(Word1==user_w1 ) %>% 
      slice_max(frecuency, n = 1,with_ties = FALSE)
    print (res_2$Word2 )
  }else { print (sample(ngram_1$Word1,1))}
}


ui <- fluidPage(
 titlePanel("Word predictor app (ATS)"),
  br(),
 p("Welcome to the next word predictor. Just write at least 3 words.
   Less than 3 words won't return any value. Enjoy!"),
  textInput("user_sentence", h3("Text input")),
  actionButton("do", "Click to send your sentence"),
  br(), br(),
  verbatimTextOutput("value"),
 br(), br(),
 p("To know more about the  code, please visit my",
   a("Github page.", href = "https://github.com/superschadenfreude/Capstone_final")),
   )


server <- function(input, output, session) { 
  observeEvent(input$do, {
  user_w1 <- stringr::word(input$user_sentence, -1)
  user_w2 <- stringr::word(input$user_sentence, -2)
  user_w3 <- stringr::word(input$user_sentence, -3)
  ##})
  ##reactive( { 
    req(user_w1, user_w2, user_w3)
  ##result <- filter_print(user_w1, user_w2 ,user_w3)
  ##result <- do.call("filter_print", user_w1, user_w2, user_w3 )
  output$value <- renderText( filter_print(user_w1, user_w2 ,user_w3))
  })
  } 
      

# Run the application 
shinyApp(ui = ui, server = server)

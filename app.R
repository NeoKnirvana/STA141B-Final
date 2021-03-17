## Install Packages


library(jsonlite)
library(httr)
library(tidyverse)
readRenviron(".Renviron")
library(shiny)


###Import the CSV file we created in Main Code

bseller_df = read.csv("bseller_df.csv")



## Create the UI

ui <- fluidPage(
    titlePanel("New York Times Bestsellers"),
    
    sidebarLayout(
        sidebarPanel(
            
            selectInput("book_type", "List:", 
                        choices = distinct(bseller_df, book_type)),
            selectInput("date_published", "Date Published (Y-M-D):", 
                        choices = distinct(bseller_df, date_published)),
            sliderInput("rank", "Rank This Week:", min = 1, max = 15, 
                        value = 10, step = 1, round = TRUE),
        ),
        
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Book Ranking", uiOutput("cover", align = "center"),
                                 textOutput("about"), hr(), 
                                 htmlOutput("creators"),hr(), htmlOutput("stats")),
                        tabPanel("Boxplot", plotOutput("bplot"), hr(),
                                 htmlOutput("box_description"))
            )
        )
        
    )
)



server <- function(input, output, session) {
    
    
    
    # A dynamically-sized plot
    output$cover = renderUI({
        
        # X is the exact row we are looking for
        x = bseller_df %>% filter(book_type == input$book_type, 
                                  date_published == input$date_published, 
                                  rank == input$rank)
        # All image dimensions are different, so we'll use average width and height
        # to make it consistent
        h = mean(bseller_df$book_image_height)
        w = mean(bseller_df$book_image_width)
        
        #Find the image url
        pic = x %>% select(book_image) 
        
        # shows the image
        img(src = pic, height = h, width = w)
    })
    
    output$about = renderText({
        # Finds the description for the book selected and prints it out
        paste(bseller_df %>% filter(book_type == input$book_type, 
                                    date_published == input$date_published, 
                                    rank == input$rank) %>% select(description))
    })
    
    output$creators <- renderUI({
        
        # Finding the title, author, and publisher based on the inputs
        t = bseller_df %>% filter(book_type == input$book_type, 
                                  date_published == input$date_published, 
                                  rank == input$rank) %>% select(title)
        
        a = bseller_df %>% filter(book_type == input$book_type, 
                                  date_published == input$date_published, 
                                  rank == input$rank) %>% select(author)
        p = bseller_df %>% filter(book_type == input$book_type, 
                                  date_published == input$date_published, 
                                  rank == input$rank) %>% select(publisher)
        
        #Put t a and p into one string, separated by a new line
        #Strong bolds the string in the tag
        x <- paste(paste("<strong>Title</strong>: ", t),
                   paste("<strong>Author</strong>: ", a), 
                   paste("<strong>Published by</strong>: ", p), sep="<br/>")
        HTML(x)
    })
    
    output$stats = renderUI({
        
        # Finds title and the rank the book was last week for that list
        # Putting these variables outside function crashed the shinyapp.
        t = bseller_df %>% filter(book_type == input$book_type, 
                                  date_published == input$date_published, 
                                  rank == input$rank) %>% select(title)
        lw = bseller_df %>% filter(book_type == input$book_type, 
                                   date_published == input$date_published, 
                                   rank == input$rank) %>% select(rank_last_week)
        #Finds all books with the same title and finds the book's average rank 
        #total finds out how many times it appears in bsellers_df
        in_list = bseller_df %>% filter(title == paste(t))
        r_avg = round(mean(in_list$rank), digits = 2)
        total = nrow(in_list)
        
        # Produces a text string, with each one on a new line
        txt = paste(paste("Last Week's Rank: ", lw),
                    paste("Found In ", total, "Lists This Month"),
                    paste("Average Rank: ", r_avg), sep="<br/>")
        HTML(txt)
    })
    
    
    
    output$bplot <- renderPlot({
        
        # r_line is the abline that will show the selected book's rank
        # t is the title of the book currently selected
        # ylab_t is a string that prints the title out for the boxplot
        r_line = bseller_df %>% filter(book_type == input$book_type, 
                                       date_published == input$date_published, 
                                       rank == input$rank) %>% select(rank)
        t = bseller_df %>% filter(book_type == input$book_type, 
                                  date_published == input$date_published, 
                                  rank == input$rank) %>% select(title)
        ylab_t = paste("Book Title: ", t)
        
        # We find the average rank for each book
        box_data = bseller_df %>% group_by(title) %>% summarize(avg_rank = mean(rank))
        
        # Create a boxplot based on the average rank for each book for lists in bsellers_df
        boxplot(box_data$avg_rank,
                xlab="Average Rank",
                ylab= ylab_t, 
                main = "Boxplot for All Books and Their Average Rank",
                horizontal = TRUE,
                col = "cyan")
        #Add a red line to show where the selected book lies
        abline(v = r_line, col = "red")
    })
    
    output$box_description <- renderUI({ 
        paste("This boxplot shows the average rank for all NYT bestseller titles in lists with 'nonfiction' in the title.  This is data taken from all the lists considered 'nonfiction' from March 7 2021, to March 21 2021.  The red abline shows the rank of the selected book from the input week and the input list (note that this is not the average rank).") })
    
}



# THE APP


#Runs the app
shinyApp(ui, server)


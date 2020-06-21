library(shiny)
library(tidyverse)
library(fpp2)
library(seasonal)
library(rio)
library(lubridate)


ui <- fluidPage(

    # Application title
    titlePanel("Time Series Seasonal Adjustment"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            fileInput("df",
                        "Choose your file (.xlsx, .xls or .csv)"),
            downloadButton("download", "Download Seasonal adjusted data")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("Introduction", 
                         p(""),
                         p("The goal of this app is to use data from your suppliers to estimate flexible indexes. It lets you adjust the relative importance of every feature and automatically standardize features (substracting the mean and dividing by the standard deviation of each variable) in order to make them comparable. The obtained score corresponds to the weighted sum of standardized attributes. It's open source and all the files are hosted on ", a("my github.", href = "https://github.com/nelson-io/supplier_benchmarking")),
                         h4(strong("Input file")),
                         p("Choose a file from your system with suppliers data. \n Each column must be an attribute and the first column", strong("MUST"), " contain supplier names (or id's)."),
                         h4(strong("Sliders")),
                         p( "Use the automatically generated sliders to adjust the relative importance of each attribute."),
                         h4(strong("Tabs")),
                         p(code("Score"), "- This tab shows with a barplot the standardized scores obtained by each supplier. It also shows the weight of every variable (which depends on the relative importance of each attribute)."),
                         p(code("Original table"),"- This tab shows the original data. It can be filtered and arranged by multiple variables."),
                         p(code("Standardized table"), "- This tab shows the standardized data. It can be filtered and arranged by multiple variables."),
                         p(""),
                         p(""),
                         p(""),
                         p(a("Here", href= "https://github.com/nelson-io/supplier_benchmarking/raw/master/sample_2.xlsx"), " is a sample .xlsx file to try the app!")
                ),
                tabPanel("Seasonal Adjusted table", DT::dataTableOutput("seas_table"))
            )
        )
    )
)

        
#         mainPanel(
#            DT::dataTableOutput("seas_table")
#         )
#     )
# )

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    data <- reactive({
        validate(
            need(input$df, message = F)
        )
        infile <- input$df
        import(input$df$datapath) %>% 
        rename(periodo = 1) %>% 
        mutate_at(vars(periodo), ~ ymd(parse_date(.)))
    })
    
    data_seas <- reactive({
        map_df(data() %>% select(-periodo), ~ ts(.x,start = c(2014,01), frequency = 12) %>% seas() %>% seasadj() %>% 
                   as.numeric()) %>% 
            cbind(data() %>% select(periodo), .)
    })


    output$seas_table <- DT::renderDataTable({
        data_seas() %>% 
            mutate_at(vars(-periodo), round, 2)
    })
    
    output$download <- downloadHandler(
        filename = "seasadj_data.xlsx",
        content = function(file){
            export(data_seas(), file)}
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)

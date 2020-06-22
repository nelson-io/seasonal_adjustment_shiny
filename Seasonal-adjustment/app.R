library(shiny)
library(tidyverse)
library(fpp2)
library(seasonal)
library(rio)
library(lubridate)
library(parsedate)


ui <- fluidPage(

    # Application title
    titlePanel("Multiple Time Series Seasonal Adjustment"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            fileInput("df",
                        "Choose your file (.xlsx, .xls or .csv)"),
            downloadButton("download", "Download seasonal adjusted data")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("Introduction", 
                         p(""),
                         p("The goal of this app is to use time series dataframes to estimate seasonaly adjusted series. It works with most formats (csv, xlsx, xls, dsv and more) and in few seconds it generates a dataframe with the same dimensions as the original table seasonally adjusting each time series included. It's open source and all the files are hosted on ", a("my github.", href = "https://github.com/nelson-io/seasonal_adjustment_shiny")),
                         h4(strong("Input file")),
                         p("Choose a file from your system with Time Series data. \n The first column ", strong("MUST"), " contain the date of each observation while the next columns ", 
                           strong("MUST"), "contain the values associated to those dates like in the following table:"),
                         img(src = "https://github.com/nelson-io/seasonal_adjustment_shiny/raw/master/table.PNG"),
                         h4(strong("Data consumption")),
                         p( "Press the download button in order to download the adjusted data or see the results at the 'Seasonal Adjusted table' tab. \n Have in mind that depending on the size of the dataset it may take some time to process and adjust every series."),
                         h4(strong("Considerations")),
                         p(code("Statistical method"),"- In order to adjust the time series, the ", strong("X13-ARIMA-SEATS")," method is used. Next updates will include the possibility to use the ", strong("X11"), " method."),
                         p(code("Time Series Legth"), "- Data must contain at least 3 years of observations in order to apply seasonal adjusting methods"),
                         p(code("NAs"), "- Data must be complete. Any single NA would invalidate the data so they must be treated before using this App. (Next updates will include time-series extrapolation methods to solve this issue)"),
                         p(code("frequency"), "- This App uses an algorithm that automatically infers the correct frequency of your data so it`s unnecessary to specificate it manually"),
                         p(code("date structure"), "- This App uses an algorithm that can parse almost any virtually existing date or datetime structure automatically!"),
                         p(""),
                         p(""),
                         p(""),
                         p(a("Here", href= "https://github.com/nelson-io/seasonal_adjustment_shiny/raw/master/emae-apertura-por-sectores-valores-mensuales-indice-base-2004.csv"), " is a sample .xlsx file to try the app!")
                ),
                tabPanel("Seasonal Adjusted table", DT::dataTableOutput("seas_table"))
            )
        )
    )
)

        

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
    

    freq <- reactive({
        data() %>%
         group_by(year(periodo)) %>%
         summarise(total = n()) %>%
         pull(total) %>%
         median()
    })
    
    data_seas <- reactive({
        map_df(data() %>% select(-periodo), ~ ts(.x,start = c(year(first(data() %>% pull(periodo))),
                                                              month(first(data() %>% pull(periodo)))),
                                                 frequency = freq()) %>% seas() %>% seasadj() %>% 
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

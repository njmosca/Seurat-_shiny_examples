# R shiny application for Gene Expression



library(tidyverse)
library(shiny)
library(ggExtra)
library(DT)



data = read_csv("Tisse_CellSource_RNA.csv")


#editing column names 
colnames(data) <- c("X1","Gene", "Gene_name", "Sample", "value", "Unit", "Abundance", "Source","Morphology", "Cancer")

#trimming dataset
edited_data <- data %>%
  filter(row_number() %% 5 == 1)

#selecting genes
genes <- edited_data %>% pull(Gene_name) %>% unique()
source_list = sort(data %>% pull(Source) %>%  unique())
#user interface

ui <- fluidPage(
  titlePanel("RNA-Seq"),
  sidebarLayout(
    sidebarPanel(
      #user will select gene of interest
      selectInput(inputId = "gene",
                  label = "Select Gene", choices = genes,
                  multiple = TRUE, selected = 'TSPAN6'),
      
      
      plotOutput("plot", brush = "user_brush")
      
      
      
    ),
    mainPanel( plotOutput("distPlot",brush = "plot_brush"),dataTableOutput('table'),verbatimTextOutput('info')
    )
  )
)



#server communication 

server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    data %>% filter(Gene_name %in% input$gene) %>% 
      
      
      ggplot( aes(x =Source , y = log(value), color = Abundance)) +
      geom_point() +
      theme(axis.text.x = element_text(angle=90)) +
      ggtitle( "Gene Expression Values vs mRNA Transcript Source ") + 
      ylab("Expression Value") +
      xlab(" mRNA Transcript Source")
    
    
  }) 
  
  # add second plot 
  
  
  #for brush info
  output$info <- renderPrint({
    cat(str_c("Window: ",
              "Source = (", input$plot_brush$xmin, ", ", input$plot_brush$xmax, ")\n", "        ",
              "Values = (", input$plot_brush$ymin, ", ", input$plot_brush$ymax, ")\n"))
    
    #only isolating data selected by brush
    # vector of strings with source names 
    #round up xmin and round down xmax
    #display only those within that window
    #source in source list within xmin and xmax
    #add condition to remove errors
    # render table nicer
    
    if (length(input$plot_brush$xmin) == 0 ){selected_data <- data} else{
      
      
      selected_data <- data %>% select(Gene_name,value,Source) %>% 
        filter(Gene_name %in% input$gene)  %>% 
        filter(between(log(value),input$plot_brush$ymin,input$plot_brush$ymax)) %>% 
        filter(Source %in% source_list[ceiling(input$plot_brush$xmin):floor(input$plot_brush$xmax)]) }
    
    #brush display variable
    
    brushed <- brushedPoints((data %>% filter(Gene_name %in% input$gene)),input$plot_brush, xvar = "Source",yvar = "value" )
    #displaying
    #print(selected_data)
    
    
    
    
  })
  
  
  output$table <- renderDataTable({ if (length(input$plot_brush$xmin) == 0 ){selected_data <- data} else{
    
    
    selected_data <- data %>% select(Gene_name,value,Source) %>% 
      filter(Gene_name %in% input$gene)  %>% 
      filter(between(log(value),input$plot_brush$ymin,input$plot_brush$ymax)) %>% 
      filter(Source %in% source_list[ceiling(input$plot_brush$xmin):floor(input$plot_brush$xmax)]) }
    
    selected_data
    
    
  })
  
  
}


shinyApp(ui = ui, server = server)

shinyApp(ui,server)


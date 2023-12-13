library(shiny)
library(jpeg)
setwd("~/Desktop/Data Science Major/stochify/R Shiny")
source("../stochify.R")
# library(parallel)


#######################
######## NOTES ########
#######################
    # - check chat gpt conversations "GGplot2 for image plotting" and "Shiny App: Plot Download"
        # -> make the download button conditional, use ggplot2 to save image.
        # -> use the switch cases to change the way the plots are made

# UI ----------------------------------------------------------------------



ui <- fluidPage(
  titlePanel("Stochify R: Create Art from Images"),
  sidebarLayout(
    sidebarPanel(
      helpText("Upload and image to get started!"),
    
      fileInput("image1","Upload an image (.jpeg)"),
      helpText("Select a method to create your new image"),
      selectInput("stochifyMethod", "Select Method", choices = c("stochify()", "self_stochify()","cross_stochify()")),
      
      
      conditionalPanel(
        condition = "input.stochifyMethod == 'cross_stochify()'",
        helpText("cross_stochify() requires two images. Upload a second image!"),
        fileInput("image2","Upload another image")
      ),
      
      actionButton("generate", "Stochify!"), #,
      
     
      # conditionalPanel(
      #   # condition = ("input.stochifyMethod == 'cross_stochify()' || input.stochifyMethod == 'self_stochify()' || input.stochifyMethod == 'stochify()'"),
      #   # conditon = "output.fileUploaded",
      #   condition = "input.generate > 0",
      #   downloadButton(outputID = "down", "Download Plot")
      # ),
      tags$hr(),
      # downloadButton(outputId = "downloadPlot", label = "Download Plot")
      downloadButton("downloadPlot", "Download Plot", style = "display:none;")
    ),
    
    mainPanel( 
        fluidRow(
        p("Learn about the different ways of creating art from images using R."),
        p("The basic idea is that images are converted to grayscale matrices which are then multiplied by another matrix. The product is then converted into a plot and rendered."),
        p(span("stochify():", style="font-weight:bold"), "The output is a plot of the original image multiplied by a randomly generated matrix."),
        p(span("self_stochify():", style="font-weight:bold"), "The output is a plot of the original image multiplied by the transpose of its grayscale matrix generated matrix."),
        p(span("cross_stochify():", style="font-weight:bold"), "The output is a plot of the original image multiplied by a matrix from another image.", span("Note: Matrix sizes are truncated to be matrix multiplication compatible", style="font-style:italic")),
        # column(4,
        #        img(src = "1.jpeg")),
        # column(4,
        #       img(src = "2.jpeg")),
        # column(4,
        #        img(src = "3.jpeg")),
        tags$hr()
        
        # h4("Uploaded File Status:"),
        # verbatimTextOutput("image1_uploaded")
        
        # display the new image
        
        ),
        
        plotOutput("newPlot")
        # downloadButton("downloadPlot", "Download Plot")
        
    )
  )
)


server <- function(input, output) {
 
  data <- reactive({
    # Check if a file is uploaded
    req(input$image1)
    
    # Read the uploaded file (adjust file reading logic as per your file format)
    inFile <- input$image1
    img <- readJPEG(inFile$datapath)
    # Add more file reading processing if needed
    
    # Perform different plotting based on selected type
    plot_data <- switch(input$stochifyMethod,
                        "stochify()" = {
                          # Generate plot type 1
                          # Replace this section with your specific plot generation logic for Type 1
                          ggplot(data = NULL, aes()) + geom_point()
                        },
                        "self_stochify()" = {
                          # Generate plot type 2
                          # Replace this section with your specific plot generation logic for Type 2
                          ggplot(data = NULL, aes()) + geom_bar()
                        },
                        "cross_stochify()" = {
                          # Generate plot type 3
                          # Replace this section with your specific plot generation logic for Type 3
                          ggplot(data = NULL, aes()) + geom_line()
                        }
    )
    
    return(plot_data)
  })
  
  observeEvent(input$show_plot, {
    output$plot <- renderPlot({
      # Check if a file is uploaded and then render the plot
      plot_data <- data()
      plot_data
    })
    
    # Show the download button after the plot is generated
    output$download_plot <- renderUI({
      if (!is.null(data())) {
        tags$button(id = "downloadButton", "Download Plot")
      }
    })
  })
  
  output$downloadPlot <- downloadHandler(
    filename = function() {
      "plot.png"  # Change the filename extension to .png or .jpeg as needed
    },
    content = function(file) {
      # Save the plot as a PNG or JPEG file (adjust based on the plot object)
      ggsave(file, data(), device = "png")  # Change 'device' parameter accordingly
    }
  )

 
# ------
  # 
  # # Function to check if new plot should be calculated
  # generatePlot <- function() {
  #   # Perform checks
  #   # if (image1_uploaded == "FALSE") {
  #   #   output$plotError <- renderText({
  #   #     "Upload an Image!"
  #   #   })
  #   # }
  #   # else if (input$stochifyMethod == "cross_stochify()" && is.null(input$image2)) {
  #   #   output$newPlot <- renderText({
  #   #     "Upload a second image!"
  #   #   })
  #   # }
  #   # else {
  #     # Generate the new plot
  #     if (input$stochifyMethod == "stochify()") {
  #       
  #       # output$plotStatus <- renderText({
  #       #   "plot generated"
  #       # })
  #       
  #       output$newPlot <- renderPlot({
  #         stochify(input$image1$datapath, input$image1$name)
  #       }, width = 800, height = 800, res = 128)
  #     } else if (input$stochifyMethod == "self_stochify()") {
  #       
  #       # output$plotStatus <- renderText({
  #       #   "plot generated"
  #       # })
  #       
  #       output$newPlot <- renderPlot({
  #         self.stochify(input$image1$datapath, input$image1$name)
  #       }, width = 800, height = 800, res = 128)
  #     } else if (input$stochifyMethod == "cross_stochify()") {
  #       
  #       # output$plotStatus <- renderText({
  #       #   "plot generated"
  #       # })
  #       
  #       output$newPlot <- renderPlot({
  #         cross.stochify(input$image1$datapath, input$image2$datapath, input$image1$name, input$image2$name)
  #       }, width = 800, height = 800, res = 128)
  #     }
  #   # }
  # }
  #   
  # # Event Handler for action Button
  # observeEvent(input$generate, {
  #   generatePlot() # Call the function to perform checks and generate plot
  # })
  #   
  # 
  # 
  # 
  # ## download plot
  #   output$downloadPlot <- downloadHandler(
  #     filename = "shinyplot",
  #     content = function(file) {
  #       png(file)
  #       generatePlot()
  #       dev.off()
  #     },
  #     contentType = "image/png"
  #   )
  # 
  # output$downloadPlot <- downloadHandler(
  #   filename = function(){
  #     paste0("shinyplot","png",sep=".") },
  #   
  #   content = function(file) {
  #     png(file)
  #     generatePlot()
  #     dev.off()
  #   }) 
  
}

shinyApp(ui = ui, server = server)





# Server ------------------------------------------------------------------

# server <- function(input, output) {
#   observeEvent(input$file1, {
#     inFile <- input$file1
#     if (is.null(inFile))
#       return()
#     
#     # Display the uploaded image
#     output$image1_output <- renderUI({
#       tags$img(src = inFile$datapath, height = "150px")
#     })
#     output$image2_output <- renderUI({
#       tags$img(src = inFile$datapath, height = "150px")
#     })
#     output$image3_output <- renderUI({
#       tags$img(src = inFile$datapath, height = "150px")
#     })
#     
#     # Render captions
#     output$caption1 <- renderText({
#       "Caption for Image 1"
#     })
#     output$caption2 <- renderText({
#       "Caption for Image 2"
#     })
#     output$caption3 <- renderText({
#       "Caption for Image 3"
#     })
#   })
# }




# Run the app --------------------------------------------------------------
shinyApp(ui = ui, server = server)




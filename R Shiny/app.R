library(shiny)
setwd("~/Desktop/Data Science Major/stochify/R Shiny")
source("../stochify.R")
# library(parallel)

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
      downloadButton(outputId = "downloadPlot", label = "Download Plot")
      
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
        tags$hr(),
        
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
    
  # image1_uploaded <- reactive({
  #   if(is.null(input$image1)){
  #     return("FALSE")
  #   }
  #   else
  #   {
  #     return("TRUE")
  #   }
  # })
  # 
  
  ## Check if image is uploaded
  # getData <- reactive({
  #   if(is.null(input$image1)) return(NULL)
  #  
  # })
  # output$fileUploaded <- reactive({
  #   return(!is.null(getData()))
  # })
  # outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)

 
  
  
  # Function to check if new plot should be calculated
  generatePlot <- function() {
    # Perform checks
    # if (image1_uploaded == "FALSE") {
    #   output$plotError <- renderText({
    #     "Upload an Image!"
    #   })
    # }
    # else if (input$stochifyMethod == "cross_stochify()" && is.null(input$image2)) {
    #   output$newPlot <- renderText({
    #     "Upload a second image!"
    #   })
    # }
    # else {
      # Generate the new plot
      if (input$stochifyMethod == "stochify()") {
        
        # output$plotStatus <- renderText({
        #   "plot generated"
        # })
        
        output$newPlot <- renderPlot({
          stochify(input$image1$datapath, input$image1$name)
        }, width = 800, height = 800, res = 128)
      } else if (input$stochifyMethod == "self_stochify()") {
        
        # output$plotStatus <- renderText({
        #   "plot generated"
        # })
        
        output$newPlot <- renderPlot({
          self.stochify(input$image1$datapath, input$image1$name)
        }, width = 800, height = 800, res = 128)
      } else if (input$stochifyMethod == "cross_stochify()") {
        
        # output$plotStatus <- renderText({
        #   "plot generated"
        # })
        
        output$newPlot <- renderPlot({
          cross.stochify(input$image1$datapath, input$image2$datapath, input$image1$name, input$image2$name)
        }, width = 800, height = 800, res = 128)
      }
    # }
  }
    
  # Event Handler for action Button
  observeEvent(input$generate, {
    generatePlot() # Call the function to perform checks and generate plot
  })
    

  
  
  ## download plot
    output$downloadPlot <- downloadHandler(
      filename = "shinyplot",
      content = function(file) {
        png(file)
        generatePlot()
        dev.off()
      },
      contentType = "image/png"
    )
  
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




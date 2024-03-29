library(imager)

stochify <- function(path, shiny_name){
  #############################################################################
  # stochify() extracts grayscale values from any image into a matrix. It     #
  # performs matrix multiplication with another, randomly generated matrix of #
  # compatible size and creates a plot of the grayscale values of the         #
  # resulting matrix.                                                         #
  #                                                                           #
  # The title of the plot uses the name of the image file.                    #
  #############################################################################
  
  # read in an image
  img <- load.image(path)
  
  # convert the image to grayscale
  gray <- grayscale(img)
  
  # extract the grayscale values into a matrix
  gray_mat <-  as.matrix(gray)
  
  # create a random matrix of a size that is matrix multiplication compatible
  rand_mat <- matrix(runif(length(gray_mat)), nrow = ncol(gray_mat), ncol = nrow(gray_mat))
  
  # matrix multiplication
  stoch <- gray_mat %*% rand_mat
  
  # display the resulting image
  name <- gsub("\\..*", "", shiny_name)
  image(stoch,col=grey(seq(0, 1, length = 256)))
  title(main= substitute(paste("Stochastic ", italic(name))))
}

self.stochify <- function(path, shiny_name){
  ##############################################################################
  # self.stochify() extracts grayscale values from any image into a matrix. It #
  # performs matrix multiplication with its own transpose matrix and creates a #
  # plot of the grayscale values of the resulting matrix.                      #
  #                                                                            #
  # The title of the plot uses the name of the image file.                     #
  #                                                                            #
  ##############################################################################
  
  # read in an image
  img <- load.image(path)
  
  # convert the image to grayscale
  gray <- grayscale(img)
  
  # extract the grayscale values into a matrix
  gray_mat <-  as.matrix(gray)
  
  # matrix multiplication with self
  stoch <- gray_mat %*% t(gray_mat)
  
  # display the resulting image
  name <- gsub("\\..*", "", shiny_name)
  image(stoch,col=grey(seq(0, 1, length = 256)))
  title(main = substitute(paste('\u00A7', " ",italic(name)," ", '\u00A7' )))
}

cross.stochify <- function(path1, path2, shiny_name1, shiny_name2){
  ##############################################################################
  # cross.stochify() extracts grayscale values from any 2 images into 2        #
  # matrices. It checks if the "raw" matrices are compatible for matrix        #
  # multiplication and trims* them appropriately if necessary to multiply them #
  # together before creating a plot of the grayscale values of the resulting   #
  # matrix.                                                                    #
  #                                                                            #
  # The title of the plot uses the name of the image file.                     #
  #                                                                            #
  #   * Since this was for a project in an art class and not a STEM class, I   #
  #   did not spend too much time on the logic of how to reshape the matrices  #
  #   "elegantly" and minimize the amount trimmed values by optimizing which   #
  #   image would be trimmed, how it would be trimmed (rows or columns         #
  #   depending on orientation of the image), and whether or not the needed    #
  #   trimming would/could be split between the two images.                    #
  ##############################################################################
  
  # read in images
  img1 <- load.image(path1)
  img2 <- load.image(path2)
  
  # convert to grayscale
  gray1 <- grayscale(img1)
  gray2 <- grayscale(img2)
  
  # extract grayscale values into 2 matrices
  A <-  as.matrix(gray1)
  B <-  as.matrix(gray2)
  
  # get dimensions of each matrix
  #       A         B
  #    |         |
  #   a|        c|
  #    |_____    |_____
  #       b         d
  
  a <- dim(A)[1]
  b <- dim(A)[2]
  c <- dim(B)[1]
  d <- dim(B)[2]
  
  # Check if "raw" matrices are able to be multiplied
  if(b == c){ stoch <-  A %*% B }
  if(a == d){ stoch <-  B %*% A }
  
  # if "raw" matrices CANNOT be multiplied, then trim
  if(b > c){  # A is too wide
    A = A[,1:c]  # trim columns of A to match the number of rows in B
    stoch <- A %*% B 
  } else { # B is too tall
    B = B[1:b,] # trim rows of B to match the number of columns in A
    stoch <- A %*% B } 
  
  # display the resulting image
  name1 <- gsub("\\..*", "", shiny_name1)
  name2 <- gsub("\\..*", "", shiny_name2)
  image(stoch,col=grey(seq(0, 1, length = 256)))
  title(main = substitute(paste( italic(name1)," ", '\u00A7', " ", italic(name2))))
}

# Code  to output the .Rmd file to a file called README.md 
rmarkdown::render("C:/Users/dolan/Desktop/NC State Grad Program/ST558/Project 2/ST558_Project2/Project 2.Rmd",
                  output_format = "github_document",
                  output_file = "C:/Users/dolan/Desktop/NC State Grad Program/ST558/Project 2/ST558_Project2/README.md",
                  output_options = list(
                  html_preview=FALSE))
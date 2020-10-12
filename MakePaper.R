library(here)
library(R.utils)
library(rmarkdown)
# If you have obtained raw data from me, you can uncomment this in order to 
# create the sample yourself
# source(here("Build", "Code", "CreateSample.R")) 

# Output of Build is Input for Analysis, creating a symbolic link 
createLink(here("Analysis", "Input"), here("Build", "Output"), skip = T)

# uncomment the line below if pdf_document compilation fails, but
# you would like to compile the latex file yourself. It will create
# a symlink to the bibliography in the folder of Paper.tex
createLink(here("Analysis", "Output", "reviewsBib.bib"), 
           here("Analysis", "Code", "reviewsBib.bib"), skip = T)

# Create the Paper. Choose "latex_document" if there are issues with compiling
# the final PDF, or choose "html_document", but some latex commands won't work
# properly

rmarkdown::render(here("Analysis", "Code", "Sorokin_JMP_2020.Rmd"),
                  output_dir = here("Analysis", "Output"),
                  output_format = "pdf_document")
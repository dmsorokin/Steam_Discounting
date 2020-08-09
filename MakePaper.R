# Load libraries 
library(here)
library(devtools) # for installing packages through GitHub, needed for starpolishr
library(data.table)
library(stringr)
library(stringi) # that is wasteful, but it happened
library(anytime)
library(dplyr)
library(magrittr)
library(fastDummies)
library(kableExtra)
library(ggplot2)
library(wesanderson)
library(ggthemes)
library(ggridges)
library(ggpubr)
library(sandwich)
library(lmtest)
library(plm)
library(stargazer)
library(starpolishr)
library(texreg)
library(R.utils)
library(rmarkdown)

# If you have obtained raw data from me, you can uncomment this in order to 
# create the sample yourself.

# source(here("Build", "Code", "CreateSample.R")) 

# Output of Build is Input for Analysis, creating a symbolic link 
createLink(here("Analysis", "Input"), here("Build", "Output"), skip = T)

# uncomment the line below if pdf_document compilation fails, but
# you would like to compile the latex file yourself. It will create
# a symlink to the bibliography in the folder of Paper.tex
createLink(here("Analysis", "Output", "reviewsBib.bib"), 
           here("Analysis", "Code", "reviewsBib.bib"), skip = T)

# Create the Paper
render(here("Analysis","Code", "Paper.Rmd"), 
       output_dir = here("Analysis", "Output"),
       output_format = "latex_document"
       )



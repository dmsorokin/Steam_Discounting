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

knitr::opts_chunk$set(echo=F,eval=T,message=F,warning=F,comment=NA,cache=T)
knitr::opts_chunk$set(fig.pos = "h", out.extra = "", fig.align = "center")

# If you have obtained raw data from me, you can uncomment this in order to 
# create the sample yourself.

# source(here("Build", "Code", "CreateSample.R")) 

# Output of Build is Input for Analysis, creating a symbolic link 
createLink(here("Analysis", "Input"), here("Build", "Output"), skip = T)

# Create the Paper
render(here("Analysis","Code", "Paper.Rmd"), output_dir = here("Analysis", "Output"))
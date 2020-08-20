# Analysis/Code 

This directory contains the code used in the analysis. `Paper.Rmd` is the R-Markdown file that contains both the text of the paper and the analysis. `AnalysisFunctions.R` is the file where all the functions used by `Paper.Rmd` are stored. `extraHeader.tex` contains extra LaTeX commands that are excluded from the YAML-header for convenience.

`NLLS.jl` is the Julia script that uses functions defined in `estimation_funcs.jl` to carry out the estimation of 
the demand model in the paper. I ran the code for you, and the results used in `Paper.Rmd` are already stored in the repository in **Analysis**/**Temp**. However, if you would like to play around with this code, you can do it: just run the `NLLS.jl` file in Julia. `Manifest.toml` and `Project.toml` are Julia package management files that contain all the information about the dependencies that I used, and should ensure a successful replication.
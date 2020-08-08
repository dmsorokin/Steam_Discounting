# Steam_Discounts

## Description
This repository contains replication files for 

**Sorokin, Dmitry, "Consumer Reviews and Product Discounts: Evidence From Video Games", 2020**

The **Build** folder takes processed raw data from the **Input** subfolder, uses scripts in the **Code** subfolder to create the sample for the analysis, and places the final data files to the **Output** subfolder. Intermediate files are placed to **Build**/**Temp**, and are deleted upon the termination of the code. The raw data is too big to upload on GitHub, but is available upon request.

The **Analysis** folder has a similar structure. **Analysis**/**Input** is a symbolic link to **Build**/**Output**, and will be created automatically when you assemble the paper (more on this later). **Code** contains the R Markdown file with the entire paper written in it, as well as the functions used in the analysis.

``MakePaper.R`` is the file that renders the paper files in **Analysis**/**Code** and produces the PDF and the TEX versions of the paper in Analysis/Output.

## Usage

The key to a successful replication lies in the correct usage of `packrat`, a dependency management system for R. The easiest way to replicate this project is through RStudio, using the following steps

1. Open RStudio, and choose File/New Project. 
2. Select “Using Version Control”, then “Git”, and paste the link to this repository: https://github.com/dmsorokin/Steam_Discounting 

RStudio will download all the files, including the `packrat` source files, which contain information about the versions of packages that I used in the analysis. The console in RStudio should notify you that the `packrat` has been successfully attached.

3. Run `packrat::restore()`.

This is a critical step, when R will try to install all the packages. Some will, probably, terminate with a mistake. You would have to install those pacakges manually. One such package would probably be `starpolishr`. As the [GitHub page](https://github.com/ChandlerLutz/starpolishr) of this package explains, installation has to be carried out using the `devtools` library:
```
# install.packages("devtools")
devtools::install_github("ChandlerLutz/starpolishr")
```

4. Once all the libraries are loaded, go ahead and source MakePaper.R. 

By default, this will reproduce the paper in an PDF format. Successful compilation requires the user to have LaTeX correctly installed in the system. By replacing `output_format = "pdf_document"`  with `output_format = "html_document"` in the body of the script, you can reproduce the paper in an HTML format, but some inline elements and formulas might not be displayed correctly. However, you should be able to see the all the empirical results.

All the analysis is done in Analysis/Code/Paper.Rmd. Change the code there, and run MakePaper.R again to see how your changes affect the results!

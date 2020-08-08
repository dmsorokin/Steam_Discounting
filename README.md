# Steam_Discounts

## Description
This repository contains replication files for 

**Sorokin, Dmitry, "Consumer Reviews and Product Discounts: Evidence From Video Games", 2020**

The Build folder takes processed raw data from the Input subfolder, uses scripts in the Code subfolder to create the sample for the analysis, and places the final data files to the Output subfolder. Intermediate files are placed to /Build/Temp, and are deleted upon the termination of the code. The raw data is too big to upload on GitHub, but is available upon request.

The Analysis folder has a similar structure: Input, Code, Output, Temp. Analysis/Input is a symbolic link to Build/Output, and will be created automatically when you assemble the paper (more on this later). Code contains the R Markdown file with the entire paper written in it, as well as the functions used in the analysis.

MakePaper.R is the file that renders the paper files in Analysis/Code and produces the PDF and the TEX versions of the paper in Analysis/Output.



# Build/Code

The scripts in this folder take processed raw data (a step that is omitted from this repository) and apply filters to generate the data used in the paper.

BuildFunctions.R stores the functions that are used. CreateSample.R calls BuildFunctions.R  and BrushUpCleanData.R, and then finalizes the sample. 

The resulting 3 files are stored in Build/Output: panel.Rda is the final panel used in the analysis, info.Rda is a data table that stores descriptive information about each game in the sample, and juliaData.csv that has a processed subset of panel.Rda for the non-linear least squares estimation carried out in Julia.
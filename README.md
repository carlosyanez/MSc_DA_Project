# MSc's Project

Repository containing MSc's Project work, including data and code used in analysis and documentation.
This repository does not include the packages developed to access and modify data:

* [auspol](https://github.com/carlosyanez/auspol), for election results, data extracted from AEC's Tally Room.
* [auscensus](https://github.com/carlosyanez/auscensus), used to extract data from the ABS's Census DataPacks
* [ausssiemaps](https://github.com/carlosyanez/aussiemaps), for spatial representation and aggregation of statistical areas in Commonwealth Electoral Divisions (CED) 

## Repo Structure

| Directory | Purpose |
|-----------|---------|
|0. Notes |Initial Notes|
|1. Proposal |Document with original proposal|
|2. Meetings|Meeting notes|
|3. Data Extraction| Code used to extract data|
|4. Data| Data used in the project|
|5. EDA|Exploratory Data Analysis|
|6. Modelling| Modelling|
|7. Forecast| Forecast of 2021 Election|
|8. Report| Project's report|
|9. Presentation| Viva slides (**created after submission of Report**) |

Each folder contains its own README.md file.
Additionally, the file *0_env_setup.R* contains the code to install all required packages.

## IMPORTANT NOTE

Please note that:

- All the code in this repository has been created using relative paths. Please run all code in folder 0-7 in RStudio using the MSc_Project.Rproj project file.
- The same code was also created in a renv R environment. It can be recreated from scratch using *0_env_setup.R* or restored using *renv.lock*
- The above does not apply to the 8. Report or 9. Presentation, who have their own R project file.

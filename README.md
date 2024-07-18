# airbnb-crime_criminology_2024

This repository contains all code used in the analyses for the following article:

CC Lanfear, DS Kirk. 2024. The Promise and Perils of the Sharing Economy: The Impact of Airbnb Lettings on Crime. *Criminology* 62(4)

[This study was preregistered on Open Science Framework (doi: 10.17605/OSF.IO/D93TP).](https://doi.org/10.17605/OSF.IO/D93TP) 


Repository contents:

* data/ - Data files
   + raw/ - Raw input files without access restrictions (but typically too large for a repo); files omitted from the repository due to size are marked in **bold** with links to their sources.
      + london_lsoa_zone_1.RData: A manually-generated Transit Zone 1 index
      + **London-wards-2018_ESRI** - This file is no longer available online but can be substituted with any 2018 Ward shapefile, including the one in "statistical-gis-boundaries-london" below.
      + **Lower_Layer_Super_Output_Areas__December_2001__EW_BFE-shp** - [https://geoportal.statistics.gov.uk/datasets/ons::lower-layer-super-output-areas-december-2001-boundaries-ew-bfc/about]() 
      + **Lower_Layer_Super_Output_Areas__December_2011__Boundaries_Full_Extent__BFE__EW_V3-shp** - [https://geoportal.statistics.gov.uk/datasets/3011969ff4e84966b2cbc3b642ae32de/about]()
      + **OS VectorMap District (ESRI Shape File) TQ** - [https://www.os.uk/business-and-government/products/vectormap-district.html]()
      + **statistical-gis-boundaries-london** - [https://data.london.gov.uk/dataset/statistical-gis-boundary-files-london]()
   + analytical/: Final analysis data files necessary for running models
* syntax/ - R syntax files
   + 00_reproduce_project.R - If you (somehow) have all the raw data, you can run this script to reproduce the entire project.
   + 01_processing/ - Assemble analysis data
   + 02_models/ - Produce all estimates in main text and appendix
   + 03_descriptive/ - Produce all tables and in-text values
   + 04_figures/ - Produce the figures in main text and appendix
   + 05_sensitivity/ - All sensitivity checks mentioned in main text or requested by reviewers
   + project_functions.R - Functions used throughout the project
* docs/ 
   + airbnb_crime.Rmd - The RMarkdown file for the resubmission after first review; subsequent revisions were made in Word and are thus not reflected in this doc.
   + airbnb_crime_appendix.Rmd - The appendix to the article prior to final revisions
   + figures/ - The final submitted figures for the paper (more recent than drafts)
* NOTE: The following files are not included in this repository and are expected in external directories, either due to size (crime data) or access restrictions (everything else):
     + [AirDNA data from the CDRC](https://data.cdrc.ac.uk/dataset/airbnb-property-rentals-and-reviews-supplied-airdna), specifically:
        + "safeguarded.airdna.Property_edited.csv" 
        + "safeguarded.airdna.Monthly.csv"
     + "tranall2011_19.csv" - [Chi et al. (2020)](https://doi.org/10.5255/UKDA-SN-854240)
     + ["DLG" crime data from data.london.gov.uk](https://data.london.gov.uk/dataset/recorded_crime_summary-london-gov-uk)
     + ["DP" crime data from data.police.uk](https://data.police.uk/data/)
     + MOPAC Public Attitudes Survey which has [info but not raw data here](https://data.london.gov.uk/dataset/mopac-surveys); you'll need to get it from MOPAC
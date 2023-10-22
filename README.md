# family-hubs-database
Database of London local authority characteristics and outcomes

Written by Matthew Jay
matthew.jay@ucl.ac.uk

Research Fellow
University College London Great Ormond Street Institute of Child Health

on secondment to the Office for Health Improvement and Disparities (OHID)

22 October 2023


---------------------------
Introduction

This collection of files includes R code for creating the contextual database used in an evaluation of London Family Hubs alongside conslutation with key stakeholders in a number of local authorities. These files are a companion to the research report which will be made available on-line (link to be provided once published).

The folder "final_database" includes the final database itself and a data dictionary, which includes links to all the sources used to create the database. Users who just wish to consult the dastabase need only use these files. Those who wish to recreate or modify the database will need to consult the R code and "raw" data folder.

The folder "out" contains the results from the analysis as presented in the main report.


---------------------------
Contents

- Folder "final_database"
Contains the final database and data dictionary. The database consists of two files: contextual_database.csv and fingertips_data_london.csv. The former is a wide-format (one row per local authority) dataset containing all information other than that derived from Fingertips. The latters is long-format (multiple rows per local authority) containing the subset of Fingertips data used in the research project.

- Folder "r_scripts"
  1. get_data.R - this script loads in and merges all data sources, including those downloaded and stored in the "raw" folder and directly from Fingertips using the fingertipsR package.
  2. AllLAs.Rmd - an RMarkDown script that produces the output in the "out" folder.

- Folder "out"
Contains the results presented in the main report.

- Folder "raw"
Contains a selection of raw data files:

  - cin_in_need_31.csv - numbers of children in need on 31 March in each local authority in 2020, 2021 and 2022 (derived from the CiN census)
  - cin_referrals.csv - numbers of children referred to children's social care in each local authority in 2020, 2021 and 2022 (derived from the CiN census)
  - cla_data.csv - numbers of children starting to be looked after in each financial year and the number of children looked after on 31 March in each local authority in 2020, 2021 and 2022 (derived from the CLA/SSDA903 return)
  - fsm_time_series.csv - number and percentage of children getting free school meals (from Department for Education data)
  - imd2019.csv - English indices of deprivation summaries for each London local authority
  - MYEB1_detailed_population_estimates_series_UK_(2020_geog21).csv - ONS mid-year population estimates
  - ofsted.csv - Ofsted social care ratings for the London local authorities
  - pop_eth.csv - ONS population estimates by white and ethnic minority (from 2021 Census)
  - pop2021.csv - ONS mid-year population estimates for 2021
  - sen_phase_type.csv - number and percentage of pupils with SEN per local authority (from Department for Education data)
  - spine.csv - a spine of London local authorities and their ONS codes, used as the basis of the database
  - web_searches_data.csv - results from our own searches of local authority websites for information on children's centres and family hubs

Replication material: *"On climate and conflict: precipitation decline and communal conflict in Ethiopia and Kenya"*    
Submitted: 2017.06.21  
Accepted: 2018.12.03    
URL: https://journals.sagepub.com/doi/full/10.1177/0022343319826409    

Data and code to replicate results as reported in the paper. 
Data processing and analysis were carried out in R.
For issues replicating the results contact me via email: vanweezel (at) pm.me.

#### Description
Following folders are included: 
1. `code`
2. `data`
3. `output`
4. `raw-data`


`code` contains all the code used for analysing the data and producing the various tables and figures in the paper. 
There are three sub-folders:

1. `estimation`
2. `revision`
3. `tables-figures` 

The `estimation` folder contains all the code for the statistical analysis

* `load.R` is run first and compiles the data
* `models.R` contains all the models and is called by `load.R`
* `fit_models.R` is the main estimation code, producing the results as reported in table 1.
* `cross_validation.R` executes the cross validation exercise
* `BEST.R` conducts the Bayesian t-test which is included in the main dataset and used in the regressions 
 
The code in folder `tables-figures` can be used to create the tables and figures in the paper and appendix.
Note that figure 9 is created by `cross-section-time-series.R`; A2 by `BEST.R`; and A4 by `analysis-rastert.R`.<br> 

A number of additional tests were carried out based on various suggestions made by the reviewers. 
Specifically the reviewers asked for 

1. Aggregating the data using a raster
2. Using different conflict data
3. Account for local wealth levels
4. Account for temperature

The code for these tests is included in `revision.R`
For the raster analysis you need to run the scripts in the following order

1. `fit-raster.R`
2. `analysis-raster.R`

To create figure 5 in the paper, using `figure5.R`, you will need to run all the robustness checks first and save the results in the `output` folder. 
The same applies to the code in the `estimation` folder. 

`data` contains the processed data that was used in the analysis, saved in RData format. 
Due to the size of the raw data I left out most of it, therefore only providing the processed data. 
If you want to carry out a replication starting with the raw data, feel free to contact me. 
The `data` folder contains the following data files that were used for the regression analysis

* `lzones.RData`, information on livelihood zones
* `ged.RData`, processed data with information on communal violence from the UCDP Georeferenced Event Dataset
* `rainfall.RData`, aggregated precipitation data from CenTrends
* `epr_d.RData`, contains information on the presence of an ethnically excluded group at district level
* `pop_dens.RData`, district population density based on the GRUMP dataset

These files are needed in conjunction with the `load.R` script.<br> 

The section on Markov transitions in the paper is based on the following data files

* `CenTrend.RData`, is the processed CenTrend data
* `communal_violence.RData`, contains information on communal violence 

Finally there are a number of data files included based on requests made by the reviewers (see also discussion on `rr`), these are

1. `acled.RData`, which contains district-aggregated conflict data from ACLED
2. `night_lights.RData` containing data on night light emissions to proxy for local wealth levels
3. `temperature.RData` containing information on temperature levels
4. `raster_lhz.RData`, `raster_population.RData`, `raster_rain.RData`, `raster_ucdp.RData` is data aggregated at grid level  

`output` includes just one data file
* `ttest.RData` 

which is needed for `load.R` to compile the data and run the regressions as well as creating figure A3 using `BEST.R`.  

`raw-data` includes some of the original unprocessed data that are required to produce some of the figures
* `ged50.RData` is the UCDP Georeferenced Conflict Event dataset (version 5.0)
* `gaulk_adm2.RData` are the GAUL district boundaries (reference year 1999), including districts in neighbouring countries, saved as RData file

The data required for figure 1 can be downloaded from the [CenTrends](https://www.chc.ucsb.edu/data) website. 
I didn't include here because of the size of the file (~80Mb)

#### BibTeX
```
@article{van2019weezel,
  title={On climate and conflict: Precipitation decline and communal conflict in Ethiopia and Kenya},
  author={van Weezel, Stijn},
  journal={Journal of Peace Research},
  pages={0022343319826409},
  year={2019},
  publisher={SAGE Publications Sage UK: London, England}
}
```
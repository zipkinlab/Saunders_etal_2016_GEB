# [Evaluating confidence in climate-based predictions of population change in a migratory species](http://onlinelibrary.wiley.com/doi/10.1111/geb.12461/full)

### Sarah P. Saunders, Leslie Ries, Karen S. Oberhauser, and Elise F. Zipkin

### Global Ecology and Biogeography

### Please contact the first author for questions about the code or data: Sarah P. Saunders (sarahpsaunders@gmail.com)
__________________________________________________________________________________________________________________________________________

## Abstract
**Aim** Forecasting ecological responses to climate change is a common objective, but there are few methods for evaluating confidence in such predictions. For migratory species, in particular, it is also essential to consider the extent of spatial synchrony among separate breeding populations in range-wide predictions. We develop a quantitative method to evaluate the accuracy of climate-based ecological predictions and use this approach to assess the extent of spatio-temporal synchrony among distinct regions within the breeding range
of a single migratory population.  
**Location** We model weekly site-specific summer abundances (1996–2011) of monarch butterflies (Danaus plexippus) in the Midwestern USA as a function of climate conditions experienced during a shared spring migration/breeding phase in Texas and separate summer recruitment periods in Ohio and Illinois.  
**Methods** Using negative binomial regression models, we evaluate spatiotemporal synchrony between monarchs in the two states and develop a novel quantitative assessment approach to determine the temporal predictive strength of our model with Bayesian P-values.  
**Results** Monarchs breeding in the Midwest exhibit spatio-temporal synchrony in Ohio and Illinois; cooler spring temperatures, average to above average precipitation in Texas and cooler than average summer temperatures are associated with higher population abundances in both states. At least 10 years of data are needed for adequate model predictability of average future counts. Because annual spring weather conditions in Texas primarily drive yearly abundances, as opposed to localized summer effects, year-specific counts are often difficult to predict reliably, specifically when predictive spring conditions are outside the range of typical regional conditions.  
**Main conclusions** Our assessment method can be used in similar analyses to more confidently interpret ecological responses to climate change. Our results demonstrate the relative importance of climatic drivers in predicting abundances of a migratory species and the difficulties in producing reliable predictions of animal populations in the face of climate change.  

## Data (proprietary; descriptions provided here for context)
SiteEffects_OH96to11.csv - Site-level data for sites where monarchs were surveyed in Ohio and Illinois. The rows contain the data for each of the 117 sites in the study area. The columns contain 1) Latitudes and Longitudes of each site (first 2 columns); 2) Site IDs; 3) the percent of each site considered 'open' habitat (% Open column; see main text for description of this covariate; and 4) average growing degree days (GDD) measured during weeks 10-28 (avgGDD10-28NEW) at each site.

SiteYear_OH96to11.csv - Site-year-level data for each site-year combination. Each row is a site-year combination (columns 1 and 2) and the additional columns contain: 1) accumulated GDD during weeks 10-28 in each year at each site; and 2) the average Palmer Drought Severity Index (PDSI) during weeks 10-28 during each year at each site.

SurveyData_OH96to11.csv - This file contains all survey-specific information (i.e. for each observation). This file is the raw data that were later manipulated to yield the sum total of monarchs observed at each site during a given survey (i.e. column SumofMonarchCount).

YearEffects_OH96to11.csv - Each row of this datasheet is a year of the study period (indicated in column 1). The remaining columns are: 1) accumulated GDD during spring in weeks 4-9 (second column); 2) average PDSI during weeks 4-9; and 3) total average spring precipitation in Texas.

weeks_OH96to11.csv - This file was used in data manipulation to note which sites were surveyed each week in each year.

## Code
[GEB 2016_Git code_SaundersSP.R](https://github.com/zipkinlab/Saunders_etal_2016_GEB/blob/master/GEB%202016_Git%20code_SaundersSP.R) - R code to manipulate data and fit the models described in the main text of the published paper, including evaluation of goodness of fit and creation of whiskerplots.

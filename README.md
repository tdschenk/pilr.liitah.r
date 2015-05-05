# pilr.liitah.r

## Installing the package

```` R
library(devtools)
install_github("tdschenk/pilr.liitah.r")
library(pilr.liitah.r)
````

## Set your options

```` R
options(pilr_server_default = "http://liitah.pilrhealth.com")
options(pilr_project_default = "liitah_testing_2")
options(pilr_default_access_code = "<access_code>")
````

## Functions

- Example function calls can also be found in tests/testing.r  
- All functions can be given one or multiple participant values to analyze data from.  
- Timestamps are all taken from local_time metadata

**full_summary:** Returns a large dataframe of summary variables with one row per participant
```` R
full <- full_summary(c(601,602,603,604,605))
full_filtered <- full_summary(c(601,602,603,604,605), filterStart = '2015-05-05T01:00:01Z',
                              filterEnd = '2015-05-06T23:59:59Z'))
````

**arrival_diff_inst:** Returns a dataframe with timestamp, venue, and trigger difference for each arrival instance    
Positive = Trigger delivered before arrival  
Negative = Trigger delivered after arrival  
NA = No trigger found  
```` R
arrival_inst <- arrival_diff_inst(c(601,609,610))
arrival_inst_filtered <- arrival_diff_inst(c(601,609,610), filterStart = '2015-05-05T01:00:01Z',
                                           filterEnd = '2015-05-06T23:59:59Z')
````

**arrival_diff_avg:** Returns a dataframe with average trigger difference for each venue  
Positive = Trigger delivered before arrival  
Negative = Trigger delivered after arrival  
NA = No trigger found  
```` R
arrival_avg <- arrival_diff_avg(c(601,609,610))
arrival_avg_filtered <- arrival_diff_avg(c(601,609,610), filterStart = '2015-05-05T01:00:01Z',
                                         filterEnd = '2015-05-06T23:59:59Z')
````

**arrival_summary:** Returns a dataframe with each at_venue instance and what category was polled prior to arrival  
```` R
arrivals <- arrival_summary(c(601,609,610))
arrivals_filtered <- arrival_summary(601, filterStart = '2015-05-05T01:00:01Z',
                                     filterEnd = '2015-05-06T23:59:59Z')
````

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

## Full summary example

```` R
example <- full_summary(c(407,409,410))
````

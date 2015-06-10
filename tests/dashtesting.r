### SETUP ###
# Load packages
library(pilr.api.r)

# Grab data
options(pilr_server_default = "http://liitah.pilrhealth.com")
options(pilr_project_default = "liitah_testing_2")
options(pilr_default_access_code = "429913fe-472f-4140-919e-40241d76ed99")
data <- read_pilr(data_set = "pilrhealth:mobile:app_log", schema = 1, query_params = list(participant = "409"))
params <- ""

### DASHBOARDS ###
# Polls per day
polldash <- polls_per_day(data, params)

# Triggers per day
triggerdash <- triggers_per_day(data, params)

# HTML Table
tabledash <- full_table(data, params)

# View dashboards
polldash
triggerdash
tabledash

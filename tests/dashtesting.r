### SETUP ###
# Load packages
library(pilr.api.r)

# Grab data
options(pilr_server_default = "http://liitah.pilrhealth.com")
options(pilr_project_default = "liitah_testing_2")
options(pilr_default_access_code = "429913fe-472f-4140-919e-40241d76ed99")
data <- list(log = read_pilr(data_set = "pilrhealth:mobile:app_log", schema = 1,
                             query_params = list(participant = "409")),
             venue = read_pilr(data_set = "pilrhealth:liitah:personal_venue", schema = 1,
                               query_params = list(participant = "409")))
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

triggers <- data[data$tag == 'ARRIVAL_TRIGGER',]
triggers$day <- substr(triggers$local_time, 0, 10)
triggers %>%
  ggvis(x = ~day, fill := "#663300") %>%
  layer_bars() %>%
  add_axis("x", title = "",
           properties = axis_props(labels = list(angle = 45, align = "left"))) %>%
  add_axis("y", title = "Triggers") %>%
  add_tooltip(function(x){
    x <- subset(x, select = -stack_lwr_)
    x <- rename(x, Date = x_)
    x <- rename(x, Total = stack_upr_)
    paste0(names(x), ": ", format(x), collapse = "<br />")
    }, "hover")

all_values <- function(x) {
  if(is.null(x)) return(NULL)
  x <- subset(x, select = -stack_lwr_)
  x <- rename(x, Date = x_)
  x <- rename(x, Total = stack_upr_)
  paste0(names(x), ": ", format(x), collapse = "<br />")
}

all_values <- function(x) {
  if(is.null(x)) return(NULL)
  paste0(names(x), ": ", format(x), collapse = "<br />")
}

mtc <- mtcars
mtc$id <- 1:nrow(mtc)

all_values <- function(x) {
  if(is.null(x)) return(NULL)
  row <- mtc[mtc$id == x$id, ]
  paste0(names(row), ": ", format(row), collapse = "<br />")
}

mtc %>% ggvis(x = ~wt, y = ~mpg, key := ~id) %>%
  layer_points() %>%
  add_tooltip(all_values, "hover")

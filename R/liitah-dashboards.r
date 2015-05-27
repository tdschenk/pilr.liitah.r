## Summary data table generated in HTML format
## (?) How to handle multiple participants (?)
#' @export
full_table <- function(data, params, ...) {
  log <- data
  
  ## BASIC SUMMARY
  log$local_time = log$local_time %>% as.POSIXlt(format = "%Y-%m-%dT%H:%M:%SZ")
  log = log[log$local_time > filterStart & log$local_time < filterEnd, ]
  venues = venues[venues$local_time > filterStart & venues$local_time < filterEnd, ]
  filterStart = filterStart %>% as.POSIXlt(format = "%Y-%m-%dT%H:%M:%SZ")
  filterEnd = filterEnd %>% as.POSIXlt(format = "%Y-%m-%dT%H:%M:%SZ")
  log$local_time = log$local_time %>% as.POSIXlt(format = "%Y-%m-%dT%H:%M:%SZ")
  log = log[log$local_time > filterStart & log$local_time < filterEnd, ]
  if (nrow(venues) != 0) venues$local_time = venues$local_time %>% as.POSIXlt(format = "%Y-%m-%dT%H:%M:%SZ")
  # ==== Summarize data ====
  # Query data for some summary measures
  polls = log[log$tag == "POLLING_SERVICE_ANDROID", ]
  polls_at_location = polls[polls$args.category == "at_venue", ]
  triggers = log[log$tag == 'ARRIVAL_TRIGGER',]
  # This line for MANUAL_ARRIVAL
  training_recs = log[log$tag == 'MANUAL_ARRIVAL',]
  
  # Table of the summary measures
  temp1 <- data.frame(#pt = paste0(pt[i]),
    Total_Polls = nrow(polls),
    Total_Triggers = nrow(triggers), Polls_at_Venue = nrow(polls_at_location),
    Hot_Polls = polls[polls$args.category == "hot", ] %>% nrow(),
    Warm_Polls = polls[polls$args.category == "warm", ] %>% nrow(),
    Cold_Polls = polls[polls$args.category == "cold", ] %>% nrow(),
    Last_Manual_Arrival_Log = max(training_recs$local_time) %>% as.character(),
    Total_Manual_Arrival_Logs = nrow(training_recs),
    Last_Poll = max(log$local_time) %>% as.character(),
    First_Poll = min(log$local_time) %>% as.character())
  htmlTable(temp1)
}

## Bar graph of hot/warm/cold polls per day for one participant
#' @export
polls_per_day <- function(data, params, ...) {
  polls <- data[data$tag == "POLLING_SERVICE_ANDROID", ]
  polls$day <- substr(polls$local_time, 0, 10)
  polls <- polls[polls$args.category != "", ]
  days <- unique(polls$day)
  
  # Count polls per day
  summary <- data.frame(day = character(), category = character(), count = numeric())
  for (i in 1:length(days)) {
    polls_sub <- polls[polls$day == days[i], ]
    cold <- nrow(polls_sub[polls_sub$args.category == "cold",])
    warm <- nrow(polls_sub[polls_sub$args.category == "warm",])
    hot <- nrow(polls_sub[polls_sub$args.category == "hot",])
    at_venue <- nrow(polls_sub[polls_sub$args.category == "at_venue",])
    summary <- rbind(summary, data.frame(day = days[i], category = "cold", count = cold))
    summary <- rbind(summary, data.frame(day = days[i], category = "warm", count = warm))
    summary <- rbind(summary, data.frame(day = days[i], category = "hot", count = hot))
    summary <- rbind(summary, data.frame(day = days[i], category = "at_venue", count = at_venue))
  }
  
  summary %>%
    ggvis(x = ~day, y = ~count, fill = ~category) %>%
    layer_bars() %>%
    add_axis("x", title = "",
             properties = axis_props(labels = list(angle = 45, align = "left"))) %>%
    add_axis("y", title = "Polls")
}

## Bar graph of total triggers per day
#' @export
triggers_per_day <- function(data, params, ...) {
  data %>%
    ggvis(x = ~args.category) %>%
    layer_bars()
}

#triggers <- data[data$tag == 'ARRIVAL_TRIGGER',]
#triggers$day <- substr(triggers$local_time, 0, 10)
#triggers %>%
#  ggvis(x = ~day) %>%
#  layer_bars() %>%
#  add_axis("y", title = "triggers")
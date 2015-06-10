## Summary data table generated in HTML format
## (?) How to handle multiple participants (?)
#' @export
full_table <- function(data, params, ...) {
  log <- data
  
  ## BASIC SUMMARY
  #log$local_time = log$local_time %>% as.POSIXlt(format = "%Y-%m-%dT%H:%M:%SZ")
  #log = log[log$local_time > filterStart & log$local_time < filterEnd, ]
  #venues = venues[venues$local_time > filterStart & venues$local_time < filterEnd, ]
  #filterStart = filterStart %>% as.POSIXlt(format = "%Y-%m-%dT%H:%M:%SZ")
  #filterEnd = filterEnd %>% as.POSIXlt(format = "%Y-%m-%dT%H:%M:%SZ")
  log$local_time = log$local_time %>% as.POSIXlt(format = "%Y-%m-%dT%H:%M:%SZ")
  #log = log[log$local_time > filterStart & log$local_time < filterEnd, ]
  #if (nrow(venues) != 0) venues$local_time = venues$local_time %>% as.POSIXlt(format = "%Y-%m-%dT%H:%M:%SZ")
  # ==== Summarize data ====
  # Query data for some summary measures
  polls = log[log$tag == "POLLING_SERVICE_ANDROID", ]
  polls_at_location = polls[polls$args.category == "at_venue", ]
  triggers = log[log$tag == 'ARRIVAL_TRIGGER',]
  # This line for MANUAL_ARRIVAL
  training_recs = log[log$tag == 'MANUAL_ARRIVAL',]
  
  # Create Matrix
  mx <- matrix(nrow = 1, ncol = 11)
  rownames(mx) <- c(paste0(data$pt[1]))
  colnames(mx) <- c("Total_Triggers", "Last_Trigger", "Polls_at_Venue",
                    "Hot_Polls", "Warm_Polls", "Cold_Polls", "Total_Polls",
                    "First_Poll", "Last_Poll", "Last_Manual_Arrival_log",
                    "Total_Manual_Arrival_Logs")
  mx[1,1] <- nrow(triggers)
  mx[1,2] <- max(triggers$local_time) %>% as.POSIXlt(format = "%Y-%m-%dT%H:%M:%S") %>% as.character()
  mx[1,3] <-  nrow(polls_at_location)
  mx[1,4] <- polls[polls$args.category == "hot", ] %>% nrow()
  mx[1,5] <- polls[polls$args.category == "warm", ] %>% nrow()
  mx[1,6] <- polls[polls$args.category == "cold", ] %>% nrow()
  mx[1,7] <- nrow(polls)
  mx[1,8] <- min(log$local_time) %>% as.POSIXlt(format = "%Y-%m-%dT%H:%M:%S") %>% as.character()
  mx[1,9] <- max(log$local_time) %>% as.POSIXlt(format = "%Y-%m-%dT%H:%M:%S") %>% as.character()
  mx[1,10] <- max(training_recs$local_time) %>% as.character()
  mx[1,11] <- nrow(training_recs)
  
  table <- htmlTable(mx, header=c("Total", "Last", "At Venue", "Hot", "Warm",
                                     "Cold", "Total", "First", "Last", "Last", "Total"),
                     cgroup = c("Triggers", "Polls", "Manual Arrivals"),
                     rnames = c(log$pt),
                     n.cgroup = c(2, 7, 2),
                     align = "|cc|ccccccc|cc",
                     col.columns = c(rep("#FFFFCC", 2),
                                     rep("#E6E6F0", 7)))
  table
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
  triggers <- data[data$tag == 'ARRIVAL_TRIGGER',]
  triggers$day <- substr(triggers$local_time, 0, 10)
  triggers %>%
    ggvis(x = ~day, fill := "#663300") %>%
    layer_bars() %>%
    add_axis("x", title = "",
             properties = axis_props(labels = list(angle = 45, align = "left"))) %>%
    add_axis("y", title = "Triggers")
}
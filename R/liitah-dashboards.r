## Summary data table generated in HTML format
## (?) How to handle multiple participants (?)
#' @export
full_table <- function(data, params, ...) {
  log <- data$log
  venue <- data$venue
  ## BASIC SUMMARY
  ## Deprecated date filter stuff
  #log$local_time = log$local_time %>% as.POSIXlt(format = "%Y-%m-%dT%H:%M:%SZ")
  #log = log[log$local_time > filterStart & log$local_time < filterEnd, ]
  #venues = venues[venues$local_time > filterStart & venues$local_time < filterEnd, ]
  #filterStart = filterStart %>% as.POSIXlt(format = "%Y-%m-%dT%H:%M:%SZ")
  #filterEnd = filterEnd %>% as.POSIXlt(format = "%Y-%m-%dT%H:%M:%SZ")
  
  log$local_time = log$local_time %>% as.POSIXlt(format = "%Y-%m-%dT%H:%M:%SZ")
  #log = log[log$local_time > filterStart & log$local_time < filterEnd, ]
  if (nrow(venue) != 0) venue$local_time = venue$local_time %>% as.POSIXlt(format = "%Y-%m-%dT%H:%M:%SZ")
  # ==== Summarize data ====
  # Query data for some summary measures
  polls = log[log$tag == "POLLING_SERVICE_ANDROID", ]
  polls_at_location = polls[polls$args.category == "at_venue", ]
  triggers = log[log$tag == 'ARRIVAL_TRIGGER',]
  # This line for MANUAL_ARRIVAL
  training_recs = log[log$tag == 'MANUAL_ARRIVAL',]
  
  # Create Matrix
  mx <- matrix(nrow = 1, ncol = 14)
  rownames(mx) <- c(paste0(data$pt[1]))
  colnames(mx) <- c("pt", "Total_Venues", "Total_Triggers", "Last_Trigger", "Last_Venue_Added",
                    "Polls_at_Venue", "Hot_Polls", "Warm_Polls", "Cold_Polls", "Total_Polls",
                    "First_Poll", "Last_Poll", "Last_Manual_Arrival_log",
                    "Total_Manual_Arrival_Logs")
  
  mx[1,1] <- log$pt[1]
  mx[1,2] <- nrow(venue)
  mx[1,3] <- nrow(triggers)
  mx[1,4] <- max(triggers$local_time) %>% as.POSIXlt(format = "%Y-%m-%dT%H:%M:%S") %>% as.character()
  mx[1,5] <- max(venue$local_time) %>% as.character()
  mx[1,6] <-  nrow(polls_at_location)
  mx[1,7] <- polls[polls$args.category == "hot", ] %>% nrow()
  mx[1,8] <- polls[polls$args.category == "warm", ] %>% nrow()
  mx[1,9] <- polls[polls$args.category == "cold", ] %>% nrow()
  mx[1,10] <- nrow(polls)
  mx[1,11] <- min(log$local_time) %>% as.POSIXlt(format = "%Y-%m-%dT%H:%M:%S") %>% as.character()
  mx[1,12] <- max(log$local_time) %>% as.POSIXlt(format = "%Y-%m-%dT%H:%M:%S") %>% as.character()
  mx[1,13] <- max(training_recs$local_time) %>% as.character()
  mx[1,14] <- nrow(training_recs)
  
  print(xtable(mx), type = "html")
}

## Bar graph of hot/warm/cold polls per day for one participant
#' @export
polls_per_day <- function(data, params, ...) {
  polls <- data$log$data
  temp <- data.frame(day = substr(data$log$metadata$local_time, 0, 10))
  polls <- cbind(polls, temp)
  polls <- polls[polls$tag == "POLLING_SERVICE_ANDROID", ]
  polls <- polls[polls$category != "", ]
  days <- unique(polls$day)
  
  # Count polls per day
  summary <- data.frame(day = character(), category = character(), count = numeric())
  for (i in 1:length(days)) {
    polls_sub <- polls[polls$day == days[i], ]
    cold <- nrow(polls_sub[polls_sub$category == "cold",])
    warm <- nrow(polls_sub[polls_sub$category == "warm",])
    hot <- nrow(polls_sub[polls_sub$category == "hot",])
    at_venue <- nrow(polls_sub[polls_sub$category == "at_venue",])
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
    add_axis("y", title = "Polls") %>%
    add_axis("x", orient = "top", ticks = 0, title = paste0("Participant: ", paste(unique(data$log$metadata$pt),collapse=",")),
             properties = axis_props(
               axis = list(stroke = "white"),
               labels = list(fontSize = 0)))
}

## Bar graph of total triggers per day
#' @export
triggers_per_day <- function(data, params, ...) {
  triggers <- data$log$data
  triggers$day <- substr(data$log$metadata$local_time, 0, 10)
  triggers <- triggers[triggers$tag == 'ARRIVAL_TRIGGER',]
  triggers %>%
    ggvis(x = ~day, fill := "#663300") %>%
    layer_bars() %>%
    add_axis("x", title = "",
             properties = axis_props(labels = list(angle = 45, align = "left"))) %>%
    add_axis("y", title = "Triggers") %>%
    add_axis("x", orient = "top", ticks = 0, title = paste0("Participant: ", paste(unique(data$log$metadata$pt),collapse=",")),
             properties = axis_props(
               axis = list(stroke = "white"),
               labels = list(fontSize = 0)))
  #add_tooltip(function(x){
  #  x <- subset(x, select = -stack_lwr_)
  #  x <- rename(x, Date = x_)
  #  x <- rename(x, Total = stack_upr_)
  #  paste0(names(x), ": ", format(x), collapse = "<br />")
  #}, "hover")
}
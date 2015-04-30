## FUNCTION: Return complete summary with one row per participant
#' @export
full_summary <- function(pt, filterStart = '2014-04-10T14:00:01Z',
                         filterEnd = '2016-04-11T23:59:59Z') {
  basic <- basic_summary(pt)
  arrivals <- arrival_table(pt)
  arrivals <- subset(arrivals, select = -c(pt))
  # Difference from manual arrival and trigger
  trigger_diff <- arrival_diff(pt)
  condensed <- arrival_condense(trigger_diff)
  condensed <- subset(condensed, select = -c(pt))
  # cbind all dataframes
  ret <- cbind(basic, arrivals, condensed)
  ret
}

## FUNCTION: Quantify arrivals
## For each arrival, report time since warm and time since cold
#' @export
arrival_summary <- function(pt) {
  log <- read_pilr(data_set = "pilrhealth:mobile:app_log", schema = "1", 
                   query_params = list(participant = pt))
  # Only include pollings with a category
  log <- log[!is.na(log$args.category),]
  log <- log[log$args.category != "",]
  ret <- data.frame(venue = character(), 
                    arrival_time = as.Date(character()),
                    last_cat = character())
  # Loop through all, find each time you enter 'at_venue'
  i <- 1
  polled_at_venue = FALSE
  while (i <= nrow(log)) {
    # Find initial instance of at_venue
    if (log$args.category[i] == "at_venue" && !is.na(log$args.category[i])) {
      # Find time since last reported as hot,warm,cold
      j <- i - 1
      while ((is.na(log$args.category[j]) || log$args.category[j] == "") && j > 1)
        j <- j - 1
      prev_cat <- log$args.category[j]
      if (j <= 1) prev_cat <- "first"
      temp <- data.frame(venue = log$args.nearest_venue[i], 
                         arrival_time = log$local_time[i], 
                         last_cat = prev_cat) 
      ret <- rbind(ret, temp)
      # Loop through current set of 'at_venue' polls
      while (log$args.category[i] == "at_venue" && i <= nrow(log)
             && !is.na(log$args.category[i])) i <- i + 1
    }
    i <- i + 1
  }
  ret
}

## FUNCTION: Return percentages of arrival_summary
#' @export
arrival_table <- function(pt) {
  # For each participant entered summarize arrivals
  for (i in 1:length(pt)) {
    arrivals <- arrival_summary(pt[i])
    totals <- as.data.frame(table(arrivals$last_cat))
    from_hot <- as.numeric(totals[totals$Var1 == "hot",][2])
    from_warm <- as.numeric(totals[totals$Var1 == "warm",][2])
    from_cold <- as.numeric(totals[totals$Var1 == "cold",][2])
    if (is.na(from_hot)) from_hot <- 0
    if (is.na(from_warm)) from_warm <- 0
    if (is.na(from_cold)) from_cold <- 0
    temp <- data.frame(pt = pt[i], Hot_Arrivals = from_hot,
                       Warm_Arrivals = from_warm,
                       Cold_Arrivals = from_cold)
    if (i == 1) ret <- temp
    else ret <- rbind(ret, temp)
  }
  ret
}

## FUNCTION: Basic summary stats
#' @export
basic_summary <- function(pt, filterStart = '2014-04-10T14:00:01Z',
                          filterEnd = '2016-04-11T23:59:59Z') {
  for (i in 1:length(pt)) {
    # Read in data
    log <- read_pilr(data_set = "pilrhealth:mobile:app_log", schema = "1", 
                     query_params = list(participant = pt[i]))
    venues <- read_pilr(data_set = "pilrhealth:liitah:personal_venue", schema = "1", 
                        query_params = list(participant = pt[i]))
    if (nrow(log) == 0) {
      temp <- data.frame(pt = paste0(pt[i], " (NO LOGIN)"), 
                         Total_Venues = nrow(venues), 
                         Total_Polls = 0,
                         Total_Triggers = 0, Polls_at_Venue = 0,
                         Hot_Polls = 0,
                         Warm_Polls = 0,
                         Cold_Polls = 0,
                         Last_Venue_Added = NA,
                         #                   Last_Manual_Arrival_Log = max(training_recs$local_time) %>% as.character(),
                         Last_Poll = NA,
                         First_Poll = NA)
    }
    else {
      #training_recs <- read_pilr(data_set = "pilrhealth:liitah:personal_venue_training_record", schema = "1", 
      #                           query_params = list(participant = pt[i]))
      filterStart = filterStart %>% as.POSIXlt(format = "%Y-%m-%dT%H:%M:%SZ")
      filterEnd = filterEnd %>% as.POSIXlt(format = "%Y-%m-%dT%H:%M:%SZ")
      log$local_time = log$local_time %>% as.POSIXlt(format = "%Y-%m-%dT%H:%M:%SZ")
      log = log[log$local_time > filterStart & log$local_time < filterEnd, ]
      if (nrow(venues) != 0) venues$local_time = venues$local_time %>% as.POSIXlt(format = "%Y-%m-%dT%H:%M:%SZ")
      #training_recs$local_time = training_recs$local_time %>% as.POSIXlt(format = "%Y-%m-%dT%H:%M:%SZ")
      #training_recs = training_recs[training_recs$local_time > filterStart & training_recs$local_time < filterEnd, ]
      # ==== Summarize data ====
      # Query data for some summary measures
      polls = log[log$tag == "POLLING_SERVICE_ANDROID", ]
      polls_at_location = polls[polls$args.category == "at_venue", ]
      triggers = log[log$tag == 'ARRIVAL_TRIGGER',]
      
      # Table of the summary measures
      temp <- data.frame(pt = paste0(pt[i]), 
                         Total_Venues = nrow(venues), 
                         Total_Polls = nrow(polls),
                         Total_Triggers = nrow(triggers), Polls_at_Venue = nrow(polls_at_location),
                         Hot_Polls = polls[polls$args.category == "hot", ] %>% nrow(),
                         Warm_Polls = polls[polls$args.category == "warm", ] %>% nrow(),
                         Cold_Polls = polls[polls$args.category == "cold", ] %>% nrow(),
                         Last_Venue_Added = max(venues$local_time) %>% as.character(),
                         #                   Last_Manual_Arrival_Log = max(training_recs$local_time) %>% as.character(),
                         Last_Poll = max(log$local_time) %>% as.character(),
                         First_Poll = min(log$local_time) %>% as.character())
    }
    if (i == 1) ret <- temp
    else ret <- rbind(ret, temp)
  }
  if (!exists("ret")) ret <- temp
  ret
}

## FUNCTION: Comparing locations in venues to training recs
#' @export
venue_diff <- function(pt) {
  # Read in data from PiLR API
  venues <- read_pilr(data_set = "pilrhealth:liitah:personal_venue", schema = "1", 
                      query_params = list(participant = pt))
  training <- read_pilr(data_set = "pilrhealth:liitah:personal_venue_training_record", schema = "1", 
                        query_params = list(participant = pt))
  # Format and merge data frame. Keep track of first location entry.
  venues <- rename(venues, c("id" = "venue_id", "trig_info.lon" = "lon", "trig_info.lat" = "lat"))
  training <- rename(training, c("info.lon" = "lon", "info.lat" = "lat"))
  # Merge venues with training records
  venues <- subset(venues, select=c(timestamp, pt, venue_id, lon, lat))
  training <- subset(training, select=c(timestamp, pt, venue_id, lon, lat))
  # For each venue, find max/min/mean distance from matching training id's
  ret <- data.frame(venue_id = character(), count = numeric(), 
                    mean = numeric(), min = numeric(), max = numeric())
  message("Distance in meters from original venue")
  for (venue in venues$venue_id) {
    training_sub <- training[training$venue_id == venue,]
    venues_sub <- venues[venues$venue_id == venue,]
    message(paste0("Venue ID: ", venue))
    distlist <- numeric()
    if (nrow(training_sub)) {
      for (i in 1:nrow(training_sub)) {
        dist <- distCosine(c(as.numeric(venues_sub$lon[1]), as.numeric(venues_sub$lat[1])), 
                           c(as.numeric(training_sub$lon[i]), as.numeric(training_sub$lat[i])))
        distlist <- append(distlist, dist)
        message(dist)
      }
      temp <- data.frame(venue_id = venue, count = length(distlist), 
                         mean = mean(distlist), min = min(distlist), max = max(distlist))
      ret <- rbind(ret, temp)
    }
    ret
  }
}

## FUNCTION: Time difference between arrival_trigger and manual_arrival
## Average per venue for one participant. Difference in minutes.
## Positive diff means arrival_trigger came first
## NA diff means no arrival_trigger detected near manual_arrival
#' @export
arrival_diff <- function(pt) {
  for (j in 1:length(pt)) {
    # Read in data
    log <- read_pilr(data_set = "pilrhealth:mobile:app_log", schema = "1", 
                     query_params = list(participant = pt[j]))
    occurences <- data.frame(venue = character(), trigger_diff = numeric())
    # Find each manual_arrival
    i <- 1
    while (i < nrow(log)) {
      if (log$tag[i] == "MANUAL_ARRIVAL") {
        venue <- log$args.response_value[i]
        # Find time difference of arrival_trigger if present
        if (i <= 10) sub <- log[1:(i+10),]
        else if (i >= nrow(log)-10) sub <- log[(i-10):nrow(log),]
        else sub <- log[(i-10):(i+10),]
        if (!any(sub$tag == "ARRIVAL_TRIGGER")) diff <- NA
        else {
          index <- match("ARRIVAL_TRIGGER", sub$tag)
          diff <- as.numeric(as.POSIXct(log$local_time[i], format = "%Y-%m-%dT%H:%M:%SZ") -
                               as.POSIXct(sub$local_time[index], format = "%Y-%m-%dT%H:%M:%SZ"))
        }
        occurences <- rbind(occurences, data.frame(venue = venue, trigger_diff = diff))
      }
      i <- i + 1
    }
    # Remove any NA venues
    occurences <- subset(occurences, !is.na(venue))
    # Summarize by venue
    temp <- data.frame(pt = character(), venue = character(),
                       avg_diff = numeric(), no_triggers = numeric())
    i <- 1
    while (i <= length(unique(occurences$venue))) {
      sub <- subset(occurences, venue == unique(occurences$venue)[i])
      num_na <- count(subset(sub, is.na(trigger_diff)))
      sub <- subset(sub, !is.na(trigger_diff))
      avg <- mean(sub$trigger_diff)
      temp <- rbind(temp, data.frame(pt = pt[j], venue = unique(occurences$venue)[i], 
                                     avg_diff = avg, no_triggers = as.numeric(num_na)))
      i <- i + 1
    }
    if (nrow(temp) == 0) temp <- data.frame(pt = pt[j], venue = "NO TRIGGERS",
                                            avg_diff = 0, no_triggers = 0)
    if (j == 1) ret <- temp
    else ret <- rbind(ret, temp)
  }
  ret
}

## FUNCTION: Condense arrival_diff results into 1 row per participant
#' @export
arrival_condense <- function(diff) {
  for (i in 1:length(unique(diff$pt))) {
    sub <- subset(diff, pt == unique(diff$pt)[i])
    avg <- mean(sub$avg_diff)
    tot <- sum(sub$no_triggers)
    temp <- data.frame(pt = unique(diff$pt)[i], Average_Trigger_Diff = avg, No_Triggers = tot)
    if (i == 1) ret <- temp
    else ret <- rbind(ret, temp)
  }
  ret
}
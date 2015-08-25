## FUNCTION: Return complete summary with one row per participant
#' @export
full_summary <- function(pt, filterStart = '2014-04-10T14:00:01Z',
                         filterEnd = '2016-04-11T23:59:59Z') {
  basic <- basic_summary(pt, filterStart = filterStart, filterEnd = filterEnd)
  arrivals <- arrival_table(pt, filterStart = filterStart, filterEnd = filterEnd)
  arrivals <- subset(arrivals, select = -c(pt))
  # Difference from manual arrival and trigger
  trigger_diff <- arrival_diff_avg(pt, filterStart = filterStart, filterEnd = filterEnd)
  condensed <- arrival_condense(trigger_diff)
  condensed <- subset(condensed, select = -c(pt))
  # cbind all dataframes
  ret <- cbind(basic, arrivals, condensed)
  ret
}

## FUNCTION: Quantify arrivals
## For each at_venue arrival, return if it came from cold, warm, or hot category
#' @export
arrival_summary <- function(pt, filterStart = '2014-04-10T14:00:01Z',
                            filterEnd = '2016-04-11T23:59:59Z') {
  for (k in 1:length(pt)) {
    log <- read_pilr(data_set = "pilrhealth:mobile:app_log", schema = "1", 
                     query_params = list(participant = pt[k]))
    filterStart = filterStart %>% as.POSIXlt(format = "%Y-%m-%dT%H:%M:%SZ")
    filterEnd = filterEnd %>% as.POSIXlt(format = "%Y-%m-%dT%H:%M:%SZ")
    ret <- data.frame(pt = character(), venue = character(), 
                      arrival_time = as.Date(character()),
                      last_cat = character())
    if (nrow(log)) {
      log$local_time = log$local_time %>% as.POSIXlt(format = "%Y-%m-%dT%H:%M:%SZ")
      log = log[log$local_time > filterStart & log$local_time < filterEnd, ]
      # Only include pollings with a category
      log <- log[!is.na(log$args.category),]
      log <- log[log$args.category != "",]
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
          temp <- data.frame(pt = pt[k], venue = log$args.nearest_venue[i], 
                             arrival_time = log$local_time[i], 
                             last_cat = prev_cat) 
          ret <- rbind(ret, temp)
          # Loop through current set of 'at_venue' polls
          while (log$args.category[i] == "at_venue" && i <= nrow(log)
                 && !is.na(log$args.category[i])) i <- i + 1
        }
        i <- i + 1
      }
    }
    else {}
    if (k == 1) ret2 <- ret
    else ret2 <- rbind(ret2, ret)
  }
  ret2
}

## FUNCTION: Return percentages of arrival_summary
#' @export
arrival_table <- function(pt, filterStart = '2014-04-10T14:00:01Z',
                          filterEnd = '2016-04-11T23:59:59Z') {
  # For each participant entered summarize arrivals
  for (i in 1:length(pt)) {
    arrivals <- arrival_summary(pt[i], filterStart = filterStart, filterEnd = filterEnd)
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
    filterStart = filterStart %>% as.POSIXlt(format = "%Y-%m-%dT%H:%M:%SZ")
    filterEnd = filterEnd %>% as.POSIXlt(format = "%Y-%m-%dT%H:%M:%SZ")
    if (nrow(log) == 0) {
      temp <- data.frame(pt = paste0(pt[i], " (NO LOGIN)"), 
                         Total_Venues = nrow(venues), 
                         Total_Polls = 0,
                         Total_Triggers = 0, Polls_at_Venue = 0,
                         Hot_Polls = 0,
                         Warm_Polls = 0,
                         Cold_Polls = 0,
                         Last_Venue_Added = NA,
                         Last_Manual_Arrival_Log = NA,
                         Total_Manual_Arrival_Logs = 0,
                         Last_Poll = NA,
                         First_Poll = NA)
    }
    else {
      log$local_time = log$local_time %>% as.POSIXlt(format = "%Y-%m-%dT%H:%M:%SZ")
      log = log[log$local_time > filterStart & log$local_time < filterEnd, ]
      venues = venues[venues$local_time > filterStart & venues$local_time < filterEnd, ]
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
      # This line for MANUAL_ARRIVAL
      training_recs = log[log$tag == 'MANUAL_ARRIVAL',]
      
      # Table of the summary measures
      temp <- data.frame(pt = paste0(pt[i]), 
                         Total_Venues = nrow(venues), 
                         Total_Polls = nrow(polls),
                         Total_Triggers = nrow(triggers), Polls_at_Venue = nrow(polls_at_location),
                         Hot_Polls = polls[polls$args.category == "hot", ] %>% nrow(),
                         Warm_Polls = polls[polls$args.category == "warm", ] %>% nrow(),
                         Cold_Polls = polls[polls$args.category == "cold", ] %>% nrow(),
                         Last_Venue_Added = max(venues$local_time) %>% as.character(),
                         Last_Manual_Arrival_Log = max(training_recs$local_time) %>% as.character(),
                         Total_Manual_Arrival_Logs = nrow(training_recs),
                         Last_Poll = max(log$local_time) %>% as.character(),
                         First_Poll = min(log$local_time) %>% as.character())
    }
    if (i == 1) ret <- temp
    else ret <- rbind(ret, temp)
  }
  if (!exists("ret")) ret <- temp
  ret[ret == -Inf] <- NA
  ret
}

## FUNCTION: Comparing locations in venues to training recs
#' @export
venue_diff <- function(pt, filterStart = '2014-04-10T14:00:01Z',
                       filterEnd = '2016-04-11T23:59:59Z') {
  # Read in data from PiLR API
  venues <- read_pilr(data_set = "pilrhealth:liitah:personal_venue", schema = "1", 
                      query_params = list(participant = pt))
  training <- read_pilr(data_set = "pilrhealth:liitah:personal_venue_training_record", schema = "1", 
                        query_params = list(participant = pt))
  
  filterStart = filterStart %>% as.POSIXlt(format = "%Y-%m-%dT%H:%M:%SZ")
  filterEnd = filterEnd %>% as.POSIXlt(format = "%Y-%m-%dT%H:%M:%SZ")
  log$local_time = log$local_time %>% as.POSIXlt(format = "%Y-%m-%dT%H:%M:%SZ")
  venues = venues[venues$local_time > filterStart & venues$local_time < filterEnd, ]
  training = training[training$local_time > filterStart & training$local_time < filterEnd, ]
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
## Average per venue. Difference in minutes.
## Positive diff means arrival_trigger came first
## NA diff means no arrival_trigger detected near manual_arrival
#' @export
arrival_diff_avg <- function(pt, filterStart = '2014-04-10T14:00:01Z',
                             filterEnd = '2016-04-11T23:59:59Z') {
  for (j in 1:length(pt)) {
    # Read in data
    log <- read_pilr(data_set = "pilrhealth:mobile:app_log", schema = "1", 
                     query_params = list(participant = pt[j]))
    filterStart = filterStart %>% as.POSIXlt(format = "%Y-%m-%dT%H:%M:%SZ")
    filterEnd = filterEnd %>% as.POSIXlt(format = "%Y-%m-%dT%H:%M:%SZ")
    temp <- data.frame(pt = character(), venue = character(),
                       avg_diff = numeric(), no_triggers = numeric())
    if (nrow(log)) {
      log$local_time = log$local_time %>% as.POSIXlt(format = "%Y-%m-%dT%H:%M:%SZ")
      log = log[log$local_time > filterStart & log$local_time < filterEnd, ]
      occurences <- data.frame(venue = character(), trigger_diff = numeric())
      # Find each manual_arrival
      i <- 1
      while (i < nrow(log)) {
        if (log$tag[i] == "MANUAL_ARRIVAL") {
          venue <- log$args.response_value[i]
          # Find time difference of arrival_trigger if present
          if (i <= 20) sub <- log[1:(i+20),]
          else if (i >= nrow(log)-20) sub <- log[(i-20):nrow(log),]
          else sub <- log[(i-20):(i+20),]
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
      i <- 1
      while (i <= length(unique(occurences$venue))) {
        sub <- subset(occurences, venue == unique(occurences$venue)[i])
        num_na <- count(subset(sub, is.na(trigger_diff)))
        sub <- subset(sub, !is.na(trigger_diff))
        avg <- mean(sub$trigger_diff, na.rm=TRUE)
        if (is.nan(avg)) avg = NA
        temp <- rbind(temp, data.frame(pt = pt[j], venue = unique(occurences$venue)[i], 
                                       avg_diff = avg, no_triggers = as.numeric(num_na)))
        i <- i + 1
      }
      if (nrow(temp) == 0) temp <- data.frame(pt = pt[j], venue = "NO TRIGGERS",
                                              avg_diff = 0, no_triggers = 0)
    }
    if (j == 1) ret <- temp
    else ret <- rbind(ret, temp)
  }
  ret
}

## FUNCTION: Condense arrival_diff results into 1 row per participant for full summary
#' @export
arrival_condense <- function(diff) {
  if (nrow(diff)) {
    for (i in 1:length(unique(diff$pt))) {
      sub <- subset(diff, pt == unique(diff$pt)[i])
      avg <- mean(sub$avg_diff, na.rm=TRUE)
      tot <- sum(sub$no_triggers)
      temp <- data.frame(pt = unique(diff$pt)[i], Average_Trigger_Diff = avg, No_Triggers = tot)
      if (i == 1) ret <- temp
      else ret <- rbind(ret, temp)
    }
  }
  else {
    ret <- data.frame(pt = NA, Average_Trigger_Diff = NA, No_Triggers = NA)
  }
  ret
}

## FUNCTION: Time difference between arrival_trigger and manual_arrival
## For every arrival instance. Difference in minutes.
## Positive diff means arrival_trigger came first
## NA diff means no arrival_trigger detected near manual_arrival
#' @export
arrival_diff_inst <- function(pt, filterStart = '2014-04-10T14:00:01Z',
                              filterEnd = '2016-04-11T23:59:59Z') {
  for (j in 1:length(pt)) {
    # Read in data
    log <- read_pilr(data_set = "pilrhealth:mobile:app_log", schema = "1", 
                     query_params = list(participant = pt[j]))
    filterStart = filterStart %>% as.POSIXlt(format = "%Y-%m-%dT%H:%M:%SZ")
    filterEnd = filterEnd %>% as.POSIXlt(format = "%Y-%m-%dT%H:%M:%SZ")
    log$local_time = log$local_time %>% as.POSIXlt(format = "%Y-%m-%dT%H:%M:%SZ")
    log = log[log$local_time > filterStart & log$local_time < filterEnd, ]
    occurences <- data.frame(pt = character(), local_time = character(),
                             venue = character(), trigger_diff = numeric())
    # Find each manual_arrival
    i <- 1
    while (i < nrow(log)) {
      if (log$tag[i] == "MANUAL_ARRIVAL") {
        venue <- log$args.response_value[i]
        time <- as.POSIXct(log$local_time[i], format = "%Y-%m-%dT%H:%M:%SZ")
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
        occurences <- rbind(occurences, data.frame(pt = pt[j], local_time = time,
                                                   venue = venue, trigger_diff = diff))
      }
      i <- i + 1
    }
    if (j == 1) ret <- occurences
    else ret <- rbind(ret, occurences)
  }
  ret
}

compliance_summary <- function(pt, filterStart = '2014-04-10T14:00:01Z',
                               filterEnd = '2016-04-11T23:59:59Z') {
  for (i in 1:length(pt)) {
    no_triggers <- 0
    compliance <- data.frame(pt = character(), Total_Reports = numeric(), Total_Triggers = numeric(),
                             Reports_without_Triggers = numeric(), Suggestion_0 = numeric(), Suggestion_1 = numeric(),
                             Suggestion_2 = numeric(), Suggestion_3 = numeric(), Pictures_Submitted = numeric(),
                             Venues_Added = numeric())
    log <- read_pilr(data_set = "pilrhealth:mobile:app_log", schema = "1", 
                     query_params = list(participant = pt[i]))
    survey <- read_pilr(data_set = "pilrhealth:mobile:survey_data", schema = "1", 
                        query_params = list(participant = pt[i]))
    venues <- read_pilr(data_set = "pilrhealth:liitah:personal_venue", schema = "1", 
                        query_params = list(participant = pt[i]))
    
    if (nrow(log) == 0) {
      compliance <- data.frame(pt = paste0(pt[i], " (NO DATA)"), Total_Reports = 0, Total_Triggers = 0,
                               Reports_without_Triggers = 0, Suggestion_0 = 0, Suggestion_1 = 0,
                               Suggestion_2 = 0, Suggestion_3 = 0, Pictures_Submitted = 0, Venues_Added = 0)
    }
    else {
      # Reports
      reports <- log[log$tag == 'MANUAL_VERIFY',]
      # Triggers
      triggers <- log[log$tag == 'ARRIVAL_TRIGGER',]
      # Find No Triggers
      for (j in 1:nrow(log)) {
        if (log$tag[j] == "MANUAL_VERIFY") {
          if (j <= 15) sub <- log[1:(j+15),]
          else if (j >= nrow(log)-15) sub <- log[(j-15):nrow(log),]
          else sub <- log[(j-15):(j+15),]
          if (!any(sub$tag == "ARRIVAL_TRIGGER")) no_triggers <- no_triggers + 1
        }
      }
      
      # Food suggestion responses
      suggestions <- survey[survey$question_code == "50295",]
      suggestions <- suggestions[!is.na(suggestions$question_code),]
      
      # Picture submissions
      pictures <- log[log$tag == "FILE_UPLOAD",]
      
      compliance <- rbind(compliance, data.frame(pt = paste0(pt[i]), Total_Reports = nrow(reports), Total_Triggers = nrow(triggers),
                                                 Reports_without_Triggers = no_triggers,
                                                 Suggestion_0 = nrow(suggestions[suggestions$response_value == 0,]),
                                                 Suggestion_1 = nrow(suggestions[suggestions$response_value == 1,]),
                                                 Suggestion_2 = nrow(suggestions[suggestions$response_value == 2,]),
                                                 Suggestion_3 = nrow(suggestions[suggestions$response_value == 3,]),
                                                 Pictures_Submitted = nrow(pictures), Venues_Added = nrow(venues)))
      
    }
    if (i == 1) ret <- compliance
    else ret <- rbind(ret, compliance)
  }
  ret
}
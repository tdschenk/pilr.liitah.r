## Summary data table generated in HTML format
## (?) How to handle multiple participants (?)
#' @export
full_table <- function(data, params, ...) {
  htmltest <- xtable(data.frame(x = c(1), y = c(2)))
  print(htmltest, type= "html")
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
  test <- data.frame(x = c(1,2), y = c(3,4))
  test %>%
    ggvis(x = ~x, y = ~y) %>%
    layer_bars()
}

#triggers <- data[data$tag == 'ARRIVAL_TRIGGER',]
#triggers$day <- substr(triggers$local_time, 0, 10)
#triggers %>%
#  ggvis(x = ~day) %>%
#  layer_bars() %>%
#  add_axis("y", title = "triggers")
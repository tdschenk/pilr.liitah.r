## Load packages
library(pilr.liitah.r)

## Set default server, project, access_code
options(pilr_server_default = "http://liitah.pilrhealth.com")
options(pilr_project_default = "liitah_testing_2")
options(pilr_default_access_code = "429913fe-472f-4140-919e-40241d76ed99")
data <- read_pilr(data_set = "pilrhealth:mobile:app_log", schema = 1, query_params = list(participant = "109"))

## Full summary - all 600 participants
full <- full_summary(c(601,602,603,604,605,606,607,608,609,610,611,612,613,614,615,616,617,618))
full <- full_summary(c(601,602,603,604,605,606,607,608,609,610,611,612,613,614,
                       615,616,617,618, filterStart = '2015-05-05T01:00:01Z',
                       filterEnd = '2015-05-06T23:59:59Z'))

## Trigger/Arrival difference - every arrival instance
arrival_inst <- arrival_diff_inst(601)
arrival_inst2 <- arrival_diff_inst(c(601,609,610))
arrival_inst3 <- arrival_diff_inst(601, filterStart = '2015-05-05T01:00:01Z',
                                   filterEnd = '2015-05-06T23:59:59Z')

## Trigger/Arrival difference - average per venue
arrival_avg <- arrival_diff_avg(601)
arrival_avg2 <- arrival_diff_avg(c(601,609,610))
arrival_avg3 <- arrival_diff_avg(601, filterStart = '2015-05-05T01:00:01Z',
                                 filterEnd = '2015-05-06T23:59:59Z')

## For each at_venue arrival, return what category was previously polled
arrivals <- arrival_summary(601)
arrivals2 <- arrival_summary(c(601,609,610))
arrivals3 <- arrival_summary(601, filterStart = '2015-05-05T01:00:01Z',
                             filterEnd = '2015-05-06T23:59:59Z')

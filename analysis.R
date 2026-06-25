# Read submissions

# read in large file not put on GitHub
# submissions <- read.csv("google_materials/all_submissions_june.csv",
#                         colClasses = c("reference_date" = "Date",
#                                        "target_end_date" = "Date"))
# subset and write out smaller file
# submissions <- subset(submissions, model %in% c("CovidHub-ensemble", "Google Retrospective (TS)"))
# write.csv(submissions, file = "google_materials/selected_submissions-june.csv")

# read in smaller file:
submissions <- read.csv("google_materials/selected_submissions-june.csv",
                        colClasses = c("reference_date" = "Date",
                        "target_end_date" = "Date"))

# split into CDC and Google
cdc <- subset(submissions, model %in% c("CovidHub-ensemble"))
dates_cdc <- unique(cdc$reference_date)

google <- subset(submissions, model %in% c("Google Retrospective (TS)") & reference_date %in% dates_cdc)

# number of rows differ as for some tasks multiple runs are stored.
nrow(google)
nrow(cdc)

# we need to average across those to get to the results from the paper
google_mean <- aggregate(wis ~ reference_date + state + target_end_date + horizon, FUN = mean, data = google)

# now number of rows is identical
nrow(google_mean)

# restrict cdc to same columns:
cdc <- cdc[, colnames(google_mean)]

# this reproduces the numbers from the paper:
(google_mean_by_date <- aggregate(wis ~ reference_date, data = google_mean, FUN = mean))
(cdc_by_date <- aggregate(wis ~ reference_date, data = cdc, FUN = mean))

# merge into one (wide):
model_comparison <- merge(google_mean, cdc, by = c("reference_date", "state", "target_end_date", "horizon"), suffixes = c(".google", ".cdc"))

# compute difference in CRPS
model_comparison$delta_wis <- model_comparison$wis.cdc - model_comparison$wis.google

# now obtain time series of initially reported values
# from csv snapshots
csvs <- list.files("snapshots")
csvs <- csvs[grepl(".csv", csvs)]
dates <- gsub(pattern = "covid-hospital-admissions-", replacement = "", csvs)
dates <- gsub(pattern = ".csv", replacement = "", dates)

# read in data snapshots
snapshots <- list()
for(i in seq_along(csvs)){
  snapshots[[i]] <- read.csv(paste0("snapshots/", csvs[i]), colClasses = c("date" = "Date"))
  max_date <- max(snapshots[[i]]$date)
  new_rows <- subset(snapshots[[i]], date == max_date)
  if(i == 1){
    initial_data <- new_rows # data.frame to be filled step by step
  }else{
    initial_data <- rbind(initial_data, new_rows)
  }
}
names(snapshots) <- dates

# final data:
final_data <- snapshots$`2025-06-18`

# check final data actually works together with Google version:
# final_data_google <- read.csv("google_materials/metadata/Weekly_Hospital_Respiratory_Data__HRD__Metrics_by_Jurisdiction__National_Healthcare_Safety_Network__NHSN___Preliminary__20250618.csv")
# final_data_google <- final_data_google[, c("Week.Ending.Date", "Total.COVID.19.Admissions", "Geographic.aggregation")]
# colnames(final_data_google) <- c("date", "value", "state")
# final_data_google$date <- as.Date(final_data_google$date)
# ca_hub <- subset(final_data, state == "CA")
# ca_google <- subset(final_data_google, state == "CA")
# ca_google <- ca_google[order(ca_google$date), ]
# plot(ca_hub$date, ca_hub$value, type = "l")
# lines(ca_google$date, ca_google$value, col = "red")
# these work together

# merge into a data set on revisions:
initial_data$location <- final_data$location <- NULL # remove redundant column
data_revisions <- merge(initial_data, final_data, by = c("state", "date"), 
                      all.x = TRUE, all.y = FALSE, suffixes = c(".initial", ".final"))

# compute absolute and relative (log) revisions
data_revisions$relative_revision <- abs(log(data_revisions$value.final + 1) - log(data_revisions$value.initial + 1))
data_revisions$absolute_revision <- abs(data_revisions$value.final - data_revisions$value.initial)

# need to shift data_revisions by one week so it aligns with reference_date values
data_revisions$date <- data_revisions$date + 7

# merge into model_comparison
model_comparison <- merge(model_comparison, data_revisions, 
                           by.x = c("reference_date", "state"),
                           by.y = c("date", "state"))


# order by absolute revisions:
ordered_by_absolute <- model_comparison[order(model_comparison$absolute_revision), c("absolute_revision", "delta_wis")]
# compute cumulative means:
ordered_by_absolute$mean_delta_wis <- cumsum(ordered_by_absolute$delta_wis)/seq_along(ordered_by_absolute$delta_wis)
# only keep last entry for each value of absolute_revision:
ordered_by_absolute <- aggregate(mean_delta_wis ~ absolute_revision, data = ordered_by_absolute, FUN = tail, 1)

# plot:
plot(ordered_by_absolute$absolute_revision, ordered_by_absolute$mean_delta_wis,
     xlab = "maximum accepted absolute revision of last data point",
     ylab = "advantage of Google in mean WIS", type = "s")


# same for relative changes: order
ordered_by_relative <- model_comparison[order(model_comparison$relative_revision), c("relative_revision", "delta_wis")]
# compute cumulative means:
ordered_by_relative$mean_delta_wis <- cumsum(ordered_by_relative$delta_wis)/seq_along(ordered_by_relative$delta_wis)
# tried to compute standard errors but normality assumption is too strongly violated
# ordered_by_relative$mean_delta_wis_squared <- cumsum(ordered_by_relative$delta_wis^2)/seq_along(ordered_by_relative$delta_wis)
# ordered_by_relative$var_delta_wis <- ordered_by_relative$mean_delta_wis_squared - ordered_by_relative$mean_delta_wis^2
# ordered_by_relative$se_delta_wis <- ordered_by_relative$var_delta_wis/sqrt(seq_along(ordered_by_relative$var_delta_wis))
# ordered_by_relative$lower_delta_wis <- ordered_by_relative$mean_delta_wis - qnorm(0.05)*ordered_by_relative$se_delta_wis
# ordered_by_relative$upper_delta_wis <- ordered_by_relative$mean_delta_wis + qnorm(0.05)*ordered_by_relative$se_delta_wis

# only keep last entry for each value of absolute_revision:
ordered_by_relative <- aggregate(mean_delta_wis ~ relative_revision, data = ordered_by_relative, FUN = tail, 1)

# plot
plot(ordered_by_relative$relative_revision, ordered_by_relative$mean_delta_wis,
     xlab = "maximum accepted log revision of last data point",
     ylab = "advantage of Google in mean WIS", type = "s")


# plot with a square-root scale on the x-axis to see more
plot(sqrt(ordered_by_relative$relative_revision), ordered_by_relative$mean_delta_wis,
     xlab = "maximum accepted log revision of last data point",
     ylab = "advantage of Google in mean WIS", type = "s", axes = FALSE)
xdash <- c(0, 0.1, 0.25, 0.5, 1, 2.5, 5)
axis(1, at = sqrt(xdash), labels = xdash)
axis(2)


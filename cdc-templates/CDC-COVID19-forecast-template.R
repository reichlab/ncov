## script to generate COVID-19 ILI forecasting template
## Nicholas Reich
## March 2020

library(dplyr)
library(readr)

## shared parameters/data across state/national templates
ili_targets <- c(paste(1:6, "wk ahead"), "Peak height")
date_targets <- c("Peak week", "Offset week")
binary_targets <- c("Offset")
ili_bins <- as.character(sprintf("%.1f", seq(0, 25, by=.1)))
date_bins <- as.character(seq.Date(as.Date("2020-03-02"), as.Date("2020-10-31"), by="1 week"))


### National/Regional template
locations <- c("US National", paste("HHS Region", 1:10))

## make ILI targets template
ili_bin_template <- expand.grid(location=locations, target=ili_targets, type="bin", bin=ili_bins, value = as.character(1/length(ili_bins)), stringsAsFactors = FALSE)
ili_points_template <- expand.grid(location=locations, target=ili_targets, type="point", value="1.234", stringsAsFactors = FALSE)

## make date targets template
date_bins_template <- expand.grid(location=locations, target=date_targets, type="bin", bin=date_bins, value = as.character(1/length(date_bins)), stringsAsFactors = FALSE)
date_points_template <- expand.grid(location=locations, target=date_targets, type="point", value = "2020-03-02", stringsAsFactors = FALSE)

## make binart target template
binary_bins_template <- expand.grid(location=locations, target=binary_targets, type="bin", bin="true", value = ".5", stringsAsFactors = FALSE)

## bind all together
full_template <- bind_rows(
    ili_bin_template, 
    ili_points_template, 
    date_bins_template, 
    date_points_template, 
    binary_bins_template
    ) %>%
    mutate(location = factor(location, levels=locations),
        type = factor(type, levels=c("point", "bin"))) %>%
    arrange(location, target, type, bin)

## sanity checking
full_template %>% group_by(target, type) %>% summarize(n()) %>% print(n=Inf)
full_template %>% filter(type=="bin") %>% group_by(location,target) %>% summarize(sum(as.numeric(value))) %>% print(n=Inf)

write_csv(full_template, path="cdc-templates/covid19-ili-forecast-national-regional-template.csv")


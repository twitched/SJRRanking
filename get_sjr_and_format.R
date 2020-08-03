library("tidyverse")
library("zoo")

start_year <- 2009
end_year <- 2019

years <- c(start_year:end_year)

url <- "www.scimagojr.com/journalrank.php?out=xls"

#function to construct the file name for a given year
sci_file_name <- function(year){
  file_prefix <- "scimagojr "
  file_suffix <- ".csv"
  paste(file_prefix, year, file_suffix, sep = "")
}

#download files if they aren't already there
for (year in years) {
  file <- sci_file_name(year)
  print(file)
  if(!file.exists(file)){
    download.file(paste(url, "&year=", year, sep = ""), file)
  }
}

col_spec <- cols(
  Rank = col_integer(),
  Sourceid = col_double(),
  Title = col_character(),
  Type = col_character(),
  Issn = col_character(),
  SJR = col_double(),
  `SJR Best Quartile` = col_character(),
  `H index` = col_integer(),
  `Total Docs. (2017)` = col_integer(),
  `Total Docs. (3years)` = col_integer(),
  `Total Refs.` = col_integer(),
  `Total Cites (3years)` = col_integer(),
  `Citable Docs. (3years)` = col_integer(),
  `Cites / Doc. (2years)` = col_double(),
  `Ref. / Doc.` = col_double(),
  Country = col_character(),
  Publisher = col_character(),
  Categories = col_character()
)

quartile_breaks <- c(0, 24.5, 49.5, 74.5, Inf)
quartile_labels <- c("Q4", "Q3", "Q2", "Q1")
cobe_breaks <- c(0, 39.5, 59.5, 79.5, 89.5, 94.5, Inf)
cobe_labels <- c(1, 2, 5, 8, 10, 13)

#combine the files and add the years.  Make sure they are unique by year.  If not, take the max SJR for that year
d <- map(years, function(year){
      suppressWarnings(read_csv2(sci_file_name(year), col_types = col_spec)) %>% 
      select(Title, SJR, BestQ = `SJR Best Quartile`) %>% 
      mutate(Year = year) %>%
      group_by(Title, Year, BestQ) %>% 
      summarize(SJR = max(SJR))}) %>% #map ends here
    reduce(bind_rows) %>%
    ungroup() %>% group_by(Year) %>% #regroup by year
    mutate(Rank = min_rank(desc(SJR)), Percentile = ntile(SJR, 100)) %>% #add rank and percentile
    mutate(Quartile = cut(Percentile, breaks = quartile_breaks, labels = quartile_labels)) %>% #add quartiles
    mutate(COBE = cut(Percentile, breaks = cobe_breaks, labels = cobe_labels)) %>% #add COBE points
    ungroup() %>% group_by(Title) %>% #regroup by title
    arrange(Title, Year, SJR) %>%
    mutate(SJR3 = rollapply(SJR, 3, FUN = mean,  align = "right", fill = NA, na.rm = TRUE)) %>% #get the three year average
    ungroup() %>% group_by(Year) %>%
    mutate(Rank3 = min_rank(desc(SJR3)), Percentile3 = ntile(SJR3, 100)) %>%
    mutate(Quartile3 = cut(Percentile3, breaks = quartile_breaks, labels = quartile_labels)) %>%
    mutate(COBE3 = cut(Percentile3, breaks = cobe_breaks, labels = cobe_labels)) %>%
    filter(!is.na(SJR))

#write it out.
write_csv(d, paste("SJR_", min(years), "_", max(years),"_with_average.csv", sep=""))

#write it out without the three-year average
write_csv(d %>% select(-SJR3, -Rank3, -Percentile3, -Quartile3, -COBE3), paste("SJR_", min(years), "_", max(years),".csv", sep=""))
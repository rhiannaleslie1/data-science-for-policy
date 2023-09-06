#Scipt that gets required data sets and processes them into correct formats
source("R/depends.R")

#loading in the CPI data
cpi_all_sectors <- readxl::read_xlsx(
  path = "data/cpi_tables.xlsx",
  sheet = "Table 19",
  range =  "B46:O70",
  col_names = c("month",
                "Food and non-alcoholic beverages",
                "Alcoholic beverages and tobacco",
                "Clothing and footwear",
                "Housing, water, electricity, gas & other fuels",
                "Furniture, household equipment & routine mainenance",
                "Health",
                "Transport",
                "Communication",
                "Recreation and Culture",
                "Education",
                "Restaurants and hotels",
                "Micellaneous goods and services",
                "CPI (overall)")
) %>%
  tidyr::pivot_longer(cols = 2:14, names_to = "sector", values_to = "perc_change") %>%
  dplyr::mutate(month = gsub(" ", "_", month))

#Loading in CPI on food types
cpi_food <- readxl::read_xlsx(
  path = "data/cpi_tables.xlsx",
  sheet = "Table 22",
  range =  "C24:Q75",
  col_names = c("Food_type",
                "Annual_Avg",
                "Jul_2022",
                "Aug_2022",
                "Sep_2022",
                "Oct_2022",
                "Nov_2022",
                "Dec_2022",
                "Jan_2023",
                "Feb_2023",
                "Mar_2023",
                "Apr_2023",
                "May_2023",
                "Jun_2023",
                "Jul_2023")
) %>%
  dplyr::mutate(Food_type = gsub("^\\d+(\\.\\d+)*(\\.\\d+/\\d+)?[ /]*", "", Food_type)) %>%
  dplyr::select(-Annual_Avg) %>%
  tidyr::pivot_longer(cols = 2:14, names_to = "month", values_to = "perc_change") %>%
  janitor::clean_names()


#creating a lookup to match the month columns to dates easily - base this off the cpi_all_sectors$month as cpi_food$month is a subset.
year <- as.integer(substr(cpi_all_sectors$month, start = 5, stop = 8))
month <- match(substr(cpi_all_sectors$month, start = 1, stop = 3), month.abb)
date_column <- as.Date(paste(year, month, "01", sep = "-"))

date_lookup <- cpi_all_sectors %>%
  dplyr::select(month) %>%
  dplyr::mutate(date = date_column) %>%
  distinct()


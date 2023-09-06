#exploring data sources
source("R/depends.R")
source("scripts/get_cpi_data.R")

#wholesale price of gas plot

sap_gas <- readxl::read_excel("data/sap_price_gas.xlsx",
                              sheet = "Data",
                              range = "A5:C2070") %>%
  filter(Date >= "2021-07-01",
         Date <= "2023-07-01") %>%
  janitor::clean_names() %>%
  ggplot() +
  geom_line(aes(x = date, y = sap_preceding_seven_day_rolling_average_p_k_wh)) +
  theme_bw() +
  labs(x = "Date",
       y = "7 day rolling average for wholesale gas",
       title = "Whole sale gas prices from Jul 2021 to Jul 2023") +
  theme(plot.title = element_text(size = 20),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16))

ggsave("outputs/wholesale_gas.png",
       height = 10,
       width = 12)

#cpi overall and by sector changes
cpi_plot <- cpi_all_sectors %>%
  left_join(date_lookup, by = c("month")) %>%
  filter(sector != "CPI (overall)") %>%
  ggplot() +
  geom_line(aes(x = date, y = perc_change, colour = sector)) +
  theme_bw() +
  labs(x = "Date",
       y = "12 month percentage change",
       title = "CPI across all sectors from Jul 2021 to Jul 2023")

ggsave("outputs/sectors_cpi_plot.png",
       height = 10,
       width = 12)

cpi_plot <- cpi_all_sectors %>%
  left_join(date_lookup, by = c("month")) %>%
  filter(sector == "CPI (overall)") %>%
  ggplot() +
  geom_line(aes(x = date, y = perc_change, colour = sector)) +
  theme_bw() +
  labs(x = "Date",
       y = "12 month percentage change",
       title = "CPI across all sectors from Jul 2021 to Jul 2023") +
  theme(legend.position = "none",
        plot.title = element_text(size = 20),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16))

ggsave("outputs/overal_cpi_plot.png",
       height = 10,
       width = 12)

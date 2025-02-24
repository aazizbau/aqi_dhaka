library(tidyverse)
library(lubridate)

# Read csv files from directory
path <- 'E:/aqi_dhaka_jds/'
years <- 2016:2024
dfs <- map(years, ~read.csv(paste0(path, 'Dhaka_PM2.5_', .x, '_YTD.csv')))

# Combine all csv dataframes to one df appending as new rows
combined_aqi <- bind_rows(dfs)

glimpse(combined_aqi)

# Create subset with specific columns and exclude rows with AQI= -999
dhaka_aqi_16_22_cleaned <- combined_aqi %>%
  filter(AQI != -999) %>%
  dplyr::select('Date..LT.', AQI, 'AQI.Category') %>%
  rename(datetime = `Date..LT.`, aqi = AQI, category = 'AQI.Category')

glimpse(dhaka_aqi_16_22_cleaned)

# Set the "date_time" column as the dataframe index
names(dhaka_aqi_16_22_cleaned)


dhaka_aqi_16_22_cleaned <- dhaka_aqi_16_22_cleaned %>%
  dplyr::mutate(date_time = lubridate::ymd_hm(datetime)) %>%
  dplyr::select(-1) %>%
  dplyr::arrange(date_time) %>%
  tibble::rownames_to_column(var = "id")

glimpse(dhaka_aqi_16_22_cleaned)

# Resample the dataframe using the "D" frequency (for daily frequency)
daily_df <- dhaka_aqi_16_22_cleaned %>%
  dplyr::group_by(date = as.Date(date_time)) %>%
  dplyr::summarise(aqi = round(mean(aqi)),
                   category = ifelse(n() > 0, names(sort(-table(category)))[1], "NA"))

glimpse(daily_df)

# Select the "date", "aqi", and "category" columns and create a new dataframe
daily_avg_aqi_category_df <- daily_df %>%
  dplyr::select(date, aqi, category) %>%
  dplyr::arrange(date)

glimpse(daily_avg_aqi_category_df)

daily_avg_aqi_category <- daily_avg_aqi_category_df %>%
  mutate(aqi_category = case_when(
    aqi <= 50 ~ "Good",
    aqi <= 100 ~ "Moderate",
    aqi <= 150 ~ "Unhealthy for Sensitive Groups",
    aqi <= 200 ~ "Unhealthy",
    aqi <= 300 ~ "Very Unhealthy",
    TRUE ~ "Hazardous"
  ))

glimpse(daily_avg_aqi_category)
write.csv("E:/aqi_dhaka_jds/daily_avg_aqi_category.csv")
# Rain Data
# read the csv file into a data frame
rain_data <- read.csv("E:/aqi_dhaka_jds/CCS_20160101_20241231.csv")

glimpse(rain_data)

# separate the date column into two columns using the separator "_1d"
rain_data <- tidyr::separate(rain_data, Time, into = c("prefix", "date"), sep = "_1d")

# remove the prefix column
rain_data <- dplyr::select(rain_data, -prefix)

rain_data_cleaned <- rain_data %>%
  dplyr::mutate(date = lubridate::ymd(date),
                rain = `Rain.mm.`) %>%
  dplyr::select(date, rain) %>%
  dplyr::arrange(date)

glimpse(rain_data_cleaned)


# Merge AQI and Rain data
merged_aqi_rain <- inner_join(daily_avg_aqi_category, rain_data_cleaned, by = "date")


glimpse(merged_aqi_rain)


category_color <- c("#00e400", "#ffff00", "#ff8000", "#ff0000", "#8f3f97", "#7e0023")

# create a named vector of colors for each AQI Category
aqi_colors <- c("Good" = "#00e400", "Moderate" = "#ffff00", "Unhealthy for Sensitive Groups" = "#ff8000", "Unhealthy" = "#ff0000",
                "Very Unhealthy" = "#8f3f97", "Hazardous" = "#7e0023")


color_pal1 <- c("#66947D", "#90E0B7", "#E0A984", "#486894", "#79A4E0")

# Visualizing AQI data 
ggplot(data = daily_avg_aqi_category, aes(x = date, y = aqi, colour = factor(aqi_category, levels = c("Good", "Moderate", "Unhealthy for Sensitive Groups", "Unhealthy", "Very Unhealthy", "Hazardous")))) +
  geom_line(color = "#66947D", size = 0.7) +
  geom_point(size = 2.7, alpha = 0.9) +
  scale_color_manual(values = aqi_colors) +  # set manual colors  
  scale_x_date(date_breaks = "3 month", date_minor_breaks = "1 month", labels = scales::date_format("%b-%y")) +  
  scale_y_continuous(limits = c(0, 400), breaks = seq(0, 400, 50)) +  
  annotate(geom = "text", x = as.Date("2018-09-01"), y = 395, label = "Air quality of Dhaka is degrading, posing serious implications for public health and the environment.",  color = "#821117", size = 6.5) +
  guides(color = guide_legend(override.aes = list(size = 5))) +
  labs(
    title = "Air Quality Index (AQI) of Dhaka",
    subtitle = "As measured since April 2016",
    caption = "Analysis & Visualization: BHUIYAN MD ABDUL AZIZ, Data:AQI www.airnow.gov",
    x = "Year",
    y = "Daily Average AQI",
    color = "AQI Category"
  ) +
  theme(plot.title = element_text(vjust = -9, color = "#2C3639", size = 25),
        plot.subtitle = element_text(vjust = -12, color = "#3F4E4F", size = 17),
        plot.caption = element_text(color = "gray50", size = 15),
        axis.text.x = element_text(color = "black", size = 12),
        axis.text.y = element_text(color = "black", size = 12),
        axis.title.x = element_text(color = "#3F4E4F", size = 15),
        axis.title.y = element_text(color = "#3F4E4F", size = 15),
        panel.background = element_rect(fill = "#DCD7C9"),
        legend.position = "top",
        legend.justification = c(1, -4),
        plot.margin = unit(c(-2.5, 0.5, 0.5, 0.5), "lines"),
        legend.title = element_text(color = "#2C3639", size = 11),
        legend.text = element_text(color = "#3F4E4F", size = 10),
        legend.background = element_rect(fill="#DCD7C9", size=0.5, linetype="solid", 
                                         colour ="#DCD7C9"))


merged_aqi_rain$rain <- ifelse(merged_aqi_rain$rain == 0, NA, merged_aqi_rain$rain)


# Dhaka AQI-Rain Viz function
dhaka_aqi_rain_viz <- function(color1, color2, color3, color4) {
  ggplot(data = merged_aqi_rain, aes(x = date, y = aqi, colour = factor(aqi_category, levels = c("Good", "Moderate", "Unhealthy for Sensitive Groups", "Unhealthy", "Very Unhealthy", "Hazardous")))) +
    geom_line(color = "#66947D", size = 0.7) +
    geom_point(size = 2.7, alpha = 0.9) +
    geom_point(aes(y = rain), size = 1.9, color = "#03befc", alpha = 0.6, na.rm = TRUE) +
    scale_color_manual(values = aqi_colors) +
    scale_x_date(date_breaks = "3 month", date_minor_breaks = "1 month", labels = scales::date_format("%b-%y")) +
    scale_y_continuous(name = "Daily Average AQI", sec.axis = sec_axis(~ ., name = "Rain (mm)"), limits = c(0, 400), breaks = c(seq(0, 50, by = 10), seq(50, 400, by = 50))) +
    annotate(geom = "text", x = as.Date("2017-04-01"), y = 390, label = "Higher levels of rainfall are associated with improved air quality", color = "#1F8A70", size = 5.5) +
    annotate(geom = "text", x = as.Date("2025-02-01"), y = 20, label = "Rainfall", color = "#03befc", size = 5) +
    guides(color = guide_legend(override.aes = list(size = 5))) +
    labs(
      title = "Air Quality Index (AQI) of Dhaka",
      subtitle = "As measured since March 2016",
      caption = "Data Analysis & Visualization: BHUIYAN MD ABDUL AZIZ, Data Source: AQI:www.airnow.gov, Rain:chrsdata.eng.uci.edu",
      x = "Year",
      y = "Daily Average AQI",
      color = "AQI Category"
    ) +
    theme(plot.title = element_text(vjust = -9, color = color4, size = 25),
          plot.subtitle = element_text(vjust = -12, color = color3, size = 17),
          plot.caption = element_text(color = "gray50", size = 15),
          axis.text.x = element_text(color = "black", size = 12),
          axis.text.y = element_text(color = "black", size = 12),
          axis.title.x = element_text(color = color3, size = 15),
          axis.title.y = element_text(color = color3, size = 15),
          panel.background = element_rect(fill = color1),
          legend.position = "top",
          legend.justification = c(1, -4),
          plot.margin = unit(c(-2.5, 0.5, 0.5, 0.5), "lines"),
          legend.title = element_text(color = color4, size = 11),
          legend.text = element_text(color = color3, size = 10),
          legend.background = element_rect(fill=color1, size=0.5, linetype="solid", 
                                           colour =color1))
}


color_vintage1 <- c("#F9F5EB", "#E4DCCF", "#EA5455", "#002B5B")
color_vintage2 <- c("#F5E9CF", "#7DB9B6", "#E96479", "#4D455D")
color_vintage3 <- c("#BFDB38", "#FC7300", "#1F8A70", "#00425A")
color_vintage4 <- c("#F2E5E5", "#E8C4C4", "#CE7777", "#2B3A55")
color_vintage5 <- c("#DCD7C9", "#A27B5C", "#3F4E4F", "#2C3639")


dhaka_aqi_rain_viz(color1=color_vintage1[1], color2=color_vintage1[2], color3=color_vintage1[3], color4=color_vintage1[4])
dhaka_aqi_rain_viz(color1=color_vintage2[1], color2=color_vintage2[2], color3=color_vintage2[3], color4=color_vintage2[4])
dhaka_aqi_rain_viz(color1=color_vintage3[1], color2=color_vintage3[2], color3=color_vintage3[3], color4=color_vintage3[4])
dhaka_aqi_rain_viz(color1=color_vintage4[1], color2=color_vintage4[2], color3=color_vintage4[3], color4=color_vintage4[4])
dhaka_aqi_rain_viz(color1=color_vintage5[1], color2=color_vintage5[2], color3=color_vintage5[3], color4=color_vintage5[4])



#it appears that higher levels of rainfall are associated with improved air quality. Specifically, for every 10mm increase in rainfall, we observed a reduction of approximately 70 in the air quality index.
# https://www.airnow.gov/international/us-embassies-and-consulates/#Bangladesh$Dhaka
# Rain data location 23.796382, 90.422507
# Opening new R google Colab link https://colab.research.google.com/#create=true&language=r, or this short URL https://colab.to/r

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(lubridate)

# Convert date to year and month
daily_avg_aqi_category_f <- daily_avg_aqi_category %>%
  mutate(year = year(date),
         month = month(date, label = TRUE, abbr = FALSE)) %>%  # Full month names
  filter(year == 2024)  # Filter for 2024 data

# Count the number of days per AQI category per month
monthly_aqi_summary <- daily_avg_aqi_category_f %>%
  group_by(month, aqi_category) %>%
  summarise(days_count = n()) %>%
  ungroup()

# Convert month to ordered factor for proper plotting
monthly_aqi_summary$month <- factor(monthly_aqi_summary$month, 
                                    levels = rev(month.name))  # Ensure correct order


# Create stacked bar plot with labels
ggplot(monthly_aqi_summary, aes(x = month, y = days_count, fill = aqi_category)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = days_count), position = position_stack(vjust = 0.5), size = 4, color = "black") +
  scale_fill_manual(values = aqi_colors) +
  coord_flip() +  # Flip for horizontal bars
  labs(title = "Monthly Distribution of AQI Categories in 2024",
       x = "Month", y = "Number of Days",
       fill = "AQI Category") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        legend.position = "bottom")  # Move legend to bottom


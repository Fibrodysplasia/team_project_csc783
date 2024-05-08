# Clean up R memory
rm(list=ls())

# R packages
list.of.packages <- c('ggplot2', 
                      'dplyr', 
                      'ggiraphExtra', 
                      'sf', 
                      'maps', 
                      'mapproj',
                      'tigris',
                      'plotly',
                      'scales',
                      'zoo',
                      'corrplot',
                      'knitr',
                      'kableExtra')

new.packages <- list.of.packages[!(list.of.packages %in%
                                     installed.packages()[,"Package"])]

if(length(new.packages)) {
  install.packages(new.packages, repo='http://cran.at.r-project.org',
                   dependencies=TRUE)
}

# Load all packages
lapply(list.of.packages, library, character.only = TRUE)

#-------------------------------------------------------------------------------

# Get ca_combined_data.rds file from github if you can
ca_combined_data <- readRDS("Data/ca_combined_data.rds")
View(ca_combined_data)

# Skip to visualizing if you loaded RDS
# Otherwise, generate a new one
# Load ca_annual_temps.rds 
# (generated in Scripts/ca_temps_bar_line.R)
ca_annual_temps <- readRDS("Data/ca_annual_temps.rds")
head(ca_annual_temps, 15)

# Load fires.rds 
fires_df <- readRDS("Data/fires.rds")
View(fires_df)

#-------------------------------------------------------------------------------
# data manipulations (only if not loading ca_combined_data.rds)

ca_fires <- fires_df %>%
  filter(state == "CA") %>%
  group_by(year) %>%
  summarise(number_of_fires = n()/100, # scaling for visual clarity
            natural_tot = sum(cause == "Natural")/100, # divide by 100 to
            human_tot = sum(cause == "Human")/100,    # match number_of_fires
            mean_acreage_burned = mean(size),
            pct_human = human_tot/number_of_fires*100)

head(ca_fires)

# join and get moving average
ca_combined_data <- left_join(ca_annual_temps, ca_fires, by = "year")
ca_combined_data <- ca_combined_data %>%
  mutate(fires_moving_avg = rollmean(number_of_fires, k = 5, fill = NA, align = 'center'),
         acres_moving_avg = rollmean(mean_acreage_burned, k = 5, fill = NA, align = 'center'),
         human_moving_avg = rollmean(human_tot, k = 5, fill = NA, align = 'center'),
         natural_moving_avg = rollmean(natural_tot, k = 5, fill = NA, align = 'center'),
         temp_moving_avg = rollmean(annual_mean, k = 5, fill = NA, align = 'center')) %>%
  rename(temp_annual_mean = annual_mean)
  
View(ca_combined_data)

# save it as rds file for later if needed
saveRDS(ca_combined_data, file = "Data/ca_combined_data.rds")

#-------------------------------------------------------------------------------
# Visualizing

combined_barplot <- ggplot(ca_combined_data, aes(x = year, y = number_of_fires, fill = temp_annual_mean)) +
  geom_col(width = 1) +
  geom_line(aes(y = human_moving_avg, group = 1, color = "Human-Caused"), size = 1) +
  geom_line(aes(y = natural_moving_avg, group = 1, color = "Natural-Caused"), size = 1) +
  geom_line(aes(y = fires_moving_avg, group = 1, color = "Number of Fires"), size = 1) +
  geom_line(aes(y = acres_moving_avg, group = 1, color = "Acreage"), size = 1) +
  labs(title = "Annual Wildfires and Temperatures",
       subtitle = "California, 1992-2020",
       x = "Year",
       y = "Number of Fires (in hundreds)",
       color = "Moving Averages") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "grey", color = "grey"),
        panel.background = element_rect(fill = "grey", color = "grey"),
        text = element_text(color = "black"),
        axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "right",
        plot.title = element_text(face = "bold", size = 14),
        axis.title = element_text(size = 12)) +
  # coord_cartesian(ylim = c(45, 175)) +
  scale_fill_gradient2(low = "yellow",
                       mid = "orange",
                       high = "red",
                       midpoint = mean(ca_combined_data$temp_annual_mean, na.rm = TRUE),
                       limits = c(min(ca_combined_data$temp_annual_mean, na.rm = TRUE),
                                  max(ca_combined_data$temp_annual_mean, na.rm = TRUE)),
                       name = "Mean Temperature (°F)") +
  scale_color_manual(values = c("Number of Fires" = "darkblue",
                                "Acreage" = "steelblue",
                                "Human-Caused" = "darkred",
                                "Natural-Caused" = "darkgreen")
                     )

print(combined_barplot)

#-------------------------------------------------------------------------------
# Correlation Testing

fires_pre_cor <- ca_combined_data %>%
  select(number_of_fires,
         temp_annual_mean,
         mean_acreage_burned,
         pct_human) %>%
  rename(annual_fires = number_of_fires,
         annual_temp = temp_annual_mean,
         annual_acreage_burned = mean_acreage_burned,
         percent_human_caused = pct_human)

fires_cor <- cor(fires_pre_cor)
corrplot(fires_cor, method = "number")

col <- colorRampPalette(c("#BB4444",
                          "#EE9988",
                          "#FFFFFF",
                          "#77AADD",
                          "#4477AA"))

corrplot(fires_cor,
         method="color",
         col=col(200),
         type="lower",
         order="hclust",
         addCoef.col="black",
         tl.col="black",
         tl.srt=45,
         diag=F)

cor_test_result <- cor.test(fires_pre_cor$annual_temp, fires_pre_cor$annual_acreage_burned)

cor_test_df <- data.frame(
  Estimate = cor_test_result$estimate,
  Statistic = cor_test_result$statistic,
  DF = cor_test_result$parameter,
  P.value = cor_test_result$p.value,
  Confidence.Interval = paste(round(cor_test_result$conf.int[1], 3), "to", round(cor_test_result$conf.int[2], 3))
)

# Use kable() for table
table_output <- kable(cor_test_df, format = "html", caption = "Temp & Acreage Correlation") %>%
  kable_styling(full_width = FALSE, position = "left", bootstrap_options = c("striped", "hover", "condensed", "bordered")) %>%
  column_spec(1, bold = TRUE, border_right = TRUE) %>%
  row_spec(0, bold = TRUE, color = "black", background = "lightgrey")

table_output

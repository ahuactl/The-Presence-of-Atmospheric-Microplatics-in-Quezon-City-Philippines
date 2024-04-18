library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)
library(ggpubr)

data <- read.csv("data.csv")

group_proportion <- function(data, group_1, group_2) {
    return (
        data %>% 
        group_by({{group_1}}, {{group_2}}) %>% 
        summarize(count = sum(!is.na(id))) %>% 
        mutate(group_sum = sum(count)) %>%
        mutate(proportion = count/group_sum)                
    )
}

total_volume <- function(n, volume_rate, hour) {
    ## volume_rate = How many m^3/hour of air in 1 unit of n?
    return (n * volume_rate * hour)
}

poisson_estimate <- function(divisions, count, direction) {
    return (divisions * poisson.test(count)$conf.int[direction]) 
}

poisson_estimates <- function(grouped_data, divisions, volume) {
    ## if direction == 1, lower interval
    ## if direction == 2, upper interval

    grouped_data %>%
    rowwise() %>%
    mutate(mean_estimate = divisions * count) %>%
    mutate(l95_estimate = poisson_estimate(divisions, count, 1)) %>%
    mutate(u95_estimate = poisson_estimate(divisions, count, 2)) %>% 
    ##
    mutate(l95_conc_estimate = l95_estimate/volume) %>% 
    mutate(mean_conc_estimate = mean_estimate/volume) %>%
    mutate(u95_conc_estimate = u95_estimate/volume)
}

# Counts
## Pooled by all
grouped_pooled_all <- group_proportion(data, NULL, NULL)

## by Shape 
grouped_shape_location <- group_proportion(data, location, shape)
grouped_pooled_shape_location <- 
    group_proportion(data, shape, NULL) %>%
    mutate(location = "Pooled")

grouped_shape_date <- group_proportion(data, date, shape)
grouped_pooled_shape_date <- 
    group_proportion(data, shape, NULL) %>%
    mutate(date = "Pooled")

## by Color
grouped_color_location <- group_proportion(data, location, color)
grouped_pooled_color_location <- 
    group_proportion(data, color, NULL) %>%
    mutate(location = "Pooled")

grouped_color_date <- group_proportion(data, date, color)
grouped_pooled_color_date <- 
    group_proportion(data, color, NULL) %>%
    mutate(date = "Pooled")

# Concentration 
## Pooled by all 
volume_pooled_all <- total_volume(6, 67.96, 24)
concentration_pooled_all <- 
poisson_estimates(grouped_pooled_all, 4, volume_pooled_all) 

## by Location
grouped_location <- group_proportion(data, location, NULL)
volume_location <- total_volume(3, 67.96, 24)
concentration_location <- 
poisson_estimates(grouped_location, 4, volume_location)

concentration_pooled_all_location <- 
    concentration_pooled_all %>% mutate(location = "Pooled")

## by Date
grouped_date <- group_proportion(data, date, NULL)
volume_date <- total_volume(2, 67.96, 24)
concentration_date <- 
poisson_estimates(grouped_date, 4, volume_date)

concentration_pooled_all_date <- 
    concentration_pooled_all %>% mutate(date = "Pooled")

## by Shape (Pooled by Date and Location)
grouped_shape <- group_proportion(data, shape, NULL)
volume_shape <- total_volume(6, 67.96, 24)
concentration_shape <- 
poisson_estimates(grouped_shape, 4, volume_shape)

# Plots

shape_location_plot <- 
    rbind(grouped_shape_location, grouped_pooled_shape_location) %>%
    ggplot(aes(x = location, y = proportion)) + 
    geom_bar(stat = "identity", 
             aes(fill = shape),
             position = position_dodge2(padding = 0, preserve = "single"),
             color = "black",
             linewidth = 0.5) + 
    theme_half_open() + 
    theme(legend.title = element_blank(),
          axis.text.x = element_text(angle = 50, hjust = 1)) +
    scale_discrete_manual(aesthetics = "fill", 
                          values = c("fiber" = "#4f93ae", 
                                     "film" = "#2a9d8f", 
                                     "fragment" = "#e9c46a", 
                                     "granule" = "#e76f51")) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(x = element_blank(), 
         y = "Proportion")

color_location_plot <- 
    rbind(grouped_color_location, grouped_pooled_color_location) %>%
    tidyr::complete(color) %>%
    ggplot(aes(x = location, y = proportion)) + 
    geom_bar(stat = "identity", 
             aes(fill = color),
             position = position_dodge2(padding = 0, preserve = "single"),
             color = "black",
             linewidth = 0.5) + 
    theme_half_open() +
    theme(legend.title = element_blank(),
          axis.text.x = element_text(angle = 50, hjust = 1)) +
    scale_discrete_manual(aesthetics = "fill", 
                          values = c("black" = "#403f3f", 
                                     "blue" = "#0d6bb2", 
                                     "gold" = "#ede029", 
                                     "green" = "#097a3a", 
                                     "translucent" = "gray")) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(x = element_blank(), 
         y = "Proportion")

shape_date_plot <- 
    rbind(grouped_shape_date, grouped_pooled_shape_date) %>%
    ggplot(aes(x = factor(date, levels = c("01/09-10/24", 
                                           "01/24-25/24", 
                                           "01/30-31/24", 
                                           "Pooled")), 
               y = proportion)) + 
    geom_bar(stat = "identity", 
             aes(fill = shape),
             position = position_dodge2(padding = 0, preserve = "single"),
             color = "black",
             linewidth = 0.5) + 
    theme_half_open() + 
    theme(legend.title = element_blank(),
          axis.text.x = element_text(angle = 50, hjust = 1)) +
    scale_discrete_manual(aesthetics = "fill", 
                          values = c("fiber" = "#4f93ae", 
                                     "film" = "#2a9d8f", 
                                     "fragment" = "#e9c46a", 
                                     "granule" = "#e76f51")) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(x = element_blank(), 
         y = "Proportion")

color_date_plot <- 
    rbind(grouped_color_date, grouped_pooled_color_date) %>%
    ggplot(aes(x = factor(date, levels = c("01/09-10/24", 
                                           "01/24-25/24", 
                                           "01/30-31/24", 
                                           "Pooled")), 
               y = proportion)) + 
    geom_bar(stat = "identity", 
             aes(fill = color),
             position = position_dodge2(padding = 0, preserve = "single"),
             color = "black",
             linewidth = 0.5) + 
    theme_half_open() + 
    theme(legend.title = element_blank(),
          axis.text.x = element_text(angle = 50, hjust = 1)) +
    scale_discrete_manual(aesthetics = "fill", 
                          values = c("black" = "#403f3f", 
                                     "blue" = "#0d6bb2", 
                                     "gold" = "#ede029", 
                                     "green" = "#097a3a", 
                                     "translucent" = "gray")) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(x = element_blank(), 
         y = "Proportion")

concentration_location_plot <-
rbind(concentration_location, concentration_pooled_all_location) %>% 
    ggplot(aes(location, mean_conc_estimate, 
               fill = location)) +
    geom_bar(stat = "identity", 
             color = "black", 
             linewidth = 0.5) + 
    geom_errorbar(width = 0.40,
                  size = 0.6,
                  aes(ymin = l95_conc_estimate, 
                      ymax = u95_conc_estimate)) + 
    theme_half_open() + 
    theme(legend.title = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) +
    scale_discrete_manual(aesthetics = "fill", 
                          values = c("Ateneo" = "#488ef0", 
                                     "East Avenue" = "#7bb0db", 
                                     "Pooled" = "#e1e5e8")) +
    labs(x = element_blank(), 
         y = parse(text = "Particles/m^3"))

concentration_date_plot <- 
rbind(concentration_date, concentration_pooled_all_date) %>% 
    ggplot(aes(x = factor(date, levels = c("01/09-10/24", "01/24-25/24", "01/30-31/24")), 
                      mean_conc_estimate, 
               fill = factor(date, 
                             levels = c("01/09-10/24", 
                                        "01/24-25/24", 
                                        "01/30-31/24", 
                                        "Pooled")))) +
    geom_bar(stat = "identity", 
             color = "black", 
             linewidth = 0.5) + 
    geom_errorbar(width = 0.40,
                  size = 0.6,
                  aes(ymin = l95_conc_estimate, 
                      ymax = u95_conc_estimate)) + 
    theme_half_open() + 
    theme(legend.title = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) +
    scale_discrete_manual(aesthetics = "fill", 
                          values = c("01/09-10/24" = "#61678B", 
                                     "01/24-25/24" = "#8488B5", 
                                     "01/30-31/24" = "#BCB9D8", 
                                     "Pooled" = "#DEDEEA")) +
    labs(x = element_blank(), 
         y = parse(text = "Particles/m^3"))

concentration_shape_plot <-
concentration_shape %>% 
    ggplot(aes(shape, mean_conc_estimate, 
               fill = shape)) +
    geom_bar(stat = "identity", 
             color = "black", 
             linewidth = 0.5) + 
    geom_errorbar(width = 0.40,
                  size = 0.6,
                  aes(ymin = l95_conc_estimate, 
                      ymax = u95_conc_estimate)) + 
    theme_half_open() + 
    theme(legend.title = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) +
     scale_discrete_manual(aesthetics = "fill", 
                          values = c("fiber" = "#4f93ae", 
                                     "film" = "#2a9d8f", 
                                     "fragment" = "#e9c46a", 
                                     "granule" = "#e76f51")) +
    labs(x = element_blank(), 
         y = parse(text = "Particles/m^3"))

# Merging Plots

shape_plot <- 
ggarrange(shape_location_plot, ggplot() + theme_void(), 
          shape_date_plot + theme(axis.title.y = element_blank()), 
          common.legend = TRUE, 
          legend = "right", 
          nrow = 1, widths = c(1, 0.05, 1), 
          align = "hv")

color_plot <- 
ggarrange(color_location_plot, ggplot() + theme_void(), 
          color_date_plot + theme(axis.title.y = element_blank()), 
          common.legend = TRUE, 
          legend = "right", 
          nrow = 1, widths = c(1, 0.05, 1), 
          align = "hv")

concentration_plot <- 
ggarrange(concentration_location_plot, ggplot() + theme_void(), 
          concentration_date_plot + theme(axis.title.y = element_blank()), 
          legend = "right", 
          nrow = 1, widths = c(1, 0.05, 1), 
          align = "hv")

forest_data <- read.csv("forest_data.csv") 
forest_data$author <- factor(forest_data$author, 
                             levels = rev(unique(forest_data$author)))
forest_data$location <- factor(forest_data$location, 
                               levels = unique(forest_data$location))



detection_rate <- 
seq(0.20, 1.80, by = 0.01) %>%
tibble(
    detection_rate = .,
    mean_pooled = (4 * concentration_pooled_all %>% pull(count)) / detection_rate,
    mean_east.ave = (4 * concentration_location %>% filter(location == "East Avenue") %>% pull(count)) / detection_rate,
    mean_ateneo = (4 * concentration_location %>% filter(location == "Ateneo") %>% pull(count)) / detection_rate
) %>% rowwise() %>%
mutate(
    min_pooled = poisson_estimate(4, concentration_pooled_all %>% pull(count), 1) / detection_rate,
    max_pooled = poisson_estimate(4, concentration_pooled_all %>% pull(count), 2) / detection_rate,
    min_ateneo = poisson_estimate(4, concentration_location %>% filter(location == "Ateneo") %>% pull(count), 1) / detection_rate,
    max_ateneo = poisson_estimate(4, concentration_location %>% filter(location == "Ateneo") %>% pull(count), 2) / detection_rate,
    min_east.ave = poisson_estimate(4, concentration_location %>% filter(location == "East Avenue") %>% pull(count), 1) / detection_rate,
    max_east.ave = poisson_estimate(4, concentration_location %>% filter(location == "East Avenue") %>% pull(count), 2) / detection_rate
) %>%
pivot_longer(cols = c(starts_with("mean"), starts_with("min"), starts_with("max")),
             names_to = c(".value", "location"),
             names_sep = "_") 
## i know the code is not pretty, but it works!

detection_probability_plot <- 
ggplot(data = NULL, aes(x = detection_rate)) + 
geom_ribbon(data = filter(detection_rate, location != "pooled"),
            aes(ymin = min / volume_location, 
                ymax = max / volume_location,
                fill = location,
                color = location),
            alpha = 0.35) + 
geom_ribbon(data = filter(detection_rate, location == "pooled"),
            aes(ymin = min / volume_pooled_all, 
                ymax = max / volume_pooled_all,
                fill = location,
                color = location), 
            alpha = 0.70) + 
theme_half_open() + 
scale_color_discrete(labels = c("ateneo" = "Ateneo", 
                                "east.ave" = "East Avenue",
                                "pooled" = "Pooled")) +
scale_fill_discrete(labels = c("ateneo" = "Ateneo", 
                                "east.ave" = "East Avenue",
                                "pooled" = "Pooled")) +     
scale_y_continuous(breaks = seq(0, 0.1, by = 0.010)) +
scale_x_continuous(labels = scales::percent_format(),
                   breaks = seq(0, 1.80, by = 0.20)) +
labs(y = parse(text = "Particles/m^3"), 
     x = "Hypothesized Accuracy in Detection",
     fill = "Location",
     color = "Location")

# Forest Plot

forest_plot <- forest_data %>% 
    ggplot(aes(y = author)) + 
    geom_errorbar(aes(xmin = lower, xmax = upper, 
                      color = location),
                  width = 0.5) +
    geom_point(aes(x = mean, color = location)) + 
    theme_half_open() +
    scale_x_log10(labels = scales::math_format(.x),
                  breaks = c(0.001, 0.01, 0.1, 1, 10, 100)) + 
    labs(x = parse(text = "Particles/m^3"),
         y = "Author", 
         color = "Location")

# Save plots

ggsave(shape_plot, file = "shape_plot.png")
ggsave(color_plot, file = "color_plot.png")
ggsave(concentration_plot, file = "concentration_plot.png")
ggsave(concentration_shape_plot, file = "concentration_shape_plot.png")
ggsave(forest_plot, file = "forest_plot.png")
ggsave(detection_probability_plot, file = "detection_probability_plot.png")

# Citation
## for R:
citation() 
version

## for packages:
citation("dplyr") ## data manipulation
citation("tidyr") ## data cleaning
citation("ggplot2") ## data visualization
citation("cowplot") ## for ggplot theme
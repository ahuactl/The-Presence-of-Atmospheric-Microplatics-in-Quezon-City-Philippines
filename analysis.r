library(dplyr)
library(ggplot2)
library(cowplot)

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

poisson_estimates <- function(grouped_data, divisions, volume) {
    ## if direction == 1, lower interval
    ## if direction == 2, upper interval
    poisson_estimate <- function(divisions, count, direction) {
        return (divisions * poisson.test(count)$conf.int[direction]) 
    }

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


grouped_pooled_all <- group_proportion(data, NULL, NULL)

# Concentration 
## Pooled by all 
volume_pooled_all <- total_volume(6, 67.96, 24)
concentration_pooled_all <- 
poisson_estimates(grouped_pooled_all, 4, volume_pooled_all) 

## by Location
grouped_location <- group_proportion(data, location, NULL)
volume_location <- total_volume(3, 67.96, 24)
concentration_location <- 
poisson_estimates(grouped_location, 4, volume)

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
    theme(legend.title = element_blank()) +
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
    theme(legend.title = element_blank()) +
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
                          values = c("01/09-10/24" = "#2d8b8c", 
                                     "01/24-25/24" = "#49a078", 
                                     "01/30-31/24" = "#9cc5a1", 
                                     "Pooled" = "#dce1de")) +
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


# ggsave(color_date_plot, file = "test.jpg")
# ggsave()
# ggsave()
# ggsave()
library(dplyr)
library(ggplot2)

data <- read.csv("data.csv")

unpooled_estimate <- data %>% 
    group_by(location) %>% 
    summarize(count = n()) %>% 
    rowwise() %>%
    mutate(mean_filter_estimate = 4 * count) %>%
    mutate(l95_filter_estimate = 4 * poisson.test(count)$conf.int[1]) %>%
    mutate(u95_filter_estimate = 4 * poisson.test(count)$conf.int[2]) %>% 
    mutate(l95_concentration_estimate = l95_filter_estimate/(3 * 67.96 * 24)) %>% 
    mutate(mean_concentration_estimate = mean_filter_estimate/(3 * 67.96 * 24)) %>%
    mutate(u95_concentration_estimate = u95_filter_estimate/(3 * 67.96 * 24))

pooled_estimate <- data %>% 
    summarize(count = n()) %>%
    mutate(location = "Pooled") %>%
    rowwise() %>%
    mutate(mean_filter_estimate = 4 * count) %>%
    mutate(l95_filter_estimate = 4 * poisson.test(count)$conf.int[1]) %>%
    mutate(u95_filter_estimate = 4 * poisson.test(count)$conf.int[2]) %>% 
    mutate(l95_concentration_estimate = l95_filter_estimate/(6 * 67.96 * 24)) %>% 
    mutate(mean_concentration_estimate = mean_filter_estimate/(6 * 67.96 * 24)) %>%
    mutate(u95_concentration_estimate = u95_filter_estimate/(6 * 67.96 * 24))

concentration_plot <-
rbind(unpooled_estimate, pooled_estimate) %>% 
    ggplot(aes(location, mean_concentration_estimate, 
               fill = location)) +
    geom_bar(stat = "identity", 
             color = "black", 
             linewidth = 0.5) + 
    geom_errorbar(width = 0.40,
                  aes(ymin = l95_concentration_estimate, 
                      ymax = u95_concentration_estimate)) + 
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

pooled_shape_data <- data %>% 
    group_by(shape) %>% 
    summarize(count = n()) %>% 
    mutate(proportion = count/sum(count)) %>%
    mutate(location = "Pooled")

unpooled_shape_data <- data %>% 
    group_by(location, shape) %>% 
    summarize(count = n()) %>% 
    mutate(proportion = count/sum(count))

shape_plot <- 
    rbind(unpooled_shape_data, pooled_shape_data) %>%
    ggplot(aes(x = location, y = proportion)) + 
    geom_bar(stat = "identity", 
             aes(fill = shape),
             position = position_dodge(),
             color = "black",
             linewidth = 0.5) + 
    theme_half_open() + 
    scale_discrete_manual(aesthetics = "fill", 
                          values = c("fiber" = "#264653", 
                                     "film" = "#2a9d8f", 
                                     "fragment" = "#e9c46a", 
                                     "granule" = "#e76f51")) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(x = element_blank(), 
         y = "Proportion")

data %>% 
    group_by(location, shape, color) %>% 
    summarize(count = n()) %>% 
    ggplot(aes(x = location, y = count, group = shape)) +
    geom_bar(stat = "identity", 
             aes(fill = color, linetype = shape),
             position = position_dodge2(),
             color = "black",
             linewidth = 0.5)



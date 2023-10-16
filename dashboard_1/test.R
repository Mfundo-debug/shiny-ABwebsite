library(tidyverse)
library(magrittr)
library(car)

data <- read_csv('website_ab_test.csv')
metrics <- c('Click Through Rate', 'Conversion Rate', 'Bounce Rate')
themes <- c('Light Theme', 'Dark Theme')

# loop through each metric
for (metric in metrics) {
  cat(paste0('Metric: ', metric, '\n'))
  
  # loop through each theme
  for (i in 1:length(themes)) {
    for (j in (i+1):length(themes)) {
      theme1 <- themes[i]
      theme2 <- themes[j]
      
      # calculate the mean and standard deviation for each theme
      mean1 <- mean(data %>% filter(Theme == theme1) %>% pull({{metric}}))
      mean2 <- mean(data %>% filter(Theme == theme2) %>% pull({{metric}}))
      std1 <- sd(data %>% filter(Theme == theme1) %>% pull({{metric}}))
      std2 <- sd(data %>% filter(Theme == theme2) %>% pull({{metric}}))
      
      # calculate the t-statistic and p-value for the two groups
      n1 <- length(data %>% filter(Theme == theme1) %>% pull({{metric}}))
      n2 <- length(data %>% filter(Theme == theme2) %>% pull({{metric}}))
      dof <- n1 + n2 - 2
      pooled_std <- sqrt(((n1-1)*(std1^2) + (n2-1)*(std2^2))/dof)
      t_stat <- (mean1-mean2)/ (pooled_std * sqrt(1/n1 + 1/n2))
      p_val <- 2 * (1 - t.test(data %>% filter(Theme == theme1) %>% pull({{metric}}),
                                data %>% filter(Theme == theme2) %>% pull({{metric}}),
                                var.equal = TRUE)$p.value)
      
      # calculate the effect size (Cohen's d)
      effect_size <- (mean1-mean2)/ pooled_std
      
      # print the results for the two groups
      cat(paste0(theme1, ' vs ', theme2, '\n'))
      cat(paste0('Mean: ', theme1, ': ', mean1, '\n'))
      cat(paste0('Mean: ', theme2, ': ', mean2, '\n'))
      cat(paste0('Standard Deviation: ', theme1, ': ', std1, '\n'))
      cat(paste0('Standard Deviation: ', theme2, ': ', std2, '\n'))
      cat(paste0('T-statistic: ', t_stat, '\n'))
      cat(paste0('P-value: ', p_val, '\n'))
      cat(paste0("Effect Size (Cohen's d): ", effect_size, '\n\n'))
    }
  }
}
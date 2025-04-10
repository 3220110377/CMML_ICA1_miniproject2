library(tidyverse)
library(ggpubr)
library(rstatix)
library(ggplot2)

default_before <- read_csv("res/default_before/L/allresult_processed.csv")
default_after <- read_csv("res/default_after/L/allresult_processed.csv")
wsls_before <- read_csv("res/wsls_before/L/allresult_processed.csv")
wsls_after <- read_csv("res/wsls_after/L/allresult_processed.csv")
highpayoff <- read_csv("high_payoff.csv")
highpayoff <- highpayoff[-c(3,5)]

data_frames <- list(
  default_before = default_before,
  default_after = default_after,
  wsls_before = wsls_before,
  wsls_after = wsls_after
)

processed_data_frames <- lapply(names(data_frames), function(name) {
  df <- data_frames[[name]]
  
  # 计算 RT_total 和 payoff_avg
  processed_df <- df %>%
    mutate(RT_total = RT_1 + RT_2 + RT_3 + RT_4) %>%
    group_by(Subject) %>%
    summarise(
      RT_total_avg = mean(RT_total),
      performance_avg = mean(performance),
    ) %>%
    ungroup()
  
  if (name %in% c("default_before", "default_after")) {
    processed_df <- processed_df %>%
      mutate(condition = "default", phase = ifelse(name %in% c("default_before"), "before", "after"))
  } else if (name %in% c("wsls_before", "wsls_after")) {
    processed_df <- processed_df %>%
      mutate(condition = "wsls", phase = ifelse(name %in% c("wsls_before"), "before", "after"))
  }

  return(processed_df)
})

combined_data <- bind_rows(processed_data_frames)
combined_data <- rbind(combined_data,highpayoff)

combined_data <- combined_data %>%
  mutate(
    x_label = paste(phase, condition)
  )

x_order <- c(
  "before default","before wsls","before high_payoff",
  "after default", "after wsls", "after high_payoff"
)

combined_data <- combined_data %>%
  mutate(x_label = factor(x_label, levels = x_order))

calculate_y_range <- function(data, column) {
  qnt <- quantile(data[[column]], probs = c(0.02, 0.98))
  y_min <- qnt[1]
  y_max <- qnt[2]
  return(c(y_min, y_max))
}

rt_y_range <- calculate_y_range(combined_data, "RT_total_avg")

ggplot(combined_data, aes(x = x_label, y = RT_total_avg)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, aes(color = condition)) +
  coord_cartesian(ylim = rt_y_range) +
  labs(title = "RT_total_avg by PCondition, and Phase", x = "Phase and Condition", y = "RT_total_avg")


rt_y_range <- calculate_y_range(combined_data, "performance_avg")

ggplot(combined_data, aes(x = x_label, y = performance_avg)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, aes(color = condition)) +
  coord_cartesian(ylim = rt_y_range) +
  labs(title = "performance_avg by PCondition, and Phase", x = "Phase and Condition", y = "performance_avg")


normality_results <- combined_data %>%
  group_by(phase, condition) %>%
  summarise(normality_p = shapiro.test(performance_avg)$p.value) %>%
  ungroup()
print("正态性检验结果：")
print(normality_results)
# >0.05则具有正态性

levene_results <- combined_data %>%
  group_by(phase) %>%
  levene_test(performance_avg ~ as.factor(condition)) %>%
  ungroup()
print("方差齐性检验结果：")
print(levene_results)
# >0.05则具有方差齐性

normality_results <- combined_data %>%
  group_by(phase, condition) %>%
  summarise(normality_p = shapiro.test(RT_total_avg)$p.value) %>%
  ungroup()
print("正态性检验结果：")
print(normality_results)
# >0.05则具有正态性

levene_results <- combined_data %>%
  group_by(phase) %>%
  levene_test(RT_total_avg ~ as.factor(condition)) %>%
  ungroup()
print("方差齐性检验结果：")
print(levene_results)
# >0.05则具有方差齐性

strategy <- c(
  "default","wsls","high_payoff"
)

combined_data <- combined_data %>%
  mutate(strategy = factor(combined_data$condition, levels = strategy))


rt_plot <- ggplot(combined_data, aes(x = strategy, y = RT_total_avg, fill = strategy)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, size = 1, alpha = 0.7) +
  facet_grid(phase ~ .) +
  stat_compare_means(method = "kruskal.test", label = "p.signif", 
                     label.y = max(combined_data$RT_total_avg, na.rm = TRUE)*1.05) +
  labs(title = "RT_total_avg Differences between Strategies",
       x = "Strategy", y = "RT_total_avg") +
  theme_minimal()

print(rt_plot)

rt_plot <- ggplot(combined_data, aes(x = strategy, y = performance_avg, fill = strategy)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, size = 1, alpha = 0.7) +
  facet_grid(phase ~ .) +
  stat_compare_means(method = "kruskal.test", label = "p.signif", 
                     label.y = max(combined_data$performance_avg, na.rm = TRUE)*1.05) +
  labs(title = "performance_avg Differences between Strategies",
       x = "Strategy", y = "performance_avg") +
  theme_minimal()

print(rt_plot)

comparisons <- list(
  c("default","high_payoff"),
  c("default","wsls")
)


payoff_plot <- ggplot(combined_data, aes(x = strategy, y = RT_total_avg, fill = strategy)) +
  geom_boxplot() +
  facet_grid(phase ~ .) +
  geom_jitter(width = 0.2, size = 1, alpha = 0.7) +
  stat_compare_means(comparisons = comparisons,
                     method = "wilcox.test",
                     label = "p.signif",
                     label.y = max(combined_data$RT_total_avg, na.rm = TRUE) * 1.05) +
  labs(title = "RT_total_avg Differences",
       x = "condition",
       y = "RT_total_avg") +
  theme_minimal()

print(payoff_plot)

payoff_plot <- ggplot(combined_data, aes(x = strategy, y = performance_avg, fill = strategy)) +
  geom_boxplot() +
  facet_grid(phase ~ .) +
  geom_jitter(width = 0.2, size = 1, alpha = 0.7) +
  stat_compare_means(comparisons = comparisons,
                     method = "wilcox.test",
                     label = "p.signif",
                     label.y = max(combined_data$performance_avg, na.rm = TRUE) * 1.05) +
  labs(title = "performance_avg Differences",
       x = "condition",
       y = "performance_avg") +
  theme_minimal()

print(payoff_plot)


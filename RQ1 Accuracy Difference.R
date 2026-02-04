# ============================
# RQ1: Accuracy (plot + tests)
# ============================
# - Correctness recoded: Correct=1, PartiallyCorrect=0.5, Incorrect=0
# - Tests: paired Wilcoxon per task (Holm-corrected) + overall paired Wilcoxon

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

AccuracyRaw <- read.csv2("Data/AccuracyRaw.csv", header = T)

# Settings
task_order <- c(
  "Choropleth1","Choropleth2","Choropleth3","Choropleth4","Choropleth5",
  "Point1","Point2","Point3","Point4"
)

device_cols <- c("Desktop" = "lightblue", "Smartphone" = "orange")

# Data preparation
acc_clean <- AccuracyRaw %>%
  mutate(
    Device = str_to_title(device),  
    task   = factor(Task2, levels = task_order),
    acc_score = case_when(
      Correctness == "Correct" ~ 1,
      Correctness == "PartiallyCorrect" ~ 0.5,
      Correctness == "Incorrect" ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%
  group_by(user_id, Device, task) %>%     # removes duplicates (P27, Point3, Smartphone & P43, Point2, Desktop)
  summarise(acc_score = mean(acc_score), .groups="drop")

# Distribution of Correctness depending on Device and Task
table(acc_clean$acc_score, acc_clean$Device, acc_clean$task)

# Plot data (mean accuracy in % per task × device)
acc_summary <- acc_clean %>%
  group_by(task, Device) %>%
  summarise(Accuracy = mean(acc_score) * 100, .groups = "drop") # means at the participants level

p_acc <- ggplot(acc_summary, aes(x = task, y = Accuracy, color = Device)) +
  geom_line(aes(group = task), color = "grey60", linewidth = 0.8) +
  geom_point(size = 3) +
  geom_vline(xintercept = 5.5, color = "grey50") +
  scale_color_manual(values = device_cols, name = "Device") +
  scale_y_continuous(
    limits = c(0, 110),
    breaks = seq(0, 100, 20),
    labels = function(x) paste0(x, "%")
  ) +
  labs(x = "Task", y = "Accuracy") +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank(),
    legend.position = "right"
  )

print(p_acc)

# Paired table for tests
acc_paired <- acc_clean %>%
  pivot_wider(names_from = Device, values_from = acc_score) %>%
  mutate(diff = Desktop - Smartphone)

median(acc_paired$diff)

# Per-task paired Wilcoxon + Holm + effect size r
wilcox_r <- function(p_value, n_pairs){
  p_value <- max(min(p_value, 1 - 1e-16), 1e-16)
  z <- qnorm(p_value / 2, lower.tail = FALSE)  # standardized z
  z / sqrt(n_pairs)
}

rq1_task_results <- acc_paired %>%
  group_by(task) %>%
  summarise(
    n_pairs = sum(!is.na(diff)),
    median_diff = median(diff, na.rm = TRUE),     # >0 desktop higher; <0 smartphone higher
    q1 = quantile(diff, 0.25, na.rm = TRUE),
    q3 = quantile(diff, 0.75, na.rm = TRUE),
    iqr_diff = IQR(diff, na.rm = TRUE),
    test = list(wilcox.test(Desktop, Smartphone, paired = TRUE, exact = FALSE)),
    .groups = "drop"
  ) %>%
  mutate(
    V = sapply(test, \(x) unname(x$statistic)),
    p_value = sapply(test, \(x) x$p.value),
    p_holm  = p.adjust(p_value, method = "holm"),
    r = mapply(wilcox_r, p_value, n_pairs)
  ) %>%
  select(task, n_pairs, median_diff, q1, q3, iqr_diff, V, p_value, p_holm, r) %>%
  arrange(task)

print(rq1_task_results)

# Overall paired Wilcoxon on participant mean accuracy
acc_overall <- acc_paired %>%
  group_by(user_id) %>%
  summarise(
    Desktop_mean = mean(Desktop, na.rm = TRUE),
    Smartphone_mean = mean(Smartphone, na.rm = TRUE),
    diff_mean = Desktop_mean - Smartphone_mean,
    .groups = "drop"
  )

overall_test <- wilcox.test(acc_overall$Desktop_mean, acc_overall$Smartphone_mean,
                            paired = TRUE, exact = FALSE)

overall_test

wilcox_r(overall_test$p.value, length(acc_overall$Desktop_mean))

# Overall paired Wilcoxon on participant mean accuracy separatelly for Choropleth and Points

acc_paired$task_type <- ifelse(grepl("^C", as.character(acc_paired$task)), "Choropleth", "Points")

acc_overall2 <- acc_paired %>%
  group_by(user_id, task_type) %>%
  summarise(
    Desktop_mean = mean(Desktop, na.rm = TRUE),
    Smartphone_mean = mean(Smartphone, na.rm = TRUE),
    diff_mean = Desktop_mean - Smartphone_mean,
    .groups = "drop"
  )

overall_test2 <- acc_overall2 %>%
  group_by(task_type) %>%
  summarise(
  n_pairs = sum(!is.na(diff_mean)),
  median_diff = median(diff_mean, na.rm = TRUE),
  test = list(wilcox.test(Desktop_mean, Smartphone_mean,
                            paired = TRUE, exact = FALSE)),
  .groups = "drop"
  ) %>%
  mutate(
    V = sapply(test, \(x) unname(x$statistic)),
    p_value = sapply(test, \(x) x$p.value),
    p_holm  = p.adjust(p_value, method = "holm"),
    r = mapply(wilcox_r, p_value, n_pairs)
  ) %>%
  select(task_type, n_pairs, median_diff, V, p_value, p_holm, r) %>%
  arrange(task_type)

overall_test2

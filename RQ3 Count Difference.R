# =====================================================
# RQ3: Actions count differences (Desktop − Smartphone)
# =====================================================
# - Tests: Per-task Wilcoxon tests

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

Count <- read.csv2("Data/Count.csv", header = T, dec = ",")

# Setings

id_cols   <- c("Participant", "Category", "Device", "Order")
task_cols <- setdiff(names(Count), id_cols)

task_order <- c(
  "Choropleth1","Choropleth2","Choropleth3","Choropleth4","Choropleth5",
  "Point1","Point2","Point3","Point4"
)

group_colors <- c(
  "01-Beginner"     = "#2E8B57",
  "02-Intermediate" = "#F0C808",
  "03-Advanced"     = "#C1121F"
)

# Paired differences

Count_diff <- Count %>%
  mutate(Device = str_to_title(Device)) %>%
  pivot_longer(
    cols = all_of(task_cols),
    names_to = "task",
    values_to = "actions"
  ) %>%
  pivot_wider(names_from = Device, values_from = actions) %>%
  filter(!is.na(Desktop) & !is.na(Smartphone)) %>%
  mutate(
    actions_diff = Desktop - Smartphone,
    task = factor(task, levels = task_order)
  )

# Boxplots

p2 <- ggplot(Count_diff, aes(x = task, y = actions_diff)) +
  geom_boxplot(
    width = 0.6,
    fill = "grey90",
    color = "black",
    outlier.shape = NA
  ) +
  geom_jitter(
    aes(color = Category),
    width = 0.15,
    alpha = 0.6,
    size = 1.6
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 5.5, color = "grey50") +  # Choropleth | Point separator
  scale_color_manual(values = group_colors, name = "Expertise level") +
  labs(
    x = "Task",
    y = "Action count difference (Desktop − Smartphone)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank(),
    legend.position = "right"
  )

print(p2)

# Per-task paired Wilcoxon + Holm + effect size r

wilcox_r <- function(p_value, n_pairs){
  p_value <- max(min(p_value, 1 - 1e-16), 1e-16)
  z <- qnorm(p_value / 2, lower.tail = FALSE)  # standardized z
  z / sqrt(n_pairs)
}

results <- Count_diff %>%
  group_by(task) %>%
  summarise(
    n_pairs = sum(!is.na(actions_diff)),
    median_diff = median(actions_diff, na.rm = TRUE),
    q1 = quantile(actions_diff, 0.25, na.rm = TRUE),
    q3 = quantile(actions_diff, 0.75, na.rm = TRUE),
    iqr_diff = IQR(actions_diff, na.rm = TRUE),
    test = list(wilcox.test(actions_diff, mu = 0, exact = FALSE)),
    .groups = "drop"
  ) %>%
  mutate(
    V = sapply(test, \(x) unname(x$statistic)),
    p_value = sapply(test, \(x) x$p.value),
    p_holm = p.adjust(p_value, method = "holm"),
    r = mapply(wilcox_r, p_value, n_pairs)
  ) %>%
  select(task, n_pairs, median_diff, q1, q3, iqr_diff, V, p_value, p_holm, r) %>%
  arrange(task)

results

# Per-task: Wilcoxon rank-sum (between-subject) on actions_diff ~ Order

order_results <- Count_diff %>%
  group_by(task) %>%
  summarise(
    n = sum(!is.na(actions_diff)),
    n_DS = sum(Order == "Desktop-Smartphone"),
    n_SD = sum(Order == "Smartphone-Desktop"),
    median_DS = median(actions_diff[Order == "Desktop-Smartphone"], na.rm = TRUE),
    median_SD = median(actions_diff[Order == "Smartphone-Desktop"], na.rm = TRUE),
    test = list(wilcox.test(actions_diff ~ Order, exact = FALSE)),
    .groups = "drop"
  ) %>%
  mutate(
    W = sapply(test, \(x) unname(x$statistic)),
    p_value = sapply(test, \(x) x$p.value),
    p_holm  = p.adjust(p_value, method = "holm")
  ) %>%
  select(task, n, n_DS, n_SD, median_DS, median_SD, W, p_value, p_holm) %>%
  arrange(task)

order_results

# Participant-level mean difference across all tasks vs Order

order_overall <- Count_diff %>%
  group_by(Participant, Order) %>%
  summarise(mean_diff = mean(actions_diff, na.rm = TRUE), .groups = "drop")

overall_order_test <- wilcox.test(mean_diff ~ Order, data = order_overall, exact = FALSE)
overall_order_test

qnorm(overall_order_test$p.value/ 2, lower.tail = FALSE)/sqrt(sum(!is.na(order_overall$mean_diff)))

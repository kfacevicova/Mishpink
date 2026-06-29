# ===============================================
# RQ2: Time differences (Desktop − Smartphone)
# ===============================================
# - Tests: Per-task Wilcoxon tests

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

Time <- read.csv2("Data/Time.csv", header = T, dec = ",")

# Setings

id_cols   <- c("Participant", "Category", "Device")
task_cols <- setdiff(names(Time), id_cols)

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

Time_diff <- Time %>%
  mutate(Device = str_to_title(Device)) %>%
  pivot_longer(
    cols = all_of(task_cols),
    names_to = "task",
    values_to = "time"
  ) %>%
  pivot_wider(names_from = Device, values_from = time) %>%
  filter(!is.na(Desktop) & !is.na(Smartphone)) %>%
  mutate(
    time_diff = Desktop - Smartphone,
    task = factor(task, levels = task_order)
  )

# Boxplots

p <- ggplot(Time_diff, aes(x = task, y = time_diff)) +
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
    y = "Time difference (Desktop − Smartphone) [s]"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank(),
    legend.position = "right"
  )

print(p)

# Per-task paired Wilcoxon + Holm + effect size r
wilcox_r <- function(p_value, n_pairs){
  p_value <- max(min(p_value, 1 - 1e-16), 1e-16)
  z <- qnorm(p_value / 2, lower.tail = FALSE)  # standardized z
  z / sqrt(n_pairs)
}

# Rank-biserial correlation
wilcox_rbc <- function(x, y){
  d <- x - y
  d <- d[d != 0]
  ranks <- rank(abs(d))
  W_plus  <- sum(ranks[d > 0])
  W_minus <- sum(ranks[d < 0])
  (W_plus - W_minus) / (W_plus + W_minus)
}

results <- Time_diff %>%
  group_by(task) %>%
  summarise(
    n_pairs = sum(!is.na(time_diff)),
    median_diff = median(time_diff, na.rm = TRUE),
    q1 = quantile(time_diff, 0.25, na.rm = TRUE),
    q3 = quantile(time_diff, 0.75, na.rm = TRUE),
    iqr_diff = IQR(time_diff, na.rm = TRUE),
    test = list(wilcox.test(time_diff, mu = 0, exact = FALSE)),
    rbc = wilcox_rbc(Desktop, Smartphone),
    .groups = "drop"
  ) %>%
  mutate(
    V = sapply(test, \(x) unname(x$statistic)),
    p_value = sapply(test, \(x) x$p.value),
    p_holm = p.adjust(p_value, method = "holm"),
    r = mapply(wilcox_r, p_value, n_pairs)
  ) %>%
  select(task, n_pairs, median_diff, q1, q3, iqr_diff, V, p_value, p_holm, r, rbc) %>%
  arrange(task)

results


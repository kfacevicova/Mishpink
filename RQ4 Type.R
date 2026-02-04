# ================================
# RQ4: Structure of interactions
# ================================
# - Visualisation, mean distribution

library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(ggplot2)
library(scales)
library(compositions)
library(ggtern)
library(tidyverse)

Type <- read.csv2("Data/Type.csv", header = F)

# names of variables are in first two rows
header_task <- as.character(Type[1, ])
header_inter <- as.character(Type[2, ])

# Build new column names

new_names <- header_task
new_names[1:4] <- c("Participant", "Category", "Device", "Order")

for (i in 5:length(new_names)) {
  task <- str_trim(header_task[i])
  inter <- str_trim(header_inter[i])
  
  new_names[i] <- paste0(task, "_", inter)
}

colnames(Type) <- new_names
Type_clean <- Type[-c(1, 2), ]


# data transformation

id_cols <- c("Participant", "Category", "Device", "Order")

Type_long <- Type_clean %>%
  mutate(
    Device = str_to_title(Device),
    Order  = factor(Order, levels = c("Desktop-Smartphone", "Smartphone-Desktop"))
  ) %>%
  pivot_longer(
    cols = -all_of(id_cols),
    names_to = "task_interaction",
    values_to = "count"
  ) %>%
  mutate(
    count = parse_number(as.character(count))
  ) %>%
  #filter(!is.na(count) & count > 0) %>%
  separate(
    task_interaction,
    into = c("task", "interaction"),
    sep = "_",
    extra = "merge",
    fill = "right"
  ) %>%
  mutate(
    task = factor(task, levels = c(
      "Choropleth1","Choropleth2","Choropleth3","Choropleth4","Choropleth5",
      "Point1","Point2","Point3","Point4"
    )),
    interaction = str_squish(interaction),
    task_type = ifelse(grepl("^Choropleth", as.character(task)),
                       "Choropleth", "Point")
  )

Type_long$count[is.na(Type_long$count)] <- 0

# Normalize interaction counts to proportions

Type_prop <- Type_long %>%
  group_by(Participant, Device, task, task_type) %>%
  mutate(
    total_actions = sum(count),
    prop = count / total_actions
  ) %>%
  ungroup()

# boxplots with distribution of proportion of interactions

p <- ggplot(
  Type_prop,
  aes(x = interaction, y = prop, fill = Device)
) +
  geom_boxplot(
    position = position_dodge(width = 0.75),
    outlier.alpha = 0.3
  ) +
  facet_grid(
    task_type ~ Order
  ) +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(
    values = c(
      "Desktop" = "lightblue",
      "Smartphone" = "orange"
    )
  ) +
  labs(
    x = "Interaction type",
    y = "Proportion of interactions",
    fill = "Device"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank(),
    legend.position = "right",
    strip.text = element_text(face = "bold")
  )

print(p)


# stacked plot with mean distributions

interaction_order <- c(
  "movestart",
  "zoom in",
  "zoom out",
  "popupopen",
  "hide layer",
  "show layer",
  "legend opened",
  "legend closed"
)

interaction_colors <- c(
  "movestart"      = "#4E79A7",
  "zoom in"        = "#59A14F",
  "zoom out"       = "#8CD17D",
  "popupopen"      = "#F28E2B",
  "hide layer"     = "#E15759",
  "show layer"     = "#FF9DA7",
  "legend opened"  = "#9C755F",
  "legend closed"  = "#BAB0AC"
)

interaction_profile_task <- Type_long %>%
  mutate(
    interaction = str_squish(interaction),
    interaction = factor(interaction, levels = interaction_order)
  ) %>%
  group_by(task, Device, interaction) %>%
  summarise(total = sum(count), .groups = "drop") %>%
  group_by(task, Device) %>%
  mutate(prop = total / sum(total))

# impute zero counts with 0.5
Type_long_imputed <- Type_long
Type_long_imputed$count[Type_long_imputed$count == 0] <- 0.5

Type_prop_imputed <- Type_long_imputed %>%
  group_by(Participant, Device, task, task_type) %>%
  mutate(
    total_actions = sum(count),
    prop = count / total_actions
  ) %>%
  ungroup()

# mean distributions

mean_distributions <- Type_prop_imputed %>%
  mutate(
    interaction = str_squish(interaction),
    interaction = factor(interaction, levels = interaction_order)
  ) %>%
  group_by(task, Device, interaction) %>%
  summarise(gmean = geometricmean(prop), .groups = "drop") %>%
  group_by(task, Device) %>%
  mutate(mean_prop = gmean / sum(gmean))

p3 <- ggplot(
  mean_distributions,
  aes(x = Device, y = mean_prop, fill = interaction)
) +
  geom_col(width = 0.7) +
  facet_wrap(~ task, ncol = 5) +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(
    values = interaction_colors,
    breaks = interaction_order,       # controls legend order
    drop = FALSE,
    name = "Interaction type"
  ) +
  labs(
    x = "Device",
    y = "Proportion of interactions"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank(),
    legend.position = "right",
    strip.text = element_text(face = "bold")
  )

print(p3)

# ternary diagram of selected interactions

data <- read.csv2("Data/Ternary.csv")

head(data)

data <- data %>% mutate(Device = str_to_title(device)) %>% 
  mutate(Device = factor(Device, levels = c("Desktop", "Smartphone")))
  
subdata <- data[data$event_name %in% c("hide layer", "legend closed", "legend opened", "zoom out",
                                       "movestart", "popupopen", "show layer", "zoom in"), ]

subdata$event_name <- as.factor(subdata$event_name)
subdata$user_id <- as.factor(subdata$user_id)

# Choropleth
subdata_choro <- subdata[subdata$TaskType == "Choropleth" & 
                           subdata$event_name %in% c("popupopen", "zoom in", "zoom out", "movestart"), ]
subdata_choro$event_name <- droplevels(subdata_choro$event_name)
choro_tab <- table(subdata_choro$user_id, subdata_choro$event_name, subdata_choro$Device)
choro_df <- data.frame(movestart = choro_tab[,1,],
                       popupopen = choro_tab[,2,],
                       zoom = choro_tab[,3,] + choro_tab[,4,])
colnames(choro_df)                       
choro_df <- choro_df[, c("movestart.Var1","movestart.Var2","movestart.Freq", "popupopen.Freq", "zoom.Freq")]
colnames(choro_df) <- c("user_id", "Device", "movestart", "popupopen", "zoom")
head(choro_df)

# Point
subdata_point <- subdata[subdata$TaskType == "Point" & 
                           subdata$event_name %in% c("legend opened", "zoom in", "zoom out", "movestart"), ]
subdata_point$event_name <- droplevels(subdata_point$event_name)
point_tab <- table(subdata_point$user_id, subdata_point$event_name, subdata_point$Device)
point_df <- data.frame(legend = point_tab[,1,],
                       movestart = point_tab[,2,],
                       zoom = point_tab[,3,] + point_tab[,4,])
colnames(point_df)                       
point_df <- point_df[, c("legend.Var1","legend.Var2","legend.Freq", "movestart.Freq", "zoom.Freq")]
colnames(point_df) <- c("user_id", "Device", "legend", "movestart", "zoom")
head(point_df)

sum(point_df$hidelayer == 0)

# normalize counts to proportions

df_norm <- choro_df %>%
  rowwise() %>%
  mutate(total = movestart + popupopen + zoom,
         movestart = movestart / total,
         popupopen = popupopen / total,
         zoom = zoom / total) %>%
  ungroup() %>%
  select(-total)
df_norm <- df_norm %>% left_join(data[, c("user_id", "Group")], by = "user_id",
                                 multiple = "first")

df_norm_point <- point_df %>%
  rowwise() %>%
  mutate(total = movestart + legend + zoom,
         movestart = movestart / total,
         legend = legend / total,
         zoom = zoom / total) %>%
  ungroup() %>%
  select(-total)
df_norm_point <- df_norm_point %>% left_join(data[, c("user_id", "Group")], by = "user_id",
                                             multiple = "first")

df_wide <- df_norm %>%
  pivot_wider(
    id_cols = user_id,
    names_from = Device,
    values_from = c(movestart, popupopen, zoom)
  )

df_wide_point <- df_norm_point %>%
  pivot_wider(
    id_cols = user_id,
    names_from = Device,
    values_from = c(movestart, legend, zoom)
  )

ggtern() +
  geom_segment(
    data = df_wide,
    aes(
      x = movestart_Desktop, y = popupopen_Desktop, z = zoom_Desktop,
      xend = movestart_Smartphone, yend = popupopen_Smartphone, zend = zoom_Smartphone
    ),
    arrow = arrow(length = unit(1.5, "mm")), col = "grey", alpha = 0.5, lwd = 0.2
  ) +
  geom_point(data = df_norm,
             aes(x = movestart, y = popupopen, z = zoom,
                 color = Device, shape = Group),
             size = 3, alpha = 0.9) +
  scale_color_manual(
               values = c(
                 "Desktop" = "lightblue",
                 "Smartphone" = "orange"
               )
             ) +
  theme_bw() +
  theme_showarrows() +
  labs(x = "Move", y = "PopUp", z = "Zoom"
  )

ggtern() +
  geom_segment(
    data = df_wide_point,
    aes(
      x = movestart_Desktop, y = legend_Desktop, z = zoom_Desktop,
      xend = movestart_Smartphone, yend = legend_Smartphone, zend = zoom_Smartphone
    ),
    arrow = arrow(length = unit(1.5, "mm")), col = "grey", alpha = 0.5, lwd = 0.2
  ) +
  geom_point(data = df_norm_point,
             aes(x = movestart, y = legend, z = zoom,
                 color = Device, shape = Group),
             size = 3, alpha = 0.9) +
  scale_color_manual(
    values = c(
      "Desktop" = "lightblue",
      "Smartphone" = "orange"
    )
  ) +
  theme_bw() +
  theme_showarrows() +
  labs(
    x = "Move", y = "Legend", z = "Zoom"
  )


# ====================================
# RQ5: Comparison of expertise groups
# ===================================

library(dplyr)
library(ggplot2)
library(scales)

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

# Choropleth - Show layer, Open legend, Popup

choro_data <- Type_long %>% subset(task_type == "Choropleth" & interaction %in% c("show layer", "legend opened", "popupopen"))
choro_data <- choro_data %>% mutate(interaction = factor(interaction, levels = c("show layer", "legend opened", "popupopen"), 
                                                         labels = c("Show layer", "Open legend", "Popup")))
choro_aggreg <- choro_data %>% group_by(Participant, Device, interaction, Category) %>% summarise(sum = sum(count)) %>% ungroup()

# percentage of participants using the individual interactions

table <- tapply(choro_aggreg$sum, list(choro_aggreg$Category, choro_aggreg$Device, choro_aggreg$interaction), function(x) mean(x!=0))
table_long <- as.data.frame(as.table(table))
colnames(table_long) <- c("Category", "Device", "interaction", "Value")

table_long <- table_long %>%
  mutate(
    Device = factor(Device, levels = c("Desktop", "Smartphone")),
    Category = factor(
      Category,
      levels = c("01-Beginner", "02-Intermediate", "03-Advanced")
    ),
    interaction = factor(interaction, levels = c("Show layer", "Open legend", "Popup"))
  ) %>%
  arrange(interaction, Category, Device)

p_choropleth <- ggplot(table_long, aes(x = Category, y = Value, fill = Device)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  facet_wrap(~ interaction, nrow = 1) +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = c("Desktop" = "lightblue", "Smartphone" = "orange")) +
  labs(
    x = "Expertise level",
    y = "Average proportion of users",
    fill = "Device"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major.x = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.position = "right"
  )

p_choropleth

# Points - Open legend, Hide layer

point_data <- Type_long %>% subset(task_type == "Point" & interaction %in% c("hide layer", "legend opened"))
point_data <- point_data %>% mutate(interaction = factor(interaction, levels = c("legend opened", "hide layer"), 
                                                         labels = c("Open", "Hide")))
point_aggreg <- point_data %>% group_by(Participant, Device, interaction, Category) %>% summarise(sum = sum(count)) %>% ungroup()

# percentage of participants using the individual interactions

table_point <- tapply(point_aggreg$sum, list(point_aggreg$Category, point_aggreg$Device, point_aggreg$interaction), function(x) mean(x!=0))
table_point_long <- as.data.frame(as.table(table_point))
colnames(table_point_long) <- c("Category", "Device", "interaction", "Value")

table_point_long <- table_point_long %>%
  mutate(
    Device = factor(Device, levels = c("Desktop", "Smartphone")),
    Category = factor(
      Category,
      levels = c("01-Beginner", "02-Intermediate", "03-Advanced")
    ),
    interaction = factor(interaction, levels = c("Open", "Hide"))
  ) %>%
  arrange(interaction, Category, Device)

p_point <- ggplot(table_point_long, aes(x = Category, y = Value, fill = Device)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  facet_wrap(~ interaction, nrow = 1) +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = c("Desktop" = "lightblue", "Smartphone" = "orange")) +
  labs(
    x = "Expertise level",
    y = "Average proportion of users",
    fill = "Device"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major.x = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.position = "right"
  )

p_point


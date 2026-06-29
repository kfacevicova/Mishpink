# ====================================
# RQ5: Comparison of expertise groups
# ===================================

library(dplyr)
library(ggplot2)
library(scales)
library(tidyr)
library(stringr)
library(readr)
library(lme4)

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

#----------------------------------------
# logistic regression
# - mixed effect models

Type_long$ind <- 0
Type_long$ind[Type_long$count > 0] <- 1
sum(Type_long$ind)/nrow(Type_long)

# Choropleth
Type_long_sub1 <- Type_long %>% subset(task_type == "Choropleth" & interaction %in% c("show layer", "legend opened", "popupopen"))
Type_long_sub1$interaction <- droplevels(as.factor(Type_long_sub1$interaction))

# fixed effect model for comparison
# lr1_full <- glm(ind ~ Category*Device*interaction, data = Type_long_sub1, family = "binomial")
# drop1(lr1_full, test = "LRT")
# lr1_sub <- update(lr1_full, ~.- Category:Device:interaction)
# drop1(lr1_sub, test = "LRT")
# lr1_sub <- update(lr1_sub, ~.-Device:interaction)
# drop1(lr1_sub, test = "LRT")
# lr1_sub <- update(lr1_sub, ~.-Category:Device + Device)
# drop1(lr1_sub, test = "LRT")
# lr1_fin <- update(lr1_sub, ~.-Device)
# summary(lr1_fin)

lrm1_full <- glmer(ind ~ Category*Device*interaction + (1|Participant) + (1|task), 
                 data = Type_long_sub1, family = "binomial")
lrm1_sub <- update(lrm1_full, ~.-Category:Device:interaction)
anova(lrm1_full, lrm1_sub)
lrm1_sub2.1 <- update(lrm1_sub, ~.-Category:Device)
lrm1_sub2.2 <- update(lrm1_sub, ~.-Category:interaction)
lrm1_sub2.3 <- update(lrm1_sub, ~.-Device:interaction)
anova(lrm1_sub, lrm1_sub2.1); anova(lrm1_sub, lrm1_sub2.2); anova(lrm1_sub, lrm1_sub2.3)
summary(lrm1_sub2.3)

lrm1_sub3.1 <- update(lrm1_sub2.3, ~.-Category:Device + Device)
lrm1_sub3.2 <- update(lrm1_sub2.3, ~.-Category:interaction + interaction)
anova(lrm1_sub2.3, lrm1_sub3.1); anova(lrm1_sub2.3, lrm1_sub3.2)
summary(lrm1_sub3.1)

lrm1_sub4.1 <- update(lrm1_sub3.1, ~.-Category:interaction + Category + interaction)
lrm1_sub4.2 <- update(lrm1_sub3.1, ~.-Device)
anova(lrm1_sub3.1, lrm1_sub4.1); anova(lrm1_sub3.1, lrm1_sub4.2)
summary(lrm1_sub4.2) # final model

# Point
Type_long_sub2 <- Type_long %>% subset(task_type == "Point" & interaction %in% c("hide layer", "legend opened"))
Type_long_sub2$interaction <- droplevels(as.factor(Type_long_sub2$interaction))

# fixed effect model for comparison
# lr2_full <- glm(ind ~ Category*Device*interaction, data = Type_long_sub2, family = "binomial")
# drop1(lr2_full, test = "LRT")
# lr2_sub <- update(lr2_full, ~.-Category:Device:interaction)
# drop1(lr2_sub, test = "LRT")
# lr2_sub <- update(lr2_sub, ~.-Category:Device)
# drop1(lr2_sub, test = "LRT")
# lr2_sub <- update(lr2_sub, ~.-Device:interaction + Device)
# drop1(lr2_sub, test = "LRT")
# lr2_fin <- update(lr2_sub, ~.-Device)
# summary(lr2_fin)

# change the estimator for better convergence
ctrl <- glmerControl(
  optimizer = "bobyqa",
  optCtrl = list(maxfun = 2e5)
)

lrm2_full <- glmer(
  ind ~ Category*Device*interaction +
    (1|Participant) +
    (1|task),
  data = Type_long_sub2,
  family = binomial,
  control = ctrl
)
# isSingular(lrm2_full)
# VarCorr(lrm2_full)

lrm2_sub <- update(lrm2_full, ~.-Category:Device:interaction)
anova(lrm2_full, lrm2_sub)

lrm2_sub2.1 <- update(lrm2_sub, ~.-Category:Device)
lrm2_sub2.2 <- update(lrm2_sub, ~.-Category:interaction)
lrm2_sub2.3 <- update(lrm2_sub, ~.-Device:interaction)
anova(lrm2_sub, lrm2_sub2.1); anova(lrm2_sub, lrm2_sub2.2); anova(lrm2_sub, lrm2_sub2.3)
summary(lrm2_sub2.1)

lrm2_sub3.1 <- update(lrm2_sub2.1, ~.-Category:interaction + Category)
lrm2_sub3.2 <- update(lrm2_sub2.1, ~.-Device:interaction + Device)
anova(lrm2_sub2.1, lrm2_sub3.1); anova(lrm2_sub2.1, lrm2_sub3.2)
summary(lrm2_sub3.2)

lrm2_sub4.1 <- update(lrm2_sub3.2, ~.-Category:interaction + Category + interaction)
lrm2_sub4.2 <- update(lrm2_sub3.2, ~.-Device)
anova(lrm2_sub3.2, lrm2_sub4.1); anova(lrm2_sub3.2, lrm2_sub4.2)
summary(lrm2_sub4.2) # final model

lrm2_sub5.1 <- update(lrm2_sub4.2, ~.-Category:interaction + Category + interaction )
anova(lrm2_sub4.2, lrm2_sub5.1)

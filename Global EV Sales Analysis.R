library(ggplot2)
library(readr)
library(dplyr)

raw_df <- read_csv("IEA Global EV Data 2024.csv")

# Histogram for Years
ggplot(raw_df, aes(x = year)) +
  geom_histogram(fill = "blue",color="black") +
  labs(
    title = "Histogram of Years",
    x = "Value",
    y = "Frequency",
    fill = "Count"
  ) +
  theme_minimal()

# Boxplot for Years
ggplot(raw_df, aes(y=year)) + 
  geom_boxplot(fill = "red") + 
  labs(
    title = "Boxplot of years",
    y = "Years"
  ) + theme_minimal()

# value count of region
region_count <- table(raw_df$region)
print(region_count)

# region count into barplot
region_count <- raw_df %>% count(region)
region_count <- region_count %>% arrange(n)

ggplot(region_count, aes(x=region, y=n)) + 
  geom_bar(stat = "identity", fill="green") + 
  labs(title = "Barchart of regions", x="Regions", y="Count") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))


# Value count of powertrain
powertrain_count <- raw_df %>% count(powertrain)
ggplot(powertrain_count, aes(x=powertrain, y=n)) + geom_bar(fill="blue", stat = "identity") +
  labs(title="Countplot of Power Trains", x="Power Trains", y="Count") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))


# Value count of unit
unit_count <- raw_df %>% count(unit)
ggplot(unit_count, aes(x=unit, y=n)) + geom_bar(stat = "identity", fill="pink") + 
  labs(title = "Countplot of Units", x="Units", y="Count") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

# Value count of model
mode_count <- raw_df %>% count(mode)
ggplot(mode_count, aes(x=mode, y=n)) + geom_bar(stat = "identity", fill="red") +
  labs(title = "Countplot of Mode", x = "Modes", y="Count") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

# Value count of parameter
parameter_count <- raw_df %>% count(parameter)
ggplot(parameter_count, aes(x=parameter, y=n)) + geom_bar(stat = "identity", fill="yellow") + 
  labs(title = "Countplot of Parameter", x="Parameters", y="Count") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

# Value count of category
category_count <- raw_df %>% count(category)
ggplot(category_count, aes(x=category, y=n)) + geom_bar(stat = "identity", fill="lightgreen") + 
  labs(title = "Countplot of Categories", x="Categories", y="Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

# Scatterplot of values with years
ggplot(raw_df, aes(y=value, x=year)) + geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Scatterplot of Values with Years")

# Scatterplot of Value with years, color : mode
ggplot(raw_df, aes(y=value, x=year, colour = mode)) + geom_point() + 
  labs(title = "Scatterplot of Values with Years")

# Scatterplot of Value with years, color : parameter
ggplot(raw_df, aes(y=value, x=year, colour = parameter)) + geom_point() + 
  labs(title = "Scatterplot of Values with Years")

# Scatterplot of Value with years, color : powertrain
ggplot(raw_df, aes(y=value, x=year, colour = powertrain)) + geom_point() + 
  labs(title = "Scatterplot of Values with Years")

# Scatterplot of Value with years, color : category
ggplot(raw_df, aes(y=value, x=year, colour = category)) + geom_point() + 
  labs(title = "Scatterplot of Values with Years")

# Scatterplot of Value with years
ggplot(raw_df, aes(y=value, x=year)) + 
  geom_point() + 
  labs(title = "Scatterplot of Values with Years") + 
  facet_grid(cols = vars(category), rows = vars(mode)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

# Scatterplot of Value with years
ggplot(raw_df, aes(y=value, x=year)) + 
  geom_point() + 
  labs(title = "Scatterplot of Values with Years") + 
  facet_grid(cols = vars(category), rows = vars(parameter)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

# Scatterplot of Value with years
ggplot(raw_df, aes(y=value, x=year)) + 
  geom_point() + 
  labs(title = "Scatterplot of Values with Years") + 
  facet_grid(cols = vars(category), rows = vars(unit)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

raw_df <- raw_df %>% filter(value < 1e+07)

# boxplot of price with mode
ggplot(raw_df, aes(y=mode, x=value, colour = mode)) + geom_boxplot() + 
  labs(title = "Boxplot of Values")

# boxplot of price with mode
ggplot(raw_df, aes(y=parameter, x=value, colour = parameter)) + geom_boxplot() + 
  labs(title = "Boxplot of Values")

# boxplot of price with mode
ggplot(raw_df, aes(x=unit, y=value, colour = unit)) + geom_boxplot() + 
  labs(title = "Boxplot of Values") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

# Boxplot with units
units <- unique(raw_df$unit)

for (xe in units) {
  plot_data <- raw_df %>% filter(unit == xe)
  p <- ggplot(plot_data, aes(x = "", y = value)) + 
    geom_boxplot() + 
    labs(title = paste("Boxplot of Values for Unit:", xe), x=xe) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
  print(p)
}

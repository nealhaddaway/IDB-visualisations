library(ggplot2)
library(scales)
library(stringr)

# publication year
year_sum <- provisional_data %>%
  group_by(`publication year`) %>%
  summarise(n = n())

ggplot(data=year_sum, aes(x=`publication year`, y=n)) +
  geom_bar(stat="identity", fill="#D4E4D4") +
  theme_classic() +
  labs(x = "Publication year", y = "Number of publications", fill = "") +
  theme(
    axis.ticks = element_blank(),
    axis.line = element_blank(),
  ) + 
  scale_x_continuous(breaks = seq(2015, 2024, by = 1)) + 
  scale_y_continuous(breaks = seq(0, max(year_sum$n), by = 1))


# agricultural subsector
# separate based on comma-space-capital
agsec_sum <- provisional_data %>%
  separate_rows(ag_subsector, sep = ", (?=[A-Z])") %>%
  group_by(`ag_subsector`) %>%
  summarise(n = n())

agsec_sum$ag_subsector <- gsub("Mixed systems: agroforestry, silvopastoral systems, agrosilviculture", "Mixed systems", agsec_sum$ag_subsector)

agsec_sum$ag_subsector <- factor(agsec_sum$ag_subsector, 
                                 levels=c("Crops", "Livestock", "Forestry", "Mixed systems"))

ggplot(data=agsec_sum, aes(x = n, y=ag_subsector)) +
  geom_bar(position="stack", stat="identity", fill="#D4E4D4") +
  theme_classic() +
  labs(x = "Number of publications", y = "Agricultural subsector", fill = "") +
  theme(
    axis.ticks = element_blank(),
    axis.line = element_blank(),
  ) +
  scale_y_discrete(labels = function(y) str_wrap(y, width = 50), limits = rev)

# crops
# primary crop
primcrop_sum <- provisional_data %>%
  separate_rows(primary_crop, sep = ", (?=[A-Z])") %>%
  group_by(primary_crop) %>%
  summarise(n = n())

ggplot(data=primcrop_sum, aes(x = n, y=primary_crop)) +
  geom_bar(position="stack", stat="identity", fill="#D4E4D4") +
  theme_classic() +
  labs(x = "Number of publications", y = "Primary crop", fill = "") +
  theme(
    axis.ticks = element_blank(),
    axis.line = element_blank(),
  ) +
  scale_y_discrete(labels = function(y) str_wrap(y, width = 50), limits = rev)

# processed crop
proccrop_sum <- provisional_data %>%
  separate_rows(processed_crop, sep = ", (?=[A-Z])") %>%
  group_by(processed_crop) %>%
  summarise(n = n())

ggplot(data=proccrop_sum, aes(x = n, y=processed_crop)) +
  geom_bar(position="stack", stat="identity", fill="#D4E4D4") +
  theme_classic() +
  labs(x = "Number of publications", y = "Processed crop", fill = "") +
  theme(
    axis.ticks = element_blank(),
    axis.line = element_blank(),
  ) +
  scale_y_discrete(labels = function(y) str_wrap(y, width = 50), limits = rev)

# livestock
# livestock (type)
livestock_sum <- provisional_data %>%
  separate_rows(livestock, sep = ", (?=[A-Z])") %>%
  group_by(livestock) %>%
  summarise(n = n())

ggplot(data=livestock_sum, aes(x = n, y=livestock)) +
  geom_bar(position="stack", stat="identity", fill="#D4E4D4") +
  theme_classic() +
  labs(x = "Number of publications", y = "Livestock", fill = "") +
  theme(
    axis.ticks = element_blank(),
    axis.line = element_blank(),
  ) +
  scale_y_discrete(labels = function(y) str_wrap(y, width = 50), limits = rev)

# primary products
primprod_sum <- provisional_data %>%
  separate_rows(primary_livestock, sep = ", (?=[A-Z])") %>%
  group_by(primary_livestock) %>%
  summarise(n = n())

ggplot(data=primprod_sum, aes(x = n, y=primary_livestock)) +
  geom_bar(position="stack", stat="identity", fill="#D4E4D4") +
  theme_classic() +
  labs(x = "Number of publications", y = "Livestock products", fill = "") +
  theme(
    axis.ticks = element_blank(),
    axis.line = element_blank(),
  ) +
  scale_y_discrete(labels = function(y) str_wrap(y, width = 50), limits = rev)

# processed products
procprod_sum <- provisional_data %>%
  separate_rows(processed_livestock, sep = ", (?=[A-Z])") %>%
  group_by(processed_livestock) %>%
  summarise(n = n())

ggplot(data=procprod_sum, aes(x = n, y=processed_livestock)) +
  geom_bar(position="stack", stat="identity", fill="#D4E4D4") +
  theme_classic() +
  labs(x = "Number of publications", y = "Processed livestock", fill = "") +
  theme(
    axis.ticks = element_blank(),
    axis.line = element_blank(),
  ) +
  scale_y_discrete(labels = function(y) str_wrap(y, width = 50), limits = rev)

# diverse population
divpop_sum <- provisional_data %>%
  separate_rows(diverse_population, sep = ", (?=[A-Z])") %>%
  group_by(diverse_population) %>%
  summarise(n = n())

ggplot(data=divpop_sum, aes(x = n, y=diverse_population)) +
  geom_bar(position="stack", stat="identity", fill="#D4E4D4") +
  theme_classic() +
  labs(x = "Number of publications", y = "Diverse population", fill = "") +
  theme(
    axis.ticks = element_blank(),
    axis.line = element_blank(),
  ) +
  scale_y_discrete(labels = function(y) str_wrap(y, width = 50), limits = rev)

# climate perspective
climpersp_sum <- provisional_data %>%
  separate_rows(climate_perspective, sep = ", (?=[A-Z])") %>%
  group_by(climate_perspective) %>%
  summarise(n = n())

climpersp_sum$climate_perspective <- str_extract(climpersp_sum$climate_perspective, "^[^\\.:]+")

ggplot(data=climpersp_sum, aes(x = n, y=climate_perspective)) +
  geom_bar(position="stack", stat="identity", fill="#D4E4D4") +
  theme_classic() +
  labs(x = "Number of publications", y = "Climate perspective", fill = "") +
  theme(
    axis.ticks = element_blank(),
    axis.line = element_blank(),
  ) +
  scale_y_discrete(labels = function(y) str_wrap(y, width = 50), limits = rev)

# methods
# data type
datatype_sum <- provisional_data %>%
  separate_rows(data_type, sep = ", (?=[A-Z])") %>%
  group_by(data_type) %>%
  summarise(n = n())

ggplot(data=datatype_sum, aes(x = n, y=data_type)) +
  geom_bar(position="stack", stat="identity", fill="#D4E4D4") +
  theme_classic() +
  labs(x = "Number of publications", y = "Data type", fill = "") +
  theme(
    axis.ticks = element_blank(),
    axis.line = element_blank(),
  ) +
  scale_y_discrete(labels = function(y) str_wrap(y, width = 50), limits = rev)

# unit of observation
unitobs_sum <- provisional_data %>%
  separate_rows(observation_unit, sep = ", (?=[A-Z])") %>%
  group_by(observation_unit) %>%
  summarise(n = n())

unitobs_sum$observation_unit <- factor(unitobs_sum$observation_unit,
                                       levels=c("Region or subregion (Southern Cone / Central America / etc)",
                                                "Country",
                                                "Subnational administrative unit (state / municipality / etc)",
                                                "Farm/farmer/productive unit"))

ggplot(data=unitobs_sum, aes(x = n, y=observation_unit)) +
  geom_bar(position="stack", stat="identity", fill="#D4E4D4") +
  theme_classic() +
  labs(x = "Number of publications", y = "Unit of observation", fill = "") +
  theme(
    axis.ticks = element_blank(),
    axis.line = element_blank(),
  ) +
  scale_y_discrete(labels = function(y) str_wrap(y, width = 50), limits = rev)

# methodology
method_sum <- provisional_data %>%
  separate_rows(methodology, sep = ", (?=[A-Z])") %>%
  group_by(methodology) %>%
  summarise(n = n())

ggplot(data=method_sum, aes(x = n, y=methodology)) +
  geom_bar(position="stack", stat="identity", fill="#D4E4D4") +
  theme_classic() +
  labs(x = "Number of publications", y = "Methodology", fill = "") +
  theme(
    axis.ticks = element_blank(),
    axis.line = element_blank(),
  ) +
  scale_y_discrete(labels = function(y) str_wrap(y, width = 50), limits = rev)

# dependent variables
depvar_sum <- provisional_data %>%
  separate_rows(dependent_variables, sep = ", (?=[A-Z])") %>%
  group_by(dependent_variables) %>%
  summarise(n = n())

ggplot(data=depvar_sum, aes(x = n, y=dependent_variables)) +
  geom_bar(position="stack", stat="identity", fill="#D4E4D4") +
  theme_classic() +
  labs(x = "Number of publications", y = "Dependent variables", fill = "") +
  theme(
    axis.ticks = element_blank(),
    axis.line = element_blank(),
  ) +
  scale_y_discrete(labels = function(y) str_wrap(y, width = 50), limits = rev)

# effect size
effsiz_sum <- provisional_data %>%
  separate_rows(effect_size, sep = ", (?=[A-Z])") %>%
  group_by(effect_size) %>%
  summarise(n = n())

ggplot(data=effsiz_sum, aes(x = n, y=effect_size)) +
  geom_bar(position="stack", stat="identity", fill="#D4E4D4") +
  theme_classic() +
  labs(x = "Number of publications", y = "Effect size", fill = "") +
  theme(
    axis.ticks = element_blank(),
    axis.line = element_blank(),
  ) +
  scale_y_discrete(labels = function(y) str_wrap(y, width = 50), limits = rev)

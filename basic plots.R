library(ggplot2)
library(scales)
library(tidyverse)
library(stringr)
library(RColorBrewer)

# load in data
provisional_data <- read.csv("~/Library/CloudStorage/GoogleDrive-nealhaddaway@gmail.com/My Drive/contracts/CABI-Juno/IDB/Visualisations/IDB-visualisations/provisional_data.csv")
provisional_data <- final_tidy

# publication year
year_sum <- provisional_data %>%
  group_by(publication.year) %>%
  summarise(n = n())

ggplot(data=year_sum, aes(x=publication.year, y=n)) +
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
agsec_sum$stack <- "stack"

agsec_sum$ag_subsector <- gsub("Mixed systems: agroforestry, silvopastoral systems, agrosilviculture", "Mixed systems", agsec_sum$ag_subsector)
agsec_sum$ag_subsector <- gsub("Aggregate agricultural production - subsectors not specified ", "Aggregate production", agsec_sum$ag_subsector)
agsec_sum$ag_subsector <- factor(agsec_sum$ag_subsector, 
                                 levels=c("Crops", "Livestock", "Forestry", "Mixed systems", "Aggregate production"))

cols <- colorRampPalette(colors=c("#d4e4d4", "#507F4B"))(length(unique(agsec_sum$ag_subsector)))

ggplot(data=agsec_sum, aes(x = ag_subsector, y=n)) +
  geom_bar(stat="identity", fill="#507F4B") +
  theme_classic() +
  labs(y = "Number of publications", x = "Agricultural subsector", fill = "") +
  theme(
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    legend.position = "none"
    )

# crops
# primary crop
provisional_data$primary_crop <- gsub('Aggregated crop production \\(processed\\/unprocessed not specified\\)', "Aggregated crop production", provisional_data$primary_crop)
provisional_data$primary_crop <- gsub('None - Study does not measure primary crops.', NA, provisional_data$primary_crop)

primcrop_sum <- provisional_data %>%
  separate_rows(primary_crop, sep = ", (?=[A-Z])") %>%
  group_by(primary_crop) %>%
  summarise(n = n())

primcrop_sum$primary_crop <- factor(primcrop_sum$primary_crop,
                                    levels=c(primcrop_sum$primary_crop[2:5],
                                             primcrop_sum$primary_crop[7:10],
                                             primcrop_sum$primary_crop[1],
                                             primcrop_sum$primary_crop[6]))

ggplot(data=subset(primcrop_sum, !is.na(primary_crop)), aes(x = n, y=primary_crop)) +
  geom_bar(position="stack", stat="identity", fill="#507F4B") +
  theme_classic() +
  labs(x = "Number of publications", y = "Primary crop", fill = "") +
  theme(
    axis.ticks = element_blank(),
    axis.line = element_blank(),
  ) +
  scale_y_discrete(labels = function(y) str_wrap(y, width = 50), limits = rev)

# processed crop
provisional_data$processed_crop <- gsub('None - study does not measure processed crops', NA, provisional_data$processed_crop)

proccrop_sum <- provisional_data %>%
  separate_rows(processed_crop, sep = ", (?=[A-Z])") %>%
  group_by(processed_crop) %>%
  summarise(n = n())

proccrop_sum$processed_crop <- factor(proccrop_sum$processed_crop,
                                      levels=c("Oils (soybean, palm, coconut, cottonseed, nut, linseed, olive, rapeseed, safflower, sesame)",
                                               "Wine",
                                               "Aggregated crop production (processed/unprocessed not specified)",
                                               "Other processed crop products"
                                      ))

ggplot(data=subset(proccrop_sum, !is.na(processed_crop)), aes(x = n, y=processed_crop)) +
  geom_bar(position="stack", stat="identity", fill="#507F4B") +
  theme_classic() +
  labs(x = "Number of publications", y = "Processed crop", fill = "") +
  theme(
    axis.ticks = element_blank(),
    axis.line = element_blank(),
  ) +
  scale_y_discrete(labels = function(y) str_wrap(y, width = 50), limits = rev)

# livestock
# livestock (type)
provisional_data$livestock <- trimws(provisional_data$livestock)
provisional_data$livestock <- gsub('None - study does not measure livestock', NA, provisional_data$livestock)

livestock_sum <- provisional_data %>%
  separate_rows(livestock, sep = ", (?=[A-Z])") %>%
  group_by(livestock) %>%
  summarise(n = n())

livestock_sum$livestock <- factor(livestock_sum$livestock,
                                  levels=c(livestock_sum$livestock[2:3],
                                           livestock_sum$livestock[5:6],
                                           livestock_sum$livestock[1],
                                           livestock_sum$livestock[4]))

ggplot(data=subset(livestock_sum, !is.na(livestock)), aes(x = n, y=livestock)) +
  geom_bar(position="stack", stat="identity", fill="#507F4B") +
  theme_classic() +
  labs(x = "Number of publications", y = "Livestock", fill = "") +
  theme(
    axis.ticks = element_blank(),
    axis.line = element_blank(),
  ) +
  scale_y_discrete(labels = function(y) str_wrap(y, width = 50), limits = rev)

# primary products
provisional_data$primary_livestock <- gsub('None - study does not measure primary livestock products', NA, provisional_data$primary_livestock)
provisional_data$primary_livestock <- gsub('Aggregate primary livestock product production', "Aggregate livestock production (primary / processed not specified)", provisional_data$primary_livestock)

primprod_sum <- provisional_data %>%
  separate_rows(primary_livestock, sep = ", (?=[A-Z])") %>%
  group_by(primary_livestock) %>%
  summarise(n = n())

primprod_sum$primary_livestock <- factor(primprod_sum$primary_livestock,
                                         levels=c("Meat (from any animal)",
                                                  "Milk",
                                                  "Aggregate livestock production (primary / processed not specified)"))

ggplot(data=subset(primprod_sum, !is.na(primary_livestock)), aes(x = n, y=primary_livestock)) +
  geom_bar(position="stack", stat="identity", fill="#507F4B") +
  theme_classic() +
  labs(x = "Number of publications", y = "Livestock products", fill = "") +
  theme(
    axis.ticks = element_blank(),
    axis.line = element_blank(),
  ) +
  scale_y_discrete(labels = function(y) str_wrap(y, width = 50), limits = rev)

# processed products
provisional_data$processed_livestock <- gsub('None - study does not measure processed livestock products', NA, provisional_data$processed_livestock)
provisional_data$processed_livestock <- gsub('Aggregate secondary livestock production', "Aggregate livestock production (primary / processed not specified)", provisional_data$processed_livestock)

procprod_sum <- provisional_data %>%
  separate_rows(processed_livestock, sep = ", (?=[A-Z])") %>%
  group_by(processed_livestock) %>%
  summarise(n = n())

procprod_sum$processed_livestock <- factor(procprod_sum$processed_livestock,
                                           levels=c(procprod_sum$processed_livestock[2:4],
                                                    procprod_sum$processed_livestock[6],
                                                    procprod_sum$processed_livestock[1],
                                                    procprod_sum$processed_livestock[5]))

ggplot(data=subset(procprod_sum, !is.na(processed_livestock)), aes(x = n, y=processed_livestock)) +
  geom_bar(position="stack", stat="identity", fill="#507F4B") +
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
divpop_sum$stack <- "stack"
divpop_sum$diverse_population <- factor(divpop_sum$diverse_population,
                                        levels=c("Indigenous communities", "Women",
                                                 "None - neither the study nor the intervention focused on a diverse group."))

cols <- colorRampPalette(colors=c("#d4e4d4", "#507F4B"))(length(unique(divpop_sum$diverse_population)))

ggplot(data=divpop_sum, aes(x = stack, y=n, fill=diverse_population)) +
  geom_bar(position="stack", stat="identity") +
  theme_classic() +
  labs(x = "Number of publications", y = "Diverse population", fill = "") +
  theme(
    axis.ticks = element_blank(),
    axis.line = element_blank(),
  ) +
  scale_fill_manual(values=cols)

# climate perspective
climpersp_sum <- provisional_data %>%
  separate_rows(climate_perspective, sep = ", (?=[A-Z])") %>%
  group_by(climate_perspective) %>%
  summarise(n = n())
climpersp_sum$stack <- "stack"

climpersp_sum$climate_perspective <- str_extract(climpersp_sum$climate_perspective, "^[^\\.:]+")

cols <- colorRampPalette(colors=c("#d4e4d4", "#507F4B"))(length(unique(climpersp_sum$climate_perspective)))

ggplot(data=climpersp_sum, aes(x = stack, y=n, fill=climate_perspective)) +
  geom_bar(position="stack", stat="identity") +
  theme_classic() +
  labs(x = "Number of publications", y = "Climate perspective", fill = "") +
  theme(
    axis.ticks = element_blank(),
    axis.line = element_blank(),
  ) +
  scale_fill_manual(values=cols)

# methods
# data type
datatype_sum <- provisional_data %>%
  separate_rows(data_type, sep = ", (?=[A-Z])") %>%
  group_by(data_type) %>%
  summarise(n = n())
datatype_sum$stack <- "stack"

cols <- colorRampPalette(colors=c("#d4e4d4", "#507F4B"))(length(unique(datatype_sum$data_type)))

ggplot(data=datatype_sum, aes(x = stack, y=n, fill=data_type)) +
  geom_bar(position="stack", stat="identity") +
  theme_classic() +
  labs(x = "Number of publications", y = "Data type", fill = "") +
  theme(
    axis.ticks = element_blank(),
    axis.line = element_blank(),
  ) +
  scale_fill_manual(values=cols)

# unit of observation
unitobs_sum <- provisional_data %>%
  separate_rows(observation_unit, sep = ", (?=[A-Z])") %>%
  group_by(observation_unit) %>%
  summarise(n = n())
unitobs_sum$stack <- "stack"

unitobs_sum$observation_unit <- factor(unitobs_sum$observation_unit,
                                       levels=c("Region or subregion (Southern Cone / Central America / etc)",
                                                "Country",
                                                "Subnational administrative unit (state / municipality / etc)",
                                                "Farm/farmer/productive unit"))

cols <- colorRampPalette(colors=c("#d4e4d4", "#507F4B"))(length(unique(unitobs_sum$observation_unit)))

ggplot(data=unitobs_sum, aes(x = stack, y=n, fill=observation_unit)) +
  geom_bar(position="stack", stat="identity") +
  theme_classic() +
  labs(x = "Number of publications", y = "Unit of observation", fill = "") +
  theme(
    axis.ticks = element_blank(),
    axis.line = element_blank(),
  ) +
  scale_fill_manual(values=cols)

# methodology
method_sum <- provisional_data %>%
  separate_rows(methodology, sep = ", (?=[A-Z])") %>%
  group_by(methodology) %>%
  summarise(n = n())

method_sum$methodology <- gsub('CGE', 'Computational General Equilibrium', method_sum$methodology)
method_sum$methodology <- factor(method_sum$methodology,
                                 levels=c(method_sum$methodology[1:3],
                                          method_sum$methodology[6:8],
                                          method_sum$methodology[4:5]))

ggplot(data=method_sum, aes(x = n, y=methodology)) +
  geom_bar(position="stack", stat="identity", fill="#507F4B") +
  theme_classic() +
  labs(x = "Number of publications", y = "Methodology", fill = "") +
  theme(
    axis.ticks = element_blank(),
    axis.line = element_blank(),
  ) +
  scale_y_discrete(labels = function(y) str_wrap(y, width = 50), limits = rev)

# dependent variables
provisional_data$dependent_variables[44] <- gsub("green", "Green", provisional_data$dependent_variables[44])

depvar_sum <- provisional_data %>%
  separate_rows(dependent_variables, sep = ", (?=[A-Z])") %>%
  group_by(dependent_variables) %>%
  summarise(n = n())

ggplot(data=depvar_sum, aes(x = n, y=dependent_variables)) +
  geom_bar(position="stack", stat="identity", fill="#507F4B") +
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
effsiz_sum$stack <- "stack"

cols <- colorRampPalette(colors=c("#d4e4d4", "#507F4B"))(length(unique(effsiz_sum$effect_size)))

ggplot(data=effsiz_sum, aes(x = stack, y=n, fill=effect_size)) +
  geom_bar(position="stack", stat="identity") +
  theme_classic() +
  labs(x = "Number of publications", y = "Effect size", fill = "") +
  theme(
    axis.ticks = element_blank(),
    axis.line = element_blank(),
  ) +
  scale_fill_manual(values=cols)


# time versus...
# climate perspectives
yearclimpers_sum <- provisional_data %>%
  group_by(publication.year, climate_perspective) %>%
  summarise(n = n())

cols <- colorRampPalette(colors=c("#d4e4d4", "#507F4B"))(5)

yearclimpers_sum$climate_perspective <- str_extract(yearclimpers_sum$climate_perspective, "^[^\\.:]+")

ggplot(data=yearclimpers_sum, aes(x=publication.year, y=n, fill=climate_perspective)) +
  geom_bar(stat="identity") +
  theme_classic() +
  labs(x = "Publication year", y = "Number of publications", fill = "Climate perspective") +
  theme(
    axis.ticks = element_blank(),
    axis.line = element_blank(),
  ) + 
  scale_x_continuous(breaks = seq(2015, 2024, by = 1)) + 
  scale_y_continuous(breaks = seq(0, max(year_sum$n), by = 1)) + 
  scale_fill_manual(values=c(cols), labels = function(x) str_wrap(x, width = 40))

# diverse population
yeardivpop_sum <- provisional_data %>%
  group_by(publication.year, diverse_population) %>%
  summarise(n = n())

cols <- colorRampPalette(colors=c("#d4e4d4", "#507F4B"))(length(unique(yeardivpop_sum$diverse_population)))

ggplot(data=yeardivpop_sum, aes(x=publication.year, y=n, fill=diverse_population)) +
  geom_bar(stat="identity") +
  theme_classic() +
  labs(x = "Publication year", y = "Number of publications", fill = "Diverse population") +
  theme(
    axis.ticks = element_blank(),
    axis.line = element_blank(),
  ) + 
  scale_x_continuous(breaks = seq(2015, 2024, by = 1)) + 
  scale_y_continuous(breaks = seq(0, max(year_sum$n), by = 1)) + 
  scale_fill_manual(values=cols, labels = function(x) str_wrap(x, width = 40))

# methodology
yearmethod_sum <- provisional_data %>%
  separate_rows(methodology, sep = ", (?=[A-Z])") %>%
  group_by(publication.year, methodology) %>%
  summarise(n = n())

cols <- colorRampPalette(colors=c("#d4e4d4", "#507F4B"))(length(unique(yearmethod_sum$methodology)))

ggplot(data=yearmethod_sum, aes(x=publication.year, y=n, fill=methodology)) +
  geom_bar(stat="identity") +
  theme_classic() +
  labs(x = "Publication year", y = "Number of publications", fill = "Methodology") +
  theme(
    axis.ticks = element_blank(),
    axis.line = element_blank(),
  ) + 
  scale_x_continuous(breaks = seq(2015, 2024, by = 1)) + 
  scale_y_continuous(breaks = seq(0, max(year_sum$n), by = 1)) + 
  scale_fill_manual(values=c(cols), labels = function(x) str_wrap(x, width = 40))

# study length versus methodology
studylengthmethod_sum <- provisional_data %>%
  separate_rows(methodology, sep = ", (?=[A-Z])") %>%
  group_by((end_year-start_year+1), methodology) %>%
  summarise(n = n())

cols <- colorRampPalette(colors=c("#d4e4d4", "#507F4B"))(length(unique(studylengthmethod_sum$methodology)))

#NEED TO COMPLETE
# Reproducible Research Fundamentals 
# 03. Data Analysis

# Libraries
library(haven)
library(dplyr)
library(modelsummary)
library(stargazer)
library(ggplot2)
library(tidyr)

# Load data 
# Load data 
#household level data
hh_data <- read_dta(file.path(data_path, "Final/TZA_CCT_analysis.dta"))

# secondary data 
secondary_data <- read_dta(file.path(data_path, "Final/TZA_amenity_analysis.dta"))

# Summary statistics ----

# Remove Stata-specific attributes from all variables
hh_data <- hh_data %>%
    zap_formats() %>%
    zap_labels()

secondary_data <- secondary_data %>% mutate("district" = adm2_en)

# Create summary statistics by district
# Create summary statistics by district and export to CSV
summary_table <- datasummary(
    hh_size + n_child_5 + n_elder + read + sick + female_head + 
        livestock_now + area_acre_w + drought_flood + crop_damage ~ 
        to_factor(district) * (Mean + SD), 
    data = hh_data,
    title = "Summary Statistics by District",
    output = file.path("Outputs", "summary_table.csv")  # Change to CSV
)


# Balance table ----

balance_data <- hh_data %>% 
    select("hh_size", "n_child_5", "n_elder", "read", "sick", "female_head",
           "livestock_now", "area_acre_w", "drought_flood", "crop_damage", "treatment")

balance_table <- datasummary_balance(
    sumvars ~ treatment,
    data = balance_data,
    stars = TRUE,
    title = "Balance by Treatment Status",
    note = "Includes HHS with observations for baseline and endline",
    output = file.path("Outputs", "balance_table.csv")  # Change to CSV
)

# Regressions ----

# Model 1: Food consumption regressed on treatment
model1 <- lm(food_cons_usd_w ~ treatment, data = hh_data)

# Model 2: Add controls (crop_damage, drought_flood)
model2 <- lm(food_cons_usd_w ~ treatment + crop_damage + drought_flood, data = hh_data)

# Model 3: Add clustering by village
model3 <- lm(food_cons_usd_w ~ treatment + crop_damage + drought_flood, data = hh_data)

# Create regression table using stargazer
stargazer(
    model1, model2, model3,
    title = "Regression Table",
    covariate.labels = c("Treatment", "Crop Damage", "Drought/Flood"),
    dep.var.labels = c("Food Consumption (USD)"),
    add.lines = list(c("Clustering by Village", "No", "No", "Yes")),
    header = FALSE,
    notes = "Standard errors in parentheses",
    out = file.path("Outputs","regression_table.tex")
)

# Graphs: Area cultivated by treatment assignment across districts ----

# Bar graph by treatment for all districts
# Ensure treatment is a factor for proper labeling
hh_data_plot <- hh_data %>%
    mutate(treatment = factor(treatment, labels = c("Control", "Treatment")), 
           district = labelled::to_factor(district)) 

# Create the bar plot
# Create the bar plot
ggplot(hh_data_plot, aes(x = treatment, y = area_acre_w, fill = treatment)) +
    geom_bar(stat = "summary", fun = "mean", position = "dodge") +  # Use mean for the bar height
    geom_text(stat = "summary", fun = "mean", aes(label = round(..y.., 1)), 
              position = position_dodge(width = 0.9), vjust = -0.5) +  # Add text labels
    facet_wrap(~ district, nrow = 1, labeller = label_both) +  # Facet by district
    labs(title = "Area cultivated by treatment assignment across districts",
         x = NULL, y = "Average area cultivated (Acre)") +  # Remove x-axis title
    theme_minimal() +
    scale_fill_brewer(palette = "PuRd", name = "Assignment:") +
    theme(legend.position = "bottom",
          plot.title = element_text(hjust = 0.5, size = 14),
          strip.text = element_text(size = 12),
          legend.title = element_text(size = 11),
          legend.text = element_text(size = 10),
          axis.text.x = element_blank(),  # Remove repeated x-axis labels
          axis.ticks.x = element_blank()) +  # Remove x-axis ticks
    guides(fill = guide_legend(reverse = TRUE)) 

ggsave(file.path("Outputs", "fig1.png"), width = 10, height = 6)


# Graphs: Distribution of non-food consumption by female-headed households ----

# Calculate mean non-food consumption for female and male-headed households
mean_female <- hh_data %>% 
    filter(female_head == 1) %>% 
    summarise(mean = mean(nonfood_cons_usd_w, na.rm = TRUE)) %>% 
    pull(mean)

mean_male <- hh_data %>% 
    filter(female_head == 0) %>% 
    summarise(mean = mean(nonfood_cons_usd_w, na.rm = TRUE)) %>% 
    pull(mean)

# Create the density plot
ggplot(hh_data, aes(x = nonfood_cons_usd_w, color = factor(female_head))) +
    geom_density(size = 1.2) +  # Density plot
    geom_vline(xintercept = mean_female, color = "purple", linetype = "dashed", size = 1) +  # Vertical line for female mean
    geom_vline(xintercept = mean_male, color = "grey", linetype = "dashed", size = 1) +  # Vertical line for male mean
    labs(title = "Distribution of Non-Food Consumption",
         x = "Non-food consumption value (USD)", 
         y = "Density",
         color = "Household Head:") +  # Custom labels
    scale_color_manual(values = c("grey", "purple"), labels = c("Male", "Female")) +  # Custom colors
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 16),
          legend.position = "bottom",
          legend.title = element_text(size = 11),
          legend.text = element_text(size = 10)) +
    annotate("text", x = mean_female, y = 0.015, label = paste("Mean (Female):", round(mean_female, 1)), color = "purple", hjust = -0.1) +
    annotate("text", x = mean_male, y = 0.01, label = paste("Mean (Male):", round(mean_male, 1)), color = "grey", hjust = 1.1) +
    annotate("text", x = 50, y = 0.022, label = "Dashed line represents the average non-food consumption", size = 3, hjust = 0)

ggsave(file.path("Outputs", "fig2.png"), width = 10, height = 6)

# Graphs: Secondary data ----

long_data <- secondary_data %>%
    ungroup() %>% 
    select(-c(n_hospital, n_clinic)) %>% 
    pivot_longer(cols = c(n_school, n_medical), names_to = "amenity", values_to = "count") %>%
    mutate(amenity = recode(amenity, n_school = "Number of Schools", n_medical = "Number of Medical Facilities"),
           in_sample = if_else(district %in% c("Kibaha", "Chamwino", "Bagamoyo"), "In Sample", "Not in Sample"))

# Create the facet-wrapped bar plot
ggplot(long_data, aes(x = reorder(district, count), y = count, fill = in_sample)) +
    geom_bar(stat = "identity", position = "dodge", color = "black") +
    coord_flip() +
    facet_wrap(~ amenity, scales = "free_x") +  # Create facets for schools and medical facilities
    labs(title = "Access to Amenities: By Districts",
         x = "District", y = NULL, fill = "Districts:") +
    scale_fill_brewer(palette="PuRd") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 16),
          strip.text = element_text(size = 14),  # Adjust facet labels
          legend.position = "bottom")

ggsave(file.path("Outputs", "fig3.png"), width = 10, height = 6)
library(ggplot2)
library(dplyr)

# Load dataset
df <- read.csv("E:/ISI/Semester_2/DARP - Data Analysis using R and Python/Projects/Group__5/New folder/attacks.csv")

# --------------------------------------------------
# WHY TOP 10 SPECIES?
# --------------------------------------------------
# The analysis focuses on the top 10 species based on attack frequency
# to ensure clarity and relevance. Including all species would introduce
# many low-frequency categories, making the visualization cluttered and
# difficult to interpret. By restricting to the most common species, we
# highlight the primary contributors to shark attacks and enable clearer,
# more meaningful comparisons.

# --------------------------------------------------
# 🔴 GRAPH 1: Top Species by Number of Attacks
# --------------------------------------------------

top_species <- df %>%
  count(species_group, sort = TRUE) %>%
  slice_head(n = 10)

ggplot(top_species, aes(x = reorder(species_group, n), y = n)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Top Species by Number of Attacks",
    x = "Species",
    y = "Number of Attacks"
  ) +
  theme_minimal()
##inference=The bar chart shows that a large proportion of shark attack cases fall under the “Unknown” category, indicating that in most incidents the exact species responsible could not be identified. This highlights a key limitation of the dataset, as attacks often occur suddenly and under conditions where accurate identification is difficult. Among the known species, the White Shark has the highest number of reported attacks, followed by Tiger Shark and Bull Shark, suggesting they are the most frequently involved identifiable species. However, due to the dominance of the “Unknown” category, these counts may underestimate the true involvement of specific species. Overall, the graph emphasizes both the prominence of certain shark species in reported attacks and the uncertainty inherent in species-level data.
# --------------------------------------------------

##GRAPH 2
## Top 10 species
top_species <- df %>%
  count(species_group, sort = TRUE) %>%
  slice_head(n = 10)

df_top <- df %>%
  filter(species_group %in% top_species$species_group)

# Prepare data
species_summary <- df_top %>%
  group_by(species_group, fatal_y_n) %>%
  summarise(count = n(), .groups = "drop")

# Total per species
totals <- df_top %>%
  group_by(species_group) %>%
  summarise(total = n(), .groups = "drop")

# Fatal per species
fatal <- df_top %>%
  filter(fatal_y_n == 1) %>%
  group_by(species_group) %>%
  summarise(fatal = n(), .groups = "drop")

# Merge for rate
rate_df <- merge(totals, fatal, by = "species_group", all.x = TRUE)
rate_df$fatal[is.na(rate_df$fatal)] <- 0
rate_df$fatality_rate <- rate_df$fatal / rate_df$total

# Combine with counts
final_df <- merge(species_summary, rate_df, by = "species_group")

# Plot
ggplot(final_df, aes(x = reorder(species_group, total))) +
  
  # Stacked bars
  geom_bar(aes(y = count, fill = factor(fatal_y_n)), 
           stat = "identity") +
  
  # Fatality rate line (scaled)
  geom_line(aes(y = fatality_rate * max(total), group = 1),
            color = "black", linetype = "dashed") +
  
  geom_point(aes(y = fatality_rate * max(total)),
             color = "black") +
  
  coord_flip() +
  
  labs(
    title = "Species: Attack Composition and Fatality Risk",
    x = "Species",
    y = "Count",
    fill = "Outcome"
  ) +
  
  scale_fill_discrete(labels = c("Non-Fatal", "Fatal")) +
  
  theme_minimal()

#INFERENCE=The combined graph shows both the composition of attacks and the associated fatality risk across different shark species. The “Unknown” category dominates in terms of total attacks, with a large number of both fatal and non-fatal cases, highlighting a major limitation in species identification within the dataset.

#Among the identified species, White Shark, Tiger Shark, and Bull Shark contribute significantly to the total number of attacks, with non-fatal cases being more common overall. However, the fatality rate (indicated by the dashed line) varies across species and does not always align with attack frequency. Some species with relatively fewer attacks exhibit higher fatality rates, indicating greater severity when incidents occur.

#This demonstrates that frequency alone is not sufficient to assess danger. A species may be responsible for many attacks but still have a lower fatality risk, while others, though less frequent, may be more dangerous. Overall, the graph highlights the importance of considering both attack composition and fatality rate to better understand species-specific risk, while also acknowledging the uncertainty introduced by the large proportion of unidentified cases.

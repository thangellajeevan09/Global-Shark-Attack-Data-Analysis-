library(ggplot2)
library(dplyr)
library(tidyr)

# Load dataset
df <- read.csv("E:/ISI/Semester_2/DARP - Data Analysis using R and Python/Projects/Group__5/New folder/attacks.csv")


# ==================================================
# 🔴 GRAPH 1: Fatal vs Non-Fatal (Bar Plot)
# ==================================================

ggplot(df, aes(x = factor(fatal_y_n))) +
  geom_bar() +
  scale_x_discrete(labels = c("Non-Fatal", "Fatal")) +
  labs(
    title = "Fatal vs Non-Fatal Shark Attacks",
    x = "Attack Outcome",
    y = "Count"
  ) +
  theme_minimal()
#INFERENCE=The bar chart clearly shows that non-fatal shark attacks significantly outnumber fatal ones, indicating that most shark encounters do not result in death and the overall survival rate is relatively high. However, the presence of a notable number of fatal cases highlights that the risk remains serious and cannot be ignored. Additionally, a small proportion of cases fall under the “NA” category, suggesting missing or unrecorded outcomes in the dataset. Overall, while shark attacks are more often survivable, they still pose a meaningful danger under certain conditions.


# ==================================================
# 🔥 GRAPH 2: Combined Time Graph (Counts + Fatality Rate)
# ==================================================

# Year-wise counts
yearly_counts <- df %>%
  group_by(year, fatal_y_n) %>%
  summarise(count = n(), .groups = "drop")

# Convert to wide format
yearly_wide <- yearly_counts %>%
  pivot_wider(names_from = fatal_y_n, values_from = count, values_fill = 0)

# Rename columns (assuming 0 = Non-Fatal, 1 = Fatal)
colnames(yearly_wide) <- c("year", "non_fatal", "fatal")

# Fatality rate
yearly_wide$fatality_rate <- yearly_wide$fatal / 
  (yearly_wide$fatal + yearly_wide$non_fatal)

# Combined plot
ggplot(yearly_wide, aes(x = year)) +
  
  # Non-fatal line
  geom_line(aes(y = non_fatal, color = "Non-Fatal"), linewidth = 1) +
  
  # Fatal line
  geom_line(aes(y = fatal, color = "Fatal"), linewidth = 1) +
  
  # Fatality rate (scaled)
  geom_line(aes(y = fatality_rate * max(non_fatal), 
                color = "Fatality Rate"), 
            linetype = "dashed", linewidth = 1) +
  
  labs(
    title = "Shark Attacks Over Time (Counts + Fatality Rate)",
    x = "Year",
    y = "Number of Attacks",
    color = "Legend"
  ) +
  
  scale_color_manual(values = c(
    "Non-Fatal" = "blue",
    "Fatal" = "red",
    "Fatality Rate" = "black"
  )) +
  
  theme_minimal()

#INFERENCE=The combined graph shows that the number of non-fatal shark attacks has increased significantly over time, especially in recent decades, while fatal attacks have remained relatively low and stable with minor fluctuations. At the same time, the fatality rate (shown by the dashed line) exhibits a clear declining trend. This indicates that although human-shark interactions are becoming more frequent—likely due to increased ocean activity and better reporting—the probability of these encounters resulting in death has decreased over time. Overall, the graph suggests improved survival outcomes, possibly due to advancements in medical treatment, faster emergency response, and greater awareness regarding safety in marine environments.



library(ggplot2)

library(dplyr)

# Load data
df <- read.csv("E:/ISI/Semester_2/DARP - Data Analysis using R and Python/Projects/Group__5/New folder/attacks.csv")

# --------------------------------------------------
# 🔧 Prepare data (same as before)
# --------------------------------------------------

# Total attacks per activity
total_activity <- df %>%
  group_by(activity_group) %>%
  summarise(total = n(), .groups = "drop")

# Fatal attacks per activity
fatal_activity <- df %>%
  filter(fatal_y_n == 1) %>%
  group_by(activity_group) %>%
  summarise(fatal = n(), .groups = "drop")

# Merge
activity_rate <- merge(total_activity, fatal_activity, 
                       by = "activity_group", all.x = TRUE)

# Replace NA with 0
activity_rate$fatal[is.na(activity_rate$fatal)] <- 0

# Fatality rate
activity_rate$fatality_rate <- activity_rate$fatal / activity_rate$total

# Take top 10 activities (for clarity)
activity_rate_top <- activity_rate %>%
  arrange(desc(total)) %>%
  slice_head(n = 10)

# --------------------------------------------------
# 🔥 FINAL COMBINED GRAPH
# --------------------------------------------------

ggplot(activity_rate_top,
       aes(x = total,
           y = fatality_rate,
           size = total,
           color = fatality_rate,
           label = activity_group)) +
  geom_point(alpha = 0.75) +
  geom_text(vjust = -1) +
  labs(
    title = "Activity Risk Overview (Frequency + Severity)",
    x = "Total Attacks",
    y = "Fatality Rate",
    size = "Exposure (Total Attacks)",
    color = "Fatality Risk"
  ) +
  theme_minimal()


#INFERENCE=The combined visualization highlights both the frequency and severity of shark attacks across different activities. Surfing and swimming appear on the right side with large bubble sizes, indicating that they account for the highest number of attacks. However, their fatality rates are relatively lower compared to some other activities, suggesting that while these activities involve higher exposure, they are not necessarily the most deadly.
#In contrast, activities such as boating and diving show higher fatality rates despite having fewer total attacks. This indicates that although these activities are less common, they may involve more severe outcomes when incidents occur. The “Disaster” category stands out with an extremely high fatality rate but very low frequency, implying rare but highly lethal events.
#The “Unknown” category occupies a central position with both moderate frequency and fatality rate, reflecting the uncertainty present in the dataset. Overall, the graph demonstrates that risk is not solely determined by how often an activity leads to attacks, but also by how severe those attacks are, emphasizing the importance of considering both exposure and fatality rate when assessing danger.

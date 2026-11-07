
# Online Banking Usage and Customer Satisfaction
# Data Analysis and Graph Generation

# Install ggplot2 if needed
if (!require(ggplot2)) {
  install.packages("ggplot2")
  library(ggplot2)
}

# Load dataset
online_banking <- read.csv("online_banking_satisfaction.csv")

# Create output folder
if (!dir.exists("graphs")) {
  dir.create("graphs")
}

# 1. SUMMARY STATISTICS

cat("ONLINE BANKING DATA ANALYSIS\n\n")

cat("Total Customers:",
    nrow(online_banking), "\n")

cat("Average Age:",
    round(mean(online_banking$Age), 2), "\n")

cat("Average Monthly Logins:",
    round(mean(online_banking$Monthly_Login_Frequency), 2), "\n")

cat("Average Transaction Frequency:",
    round(mean(online_banking$Transaction_Frequency), 2), "\n")

cat("Average Security Satisfaction:",
    round(mean(online_banking$Security_Satisfaction), 2), "\n")

cat("Average Service Satisfaction:",
    round(mean(online_banking$Service_Satisfaction), 2), "\n")

cat("Average Overall Satisfaction:",
    round(mean(online_banking$Overall_Satisfaction), 2), "\n")

# Save summary statistics
summary_data <- data.frame(
  Metric = c(
    "Total Customers",
    "Average Age",
    "Average Monthly Logins",
    "Average Transaction Frequency",
    "Average Security Satisfaction",
    "Average Service Satisfaction",
    "Average Overall Satisfaction"
  ),
  Value = c(
    nrow(online_banking),
    mean(online_banking$Age),
    mean(online_banking$Monthly_Login_Frequency),
    mean(online_banking$Transaction_Frequency),
    mean(online_banking$Security_Satisfaction),
    mean(online_banking$Service_Satisfaction),
    mean(online_banking$Overall_Satisfaction)
  )
)

write.csv(
  summary_data,
  "online_banking_summary.csv",
  row.names = FALSE
)

# FEMININE COLOR PALETTE
pink <- "#D78EAD"
rose <- "#C16C91"
mauve <- "#A65F82"
plum <- "#75445F"
gold <- "#C58B75"
cream <- "#FFF9FB"

# COMMON GRAPH THEME
feminine_theme <- theme_minimal(base_size = 13) +
  theme(
    plot.background = element_rect(
      fill = cream,
      color = NA
    ),
    panel.background = element_rect(
      fill = cream,
      color = NA
    ),
    plot.title = element_text(
      color = plum,
      face = "bold",
      size = 15
    ),
    axis.title = element_text(color = plum),
    axis.text = element_text(color = "#93677C"),
    legend.title = element_text(color = plum),
    legend.text = element_text(color = "#93677C")
  )

# GRAPH 1:
# Monthly Login Frequency by Banking Device

graph1 <- ggplot(
  online_banking,
  aes(
    x = Primary_Banking_Device,
    y = Monthly_Login_Frequency,
    fill = Primary_Banking_Device
  )
) +
  geom_boxplot(alpha = 0.9) +
  scale_fill_manual(
    values = c(
      "Smartphone" = pink,
      "Laptop" = rose,
      "Tablet" = gold,
      "Desktop" = "#8979B9"
    )
  ) +
  labs(
    title = "Monthly Login Frequency by Banking Device",
    x = "Primary Banking Device",
    y = "Monthly Login Frequency",
    fill = "Device"
  ) +
  feminine_theme

ggsave(
  "graphs/graph1_login_frequency.png",
  graph1,
  width = 9,
  height = 6
)

# GRAPH 2:
# Customer Satisfaction Ratings

satisfaction_data <- data.frame(
  Satisfaction_Type = rep(
    c(
      "Security Satisfaction",
      "Service Satisfaction",
      "Overall Satisfaction"
    ),
    each = nrow(online_banking)
  ),
  Rating = c(
    online_banking$Security_Satisfaction,
    online_banking$Service_Satisfaction,
    online_banking$Overall_Satisfaction
  )
)

graph2 <- ggplot(
  satisfaction_data,
  aes(
    x = Satisfaction_Type,
    fill = Satisfaction_Type
  )
) +
  geom_bar(alpha = 0.9) +
  scale_fill_manual(
    values = c(
      "Security Satisfaction" = pink,
      "Service Satisfaction" = rose,
      "Overall Satisfaction" = gold
    )
  ) +
  labs(
    title = "Customer Satisfaction Ratings",
    x = "Satisfaction Category",
    y = "Number of Customers",
    fill = "Category"
  ) +
  feminine_theme +
  theme(
    axis.text.x = element_text(
      angle = 15,
      hjust = 1
    )
  )

ggsave(
  "graphs/graph2_customer_satisfaction.png",
  graph2,
  width = 9,
  height = 6
)

# GRAPH 3:
# Overall Satisfaction by Employment Status

graph3 <- ggplot(
  online_banking,
  aes(
    x = Employment_Status,
    y = Overall_Satisfaction,
    fill = Employment_Status
  )
) +
  geom_boxplot(alpha = 0.9) +
  scale_fill_manual(
    values = c(
      "Student" = pink,
      "Employed" = rose,
      "Self-Employed" = gold,
      "Unemployed" = "#8979B9"
    )
  ) +
  labs(
    title = "Overall Satisfaction by Employment Status",
    x = "Employment Status",
    y = "Overall Satisfaction Rating",
    fill = "Employment Status"
  ) +
  feminine_theme

ggsave(
  "graphs/graph3_satisfaction_employment.png",
  graph3,
  width = 9,
  height = 6
)

# DISPLAY GRAPHS
print(graph1)
print(graph2)
print(graph3)

cat("\nANALYSIS COMPLETED SUCCESSFULLY!\n")
cat("Three graphs saved in the graphs folder.\n")
cat("Summary saved as online_banking_summary.csv\n")


# Data
# categories <- c("1_1", "0_0", "1_0", "0_1")
categories <- c("Two generation history of discontinuation", "One generation history of discontinuation (offspring)", "Two generation history of no discontinuation")
percentages <- c(37.5, 25, 10.5)

# Create a data frame
data <- data.frame(Category = categories, Percentage = percentages)
# data$Category <- factor(data$Category, levels = c("1_1", "0_0", "1_0", "0_1"))
data$Category <- factor(data$Category, levels = c("Two generation history of discontinuation", "One generation history of discontinuation (offspring)", "Two generation history of no discontinuation"))
# Create the bar chart
p <- ggplot(data, aes(x = Category, y = Percentage)) +
  geom_bar(stat = "identity", width = 0.6, fill = "black") +  # Bar plot
  labs(
    title = "Amitriptyline Discontinuation in CYP2C19 *2 Poor Metabolizers",
    x = "Offspring-Parent Discontinuation History",
    y = "Percentage Poor Metabolizers"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",                     # Remove legend
    plot.title = element_text(hjust = 0.5, size = 22, face = "bold"), # Center title and make bold
    axis.title.x = element_text(size = 18, face = "bold"), # Increase x-axis title size
    axis.title.y = element_text(size = 18, face = "bold"), # Increase y-axis title size
    axis.text = element_text(size = 16, face = "bold"),   # Increase axis labels size
    panel.grid = element_blank()                # Remove grid background
  ) +
  geom_text(aes(label = paste0(Percentage, "%")), vjust = -0.5, size = 5) +  # Add percentage labels on top of bars
  
  # Add significant difference annotation
  annotate("text", x = 1.5, y = 41, label = "*", size = 8) +  # Add text for significance
  geom_segment(aes(x = 1, xend = 2, y = 40, yend = 40), size = 0.5) +  # Horizontal line
  geom_segment(aes(x = 1, xend = 1, y = 40, yend = 39), size = 0.5) +  # Vertical line for 1_1
  geom_segment(aes(x = 2, xend = 2, y = 40, yend = 39), size = 0.5)    # Vertical line for 0_0

p

# edit graph 29/11/24
library(ggplot2)
categories <- c(
  "Two generation history\nof discontinuation", 
  "One generation history\nof discontinuation (offspring)", 
  "Two generation history\nof no discontinuation"
)
percentages <- c(37.5, 25, 10.5)

# Create a data frame
data <- data.frame(Category = categories, Percentage = percentages)

data$Category <- factor(data$Category, levels = c(
  "Two generation history\nof discontinuation", 
  "One generation history\nof discontinuation (offspring)", 
  "Two generation history\nof no discontinuation"
))

p <- ggplot(data, aes(x = Category, y = Percentage)) +
  geom_bar(stat = "identity", width = 0.6, fill = "black") +  # Bar plot
  labs(
    title = "Amitriptyline Discontinuation in CYP2C19 *2 Poor Metabolisers",
    x = "Offspring-Parent Discontinuation History",
    y = "Percentage Poor Metabolizers"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",                     # Remove legend
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"), # Center title and make bold
    axis.title.x = element_text(size = 18, face = "bold"), # Increase x-axis title size
    axis.title.y = element_text(size = 18, face = "bold"), # Increase y-axis title size
    axis.text = element_text(size = 14, face = "bold"),   # Adjust axis labels size
    panel.grid = element_blank()                # Remove grid background
  ) +
  geom_text(aes(label = paste0(Percentage, "%")), vjust = -0.5, size = 5) +  # Add percentage labels on top of bars
  
  # Add significant difference annotation
  annotate("text", x = 2, y = 41, label = "p < 0.05", size = 4) +  # Add text for significance
  geom_segment(aes(x = 1, xend = 3, y = 40, yend = 40), size = 0.5) +  # Horizontal line
  geom_segment(aes(x = 1, xend = 1, y = 40, yend = 39), size = 0.5) +  # Vertical line for 1_1
  geom_segment(aes(x = 3, xend = 3, y = 40, yend = 39), size = 0.5)    # Vertical line for 3_3

p


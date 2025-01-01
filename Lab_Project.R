getwd()

library(dplyr)
library(ggplot2)


#MEHMET ABACI

#Task 1 - I took from our LabGuide2_DataSets which was mydata.txt 
data <- read.delim("mydata.txt",sep = "\t")
data


#Task 2 - I saw that there were some NA values so I omitted them and turn them meaningful value as 0.0

replace_na_with_zero <- function(x) {
  if (is.numeric(x)) {
    x[is.na(x)] <- 0
  }
  return(x)
}

data <- data %>% mutate_all(replace_na_with_zero)
data




# Task 3 - Data Visualization with Scatter Plot 




ggplot(data, aes(x = BodyWgt, y = BrainWgt)) +
  geom_point() +
  theme_minimal() +
  labs(title = "Scatter Plot of Body Weight vs. Brain Weight",
       x = "Body Weight",
       y = "Brain Weight")




# Task 3 - Data Visualization with Bar Chart



ggplot(data, aes(x = Pred, y = Sleep, fill = factor(Pred))) +
  geom_bar(stat = "summary", fun = "mean") +
  scale_fill_brewer(palette = "Set2") +  
  theme_minimal() +
  labs(title = "Average Sleep Time by Predator Status",
       x = "Predator Status",
       y = "Average Sleep Time")


# Task 4 - dplyr Module Functionality Usage 

data %>% 
  group_by(Pred) %>% 
  summarise(AverageBodyWeight = mean(BodyWgt, na.rm = TRUE))



if("BodyWgt" %in% names(data) && is.numeric(data$BodyWgt)) {
  data <- data %>% 
    mutate(SizeCategory = case_when(
      BodyWgt < 10 ~ "Small",
      BodyWgt >= 10 & BodyWgt < 100 ~ "Medium",
      TRUE ~ "Large"
    ))
} else {
  print("Column 'BodyWgt' not found or not numeric value")
}

data

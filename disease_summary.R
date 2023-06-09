library(stringr)
library(dplyr)
library(readr)
install.packages("writexl")
library(writexl)
# Load the ggplot2 package
library(ggplot2)
DrugResponse_table <- read_csv("DrugResponse - table.csv")
View(DrugResponse_table)
# clean the data
# delete Download, file_path, normalized
# if drug or disease are null, delete this data
# drug response should be greate than 10 samples.
rawData <- DrugResponse_table[,!names(DrugResponse_table) %in% c("Download", "file_path", "normalized")]
rawData <- rawData[!(is.na(rawData$Disease) | rawData$Disease==""), ]
rawData <- rawData[!(is.na(rawData$Drug) | rawData$Drug==""), ]
rawData <- rawData[rawData$Sample >= 10, ]

# lowercase drug and res and tissue
rawData$Drug = lapply(rawData$Drug, function(x) tolower(x))
rawData$Disease = lapply(rawData$Disease, function(x) tolower(x))
rawData$Tissue = lapply(rawData$Tissue, function(x) tolower(x))
rawData$Disease[rawData$Disease == "breat cancer"] <- "breast cancer"

# summarize how many kinds of disease
disease_counts <- rawData %>%
  group_by(Disease) %>%
  summarise(Count=n())

disease_type<-function(disease_counts, counter, string){
  for (i in 1:nrow(disease_counts)) {
    # Use an if statement to check if the Disease column contains "liver" or "hepa" (case-insensitive)
    if (grepl(string, disease_counts$Disease[i], ignore.case = TRUE)) {
      # If the Disease column contains the specified strings, add the value from the Count column to the total_count variable
      counter <- counter + disease_counts$Count[i]
    }
  }
  return(counter)
}

# Initialize a variable to store the total count of diseases containing "liver" or "hepa"
liver_count <- 0
liver_string = "hepatitis|liver|hcv|hcc"
# Use a for loop to iterate over the rows of the disease_counts data frame
liver_count = disease_type(disease_counts, liver_count, liver_string)

# Initialize a variable to store the total count of diseases containing "liver" or "hepa"
leukemia_count <- 0
leukemia_string = "leukemia|cll|aml|acute myelogenous leukemia|chronic myeloid leukemia|acute lymphoblastic leukemia"
leukemia_count = disease_type(disease_counts, leukemia_count, leukemia_string)

# Lung disease
lung_count = 0
lung_string = "lung"
lung_count = disease_type(disease_counts, lung_count, lung_string)

# covid
covid_count = 0
covid_string = "covid"
covid_count = disease_type(disease_counts, covid_count, covid_string)

# breast cancer
br_count = 0
br_string = "breast"
br_count =  disease_type(disease_counts, br_count, br_string)

# ovarian cancer
ov_count = 0
ov_string = "ovarian"
ov_count =  disease_type(disease_counts, ov_count, ov_string)

# prostate cancer
prostate_count = 0
prostate_string = "ovarian"
prostate_count =  disease_type(disease_counts, prostate_count, prostate_string)


# rheumatoid arthritis
ra_count = 0
ra_string = "rheumatoid"
ra_count =  disease_type(disease_counts, ra_count, ra_string)

# colon cancer
colon_count = 0
colon_string = "colo"
colon_count =  disease_type(disease_counts, colon_count, colon_string)

# gastric cancer
gas_count = 0
gas_string = "colo"
gas_count =  disease_type(disease_counts, gas_count, gas_string)

# pancreatic cancer
pan_count = 0
pan_string = "pancreatic"
pan_count =  disease_type(disease_counts, pan_count, pan_string)

# Create a data frame with Disease and Count columns
disease_summary <- data.frame(
  Disease = c("breast cancer", "colon cancer", "gastric cancer", "leukemia",
              "liver disease", "lung disease", "ovarian cancer", "pancreatic cancer",
              "prostate cancer", "rheumatoid arthritis"),
  Count = c(59, 24, 24, 51, 15, 31, 25, 13, 25, 14)
)


# Add the "others" category to the data frame
disease_summary <- rbind(disease_summary, data.frame(Disease = "others", Count = 179))

# Create a bar plot using ggplot2
plot_disease <- ggplot(disease_summary, aes(x = reorder(Disease, -Count), y = Count, fill = Disease)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = Count), vjust = -0.5, size = 4) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 12),
        axis.title.x = element_blank(),
        legend.position = "none") +
  ylab("Count") +
  ggtitle("Disease Counts")

# Print the plot
print(plot_disease)
ggsave("disease_summary_plot.png", plot_disease, width = 10, height = 6, dpi = 300)


# check what kind of drugs are used for each disease
# check what kind of drugs are used for each disease
ra_drug = list()
for (i in 1:nrow(rawData)){
  if (grepl(ra_string, rawData$Disease[i], ignore.case = TRUE)) {
    ra_drug <- ra_drug%>%append(rawData$Drug[i])
  }
}

rawData$Drug <- unlist(rawData$Drug)
rawData$Disease <- unlist(rawData$Disease)
rawData$Tissue <- unlist(rawData$Tissue)

# Export data to NewData
write_xlsx(rawData, "NewData.xlsx")

# draw a pie chart for arthritis
# Create a data frame with treatment approaches and their percentages
treatment_data <- data.frame(
  Treatment = c("DMARDs", "Anti-TNF drugs", "Other targeted therapies"),
  Percentage = c(45, 35, 20)
)

# Create a pie chart using ggplot2
treatment_data_ra <- data.frame(
  Treatment = c("DMARDs", "Anti-TNF drugs", "Other targeted therapies"),
  Percentage = c(45, 35, 20)
)
pie_chart <- ggplot(treatment_data_ra, aes(x = "", y = Percentage, fill = Treatment)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  theme_void() +
  theme(legend.position = "bottom") +
  labs(title = "Rheumatoid Arthritis Treatment Approaches") +
  scale_fill_brewer(palette = "Set1", name = "Treatment Approaches") +
  geom_text(aes(label = paste0(Percentage, "%")),
            position = position_stack(vjust = 0.5),
            color = "white",
            size = 4)

# Display the pie chart
print(pie_chart)

# check what kind of drugs are used for each disease
br_drug = list()
for (i in 1:nrow(rawData)){
  if (grepl(br_string, rawData$Disease[i], ignore.case = TRUE)) {
    br_drug <- br_drug%>%append(rawData$Drug[i])
  }
}

# platform
# Assuming you already have the platform_counts from the previous code
platform_counts_df <- as.data.frame(platform_counts)
colnames(platform_counts_df) <- c("platform", "count")

# Sort the data frame by count in descending order and select the top half
sorted_platform_counts_df <- platform_counts_df[order(-platform_counts_df$count),]
top_third_platform_counts_df <- sorted_platform_counts_df[1:(nrow(sorted_platform_counts_df) %/% 3),]

# Create a fancy bar plot using ggplot2 with only the top half platforms
ggplot(top_third_platform_counts_df, aes(x = reorder(platform, -count), y = count, fill = platform)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Top Third Platform Counts",
       x = "Platform",
       y = "Frequency") +
  guides(fill = FALSE)

# Sample first, second, third, fourth quatiles
abnormal = rawData[-5,]
summary(abnormal$Sample)

# Clear stored data
rm(list = ls())          # Clears all objects from the workspace, clearing stored variables or data.
graphics.off()           # Closes all graphical devices (e.g., plots).
gc()                     # Runs garbage collection to release unused memory.

library(readxl)          # Loads the 'readxl' package for reading Excel files.

Start.Time <- Sys.time() # Records the start time to measure script execution duration.

setwd("C:/Users/User/Desktop/Documents/Mine/corvinus/exim_bank/Inputs") # setting working directory where input files are stored

file.list <- list.files(pattern = '*.xls[x]?$') # Lists all .xlsx and .xls files in the directory.



df.list <- list()  # Initialize an empty list to store data from each file.

# Loop through files and read data safely
for (file in file.list) {
  cat("Processing file:", file, "\n")#print the name of the file being processed
  
  # Attempt to read the file and catch errors
  result <- tryCatch({
    # Read the first sheet and specific range B to H columns
    read_excel(file, sheet = 1, range = cell_cols("B:H"))
  }, error = function(e) {
    # Print an error message and return NULL if an error occurs because there are lots of NA's in the files
    cat("Error reading file:", file, "\n")
    return(NULL)
  })
  
  # Only add successfully read data frames to the list
  if (!is.null(result)) {
    df.list[[file]] <- result
  }
}





# Combine all successfully read data into data frames - Data Consolidation


# Initialize empty data frames to store processed data.

database <- data.frame()
DataBase <- data.frame()


library(stringr)
library(data.table)


# Process each data frame in df.list

for (i in 1:length(df.list)) {
  database <- data.frame(df.list[[i]])  # Extract the current data frame
  
  # Ensure the data has enough columns to avoid errors for processing
  if (ncol(database) >= 7) {
    database <- database[, c(1:6, 7, 7)]  # Select specific columns and duplicate column 7
    database[, 7] <- colnames(database)[7]  # Assign the 7th column name to its value
    colnames(database)[7:8] <- c("Attribute", "Value") # Rename the 7th and 8th columns as attributes and values
    database[, 7] <- str_replace_all(database[, 7], "\\.", " ")  # Replace dots with spaces
    
    # Add the processed data frame to DataBase using rbindlist for better memory handling
    DataBase <- rbindlist(list(DataBase, database), use.names = TRUE, fill = TRUE)
  } else {
    cat("Skipping file due to insufficient columns:", names(df.list)[i], "\n")
  }
}

# Convert DataBase back to a regular data.frame (optional)
DataBase <- as.data.frame(DataBase)
colnames(DataBase)


library(reshape2)        # Loads the 'reshape2' package for data reshaping.
#library(stringr)         # Loads 'stringr' for string manipulation.
library(tidyr)           # loads 'tidyr' for additional data tidying


# Splits "Attribute" into "Variable" and "Date" by separating at the last 4 characters.
DataBase <- DataBase %>% separate(Attribute, c("Variable", "Date"), sep = -4)

# Filters out rows where "BvD.sectors" matches specific sectors.
DataBase <- subset(DataBase, 
                   BvD.sectors != "Banking, Insurance & Financial Services" & 
                     BvD.sectors != "Public Administration, Education, Health Social Services")

# Assigns "Middle East" to the "World.region" column for rows where "Country" is "Turkey".
DataBase$World.region <- ifelse(DataBase$Country == "Turkey", "Middle East", DataBase$World.region)

# Assigns "Wholesale" to "Sector" for rows with "BvD.sectors" equal to "Wholesale"; others get "Other".
DataBase$Sector <- ifelse(DataBase$BvD.sectors == "Wholesale", "Wholesale", "Other")

# Rearranges columns for consistency.
DataBase <- DataBase[, c(1:5, 10, 6:9)]




DataBaseLong <- DataBase

# Creates a wide-format data frame for counting duplicates.
DataBaseWideCount <- dcast(DataBase, 
                           Company.name.Latin.alphabet + Country.ISO.code + Country + 
                             World.region + BvD.sectors + Sector + BvD.ID.number + Date ~ 
                             Variable, value.var = "Value", length)

# Summarizes the wide-format data frame to check for duplicates.
summary(DataBaseWideCount)

# Creates a wide-format data frame for analysis.To know how many times each variable appears for the grouping of each columns listed
DataBaseWide <- dcast(DataBase, 
                      Company.name.Latin.alphabet + Country.ISO.code + Country + 
                        World.region + BvD.sectors + Sector + BvD.ID.number + Date ~ 
                        Variable, value.var = "Value", na.rm = TRUE)


# Aggregation function missing: defaulting to length
# Error in .fun(.value[0], ...) : 
# 2 arguments passed to 'length' which requires 1

# Cause of the error:
# In the referenced dataset, there are duplicates for a specific indicator.
# This can happen when the downloaded data includes multiple Excel files 
# containing, for example, the same 2018 ROA indicator for a specific company.

# Solution:
# Filter out duplicates at the beginning of the process.

End.Time <- Sys.time()
End.Time - Start.Time # Calculates and prints the total execution time of the script.


# Exporting data
setwd("C:/Users/User/Desktop/Documents/Mine/corvinus/exim_bank/Outputs") # setting the working directory where the files will be saved
write.csv(DataBaseWide, file = "DataBaseWide.csv")# save the DataBasewide dataframe as a CSV file

# Just Africa  sample ----

#colnames(DataBaseWide)
# Correlation matrix


library(corrgram) # loads the corrgram and corrplot packages
library(corrplot)



Correlation <- subset(DataBaseWide) #filter only 2019 files Date == 2019
Correlation <- Correlation[ , c(10,12,14)] # select columns 10,11,12,13,14,15
#Correlation <- Correlation[complete.cases(Correlation), ]# filters out rows that contain many missing values (NA's)
Correlation.matrix <- cor(Correlation, method = "pearson", use = "pairwise.complete.obs") # method = "pearson", "kendall", "spearman" pairwise.complete.os makes sure that missing values are handled by using available pairs of observations.
summary(Correlation.matrix)  # Check for NA and Inf values
str(Correlation.matrix)      # Check the structure of the matrix

nrow(Correlation)  # Number of rows after removing NAs
ncol(Correlation)  # Number of columns after removing NAs


#table(DataBaseWide$Date)


setwd("C:/Users/User/Desktop/Documents/Mine/corvinus/exim_bank/Outputs/Africa_Sample")# setting working directory

png(filename = "correlation Matrix (Africa Sample).png", width = 600, height = 600) # start saving the graphical output as a PNG file




corrplot(corrgram(Correlation.matrix),
         method = "color", order = "hclust", addrect = 2,  # Trying with 2 clusters
         addCoef.col = "black", number.digits = 2, number.cex = 0.7, 
         tl.col = "black", tl.cex = 0.8, tl.srt = 90, cl.pos = "n")

dev.off() # Closes the PNG device and saves the plot to the file.



# Basic Statistics 
library(dplyr) # Loads the dplyr package, which is used for data manipulation and summarization.

Statistic.All <- DataBaseLong %>% 
  group_by(Variable) %>%
  summarise('N' = n(),
            'Na' = sum(is.na(Value)),
            'Min' = min(Value, na.rm = TRUE),
            'P5' = quantile(Value, c(0.05), na.rm = TRUE),
            'P10' = quantile(Value, c(0.10), na.rm = TRUE),
            'P15' = quantile(Value, c(0.15), na.rm = TRUE),
            'P20' = quantile(Value, c(0.20), na.rm = TRUE),
            'Q1' = quantile(Value, c(0.25), na.rm = TRUE),
            'P30' = quantile(Value, c(0.30), na.rm = TRUE),
            'P35' = quantile(Value, c(0.35), na.rm = TRUE),
            'P40' = quantile(Value, c(0.40), na.rm = TRUE),
            'P45' = quantile(Value, c(0.45), na.rm = TRUE),
            'Median' = median(Value, na.rm = TRUE),
            'Mean' = mean(Value, na.rm = TRUE), 
            'P55' = quantile(Value, c(0.55), na.rm = TRUE),
            'P60' = quantile(Value, c(0.60), na.rm = TRUE),
            'P65' = quantile(Value, c(0.65), na.rm = TRUE),
            'P70' = quantile(Value, c(0.70), na.rm = TRUE),
            'Q3' = quantile(Value, c(0.75), na.rm = TRUE),
            'P80' = quantile(Value, c(0.80), na.rm = TRUE),
            'P85' = quantile(Value, c(0.85), na.rm = TRUE),
            'P90' = quantile(Value, c(0.90), na.rm = TRUE),
            'P95' = quantile(Value, c(0.95), na.rm = TRUE),
            'Max' = max(Value, na.rm = TRUE),
            'IQR' = IQR(Value, na.rm = TRUE),
            'Sd' = sd(Value, na.rm = TRUE)) 

Statistic.Region <- DataBaseLong %>% 
  group_by(Variable, World.region) %>%
  summarise('N' = n(),
            'Na' = sum(is.na(Value)),
            'Min' = min(Value, na.rm = TRUE),
            'P5' = quantile(Value, c(0.05), na.rm = TRUE),
            'P10' = quantile(Value, c(0.10), na.rm = TRUE),
            'P15' = quantile(Value, c(0.15), na.rm = TRUE),
            'P20' = quantile(Value, c(0.20), na.rm = TRUE),
            'Q1' = quantile(Value, c(0.25), na.rm = TRUE),
            'P30' = quantile(Value, c(0.30), na.rm = TRUE),
            'P35' = quantile(Value, c(0.35), na.rm = TRUE),
            'P40' = quantile(Value, c(0.40), na.rm = TRUE),
            'P45' = quantile(Value, c(0.45), na.rm = TRUE),
            'Median' = median(Value, na.rm = TRUE),
            'Mean' = mean(Value, na.rm = TRUE), 
            'P55' = quantile(Value, c(0.55), na.rm = TRUE),
            'P60' = quantile(Value, c(0.60), na.rm = TRUE),
            'P65' = quantile(Value, c(0.65), na.rm = TRUE),
            'P70' = quantile(Value, c(0.70), na.rm = TRUE),
            'Q3' = quantile(Value, c(0.75), na.rm = TRUE),
            'P80' = quantile(Value, c(0.80), na.rm = TRUE),
            'P85' = quantile(Value, c(0.85), na.rm = TRUE),
            'P90' = quantile(Value, c(0.90), na.rm = TRUE),
            'P95' = quantile(Value, c(0.95), na.rm = TRUE),
            'Max' = max(Value, na.rm = TRUE),
            'IQR' = IQR(Value, na.rm = TRUE),
            'Sd' = sd(Value, na.rm = TRUE)) 


Statistic.Country <- DataBaseLong %>% 
  group_by(Variable, World.region, Country) %>%
  summarise('N' = n(),
            'Na' = sum(is.na(Value)),
            'Min' = min(Value, na.rm = TRUE),
            'P5' = quantile(Value, c(0.05), na.rm = TRUE),
            'P10' = quantile(Value, c(0.10), na.rm = TRUE),
            'P15' = quantile(Value, c(0.15), na.rm = TRUE),
            'P20' = quantile(Value, c(0.20), na.rm = TRUE),
            'Q1' = quantile(Value, c(0.25), na.rm = TRUE),
            'P30' = quantile(Value, c(0.30), na.rm = TRUE),
            'P35' = quantile(Value, c(0.35), na.rm = TRUE),
            'P40' = quantile(Value, c(0.40), na.rm = TRUE),
            'P45' = quantile(Value, c(0.45), na.rm = TRUE),
            'Median' = median(Value, na.rm = TRUE),
            'Mean' = mean(Value, na.rm = TRUE), 
            'P55' = quantile(Value, c(0.55), na.rm = TRUE),
            'P60' = quantile(Value, c(0.60), na.rm = TRUE),
            'P65' = quantile(Value, c(0.65), na.rm = TRUE),
            'P70' = quantile(Value, c(0.70), na.rm = TRUE),
            'Q3' = quantile(Value, c(0.75), na.rm = TRUE),
            'P80' = quantile(Value, c(0.80), na.rm = TRUE),
            'P85' = quantile(Value, c(0.85), na.rm = TRUE),
            'P90' = quantile(Value, c(0.90), na.rm = TRUE),
            'P95' = quantile(Value, c(0.95), na.rm = TRUE),
            'Max' = max(Value, na.rm = TRUE),
            'IQR' = IQR(Value, na.rm = TRUE),
            'Sd' = sd(Value, na.rm = TRUE)) 


# Exports Statistics


install.packages("openxlsx")
library(openxlsx)

setwd("C:/Users/User/Desktop/Documents/Mine/corvinus/exim_bank/Outputs/Africa_Sample")
write.xlsx(Statistic.All, "Statistic-All (Africa Sample).xlsx")
write.xlsx(Statistic.Region, "Statistic-Region (Africa Sample).xlsx")
write.xlsx(Statistic.Country, "Statistic-Country (Africa Sample).xlsx")


# Depiction
library(ggplot2) # loads for creating plots


# Extract and Converts these unique values into data frames.

DataBaseLong.Variable <- data.frame(unique(DataBaseLong$Variable))
DataBaseLong.Year <- data.frame(unique(DataBaseLong$Date))


install.packages("stringr")
library(stringr)

nrow(DataBaseLong)
DataBaseLong.Variable[, 2] <- str_replace(DataBaseLong.Variable[, 1], "/", "per")
DataBaseLong.Variable[, 2] <- str_replace(DataBaseLong.Variable[, 2], "%", "percent")

DataBaseLong$Sector <- factor(DataBaseLong$Sector, levels = c("Wholesale", "Other"))


# Box plot
for (i in 1:nrow(DataBaseLong.Variable)) {
  ggplot(subset(DataBaseLong, Variable == DataBaseLong.Variable[i, 1])) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    geom_boxplot(aes(x = Date, y = Value, fill = Sector), color = 'black', outlier.alpha = 0.1) + # outlier.shape = NA
    labs(title = substr(DataBaseLong.Variable[i, 2], 1, str_length(DataBaseLong.Variable[i, 2]) - 2), subtitle = "2012 - 2020", caption = "") +
    theme_classic() +
    theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = "top")
  
  setwd("C:/Users/User/Desktop/Documents/Mine/corvinus/exim_bank/Outputs/Africa_Sample/box plot")
  ggsave(paste(i, "-", substr(DataBaseLong.Variable[i, 2], 1, str_length(DataBaseLong.Variable[i, 2]) - 2), " (Africa)", ".png", sep = ""), width = 5, height = 4)
}


# Histogram
for (i in 1:nrow(DataBaseLong.Variable)) {
    ggplot(subset(DataBaseLong, Variable == DataBaseLong.Variable[i, 1])) +
      geom_histogram(aes(x = Value), fill = rgb(43, 129, 125, max = 255), color = 'black') + #  binwidth = 10
      geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
      labs(title = substr(DataBaseLong.Variable[i, 2], 1, str_length(DataBaseLong.Variable[i, 2]) - 2), caption = "") +
      theme_classic() +
      theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = "top")
    
    setwd("C:/Users/User/Desktop/Documents/Mine/corvinus/exim_bank/Outputs/Africa_Sample/Histogram")
    ggsave(paste(i, "-", substr(DataBaseLong.Variable[i, 2], 1, str_length(DataBaseLong.Variable[i, 2]) - 2), " (Africa)", ".png", sep = ""), width = 5, height = 3) 
  }




# Region box plot
for (i in 1:nrow(DataBaseLong.Variable)) {
    ggplot(subset(DataBaseLong, Variable == DataBaseLong.Variable[i, 1])) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
      geom_boxplot(aes(x = World.region, y = Value, fill = Sector), color = 'black', outlier.alpha = 0.1) + # aes(colour = Date)
      labs(title = substr(DataBaseLong.Variable[i, 2], 1, str_length(DataBaseLong.Variable[i, 2]) - 2), caption = "") +
      coord_flip() + theme_classic() +
      theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = "top")
    
    setwd("C:/Users/User/Desktop/Documents/Mine/corvinus/exim_bank/Outputs/Africa_Sample/Region")
    ggsave(paste(i, "-Region-", substr(DataBaseLong.Variable[i, 2], 1, str_length(DataBaseLong.Variable[i, 2]) - 2)," ", " (Africa)", ".png", sep = ""), width = 5, height = 4) 
  }



#  Sample without outliers----
# filtering outliers  
Statistic.All$Lower <- Statistic.All$Q1 - 1.5 * Statistic.All$IQR
Statistic.All$Upper <- Statistic.All$Q3 + 1.5 * Statistic.All$IQR

DataBase$Lower <- Statistic.All$Lower[match(paste(DataBase$Variable, DataBase$Date), paste( Statistic.All$Variable,  Statistic.All$Date))]
DataBase$Upper <- Statistic.All$Upper[match(paste(DataBase$Variable, DataBase$Date), paste( Statistic.All$Variable,  Statistic.All$Date))]

DataBase <- subset(DataBase, Value > Lower & Value < Upper)
#ncol(DataBase)
#colnames(DataBase)
DataBase <- DataBase[, -c(11:12)] #Omitting columns 11 and 12
#colnames(DataBase)



# DataBase  Formating


DataBaseLong <- DataBase
DataBaseWide <- dcast(DataBase, Company.name.Latin.alphabet + Country.ISO.code + Country + World.region + BvD.sectors + BvD.ID.number + Date ~ Variable, value.var = 'Value',  na.rm = TRUE)
colnames(DataBaseWide)
colnames(DataBaseLong)

# Correlaton Matrix
Correlation <- subset(DataBaseWide, Date == 2019)
colnames(Correlation)
Correlation <- Correlation[, c(9, 11, 13)]
#Correlation <- Correlation[complete.cases(Correlation), ]
Correlation.matrix <- cor(Correlation, method = "pearson", use = "pairwise.complete.obs") # method = "pearson", "kendall", "spearman"
summary(Correlation.matrix)  # Check for NA and Inf values
str(Correlation.matrix)      # Check the structure of the matrix




setwd("C:/Users/User/Desktop/Documents/Mine/corvinus/exim_bank/Outputs/Africa_Sample/No_Outliers")
png(filename = "Correlation Matrix (Without Outliers).png", width = 600, height = 600)

corrplot(corrgram(Correlation.matrix),
         method = "color", order = "hclust", addrect = 2,
         addCoef.col = "black", number.digits = 2, number.cex = 0.7, 
         tl.col = "black", tl.cex = 0.8, tl.srt = 90, cl.pos = "n")

dev.off()  

# Basic Statistics
Statistic.All <- DataBaseLong %>% 
  group_by(Variable, Date) %>%
  summarise('N' = n(),
            'Na' = sum(is.na(Value)),
            'Min' = min(Value, na.rm = TRUE),
            'P5' = quantile(Value, c(0.05), na.rm = TRUE),
            'P10' = quantile(Value, c(0.10), na.rm = TRUE),
            'P15' = quantile(Value, c(0.15), na.rm = TRUE),
            'P20' = quantile(Value, c(0.20), na.rm = TRUE),
            'Q1' = quantile(Value, c(0.25), na.rm = TRUE),
            'P30' = quantile(Value, c(0.30), na.rm = TRUE),
            'P35' = quantile(Value, c(0.35), na.rm = TRUE),
            'P40' = quantile(Value, c(0.40), na.rm = TRUE),
            'P45' = quantile(Value, c(0.45), na.rm = TRUE),
            'Median' = median(Value, na.rm = TRUE),
            'Mean' = mean(Value, na.rm = TRUE), 
            'P55' = quantile(Value, c(0.55), na.rm = TRUE),
            'P60' = quantile(Value, c(0.60), na.rm = TRUE),
            'P65' = quantile(Value, c(0.65), na.rm = TRUE),
            'P70' = quantile(Value, c(0.70), na.rm = TRUE),
            'Q3' = quantile(Value, c(0.75), na.rm = TRUE),
            'P80' = quantile(Value, c(0.80), na.rm = TRUE),
            'P85' = quantile(Value, c(0.85), na.rm = TRUE),
            'P90' = quantile(Value, c(0.90), na.rm = TRUE),
            'P95' = quantile(Value, c(0.95), na.rm = TRUE),
            'Max' = max(Value, na.rm = TRUE),
            'IQR' = IQR(Value, na.rm = TRUE),
            'Sd' = sd(Value, na.rm = TRUE)) 

Statistic.Region <- DataBaseLong %>% 
  group_by(Variable, World.region, Date) %>%
  summarise('N' = n(),
            'Na' = sum(is.na(Value)),
            'Min' = min(Value, na.rm = TRUE),
            'P5' = quantile(Value, c(0.05), na.rm = TRUE),
            'P10' = quantile(Value, c(0.10), na.rm = TRUE),
            'P15' = quantile(Value, c(0.15), na.rm = TRUE),
            'P20' = quantile(Value, c(0.20), na.rm = TRUE),
            'Q1' = quantile(Value, c(0.25), na.rm = TRUE),
            'P30' = quantile(Value, c(0.30), na.rm = TRUE),
            'P35' = quantile(Value, c(0.35), na.rm = TRUE),
            'P40' = quantile(Value, c(0.40), na.rm = TRUE),
            'P45' = quantile(Value, c(0.45), na.rm = TRUE),
            'Median' = median(Value, na.rm = TRUE),
            'Mean' = mean(Value, na.rm = TRUE), 
            'P55' = quantile(Value, c(0.55), na.rm = TRUE),
            'P60' = quantile(Value, c(0.60), na.rm = TRUE),
            'P65' = quantile(Value, c(0.65), na.rm = TRUE),
            'P70' = quantile(Value, c(0.70), na.rm = TRUE),
            'Q3' = quantile(Value, c(0.75), na.rm = TRUE),
            'P80' = quantile(Value, c(0.80), na.rm = TRUE),
            'P85' = quantile(Value, c(0.85), na.rm = TRUE),
            'P90' = quantile(Value, c(0.90), na.rm = TRUE),
            'P95' = quantile(Value, c(0.95), na.rm = TRUE),
            'Max' = max(Value, na.rm = TRUE),
            'IQR' = IQR(Value, na.rm = TRUE),
            'Sd' = sd(Value, na.rm = TRUE)) 

Statistic.Country <- DataBaseLong %>% 
  group_by(Variable, World.region, Country, Date) %>%
  summarise('N' = n(),
            'Na' = sum(is.na(Value)),
            'Min' = min(Value, na.rm = TRUE),
            'P5' = quantile(Value, c(0.05), na.rm = TRUE),
            'P10' = quantile(Value, c(0.10), na.rm = TRUE),
            'P15' = quantile(Value, c(0.15), na.rm = TRUE),
            'P20' = quantile(Value, c(0.20), na.rm = TRUE),
            'Q1' = quantile(Value, c(0.25), na.rm = TRUE),
            'P30' = quantile(Value, c(0.30), na.rm = TRUE),
            'P35' = quantile(Value, c(0.35), na.rm = TRUE),
            'P40' = quantile(Value, c(0.40), na.rm = TRUE),
            'P45' = quantile(Value, c(0.45), na.rm = TRUE),
            'Median' = median(Value, na.rm = TRUE),
            'Mean' = mean(Value, na.rm = TRUE), 
            'P55' = quantile(Value, c(0.55), na.rm = TRUE),
            'P60' = quantile(Value, c(0.60), na.rm = TRUE),
            'P65' = quantile(Value, c(0.65), na.rm = TRUE),
            'P70' = quantile(Value, c(0.70), na.rm = TRUE),
            'Q3' = quantile(Value, c(0.75), na.rm = TRUE),
            'P80' = quantile(Value, c(0.80), na.rm = TRUE),
            'P85' = quantile(Value, c(0.85), na.rm = TRUE),
            'P90' = quantile(Value, c(0.90), na.rm = TRUE),
            'P95' = quantile(Value, c(0.95), na.rm = TRUE),
            'Max' = max(Value, na.rm = TRUE),
            'IQR' = IQR(Value, na.rm = TRUE),
            'Sd' = sd(Value, na.rm = TRUE)) 

# Exports of  Statistics
setwd("C:/Users/User/Desktop/Documents/Mine/corvinus/exim_bank/Outputs/Africa_Sample/No_Outliers")
write.xlsx(Statistic.All, "Statistic-All (No Outliers).xlsx")
write.xlsx(Statistic.Region, "Statistic-Region (No Outliers).xlsx")
write.xlsx(Statistic.Country, "Statistic-Country ( No Outliers).xlsx")

# Representations


DataBaseLong.Variable <- data.frame(unique(DataBaseLong$Variable))
DataBaseLong.Year <- data.frame(unique(DataBaseLong$Date))
DataBaseLong.Variable[, 2] <- str_replace(DataBaseLong.Variable[, 1], "/", "per")
DataBaseLong.Variable[, 2] <- str_replace(DataBaseLong.Variable[, 2], "%", "percent")

DataBaseLong$Sector <- factor(DataBaseLong$Sector, levels = c("Wholesale", "Other"))

# Box plot
for (i in 1:nrow(DataBaseLong.Variable)) {
  ggplot(subset(DataBaseLong, Variable == DataBaseLong.Variable[i, 1])) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    geom_boxplot(aes(x = Date, y = Value, fill = Sector), color = 'black', outlier.alpha = 0.1) + # outlier.shape = NA
    labs(title = substr(DataBaseLong.Variable[i, 2], 1, str_length(DataBaseLong.Variable[i, 2]) - 2), subtitle = "2012 - 2020", caption = "") +
    theme_classic() +
    theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = "top")
  
  setwd("C:/Users/User/Desktop/Documents/Mine/corvinus/exim_bank/Outputs/Africa_Sample/No_Outliers/box plot")
  ggsave(paste(i, "-", substr(DataBaseLong.Variable[i, 2], 1, str_length(DataBaseLong.Variable[i, 2]) - 2), " (No Outliers)", ".png", sep = ""), width = 5, height = 4)
}

# Histogram
for (i in 1:nrow(DataBaseLong.Variable)) {
    ggplot(subset(DataBaseLong, Variable == DataBaseLong.Variable[i, 1])) +
      geom_histogram(aes(x = Value), fill = rgb(43, 129, 125, max = 255), color = 'black') + #  binwidth = 10
      geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
      labs(title = substr(DataBaseLong.Variable[i, 2], 1, str_length(DataBaseLong.Variable[i, 2]) - 2), caption = "") +
      theme_classic() +
      theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = "top")
    
    setwd("C:/Users/User/Desktop/Documents/Mine/corvinus/exim_bank/Outputs/Africa_Sample/No_Outliers/Histogram")
    ggsave(paste(i, "-", substr(DataBaseLong.Variable[i, 2], 1, str_length(DataBaseLong.Variable[i, 2]) - 2)," ",  " (No Outliers)", ".png", sep = ""), width = 5, height = 3) 
  }





# STATISTICAL ANALYSIS ----------------------------------------------------
# Descriptive statistics - calculating the scores



quantile.list <- c(seq(0, 1, 1/7)) # Define the number of quantiles here. For 7 quantiles: c(seq(0, 1, 1/7))
Statistics.Quantiles <- as.data.frame(unique(DataBaseLong$Variable))
colnames(Statistics.Quantiles) <- "Variable"

for (i in 1:length(quantile.list)) {
  statistics.quantiles <- DataBaseLong %>% group_by(Variable) %>% summarise(quantile(Value, quantile.list[i], na.rm = T))
  colnames(statistics.quantiles)[2] <- quantile.list[i]
  Statistics.Quantiles <- full_join(Statistics.Quantiles, statistics.quantiles, by = "Variable")
}



# Gross Margin

Grossmargin = DataBaseWide[,c(1:7,9)]
Grossmargin = na.omit(Grossmargin)
GM = quantile(Grossmargin$`Gross margin      `, probs = seq(0,1,1/7))
Grossmargin$scores = cut(Grossmargin$`Gross margin      `, GM, include.lowest=TRUE,labels=c("1","2","3","4","5","6","7")) # Assigning scores
Grossmargin_scores = as.data.frame(Grossmargin) # scores as data frame




# Solvency Ratio liability based

Solvency = DataBaseWide[,c(1:7,11)]
Solvency = na.omit(Solvency)
SR = quantile(Solvency$`Solvency ratio  Liability based       `, probs = seq(0,1,1/7))
Solvency$scores = cut(Solvency$`Solvency ratio  Liability based       `, SR, include.lowest = TRUE,labels=c("1","2","3","4","5","6","7")) # Assigning scores 
Solvency_scores = as.data.frame(Solvency) # scores as data frame


# Stock Turnover

Turnover = DataBaseWide[,c(1:7,13)]
Turnover = na.omit(Turnover)
ST = quantile(Turnover$`Stock turnover  `, probs = seq(0,1,1/7))
Turnover$scores = cut(Turnover$`Stock turnover  `  , ST, include.lowest = TRUE,labels=c("1","2","3","4","5","6","7")) # Assigning scores 
Turnover = as.data.frame(Turnover) # scores as data frame





#NB: Below variables are not included in the africa sample due to insufficient space on computer




#cash flow operating revenue 


cashflow= DataBaseWide[1:3]
cashflow=na.omit(cashflow)
cash=quantile(cashflow$`Cash flow Operating revenue`, probs = seq(0, 1, 1/7))    
cashflow$scores=cut(cashflow$`Cash flow Operating revenue`, 
                    cash,include.lowest=TRUE,labels=c("1","2","3","4","5","6","7"))
cashflow_scores=as.data.frame(cashflow)






#collection period days
collect= DataBaseWide%>%select(1,2,4)
collect=na.omit(collect)
collection=quantile(collect$`Collection period days`, probs = seq(0, 1, 1/7))    
collect$scores=cut(collect$`Collection period days`, 
                   collection,include.lowest=TRUE,labels=c("1","2","3","4","5","6","7"))
collection_scores=as.data.frame(collect)






#cost of employees operating revenue 

Employee= DataBaseWide%>%select(1,2,5)
Employee=na.omit(Employee)
employee=quantile(Employee$`Costs of employees Operating revenue`, probs = seq(0, 1, 1/7))    
Employee$scores=cut(Employee$`Costs of employees Operating revenue`, 
                    employee,include.lowest=TRUE,labels=c("1","2","3","4","5","6","7"))
Employee_scores=as.data.frame(Employee)




#credit period  

Credit= DataBaseWide%>%select(1,2,6)
Credit=na.omit(Credit)
credit=quantile(Credit$`Credit period days`, probs = seq(0, 1, 1/7),na.rm = TRUE)    

credit

## we extract a vector with the credit period

credit_period_values <- Credit$`Credit period days`



## We need to get the numbers score for every item using a loop and stored in credit period_score character vector

credit_period_score_number <- character()

for (i in 1:length(credit_period_values)) {
  
  # we iterate and calculate the number_grade one credit period at a time
  
  one_credit_period_values_score <- credit_period_values[i]
  
  if(one_credit_period_values_score >= credit[7]) { credit_period_score_number[i] <- "7"
  } else if (one_credit_period_values_score >= credit[6] & one_credit_period_values_score< credit[7]) {credit_period_score_number[i] <- "6"
  } else if (one_credit_period_values_score >= credit[5] & one_credit_period_values_score < credit[6]) {credit_period_score_number[i] <- "5"
  } else if (one_credit_period_values_score >= credit[4] & one_credit_period_values_score < credit[5]) {credit_period_score_number[i] <- "4"
  } else if (one_credit_period_values_score >= credit[3] & one_credit_period_values_score < credit[4]) {credit_period_score_number[i] <- "3"
  } else if (one_credit_period_values_score >= credit[2] & one_credit_period_values_score < credit[3]) {credit_period_score_number[i] <- "2"
  } else {credit_period_score_number[i] <- "1"}
  
}
rm <- i # we remove the i index

## These are the scores for each credit period

credit_period_score_number
credit_period_with_scores=cbind.data.frame(Credit,credit_period_score_number)



#current ratio

Current= DataBaseWide%>%select(1,2,7)
Current=na.omit(Current)
current=quantile(Current$`Current ratio`, probs = seq(0, 1, 1/7),na.rm = TRUE)    

current

## we extract a vector with the current ratio

current_ratio_values <- Current$`Current ratio`



## We need to get the numbers score for every item using a loop and stored in current ratio_score character vector

current_ratio_score_number <- character()

for (i in 1:length(current_ratio_values)) {
  
  # we iterate and calculate the number_grade one current ratio at a time
  
  one_current_ratio_values_score <- current_ratio_values[i]
  
  if(one_current_ratio_values_score >= current[7]) { current_ratio_score_number[i] <- "7"
  } else if (one_current_ratio_values_score >= current[6] & one_current_ratio_values_score< current[7]) {current_ratio_score_number[i] <- "6"
  } else if (one_current_ratio_values_score >= current[5] & one_current_ratio_values_score< current[6]) {current_ratio_score_number[i] <- "5"
  } else if (one_current_ratio_values_score >= current[4] & one_current_ratio_values_score < current[5]) {current_ratio_score_number[i] <- "4"
  } else if (one_current_ratio_values_score >= current[3] & one_current_ratio_values_score < current[4]) {current_ratio_score_number[i] <- "3"
  } else if (one_current_ratio_values_score >= current[2] & one_current_ratio_values_score < current[3]) {current_ratio_score_number[i] <- "2"
  } else {current_ratio_score_number[i] <- "1"}
  
}
rm <- i # we remove the i index

## These are the scores for each current ratio

current_ratio_score_number
current_ratio_with_scores=cbind.data.frame(Current,current_ratio_score_number)





#EBIT margin

ebit= DataBaseWide%>%select(1,2,8)
ebit=na.omit(ebit)
EBIT=quantile(ebit$`EBIT margin`, probs = seq(0, 1, 1/7))    

EBIT

## we extract a vector with the ebit margin

EBIT_margin_values <- ebit$`EBIT margin`



## We need to get the numbers score for every item using a loop and stored in ebit margin_score character vector

EBIT_margin_score_number <- character()

for (i in 1:length(EBIT_margin_values)) {
  
  # we iterate and calculate the number_grade one ebit margin at a time
  
  one_EBIT_margin_values_score <- EBIT_margin_values[i]
  
  if(one_EBIT_margin_values_score >= EBIT[7]) { EBIT_margin_score_number[i] <- "7"
  } else if (one_EBIT_margin_values_score >= EBIT[6] & one_EBIT_margin_values_score< EBIT[7]) {EBIT_margin_score_number[i] <- "6"
  } else if (one_EBIT_margin_values_score >= EBIT[5] & one_EBIT_margin_values_score < EBIT[6]) {EBIT_margin_score_number[i] <- "5"
  } else if (one_EBIT_margin_values_score >= EBIT[4] & one_EBIT_margin_values_score < EBIT[5]) {EBIT_margin_score_number[i] <- "4"
  } else if (one_EBIT_margin_values_score >= EBIT[3] & one_EBIT_margin_values_score < EBIT[4]) {EBIT_margin_score_number[i] <- "3"
  } else if (one_EBIT_margin_values_score >= EBIT[2] & one_EBIT_margin_values_score < EBIT[3]) {EBIT_margin_score_number[i] <- "2"
  } else {EBIT_margin_score_number[i] <- "1"}
  
}
rm <- i # we remove the i index

## These are the scores for each EBIT margin

EBIT_margin_score_number
EBIT_margin_with_scores=cbind.data.frame(ebit,EBIT_margin_score_number)




#EBITDA margin

ebitda= DataBaseWide%>%select(1,2,9)
ebitda=na.omit(ebitda)
EBITDA=quantile(ebitda$`EBITDA margin`, probs = seq(0, 1, 1/7))    

EBITDA

## we extract a vector with the ebit margin

EBITDA_margin_values <- ebitda$`EBITDA margin`



## We need to get the numbers score for every item using a loop and stored in ebitda margin_score character vector

EBITDA_margin_score_number <- character()

for (i in 1:length(EBITDA_margin_values)) {
  
  # we iterate and calculate the number_grade one ebitda margin at a time
  
  one_EBITDA_margin_values_score <- EBITDA_margin_values[i]
  
  if(one_EBITDA_margin_values_score >= EBITDA[7]) { EBITDA_margin_score_number[i] <- "7"
  } else if (one_EBITDA_margin_values_score >= EBITDA[6] & one_EBITDA_margin_values_score< EBITDA[7]) {EBITDA_margin_score_number[i] <- "6"
  } else if (one_EBITDA_margin_values_score >= EBITDA[5] & one_EBITDA_margin_values_score < EBITDA[6]) {EBITDA_margin_score_number[i] <- "5"
  } else if (one_EBITDA_margin_values_score >= EBITDA[4] & one_EBITDA_margin_values_score < EBITDA[5]) {EBITDA_margin_score_number[i] <- "4"
  } else if (one_EBITDA_margin_values_score >= EBITDA[3] & one_EBITDA_margin_values_score < EBITDA[4]) {EBITDA_margin_score_number[i] <- "3"
  } else if (one_EBITDA_margin_values_score >= EBITDA[2] & one_EBITDA_margin_values_score < EBITDA[3]) {EBITDA_margin_score_number[i] <- "2"
  } else {EBITDA_margin_score_number[i] <- "1"}
  
}
rm <- i # we remove the i index

## These are the scores for each EBITDA margin

EBITDA_margin_score_number
EBITDA_margin_with_scores=cbind.data.frame(ebitda,EBITDA_margin_score_number)





#gearing

gear1= DataBaseWide%>%select(1,2,10)
gear=na.omit(gear1)
GEARING=quantile(gear$Gearing, probs = seq(0, 1, 1/7))    

GEARING

## we extract a vector with the GEARING 

gearing_values <- gear$Gearing



## We need to get the numbers score for every item using a loop and stored in gearing_score character vector

gearing_value_score_number <- character()

for (i in 1:length(gearing_values)) {
  
  # we iterate and calculate the number_grade one gearing  at a time
  
  one_gearing_values_score <- gearing_values[i]
  
  if(one_gearing_values_score >= GEARING[7]) { gearing_value_score_number[i] <- "7"
  } else if (one_gearing_values_score >= GEARING[6] & one_gearing_values_score< GEARING[7]) {gearing_value_score_number[i] <- "6"
  } else if (one_gearing_values_score >= GEARING[5] & one_gearing_values_score < GEARING[6]) {gearing_value_score_number[i] <- "5"
  } else if (one_gearing_values_score >= GEARING[4] & one_gearing_values_score < GEARING[5]) {gearing_value_score_number[i] <- "4"
  } else if (one_gearing_values_score >= GEARING[3] & one_gearing_values_score < GEARING[4]) {gearing_value_score_number[i] <- "3"
  } else if (one_gearing_values_score >= GEARING[2] & one_gearing_values_score < GEARING[3]) {gearing_value_score_number[i] <- "2"
  } else {gearing_value_score_number[i] <- "1"}
  
}
rm <- i # we remove the i index

## These are the scores for each EBIT margin

gearing_value_score_number
gearing_value_score_number_with_scores=cbind.data.frame(gear,gearing_value_score_number)

















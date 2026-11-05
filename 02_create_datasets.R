# Import your CSV file

data <- read.csv(file.choose())

# Display the first 6 records

head(data)

# Display column names

colnames(data)

# Check number of rows and columns

dim(data)

# Check dataset structure

str(data)

# Open dataset in RStudio

View(data)
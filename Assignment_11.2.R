#Assignment_11.2

# dataset : https://www.kaggle.com/hugodarwood/epirecipes/data
# Perform the below given activities:
# a. apply K-means clustering to identify similar recipies
# b. apply K-means clustering to identify similar attributes
# c. how many unique recipies that people order often
# d. what are their typical profiles

# -----------------------------------------------------------------

# Import Zip File
getwd()
setwd("C:\\Users\\Aadhya Singh\\Documents\\Assignment")
data <- read.csv(unzip("epi_r.zip"))
View(data)
dim(data)
str(data)

# Preprocessing the data set
colnames(data)
library(maps)
data("world.cities")
world.cities$country.etc <- toupper(world.cities$country.etc)

raw <- colnames(data)               # column names tored in a vector
raw <- gsub("[[:punct:]\n]","",raw) # Removing punctuation
raw <- strsplit(raw, " ")          # Split data at word boundaries
raw <- toupper(raw)                # convert to upper case
length(raw)

# Match on country / cities in world.countries
CountryList_raw <- (lapply(raw, function(x)x[which(x %in% world.cities$country.etc)]))

colnames(data) <- raw

# check for NA
sum(is.na(data))
sort(sapply(data, function(x) sum(is.na(x))))

# impute missing values
library(mice)
imputed = mice(data[,c("CALORIES", "SODIUM", "PROTEIN", "FAT")], method='cart', m=5)

imputed <- mice::complete(imputed)

# replacing NAs with imputed values
data$CALORIES <- imputed$CALORIES
data$PROTEIN <- imputed$PROTEIN
data$SODIUM <- imputed$SODIUM
data$FAT <- imputed$FAT
sum(is.na(data))

# checking for outliers
library(ggplot2)
ggplot(reshape2::melt(data[,c("CALORIES", "SODIUM", "PROTEIN", "FAT")]), 
      aes(x= variable, value, fill = variable))+
 geom_boxplot()+facet_wrap(~variable, scales = 'free_y')
# yes there are outliers

# removing these outliers
df <- outliers::rm.outlier(data[,c("CALORIES", "SODIUM", "PROTEIN", "FAT")], fill = TRUE)
data$CALORIES <- df$CALORIES
data$PROTEIN <- df$PROTEIN
data$SODIUM <- df$SODIUM
data$FAT <- df$FAT
dim(data)

TITLE <- data$TITLE

# Load required libraries
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization

#----------------------------------------------------------------------
# a. apply K-means clustering to identify similar recipies

# preparing data set for receipe
set.seed(123)
data_recipe <- data[,-c(1,unlist(which(raw %in% world.cities$country.etc)))]
data_recipe <- scale(data_recipe)

# Compute k-means clustering with k = 5
final_recipe <- kmeans(data_recipe, 5, nstart = 25)
summary(final_recipe)

table(final_recipe$cluster)            # cluster for similar recipes
fviz_cluster(final_recipe, data = data_recipe)

# ----------------------------------------------------------------------------
# b. apply K-means clustering to identify similar attributes

# preparing data set for receipe
set.seed(123)
data_att <- data[,c(unlist(which(raw %in% world.cities$country.etc)))]

# Compute k-means clustering with k = 2
final_att <- kmeans(data_att, 2, nstart = 25)
summary(final_att)

table(final_att$cluster)            # cluster for similar attributes
fviz_cluster(final_att, data = data_att)

# -----------------------------------------------------------------------------
# c. how many unique recipies that people order often
df$Clusters <- final_recipe$cluster
df$TITLE <- TITLE
by_cluster <- df %>% group_by(Clusters) %>% summarise_all("length") %>% select(Clusters, TITLE)
by_cluster
max(by_cluster$TITLE)

# -----------------------------------------------------------------------------
# d. what are their typical profiles
profile <- (df[,-6] %>% group_by(Clusters) %>% summarise_all("mean") %>%
            select("CALORIES", "SODIUM", "PROTEIN", "FAT"))[1,]
profile

# ------------------------------------------------------------------------------

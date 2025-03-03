# Fashion-Brand-Analysis
Analyzing fashion brands using R


# Loading Packages and Data

First we need to load the package `tidyverse` and the data frame `fashion_brand`. We got the data from Kaggle.

```{r}
pacman::p_load(tidyverse, readxl, janitor, knitr, kableExtra, stringr)
fashion_brand<-read_csv("/Users/yuxuanyang/Downloads/fashion_data.csv")
```

# Introduction
This project explores and analyzes the `fashion_brand` dataset, sourced from Kaggle, which contains data on 30 fashion brands across 68 variables. The primary goal is to investigate trends and correlations related to brand rankings, growth rates, and the *luxury* fashion industry through exploratory data analysis.

# Data Description and Exploration
First, we need to have an overview of the structure of the dataset `fashion_brand` to get ready for further analysis. Since our raw data is very wide and contains many columns that describe the same variable across different years, applying `str()` for the whole dataset will generate a report that is too long and redundant. Therefore, we decided to select the column for each variable (rank, Equity, and growth rate) for the year 2001. In other words, apart from `BrandName`, `BrandOriginCountry`, `BrandOriginRegion`, `BrandSector`, and `BrandSubSector`, only `Rank2001`, `Equity2001`, and `GrowthRate2001` will be included and used with `str()`.

```{r}
# get the structure of the dataset and a summary of a part of 
# the selected dataset
selected_data <- select (fashion_brand, 
                         BrandName, 
                         BrandOriginCountry, 
                         BrandOriginRegion, 
                         BrandSector, 
                         BrandSubSector, 
                         Rank2001, 
                         Equity2001, 
                         GrowthRate2001)
```

Then, we will first apply `str()` to `selected_data`. `str()` gives us an overview of the dataset's structure, including the data types and the first few values of each column.
```{r}
str(selected_data)
```
We also apply `summary()` to `selected_data`. `summary()` provides a statistical summary of each column, showing key metrics like mean, median, minimum, maximum, and quartiles for numerical columns, and frequency counts for categorical columns.
```{r}
summary(selected_data)
```
There are **30 observations** in total, and each observation represents a single fashion brand. There are **68 variables** in the raw dataset. 

## Data Dictionary
The following is the data dictionary of the dataset. This helps us to be clearer with what each variable in the dataset means.

`BrandName`: The names of Brands that the data analyzes. They are all character strings.

`BrandOriginCountry`: The country where the brand originated. They are all character strings.

`BrandOriginRegion`: The continent where the brand originated, either Europe or America. They are all character strings. 

`BrandSector`: The consumer sector that the brand falls under. They are all character strings and have the same value of `Fashion`.

`BrandSubSector`: The sub-sector that the brand falls under. They are all character strings. Values contain `Luxury`, `Cosmetics`, `Sportswear`, and `Apparel`.

`Rank2001`-`Rank2021`: The brand’s ranking in Interbrand’s Top 100 Global Brand list from 2001 to 2021 ranges from 1 to 100. The smaller the number, the higher the ranking. For example, 1 would be the best, and the 100 would be the worst among all brands that made it to the list. They are doubles. 

`Equity2001`-`Equity2021`: The overall market value of the brand, in USD billion from 2001 to 2021, number range can vary depending on the company size but should be always positive. They are all doubles.

`GrowthRate2001`-`GrowthRate2021`: The percentage growth in brand equity from 2001 to 2021. A 0.01417836 would mean a 0.014% growth in brand equity. The number can be both positive and negative, ranging from 0 to 100. They are all doubles. 

# Research Questions
The first research question we will answer is which brand has the highest mean ranking among *all brands*. We will group the data based on the `BrandName` and calculate the mean ranking for each of the brand. Then, we will use `arrange` to find out which brand has the highest mean ranking, which reflect. This research question help us find the brand that has the highest customer preferences or satisfaction levels based on aggregated ratings.

The second research question we want to answer is the correlation between **brand ranking** and **brand growth rate** for *all brands*. We want to make a scatterplot where the x-axis is brand ranking and the y-axis is growth rate. We want to plot each brand’s performance on these two metrics for each year from 2001 to 2021. By analyzing this relationship. We will break the plot into four sub-plots based on `BrandSubSector`. With the result, we can gain insights into whether higher-ranked brands consistently achieve greater growth, which can inform strategies for brand positioning, marketing investments, and long-term planning in the highly competitive luxury market.

Our third research question is to figure out among all *luxury brands*, which one of them had the highest ranking the most number of times, and we also want to know the trend of the ranking of that luxury brand. Understanding which brand consistently achieves the highest ranking and examining its trend can provide valuable insights into the evolving dynamics of consumer preferences and market competition in the luxury industry.

# Data Cleaning
Before we get into our research questions, we need to clean the data first and turn it into something easily to be dealt with. 

## Methods
We need to perform the following cleaning tasks:

1. Use `pivot_longer()` to reshape the current wide dataset into a long format.

2. Use `stringr` package, or to be more specific, `str_remove_all()`. to remove any useless dot in `BrandOriginCountry`.

3. Use `factor()` to convert `BrandSubSector` and `BrandOriginCountry` from character strings into factors.

4. Use `as.numeric()` to convert the column `year`, which is currently stored as **characters**, into **doubles**.

5. Use `clean_names()` to standardize all variable names by converting them into snake-case.

6. Use `summary()` to check for implausible values in the dataset and get rid of them if there are implausible values.

## Cleaning the data
We need to first transform the data and make it longer and narrower.

```{r}
cleaned_1<-fashion_brand|>
  pivot_longer(
    cols = c(Rank2001:Rank2021, 
             Equity2001:Equity2021, 
             GrowthRate2001:GrowthRate2021),
    names_to = c(".value", "year"),
    names_pattern = "(^[[:alpha:]]*)([[:digit:]]{4})")
cleaned_1
```
To make the data look tidier, we want to remove the extra "." in BrandOriginCountry, such as the extra "." "in U.S.A."
```{r}
cleaned_2 <- cleaned_1|>
  mutate(BrandOriginCountry = str_remove_all(BrandOriginCountry, "\\."))
```

Then, we want to combine `BrandOriginCountry` and `BrandOriginRegion` as we think only one variable is needed to represent the origin of the brand.
```{r}
cleaned_3 <- cleaned_2|>
  unite(brand_origin, # creating a new brand_orgin column
        BrandOriginCountry,
        BrandOriginRegion,
        sep = ", ") # Split the values using ", " in the new column
```

Next, we will convert the column `year`, which is currently stored as **characters**, into **doubles** for further analysis.

```{r}
cleaned_4 <- cleaned_3|>
  mutate(year = as.numeric(year))
```

Additionally, we will convert the titles of the variables into snake cases.

```{r}
cleaned_5 <- cleaned_4|>
  clean_names()
cleaned_5
```

As shown above, here is the cleaned data up to this point.

The last step of data cleaning is checking for implausible values, particularly the numerical variables. This requires an analysis of the summary of the cleaned data.

```{r}
summary(cleaned_5)
```
From the summary, we can see that for all the other variables, especially numerical ones, the values all lie in reasonable ranges: `Rank` is between 1 and 100, and `growth_rate` is between -100 and 100, the unit of which is %. However, for `equity`, given the unit is \$bn, we find that even the minimum equity value is  \$1002 bn, which is impossible. Initially, we thought it might be that the creator of the dataset meant to write **mn** instead of **bn**, but after we checked the equity value of several brands for several years, we found that the data still doesn't match the actual equity values of brands even had the author put those values in million dollars. Therefore, we decided to abandon the `equity` column and design research questions that are irrelevant to equity values.

```{r}
cleaned_final<-cleaned_5|>
  select(-equity)
cleaned_final
```

Now, the data is cleaned and ready to be analyzed.

# Investigation and Data Visualization
Now, we will begin investgating and answering our three research questions.

## Finding the Brand with Highest Mean Ranking
For the first research question, we want to find out among *all brands*, which brand has the highest mean ranking. 
```{r}
# Group the cleaned data by brand name 
# and calculate the average ranking for each brand
rank <- cleaned_final |>
  # Transformation skill 1: grouped calculation to 
  # find the average ranking for each brand
  group_by(brand_name) |>
  summarize(mean_ranking = mean(rank, na.rm = TRUE))|>
  # Transformation skill 2: sorting to find 
  # the brand with the highest mean ranking
  arrange(mean_ranking)

print(rank)

```

From the above, we can see that **Louis Vuitton** has the highest mean ranking, which is **22**, among *all the brands*. This suggests that the brand holds a strong and stable position in the fashion industry, possibly due to its equity value, growth rate, or effective marketing strategies. It may also indicate that Louis Vuitton is perceived as one of the top luxury brands, maintaining its competitive edge over time. 

## Correlation Between Ranking and Equity for *All Brands*
Next, we want to unveil the correlation between `rank` and `growth_rate` for *all brands* across the four sub-sectors: *Apparel*, *Cosmetics*, *Luxury*, and *Sportswear*.

We want to draw a scatterplot to show the correlation between `rank` and `growth_rate` for *all brands* between 2001 and 2021. We will break the plot into four plots based on `brand_sub_sector`. 

```{r}
# Creating a scatter plot using ggplot
# Specifying the data and aesthetics 
# (rank on x-axis, growth_rate on y-axis)
ggplot(cleaned_final, aes(x = rank, y = growth_rate))+
  # Adding points to the scatter plot to 
  # represent individual data points; plotting
  # the points in green
  geom_point(color = "green") +
  # Adding titles and axis titles to the plot
  labs(
    title = "Correlation between Rank and Growth Rate for Luxury Brands",
    x = "Rank", 
    y = "Growth Rate (%)" 
  ) +
  # Removing the legend since color is used only for 
  # visual aesthetics, not to represent data categories
  theme(legend.position = "none") +
  # Breaking the plot into four sub-plots based on brand_sub_sector
  facet_wrap(~ brand_sub_sector)
```

### Interpretation of the result
The scatterplot reveals a general pattern across the four sub-sectors: a higher rank does not necessarily correspond to a higher growth rate. There does not appear to be a clear upward or downward trend in all the four plots. This indicates that a brand’s ranking may not be the primary driver of its growth rate. Other factors, such as brand strategy, market conditions, or consumer preferences, could play a more significant role in the growth rate of fashion brands.

## Ranking Trend of the Luxury Brand with Most Highest Rankings
As a part of our research, we also want to know among the *luxury brands*, which one of then has the highest ranking for the most of times. Additionally, we want to know the trend of the ranking of that brand.

### Selecting Luxury Brand Data
First, we need to transform the cleaned dataset into the one that only contain information related to *luxury brands* between 2001 to 2021.
```{r}
# create new variable called `is_luxury` to detect 
# which brands are luxury brands, and `this`luxury_data`
# dataset will be used in both the second and the third
# research questions. 
# Tranformation skill 3: mutate()
luxury_data <- cleaned_final |>
  mutate(is_luxury = (brand_sub_sector == "Luxury"))|>
  # Tranformation skill 4: filter() to only include
  # luxury brands in our dataset
  filter(is_luxury == TRUE)|>
  # Transformation skill 5: select() to only include
  # variables relevant to this research question
  select(brand_name, year, rank)

# Getting an overview of the dataset
head (luxury_data, n = 10)
```

### Identifying the Brand
We will first find the brand that was ranked as the highest for the most number of times.

```{r}
# Create a data frame to store brand names 
# and their respective counts of achieving the highest ranking
rank_count <- data.frame(
  # List of all brand names
  Brand = fashion_brand$BrandName,
  # Initialize the count for each brand as 0
  Count = 0
)

# Loop through each year from 2001 to 2021
for (i in 2001:2021){
  # Filter the dataset to include only rows for the current year
  # Then, find the brand with the highest rank (minimum rank value)
  highest <- luxury_data |>
    filter(year == i) |>
    filter(rank == min(rank, na.rm = TRUE))
  
  # Increment the count for the brand that achieved 
  # the highest rank in the current year
  rank_count$Count[rank_count$Brand == highest$brand_name] <- 
    rank_count$Count[rank_count$Brand == highest$brand_name] + 1
}
```

### Plotting Count of Top Rankings by Company
```{r}
rank_count |>
  ggplot(aes(x = Brand, 
             y = Count)) + 
  # Create bars with heights corresponding to the counts
  geom_bar(stat = "identity",
           fill = "red") + 
  # Add labels for the y-axis and title
  labs(y = "Highest Ranking Count", 
       title = "Number of Times Getting Highest Ranking") +
  # remove the legend in the plot
  theme(legend.position = "none") +
  # Flip the coordinates to make the bar chart horizontal
  coord_flip()  
```
Surprisingly, **Louis Vuitton**, among all *luxury brands*, had the *highest* ranking consecutively in all **21** years from 2001 to 2021. 

### Trend in Ranking of the Brand with the Most Highest Rankings
After knowing that **Louis Vuitton** is the brand that has the highest ranking for the most number of times, we want to know how consistently its rankings are over years. 
```{r}
# Filter the dataset to include only data 
# for the brand "Louis Vuitton"
LV <- cleaned_final |>
  filter(brand_name == "Louis Vuitton")

# Plot how the ranking of Louis Vuitton changes over the years
ggplot(LV, aes(x = year,
               y = rank)) +
  geom_point(color = "blue") + 
  labs(
    title = "Ranking of LV Over Time",  
    x = "Year",                      
    y = "Ranking"                     
  ) +
   # Add a line to connect the points, showing the trend
  geom_line(color = "lightblue") +
   # Remove the legend in the plot
  theme(legend.position = "none")
```
### Interpretation of the Result 
Among all the *luxury brands*, **Louis Vuitton** is the one that had the highest ranking for most of the times. From the above graph, we can see that the ranking of **Louis Vuitton** experienced a large jump from *2004* to *2005*, and it has maintained its position in the 20s until 2021, where it jumped into the **top 10**.

# Conclusion
In this research, we analyzed the `fashion_brand` dataset to uncover trends and relationships within the fashion industry. We found that **Louis Vuitton** held the **highest mean ranking** among all brands from 2001 to 2021, indicating its strong and sustained position in the market. Additionally, when examining the correlation between **brand ranking** and **growth rate** for *all brands*, we observed **no clear relationship** among all brand sub-sectors, suggesting that factors beyond ranking, such as marketing strategies or market conditions, may drive a brand's growth rate. Further investigation revealed that among *luxury brands*, **Louis Vuitton** had the highest ranking most frequently, achieving the top spot every year from 2001 to 2021, and its ranking trend showed a significant rise in 2005 before stabilizing in the top 20, finally entering the top 10 in 2021. These findings highlight Louis Vuitton’s dominance in the luxury fashion market and its ability to maintain a leading position. This research provides valuable insights into the performance of luxury brands and opens the door for further exploration of the factors that contribute to their success.

library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(purrr)
library(httpgd)
library(cowplot)

# For this assignment, you will work with the [Abalone dataset](http://archive.ics.uci.edu/ml/datasets/Abalone) from the UCI Machine Learning Repository. The dataset consists of physical measurements of abalone (a type of marine snail) and includes information on the age, gender, and size of the abalone.

# 1. Load the "Abalone" dataset as a tibble called `abalone` using the URL provided below. The `abalone_col_names` variable contains a vector of the column names for this dataset (to be consistent with the R naming pattern). Make sure you read the dataset with the provided column names. 

url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data"

abalone_col_names <- c(
  "sex", 
  "length", 
  "diameter", 
  "height", 
  "whole_weight", 
  "shucked_weight", 
  "viscera_weight", 
  "shell_weight", 
  "rings"
)

abalone <- read_csv(url, col_names = abalone_col_names)


# Remove missing values and `NA`s from the dataset and store the cleaned data in a tibble called `df`. How many rows were dropped?
df <- abalone %>% 
drop_na()


# 2. Create some visualizations to explore the relationships between different variables:
p <- ggplot(df)


# 2.1 Plot histograms of all the quantitative variables in a **single plot**

df %>% 
  select(where(is.numeric)) %>% 
  gather() %>% 
  ggplot(.) +
  geom_histogram(aes(value)) +
  facet_wrap(~key, scales = 'free_x')}


# 2.2 Create a boxplot of `length` for each `sex`
p + geom_boxplot(aes(y=length, fill=sex))

# 2.3 Create a violin plot of `diameter` for each `sex`
p + geom_violin(aes(y=diameter, x=sex, fill=sex))

# 2.4 Are there any notable differences between `F` and `M` based on the physical appearences examined above?

> No



# 2.5 Create a scatter plot of `length` and `diameter`, and modify the shape and color of the points based on the `sex` variable. Change the size of each point based on the `shell_wight` value for each observation. Are there any notable anomalies in the dataset?
p + 
  geom_point(
    aes(x=length, y=height, group=sex, color=sex, shape=sex, size=shell_weight), alpha=0.5
  )


# 2.6 For each `sex`, create separate scatter plots of `length` and `diameter`. For each plot, also add a **linear** trendline to illustrate the relationship between the variables. Use the `facet_wrap()` function in R for this, and ensure that the plots are vertically stacked **not** horizontally. 
p + 
  geom_point(aes(x=length, y=height, shape=sex, color=sex)) + 
  geom_smooth(aes(x=length, y=height), method=lm) +
  facet_wrap(~sex, 3, 1)



# 3.1 Use `dplyr` for the following tasks. Filter the data to only include abalone with a length of at least 0.5 meters
# Group the data by gender and calculate the mean of each variable for each group
# Create a bar plot to visualize the differences in mean values for each variable by gender

df %>% 
  filter(length > 0.5) %>% 
  group_by(sex) %>% 
  summarise_if(is.numeric, mean, na.rm=TRUE) %>% 
  pivot_longer(-c(sex)) %>% 
  ggplot(.) + 
    geom_bar(
      aes(x=name, y=value, fill=sex), 
      stat='identity', position='dodge'
    )


# 3.2 Temporarily create a new variable called `num_rings` which takes a value of `"low"` if `rings < 10`, a value of `"high"` if `rings > 20` and a value of `"med"` otherwise. 
# Group `df` by this new variable and `sex` and compute `avg_weight` as the average of the `whole_weight + shucked_weight + viscera_weight + shell_weight` for each combination of `num_rings` and `sex`. 
# Use the `geom_tile()` function to create a tile plot of `num_rings` vs `sex` with the color indicating of each tile indicating the `avg_weight` value. 

df %>% 
  mutate(
    num_rings = case_when(
    rings < 10 ~ "low", 
    rings > 20 ~ "high", 
    TRUE ~ "med")
  ) %>% 
  group_by(num_rings, sex) %>% 
  summarise(
    avg_weight = mean(
    whole_weight + 
    shucked_weight + 
    viscera_weight + 
    shell_weight
    )
  ) %>% 
  mutate(
    num_rings = factor(
      num_rings, 
      levels=c("low", "med", "high")
      )
    ) %>%
  ggplot(.) + geom_tile(aes(x=num_rings, y=sex, fill=avg_weight))


# 3.3 Make a table of the pairwise correlations between all the numeric variables rounded to 2 decimal points

df %>% 
  select(where(is.numeric)) %>% 
  cor() %>%
  round(digits=2) %>% 
  knitr::kable()

# 3.4 Use the map function to create a scatter plot for each variable against the number of rings variable. You can use the `cowplot::plot_grid()` function to finally make the following grid of plots. 

df %>% 
  select(where(is.numeric)) %>% 
  map2(
    ., colnames(.),
    ~{
      ggplot(df) + 
      geom_point(aes(x = rings, y=., color=sex)) +
      ggtitle(as.character(.y))
    }
  ) %>% 
  cowplot::plot_grid(plotlist=.)

# 4.1 Perform a simple linear regression with `diameter` as the covariate and `height` as the response. 

model <- lm(height ~ diameter, df)

# 4.2 Interpret the model coefficients and their significance values. 

summary(model)
report(model)

# 4.3 Make a scatterplot of height` vs `diameter` and plot the regression line in `color="red"`. You can use the base `plot()` function in R for this. 

plot(height ~ diameter, df, pch=20)
abline(model, col="red")

# 4.4 Suppose we have collected observations for "new" abalones with the following `height` values. What is the expected value of their `diameter` based on your model above? Plot these new observations along with your predictions in your plot from earlier using `color="violet"`

new_diameters <- c(
  0.15218946,
  0.48361548,
  0.58095513,
  0.07603687,
  0.50234599,
  0.83462092,
  0.95681938,
  0.92906875,
  0.94245437,
  0.01209518
)

new_data_frame <- data.frame(diameter=new_diameters)

new_heights <- predict(model, new_data_frame)

abline(v=new_diameters, col="purple")
points(new_diameters, new_heights, col="purple", pch=20, cex=2)

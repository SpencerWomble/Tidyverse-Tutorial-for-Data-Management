###############################################################################

# tidyverse-dplyr data management tutorial

# Created by: Spencer Womble
# Email: sgwomble42@tntech.edu
# Date: 7/15/2022

########################################################

# Objectives

# This tutorial is designed for beginner tidyverse users. Many basic dplyr/tidyverse 
# functions related to data management are demonstrated here; however, this is certainly
# not an all-inclusive list. Refer to the R documentation/vignettes for these packages for details
# about their features. This tutorial also briefly touches on some aspects of 
# using ggplot (mostly within the context of plotting data from dplyr tibbles).
# The ggplot portion of this tutorial may be remedial for those familiar with the 
# package. However, those without much ggplot experience may find it beneficial.
# Please feel free to email me with any questions about or issues with this tutorial.

# - Spencer

########################################################################################

# Contents

# This tutorial will cover many basic function within the tidyverse-dplyr package including:

#   1) Different methods for sub-setting dataframes
#   2) Creating new columns based on mathematical operations and condition criteria
#   3) Data cleaning (reording columns, renaming columns, pivoting dataframe)
#   4) Calculating summary statistics
#   5) Basic operations for integrating dplyr output into ggplot2 and how to create a
#       basic ggplot2 for near publication-ready figures

#######################################################################################



##########################################################

# General Information about dplyr

##########################################################

### The structure of a pipe in the tidyverse ###################################

# a pipe is a useful method for linking multiple functions in r. It is especially valuable
# when you want to nest functions within each other or perform them in a specific order
# pipes, and the package within the tidyverse called "dplyr", are also very useful as data management tools


# example of a pipe

#Important note: %>% indicates the end or beginning of a new function
#   every time you want to add a new function to a pipe, put %>% following the end of the 
#   previous function

# data <- "the dataframe you want to use" %>% "tidyverse or base-R argument or function" %>%
#           "next argument or functinon"...ect

# note: newly created dataframes using tidyverse packages are referred to as "tibbles", they operate the same way a normal dataframe does


# IMPORTANT NOTE - if you have the "car" package loaded you will recieve and error 
#                   warning because both "car" and "dplyr" have a "select" function.
#                   to overcome this warning, use the command "dplyr::select" to tell R
#                   that you want it to use dplyr's "select" function.

##############################################

# Common function in dplyr

##############################################

# tibble - these are dataframe created by dplyr and generally operate the same way a dataframe created in base-R would
# mutate - create new columns/variables, perform mathematical functions
# select - subset data based on column names - very helpful during analyses when using datasheets with many variables
# group_by - tells dplyr how it should group results when performing row-wise tasks or when filtering
# filter - a method for sub-setting data. Works like "subset" in base R, but can be more useful as you can stack functions easier
# pivot_longer -  a method for turning data from a wide format to a long format
# pivot_wider -  a method for turning data from long format to a wide format
# rename - how to rename a column header
# relocate - used to change the order of columns in a data frame
# summarise - a way to calculate summary statistics like means, standard deviations, standard errors, and confidence intervals
# starts_with - a way to select data within a column based on a character match
# ends_with - same function as start_with just for sufix matching
# str_detect - used to tell R to perform functions on a column based on an exact character string match
# ! - used to tell R NOT to perform any action on a column listed with a "!" preceding the column name
# dplyr::count - a method for seeing how many factor levels are contained within a data frame
# arrange - sort rows based on a grouping critera (like species name)
#######################################################################################################################################
#######################################################################################################################################

# Begin R script for dplyr tutorial

#######################################################################################################################################
#######################################################################################################################################


library(dplyr) # dplyr is nested in the tidyverse package, but I loaded it separately here to point out that this is what we will 
# be working with mostly in this tutorial
library(tidyverse)
library(tidyr)

data<-iris
str(data)

# check for any NA's in the data ####
any(is.na(data))
# this returns "FALSE" meaning there no NA's. 

#########################################################


## Let's make a new column called "plant_ID"  so that we have a second (besides Species) grouping variable #######

data$plant_ID<-rep(1:150) # "rep" is a call to repeat a set or string of numbers, see "rep" function for more advanced options



#################################################################################


# let's move our new "plant_ID" column to the first column location

data<- data %>% relocate(plant_ID, .before = "Sepal.Length") # moving the column over by veg treatment; you must have the "." in front of "before or it will create a new column called "before"
str(data)



##########################################################################

# Now let's use dplyr to create new variable ###########

#### make a new column that shows "petal area" ####

# note: we're going to add to our dataframe, but we could make a new one by naming it something else like data2 ect.

data<- data %>% # this calls the data frame
  group_by(plant_ID) %>% # this is telling R that we want to to do this calc. for each row. Always choose a group option if performing mutations or filtering
  # we don't have to have "group_by" for this but it's good practice
  rowwise() %>% # telling R to do these sums according to each row. This is needed to deal with the NA's
  mutate(Petal.Area = sum(Petal.Length*Petal.Width, na.rm = TRUE)) %>%
  ungroup() # it's always a good idea to ungroup so you don't accidentally carry that argument to another section of code
str(data)

##########################################################################

# Now let's subset and filter rows and columns based on conditions we want each observation to meet ####

# let's make a new tibble that only has plant ID, species, and Petal Width
subdata<- data %>% group_by(plant_ID) %>% select(plant_ID,Petal.Width:Species) %>% # select is telling dplyr which columns we want included in our new tibble
  #       use commas after each column if they are in different locations, you can use : to select a range of columns
  ungroup() # you don't have to use the "group_by" function here, but it doesn't hurt
str(subdata)



# here we're going to build a tibble that only includes the species "setosa"
sub_data1<- data %>% group_by(Species) %>% filter(Species == "setosa") %>% # the "==" tells R we want an exact character match
  # we are grouping by "Species" here because that's the variable we're interested in filtering for
  # this means that you need to put the exact name "setosa" in or the code won't work
  ungroup() # it is important to ungroup here in case you use similar calls later in the code and it makes the newly created tibble easier to read
print(sub_data1)



# here we'll create a new tibble with only the "setosa" species, plant ID, and petal width columns
sub_data2<- data %>% group_by(Species) %>% filter(Species == "setosa") %>%
  select(plant_ID, Petal.Width:Species) %>% ungroup() # here the order we do these functions in does not matter
# but it can be important sometimes, especially if you are performing mathematical operations
print(sub_data2)



# here we'll create a tibble that has only rows 
# where the petal area is greater than the mean petal area
# We can do a number of filters using =, <, >, ect. 
sub_data3<- data %>% group_by(Species) %>%
  filter(Petal.Area > mean(Petal.Area, na.rm = TRUE)) %>% # we put na.rm in here to exclude any cells with NA
  # we don't have any cells with NA's in this example, but the code is here in case your data does
  ungroup()
str(sub_data3)



# let's create a new dataframe using "str_detect" functions. "str_detect" can be used
#   like the "stars_width" and "ends_with" selecting verbs; however, "str_detect" is
#   more compatible than "starts_with" and "ends_with" when performing operations
#   like filter or mutate on the data. In this new tibble,
#   we will create a new variable called "sepal width average" for the species "setosa" 

starts_ends_subset<- data %>% dplyr::select(plant_ID:Sepal.Length, 
                                            starts_with("Speci"), ends_with("width"),
                                            !Petal.Area) %>% # "starts_width" and "ends_width to select variable matching the designated character string
  # the "!Petal.Area" tells R to not include that column in our new dataframe
  filter(str_detect(Species,"osa")) %>% # "str_detect" is telling R to filter rows that contain "osa" in the column "Species"
  mutate(sepal_width_average = mean(Sepal.Width, na.rm = TRUE))
print(starts_ends_subset)

#########################################################################

# Now let's explore using the pivot_wider and pivot_longer functions.
# We will use Species as our grouping variable for pivoting and 
# Petal Width and Sepal Length as our values to pivot.

# Our data is currently in long form. So, let's first pivot to wide form.
# Pivoting is one of the most complicated data management processes in dplyr/tidyverse (in my opinion).
# Here there are the steps we need to take to properly pivot our data from long to wide form:

# Step 1) Determine how many observation of our grouping variable (Species)
#         Are in the data frame. We need to do this because we will need to 
#         create a helper column that R can use as a row reference so it does
#         not get confused and include all 150 rows in our wider-form data set
#         (not doing this results in columns with NA's that is very messy)

dplyr::count(data,Species) # the "count" we can determine how many observations
#                          of the factor "Species" we have"
# based on count we see we have 50 observations per species


# Step 2) Create a helper reference column and pivot the data

# now let's create our wide-form data frame
iris_data_wide_form<-data %>% select(Species, Sepal.Length, Petal.Width) %>% # selecting our variables of interest
    mutate(row_ref = rep((1:50), times = 3)) %>% # here we are creating our helper reference column
    # there are 50 observations of each species; therefore, we need 3 sets of the number 1:50
    # this enables R to eliminate the excess rows so that our new columns are have 50 rows each (which is what we want)
pivot_wider(names_from = Species, values_from = c(Petal.Width,Sepal.Length)) %>% # names_from is telling R which column the new column names
  #   should be taken from (here it will yield one new column per species for Petal Width and Sepal Length). 
  #   values_from tells R which columns it should be pulling values from to make.
  dplyr::select(-row_ref) #this line removes our helper column

print(iris_data_wide_form) # now we see that we successfully pivoted our data to wide form!
      
#############################



# Now, we are going to bring our data back to long-form with pivot_longer

iris_data_long_form<- iris_data_wide_form %>%
    mutate(plant_ID = rep((1:50), times = 1)) %>% # making helper column again
  # we need this so R knows how many rows it should end with
  # (3 species x 50 rows = 150 rows in the long-form)
  # SIDE NOTE: "rep()" is a base-R function that creates repeating strings of numbers.
  # see help(rep) for more details on how the code is structured and what all it can do
  
    pivot_longer(cols = !plant_ID, # this tells R: 1) our reference column
                 # is going to be plant_ID; 2) DO NOT treat this column like a value
                 
    names_to = c(".value", "Species"), # here is a nice bit of code when working with
    # a large number of columns. names_to tells R what we want our new names to be,
    # ".value" tells R to pivot all the columns in our data frame (excluding the reference column we made)
    # "Species" tells R that we want our new grouping column to be names "Species"
    
    names_sep = "_") %>% # names_sep is telling R that we want our columns 
  # separated where the character "_" is encountered. Here, it is splitting
  # the species name (like setosa) from Petal.Width and Sepal.Length so that
  # the species names go into the new column we are creating when we referenced
  # "Species" in our names_to argument.
  
  arrange(Species) # here we are telling R to arrange our "new" column
  #  called "Species" by alphabetical order.


# Congratulations! We've now pivoted to both wide and long-form data frames.
# This is often a particularly challenging data management function in dply/tidyverse for beginners and intermediate users to learn
# so you may need to go through the annotations multiple times. If you're confused,
# don't feel discouraged! You'll get it quickly as long as your keep at it!




#########################################################################

# now let's make some summary tibbles. Summary tibbles can be very useful for plotting 
# things in ggplot. We can pass our data means, ranges, ect... on to ggplot

# summarise differs from mutate in some ways. Mutate is more useful for performing complex
# mathmaticial operations or creating a new variable whereas summarise is better when wanting to create 
# a summary tibble. This especially useful when you want to quickly compare means and standard deviations
# among groups

# IMPORTANT NOTE: the following summary statistics are for ARITHMATIC summary statistics
#                 NOT for model-generated means. For things like estimated marginal means, 
#                 use the emmeans package 

# summary table showing mean petal width for each species
mean_data<- data %>% group_by(Species) %>%
  summarise(Species_means = mean(Petal.Width), n = n()) %>% # n = n() tells you how many observations were used to calc. each mean
  ungroup()
print(mean_data)

##########################################################################

## now let's get more complicated with our summary statistic calculations
# this is very useful when constructing bar charts and line graphs as you will be
# able to load in error bars. One of the goals of this tutorial is to be able 
# to pass data to ggplot and edit ggplots to have figures that are at or near publication ready


# calculating summary statistics: mean, standard deviation (sd), standard error (se), and building 95% confidence intervals ####

# we'll do this for petal width

# IMPORTANT NOTE - these calculate arithmatic means, not model means.
#   for model means, users should refer to the emmeans package (https://cran.r-project.org/web/packages/emmeans/emmeans.pdf)


# getting arithmatic confidence intervals and other summary statistics
summary_stats<- data %>%
  group_by(Species) %>%
  summarise(mean_petal_width = mean(Petal.Width), # first, we'll calc some of the summary stats
            sd = sd(Petal.Width, na.rm = TRUE), # "sd" call means standard deviation
            n = n()) %>%
  mutate(se = sd / sqrt(n), # calculating standard error
         lower_bound = mean_petal_width - qt(1 - (0.05 / 2), n - 1) * se, # calculating CI lower bound
         upper_bound = mean_petal_width + qt(1 - (0.05 / 2), n - 1) * se) # calculating CI upper bound
print(summary_stats)


#######################################################################

# incorporating tibbles and summary statistics into ggplot


# Now we can begin plotting our data
# Let's create a barchart of means with CI's included

ggplot(data = summary_stats, aes(x = Species, 
                                 y = mean_petal_width, 
                                 fill = Species)) +
  geom_bar(stat = "identity") + # "identity tells ggplot to use the y vals for the error bar. 
  geom_errorbar(aes(x=Species, ymin=lower_bound, ymax=upper_bound), # calling our CI's we calculated in the summary statistics
                width=0.4, colour="black", alpha=0.95, size=1.5) +
  ggtitle("Mean Petal Width")

#############################

# now lets clean up the graph to make it more publication ready. 

ggplot(data = summary_stats, aes(x = Species, 
                                 y = mean_petal_width, fill = Species)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(x=Species, ymin=lower_bound, ymax=upper_bound), 
                width=0.4, colour="black", alpha=0.95, size=1.5) + # setting preferences on color and size of error bars
  xlab( "Species") +
  ylab("Mean Petal Width") +
  scale_x_discrete(labels=as.character # renaming species on graph so they are capitalized. You can do this for any type of 
                   #  grouping variables like species, treatment, site, ect... You can also use this to remove underscores and periods
                   #  to help make the graph more publication ready. For example, use this code to change "Treatment_1" to "Treatment 1" on the graph
                   (c("Setosa","Versicolor", "Virginica")))+
  # Here's a bit of code to manually select colors for each group
  # This part is a pain, but it's how you change names in and of your legend
  # you have to provide ggplot with the same number of color choices as you have items
  # in the legend. Here I used scale_fill_manual because I used fill = in the aes mapping
  # if I was to use something like "color = _____" in aes, I would need to use
  # scale_color_manual
  # you can also choose color pallets to do this as well. The scale_color_brewer pallet
  # called "Dark2" is colorblind-friendly and very visible
  scale_fill_manual(name = "Vegetation Treatment", 
                    labels = (c("Setosa", "Versicolor", "Virginica")),
                    values = c('lightgreen','dodgerblue2','purple'))+ # this tells ggplot which colors to use
  theme_bw() + # theme bw stands for black and white. This theme removes background grids and is the most publication-ready built-in theme
  # you can also manually specify elements of the theme as we do below
  theme(panel.grid.major = element_blank(), # add: legend.position = 'none' to the theme() call to remove legend
        panel.grid.minor = element_blank(), # remove grids
        panel.background = element_blank(), # make background white
        panel.border = element_blank(), # remove borders
        axis.line = element_line(colour = "black"), # make axis lines black
        axis.text = element_text(size = 10, color = 'black'), # set text size
        axis.title.y = element_text(margin = 
                                      margin(t = 0, r = 20, b = 0, l = 0), size = 12), # moving axis title away from values and increase title size
        axis.title.x = element_text(margin = margin(t = 20), size = 12))+
  ggtitle("Mean Petal Width") # remove this line if you don't want a title



###############################################################################################33

# now let's create a regression plot that is near publication ready
# this is not a tidyverse-specific exercise, but it provides code to 
# set axes with special characters that can be difficult to code properly for those
# new to ggplot or R


# basic regression plot (like you would make in excel) ####
# let's regress petal width against petal length
ggplot(data, aes(x=Petal.Length, y = Petal.Width))+
  geom_point()+ # geom_point tells ggplot that this will be a scatter plot
  geom_smooth(method = 'lm', se = FALSE) #this line calls the basic regression equation; ggplot defaluts to loess regression
# se = false removes the error bars around the regression line



# now let's fit a regression ourslves - we will use this information to display the 
# R2, p-values, and regression equations directly on the graph
mod1<- lm(Petal.Width ~ Petal.Length, na.action = na.omit, data = data)
summary(mod1)


# plot the regression and include p.val, R2, and regression equation ####

ggplot(data, aes(x=Petal.Length, y = Petal.Width))+
  geom_point()+
  geom_smooth(method = 'lm', se = FALSE)+
  theme_classic() +
  annotate('text', x=2, y = 4, label = 'p-value: <0.001')+ # x = and y = are telling ggplot where on the graph to put the annotations
  # these locations correspond to the respective axes
  annotate('text', x=2, y = 3.7, label = 'Adjusted R-squared:0.93')+
  annotate('text', x=2, y = 3.4, label = 'Petal Width = 0.4158.*(Petal Width)-0.3631')

# same as above but with simplified code
ggplot(data, aes(x=Petal.Length, y = Petal.Width))+
  geom_point()+
  geom_smooth(method = 'lm', se = FALSE)+
  labs(x = bquote(Petal~Length~(cm)), y = bquote(Petal.Width~(cm)))+# bquote can also be used to make sub and superscripts characters
  theme_classic()+ # this calls a pre-designed format
  theme(axis.text = element_text(size = 13, color = 'black'))+ # need a separate line of "theme()" to make manual changes
  annotate('text', x = c(2,2,2), y = c(4,3.7,3.4), 
           label = c('p-value: <0.001','Adjusted R-squared:0.93',
                     'Petal Width = 0.4158*(Petal Width)-0.3631'))







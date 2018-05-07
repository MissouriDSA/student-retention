library(dplyr)
library(ggplot2)
library(caret)

# Get Data

library(readxl)
df <- read_excel("C:/Users/reillyd/Desktop/Machine Learning A-Z Template Folder/2018_05_05_Full.xlsx")
View(df)

# survey as factor
df$survey <- ordered(df$survey,
                     levels = c(0,1),
                     labels = c("No", "Yes")) 

str(df)
summary(df)

# create subset with just those in survey.
df_survey <- subset(df, survey == "Yes")
    
# Compare Survey to not Survey for Retention and GPA
ggplot(df, aes(x=gpa)) + 
    geom_density(aes(group=survey, colour=survey)) + 
    labs(x = "GPA", title ="GPA Density by Survey Participation") 
ggplot(df, aes(x=retent)) + 
    geom_density(aes(group=survey, colour=survey)) + 
    labs(x = "Retention Score ", title ="Rentiontion Score Density by Survey Participation") + 
    xlim(.25, 1)

# Graphs for predictors
ggplot(df_survey, aes(x=sob, y= (..count..)/sum(..count..))) + 
    geom_histogram(bins = 5) + 
    labs(x = "Sense of Belonging", y= "Frequency", title ="Sense of Belonging Frequency Breakdowns") +
    scale_y_continuous(labels = scales::percent)
ggplot(df_survey, aes(engage, y= (..count..)/sum(..count..))) + 
    geom_histogram(bins = 3) + 
    labs(x = "Level of Engagement", y= "Frequency", title ="Level of Engagement Frequency Breakdowns") +
    scale_y_continuous(labels = scales::percent)
ggplot(df_survey, aes(x =leave, y= (..count..)/sum(..count..))) + 
    geom_histogram(bins = 2)+ 
    labs(x = "Thoughts of Leaving", y= "Frequency", title ="Thoughts of Leaving School Frequency Breakdowns") +
    scale_y_continuous(labels = scales::percent)
ggplot(df_survey, aes(resil, y= (..count..)/sum(..count..))) + 
    geom_histogram(bins = 8) + 
    labs(x = "Resiliency", y= "Frequency", title ="Degree of Resiliency Frequency Breakdowns") +
    scale_y_continuous(labels = scales::percent)
ggplot(df_survey, aes(sat, y= (..count..)/sum(..count..))) + 
    geom_histogram(bins = 6) + 
    labs(x = "Satisfaction", y= "Frequency", title ="Level of Satisfaction Frequency Breakdowns") +
    scale_y_continuous(labels = scales::percent)

# Graphs with ggplot2 of outcomes
head (df)

ggplot(df) + geom_histogram(aes (x=retent, y= (..count..)/sum(..count..)), binwidth = .01) + 
    labs(x = "Retention - All Students", y= "Frequency", title ="Retention Score All Students Frequency Breakdowns") +
    scale_y_continuous(labels = scales::percent) +
    xlim(.01, 1) +
    ylim (.0, .2)

ggplot(df_survey) + geom_histogram(aes (x=retent, y= (..count..)/sum(..count..)), binwidth = .01) + 
    labs(x = "Retention - Survey Participants", y= "Frequency", title ="Survey Participants Frequency Breakdowns") +
    scale_y_continuous(labels = scales::percent) +
    xlim(.01, 1) +
    ylim (.0, .2)

ggplot(df) + geom_histogram(aes (x=gpa, y= (..count..)/sum(..count..)), binwidth = .1) + 
    labs(x = "GPA - All Students", y= "Frequency", title ="GPA -  All Students Frequency Breakdowns") +
    scale_y_continuous(labels = scales::percent) +
    xlim(.0, 4) +
    ylim (.0, .1)

ggplot(df_survey) + geom_histogram(aes (x=gpa, y= (..count..)/sum(..count..)), binwidth = .1) + 
    labs(x = "GPA - Survey Participants", y= "Frequency", title ="GPA Survey Participants Frequency Breakdowns") +
    scale_y_continuous(labels = scales::percent) +
    xlim(.0, 4) +
    ylim (.0, .1)



# Scatter plot
g <- ggplot(df, aes (x = retent, y=gpa))
g + geom_point(aes (color=survey)) + labs(x = "Retention - All Students", title ="Retention Scores by Survey Participaton") 

# Balance data for vizualization. Undersample Not in Survey.
table(df$survey)

# library(caret);
set.seed(9560)
down_df <- downSample(df, y = df$survey)
down_df <- as.data.frame(down_df)

table(down_df$survey)
summary(down_df)

g <- ggplot(down_df, aes (x = retent, y=gpa))
g + geom_point(aes (color=survey))

# Boxplot and violin by survey
ggplot(df, aes(y=retent, x=survey)) + geom_boxplot()
ggplot(df, aes(y=gpa, x=survey)) + geom_boxplot()


ggplot(df, aes(y=retent, x=survey)) + geom_violin()
ggplot(df, aes(y=gpa, x=survey)) + geom_violin()

g <- ggplot(df, aes(y=retent, x=survey))
g + geom_point() + geom_violin()
g + geom_point() + geom_violin() 
g + geom_jitter() + geom_violin()

g <- ggplot(df, aes(y=gpa, x=survey))
g + geom_point() + geom_violin()
g + geom_point() + geom_violin() 
g + geom_jitter() + geom_violin()

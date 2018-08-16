
# One-way Anova
# source: https://rcompanion.org/rcompanion/d_05.html
# The following commands will install these packages if they are not already installed

# dplyr
if(!require(dplyr)){install.packages("dplyr")}
# dplyr is a grammar of data manipulation, providing a consistent set of verbs that help you solve the most common data manipulation challenges
if(!require(FSA)){install.packages("FSA")}
# Simple Fisheries Stock Assessment Methods 
if(!require(car)){install.packages("car")}
# The car package (Fox and Weisberg, 2011) provides many functions
# that are applied to a fitted regression model, perform additional calcula-
#  tions on the model or possibly compute a different model, and then return
# values and graphs
if(!require(agricolae)){install.packages("agricolae")}
# This package contains functionality for the Statistical Analysis of experimental 
# designs applied specially for field experiments in agriculture and plant breeding.
if(!require(multcomp)){install.packages("multcomp")}
# onvert a logical vector or a vector of
# p-values or a correlation, difference, or distance
# matrix into a display identifying the pairs for
# which the differences were not significantly
# different.
if(!require(DescTools)){install.packages("DescTools")}
if(!require(lsmeans)){install.packages("lsmeans")}
if(!require(multcompView)){install.packages("multcompView")}
if(!require(Rmisc)){install.packages("Rmisc")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(pwr)){install.packages("pwr")}


### --------------------------------------------------------------
### One-way anova, SAS example, pp. 155-156
### --------------------------------------------------------------

Input <- ("
        Location   Aam
        Tillamook  0.0571 
        Tillamook  0.0813 
        Tillamook  0.0831 
        Tillamook  0.0976 
        Tillamook  0.0817 
        Tillamook  0.0859 
        Tillamook  0.0735 
        Tillamook  0.0659 
        Tillamook  0.0923 
        Tillamook  0.0836 
        Newport    0.0873 
        Newport    0.0662 
        Newport    0.0672 
        Newport    0.0819 
        Newport    0.0749 
        Newport    0.0649 
        Newport    0.0835 
        Newport    0.0725 
        Petersburg 0.0974 
        Petersburg 0.1352 
        Petersburg 0.0817 
        Petersburg 0.1016 
        Petersburg 0.0968 
        Petersburg 0.1064 
        Petersburg 0.1050 
        Magadan    0.1033 
        Magadan    0.0915 
        Magadan    0.0781 
        Magadan    0.0685 
        Magadan    0.0677 
        Magadan    0.0697 
        Magadan    0.0764 
        Magadan    0.0689 
        Tvarminne  0.0703 
        Tvarminne  0.1026  
        Tvarminne  0.0956 
        Tvarminne  0.0973 
        Tvarminne  0.1039 
        Tvarminne  0.1045
        ")

Data = read.table(textConnection(Input),header=TRUE)

# Specify the order of factor levels for plots and Dunnett comparison

library(dplyr)

Data <- 
  mutate(Data,
         Location = factor(Location, levels=unique(Location)))

# Remark for 'unique' 
# x <- unique(c("a","a","b","b","c","c","c","d"))
# x
# [1] "a" "b" "c" "d"


# Produce summary statistics

library(FSA)   

Summarize(Aam ~ Location,
          data=Data,
          digits=2)
# Fit the linear model and conduct ANOVA
model <- lm(Aam ~ Location, 
           data=Data)

library(car)

Anova(model, type="II")                    # Can use type="III"

### If you use type="III", you need the following line before the analysi
### options(contrasts = c("contr.sum", "contr.poly"))

summary(model)     # Produces r-square, overall p-value, parameter estimates


# Checking assumptions of the model
# A histogram of residuals from a linear model.  The distribution of 
# these residuals should be approximately normal.
hist(residuals(model), 
     col="darkgray")

# A plot of residuals vs. predicted values.  
# The residuals should be unbiased and homoscedastic.  
# For an illustration of these properties, 
# see this diagram by Steve Jost at DePaul 
# University: condor.depaul.edu/sjost/it223/documents/resid-plots.gif.
plot(fitted(model), 
     residuals(model))

# Tukey and Least Significant Difference mean separation tests 
# (pairwise comparisons): 

library(agricolae)

(HSD.test(model, "Location"))          # outer parentheses print result

# Aam groups
# Petersburg 0.1034429      a
# Tvarminne  0.0957000     ab
# Tillamook  0.0802000     bc
# Magadan    0.0780125     bc
# Newport    0.0748000      c

comparison <- scheffe.test(model,"Location", group=TRUE,console=TRUE,
                           main="Aam vs. city")
# Aam groups
# Petersburg 0.1034429      a
# Tvarminne  0.0957000     ab
# Tillamook  0.0802000      b
# Magadan    0.0780125      b
# Newport    0.0748000      b


# Multiple comparisons in multcomp package
# Note that “Tukey” here does not mean Tukey-adjusted comparisons.  It just sets up a matrix to compare each mean to each other mean.

library(multcomp)

mc <-  glht(model,
          mcp(Location = "Tukey"))

mcs <- summary(mc, test=adjusted("single-step"))

mcs

### Adjustment options: "none", "single-step", "Shaffer",
###                     "Westfall", "free", "holm", "hochberg",
###                     "hommel", "bonferroni", "BH", "BY", "fdr"

cld(mcs,
    level=0.05,
    decreasing=TRUE)


### Means sharing a letter are not significantly different


## Multiple comparisons to a control with Dunnett Test


### The control group can be specified with the control option,
###   or will be the first level of the factor

library(DescTools)

dun <- DunnettTest(Aam ~ Location,
            data = Data)



# Dunnett's test for comparing several treatments with a control : 
# 95% family-wise confidence level

 
# Graphing the results


## Simple box plots of values across groups


boxplot(Aam ~ Location,
        data = Data,
        ylab="aam / height",
        xlab="Location")



# Bar plot of means with error bars across groups



### Summarize the data frame (Data) into a table

library(Rmisc)   

Data2 <- summarySE(data=Data, 
                  "Aam", 
                  groupvars="Location", 
                  conf.interval = 0.95)

Tabla <- as.table(Data2$Aam)          
rownames(Tabla) <- Data2$Location

Tabla

barplot(Tabla, 
        ylab="aam / height", 
        xlab="Location")


library(ggplot2)


offset.v = -5     # offsets for mean letters
offset.h = 0.5

ggplot(Data2, 
       aes(x = Location, y = Aam, 
           ymax=0.12, ymin=0.0))  +
  geom_bar(stat="identity", fill="gray50",
           colour = "black", width = 0.7, position=position_dodge())  +
  geom_errorbar(aes(ymax=Aam+se, ymin=Aam-se), 
                width=.1, size=.5,
                color="black",
                position=position_dodge(.9))  +
  geom_text(aes(label=c("bc","c","a","bc","ab"),
                #fontface="italic",
                hjust=offset.h, vjust=offset.v)) +              
  labs(x = "Sample location", 
       y = "aam / height")  +
  ## ggtitle("Main title") + 
  theme_bw() +
  theme(text = element_text(size=14),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.title = element_text(size = rel(1.5), 
                                  face = "bold", vjust = 1.5),
        axis.title = element_text(face = "bold"),
        axis.title.y = element_text(vjust= 1.8),
        axis.title.x = element_text(vjust= -0.5),
        panel.border = element_rect(colour="black")
  )
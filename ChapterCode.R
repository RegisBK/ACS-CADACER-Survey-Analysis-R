## ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ Survey Data Analysis with R and RStudio ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ##

## The pound symbol tells R to ignore text, it is used to write comments.           


                   ############## INSTALLING PACKAGES IN RSTUDIO ##############

## To begin, install the likert package in order to access its functions and built in dataset.

## Install the package
install.packages("likert") 

## Load the package
library(likert) 

              ############## VISUALIZING RESPONSE PATTERNS WITH likert ##############

##### Importing, Viewing, and Summarizing Data #####

## Load the pisaitems dataset
data(pisaitems) 
                   
## Bring up the help page for the dataset
?pisaitems 

## View the dataset as a spreadsheet. Notice that the responses to each question are saved 
## as factors labeled with the response option text (Strongly disagree, Disagree, etc.).
View(pisaitems)                    

## There are two ways to use the summary function to generate a count of the number of responses 
## in each category for the second variable in the dataset.
summary(pisaitems$ST24Q01) 
summary(pisaitems[,2]) 


##### Visualizing Response Distributions #####

## Create a smaller dataset containing only the first six variables
minipisa<-pisaitems[,1:6] 

## Rename the columns with more descriptive text/the question text
names(minipisa)<-c(
  CNT="Country",
  ST24Q01="I read only if I have to.",
  ST24Q02="Reading is one of my favorite hobbies.",
  ST24Q03="I like talking about books with other people.",
  ST24Q04="I find it hard to finish books.",
  ST24Q05="I feel happy if I receive a book as a present.")

## Summarize the data in a way that is easy to plot
likert.out<- likert(minipisa[,2:6]) 

## Generate the default centered bar plot
plot(likert.out) 

## Generate a filled bar plot ordered by question with larger axis font
plot(likert.out, group.order=names(minipisa[,2:6]), centered=FALSE) + 
  theme(text=element_text(size=14))

## Customize the colors on the bar plot
plot(likert.out, group.order=names(minipisa[,2:6]), centered=FALSE, 
     low.color="firebrick", high.color="forestgreen") + theme(text=element_text(size=14))

## Create a likert summary by group
likert.out.group<-likert(minipisa[,2:4], grouping=minipisa$Country) 

## Plot the centered bar plot grouped by country
plot(likert.out.group, group.order=c('Mexico', 'Canada', 'United States')) + 
  theme(text=element_text(size=14))

## Generate a heat plot
plot(likert.out, type="heat")

     ############## COMPUTING DESCRIPTIVE STATISTICS AND RELIABILITIES WITH psych ##############

## Install and load the psych package
install.packages("psych")
library(psych)

##### Computing and Exporting Descriptive Statistics #####

## Generate descriptive statistics
describe(minipisa) 

## Construct a cross tabulation of a factor variable and its underlying numeric representation
table(minipisa[,2], as.numeric(minipisa[,2]))

## Generate descriptive statistics grouped by the Country variable
## NaN stands for Not a Number and indicates an unidentified calculation result.
describeBy(minipisa, group=minipisa$Country)

## The descriptive statistics can be stored as variables and written to .csv files.
descriptives<-describe(minipisa)
write.csv(descriptives, file = "minipisa descriptives.csv")

## To write the results of describeBy(), use the mat=TRUE argument.
descriptives.group<-describeBy(minipisa, group=minipisa$Country, mat = TRUE)
write.csv(descriptives.group, file = "minipisa descriptives by country.csv")


##### Computing Evidence for Internal Consistency (Cronbach's alpha) #####

## The data must be in numeric format for the alpha() function. 
## This code produces an error.
alpha(minipisa[,2:6]) 

## Wrap the dataset with data.matrix() to coerce the factor variables into 
## their underlying numeric representations so that reliabilities can be calculated.
alpha(data.matrix(minipisa[,2:6]))

## The check.keys = TRUE argument will look for items that should be reverse coded.
alpha(data.matrix(minipisa[,2:6]), check.keys = TRUE)

## It is also possible to specify which items to reverse code with the keys argument.
alpha(data.matrix(minipisa[,2:6]), keys = c(-1,1,1,-1,1))


            ############## FACTOR ANALYSIS WITH psych AND lavaan ##############

##### Exploratory Factor Analysis with psych #####

## These two tests can be used to evaluate the suitability of data for factor analysis
cortest.bartlett(data.matrix(minipisa[,2:6]))
KMO(data.matrix(minipisa[,2:6]))

## Principal Components Analysis (PCA) is conducted with the principal() function.
## Here two factors are extracted with no rotation (default is varimax).
pca2<-principal(data.matrix(minipisa[,2:6]), nfactors=2, rotate="none")

## View the output
pca2

## Exploratory Factor Analysis (EFA) is conducted with the fa() function. Here two 
## factors are extracted with no rotation (default is oblimin) using maximum likelihood estimation.
efa2<-fa(r = data.matrix(minipisa[, 2:6]), nfactors = 2, rotate = "none", fm = "ml")

## View the output
efa2

## Generate a plot showing how the items load on each factor
plot(efa2$loadings, xlim=c(-1,1.25), ylim=c(0,.5))
## Add item names as labels
text(efa2$loadings,labels=row.names(efa2$loadings), cex=.8, pos=3)

## A scree plot of eigenvalues can be plotted from both the EFA and PCA results
plot(efa2$e.values, type="b")
plot(pca2$values, type="b")

## psych also provides a function for Velicer's Minimum Average Partial (MAP)
vss(data.matrix(minipisa))


##### Confirmatory Factor Analysis with lavaan #####

## Install and load lavaan
install.packages("lavaan")
library(lavaan)

## Use the original pisaitems dataset to create a new minipisa dataset where all values are numeric
## and the original (shorter) column names are used
minipisa<- data.matrix(pisaitems[,1:6])

## This is the lavaan model syntax for a one-factor model with READ as the latent variable
## defined by the first five measured variables in the pisaitems dataset
pisa.CFA.1F<-'
                      READ =~ ST24Q01 + ST24Q02 + ST24Q03 + ST24Q04 + ST24Q05
'

## Confirmatory factor analysis (CFA) is conducted with cfa() by providing the model and data
CFA.1F.out<-cfa(pisa.CFA.1F, minipisa)

## View data-model fit information with summary(), request standardized values and R square values
summary(CFA.1F.out, fit.measures=TRUE, standardized=TRUE ,rsquare=TRUE)

## Request modification indices
MI<-modindices(CFA.1F.out)

## Sort the modification indices from highest to lowest, then printing the first 10
MI[order(MI$mi, decreasing=T),][1:10,]

## Fit information and modification indices can be saved to a text file
capture.output(print("Summary of 1 Factor CFA Model"), 
               summary(CFA.1F.out, fit.measures=TRUE, standardized=TRUE ,rsquare=TRUE), 
               print("Modification Indices"), MI[order(MI$mi, decreasing=T),][1:10,], 
               file="Summary of 1 Factor CFA Model.txt")

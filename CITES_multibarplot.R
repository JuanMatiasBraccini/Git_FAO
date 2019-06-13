
#source: http://www.sthda.com/english/wiki/ggplot2-barplot-easy-bar-graphs-in-r-software-using-ggplot2
library(easyGgplot2)

df1 <- restaurant
# Plot of variable 'total_bill' according to xName 'time'.
# The plot is colored by the groupName 'sex'
ggplot2.barplot(data=df1, xName='time', yName="total_bill",
                groupName='sex')
# Use position=position_dodge()
ggplot2.barplot(data=df1, xName='time', yName="total_bill",
                groupName='sex', position=position_dodge())
# change colors
ggplot2.barplot(data=df1, xName='time', yName="total_bill",
                groupName='sex', groupColors=c('#999999','#E69F00'),
                position=position_dodge(),
                #background and line colors
                backgroundColor="white", color="black", 
                xtitle="Time of day", ytitle="Total bill", 
                mainTitle="Total bill\n per time of day",
                removePanelGrid=TRUE,removePanelBorder=TRUE,
                axisLine=c(0.5, "solid", "black")
)


# stacked bar chart; Barplot of the count
ggplot2.barplot(data=diamonds, xName="clarity",
                groupName="cut")
# change the color
ggplot2.barplot(data=diamonds, xName="clarity", 
                groupName="cut", brewerPalette="Blues")


#split plot into facets
# Facet according to the cut variable
ggplot2.barplot(data=diamonds, xName="clarity", 
                faceting=TRUE, facetingVarNames="cut")
# Change the direction. 
# possible values are "vertical", "horizontal".
# default is vertical.
ggplot2.barplot(data=diamonds, xName="clarity",
                faceting=TRUE, facetingVarNames="cut", 
                facetingDirection="horizontal") 

# Facet by two variables: cut and color.
# Rows are cut and columns are color
ggplot2.barplot(data=diamonds, xName="clarity", 
                faceting=TRUE,
                facetingVarNames=c("cut", "color"))
# Facet by two variables: reverse the order of the 2 variables
# Rows are color and columns are cut
ggplot2.barplot(data=diamonds, xName="clarity", 
                faceting=TRUE, facetingVarNames=c( "color", "cut"))

#facet with free scale
# Facet with free scales
ggplot2.barplot(data=diamonds, xName="clarity", 
                faceting=TRUE, facetingVarNames=c("cut", "color"),
                facetingScales="free")

#label appeareance
# Change facet text font
# Possible values for the font style:'plain', 'italic', 'bold', 'bold.italic'.
ggplot2.barplot(data=diamonds, xName="clarity", 
                faceting=TRUE, facetingVarNames=c("cut", "color"),
                facetingFont=c(12, 'bold.italic', "red"))
#Change the apperance of the rectangle around facet label
ggplot2.barplot(data=diamonds, xName="clarity", 
                faceting=TRUE, facetingVarNames=c("cut", "color"),
                facetingRect=list(background="white", lineType="solid",
                                  lineColor="black", lineSize=1.5)
)

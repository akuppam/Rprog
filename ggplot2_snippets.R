# From Computerworld: ggplot2 code snippets

snippet myg_barplot_basic
	ggplot(${1:mydataframe}, aes(x=${2:myxcolname}, y=${3:myycolname})) + 
	  geom_bar(stat="identity")

snippet myg_barplot_basic_reorderXaxisByYvalue
	ggplot(${1:mydataframe}, aes(x=reorder(${2:myxcolname}, -${3:myycolname}), y=${3:myycolname})) + 
	  geom_bar(stat="identity")  

snippet myg_barplot_groupedByColor
	ggplot(${1:mydataframe}, aes(x=${2:myxcolname}, y=${3:myycolname}, fill=${4:mygroupcolname})) + 
	  geom_bar(stat="identity", position="dodge")

snippet myg_barplot_basic_barsSetOneColor
	ggplot(${1:mydataframe}, aes(x=${2:myxcolname}, y=${3:myycolname})) + 
	  geom_bar(fill="${4:mycolor}", stat="identity")

snippet myg_barplot_basic_bars_blue
	ggplot(${1:mydataframe}, aes(x=${2:myxcolname}, y=${3:myycolname})) + 
	  geom_bar(stat="identity", color = "black", fill="#0072B2")

snippet myg_barplot_barslabeled
	# Code from an R Graphics Cookbook recipe, by Winston Chang
	ggplot(data = ${1:mydataframe}, aes(x=${2:myxcolname}, y=${3:myycolname})) +
	  geom_bar(stat="identity", color = "black", fill="#0072B2") +
	  ggtitle("${4:My Headline}") +
	  xlab("${5:myXaxisname}") +
	  ylab("${6:myYaxisname}") +
	  theme_classic() +
	  geom_text(aes(label=${3:myycolname}), vjust=1.5, colour="white", position=position_dodge(.9), size=5)  +
	  theme(plot.title=element_text(size=24))  

snippet myg_barplot_barslabeled_reorderXaxisByYvalue
	# Code adapted from an R Graphics Cookbook recipe, by Winston Chang
	ggplot(${1:mydataframe}, aes(x=reorder(${2:myxcolname}, -${3:myycolname}), y=${3:myycolname})) + 
	  geom_bar(stat="identity", color = "black", fill="#0072B2") +
	  ggtitle("${4:myheadline}") +
	  xlab("${5:myXaxisname}") +
	  ylab("${6:myYaxisname}") +
	  theme_classic() +
	  geom_text(aes(label=${3:myycolname}), vjust=1.5, colour="white", position=position_dodge(.9), size=5)  +
	  theme(plot.title=element_text(size=24))  

snippet myg_barplot_grouped_withColorPaletteAndHeadline
	ggplot(data = ${1:mydataframe}, aes(x=${2:myXcolname}, y=${3:myYcolname}, fill=${4:mygroup/colorBycolname} )) +
	  geom_bar(stat="identity", position="dodge") +
	  scale_fill_brewer(palette="Dark2") +
	  ggtitle("${5:My Headline}") +
	  theme(plot.title=element_text(size=22))

snippet myg_scatterplot_basic
	ggplot(${1:mydataframe}, aes(x=${2:myxcolname}, y=${3:myycolname})) + 
	  geom_point()

snippet myg_scatterplot_setPointSize
	ggplot(${1:mydataframe}, aes(x=${2:myxcolname}, y=${3:myycolname})) + 
	  geom_point(size=${4:myPointSizeNumber})

snippet myg_scatterplot_setColorByDataValue
	ggplot(${1:mydataframe}, aes(x=${2:myxcolname}, y=${3:myycolname})) + 
	  geom_point(aes(color=${4:myColorBycolname})) + scale_color_gradient(low="${5:mylowcolor}", high="${6:myhighcolor}")

snippet myg_scatterplot_setColorBrewerPaletteByDataValue
	ggplot(${1:mydataframe}, aes(x=${2:myxcolname}, y=${3:myycolname})) + 
	  geom_point(aes(color=${4:myColorBycolname})) + scale_color_brewer(type="seq", palette="${5:mypalettechoice}")

snippet myg_scatterplot_groupAndAutoAnnotateByColor
	# requires package directlabels
	library("directlabels")
	myplot <- ggplot(${1:mydataframe}, aes(x=${2:myxcolname}, y=${3:myycolname}, color=${4:mygroupingcol})) + geom_point()
	direct.label(myplot, "smart.grid")

snippet myg_scatterplot_addJitter
	 + geom_point(position="jitter")

snippet myg_line_basic
	ggplot(${1:mydataframe}, aes(x=${2:myxcolname}, y=${3:myycolname})) + 
	  geom_line()
	  
snippet myg_line_linesByCategory
	ggplot(${1:mydataframe}, aes(x=${2:myxcolname}, y=${3:myycolname})) + 
	  geom_line(aes(color=${4:mycolnameForEachLine}))	  

snippet myg_line_goupAndAutoAnnotateByColor
	# requires package directlabels; code via https://goo.gl/jpSDtw 
	library("directlabels")
	myplot <- ggplot(${1:mydataframe}, aes(x=${2:myxcolname}, y=${3:myycolname})) + geom_line(aes(color=${4:mycolnameForEachLine}))
	direct.label(myplot, list(last.points, hjust = 0.7, vjust = 1))

snippet myg_add_horizontalLine
	 + geom_hline(yintercept=${1:theYintercept})

snippet myg_add_vertictalLine
	 + geom_vline(xintercept=${1:theXintercept})

snippet myg_headline_setTextAndSize
	 + ggtitle("${1:myheadlinetext}") + 
	  theme(plot.title = element_text(size = ${2:myintegerPointSize}))

snippet myg_headline_makeBold
	 + theme(plot.title = element_text(face="bold"))

snippet myg_Xaxis_setTitle
	 + xlab("${1:My x-axis title text}")

snippet myg_Yaxis_setTitle
	 + ylab("${1:My y-axis title text}")

snippet myg_Xaxis_setCatVariableLabels
	 + scale_x_discrete(labels=${1:myvectoroflabels})

snippet myg_Yaxis_setContVariableLabels
	 + scale_y_continuous(breaks=${1:myvectorofbreaks})

snippet myg_Yaxis_setLimits
	 + ylim(${1:myYmin}, ${2:myYmax})

snippet myg_Xaxis_rotateLabels45degrees
	 + theme(axis.text.x= element_text(angle=45, hjust = 1.3, vjust = 1.2))

snippet myg_Yaxis_rotateTitle
	 + theme(axis.title.y = element_text(angle = 0))

snippet myg_legend_off
	 + theme(legend.position = "none")

snippet myg_multiplots_1factor_2columns
	 + facet_wrap(~ ${1:mycolname}, ncol=2)

snippet myg_multiplots_2factors_2columns
	 + facet_wrap(${1:mycolname1} ~ ${2:mycolname2}, ncol=2)

snippet myg_multiplots_1factor_2rows
	 + facet_wrap(~ ${1:mycolname}, nrow=2)

snippet myg_multiplots_2factors_2rows
	 + facet_wrap(${1:mycolname1} ~ ${2:mycolname2}, nrow=2)

snippet myg_multiplots_2factors
	  +  facet_grid(${1:mycolname1} ~ ${2:mycolname2})
	  
snippet myg_multiplots_unrelatedPlots_1column
	# gridExtra package required; enter any number of existing plots, each separated by a comma
	gridExtra::grid.arrange(${1:plot1}, ${2:plot2}, ${3:plot3})	
	
snippet myg_multiplots_unrelatedPlots_SetNumColumns
	# gridExtra package required; enter any number of existing plots, each separated by a comma
	gridExtra::grid.arrange(${1:plot1}, ${2:plot2}, ${3:plot3}, ncol=${4:numcolumns})		  

snippet myg_annotate_addByXYposition
	 + annotate("text", x=${1:myxposition}, y=${2:myyposition}, label="${3:My text}")

snippet myg_corrplot
	# requires package corrplot
	mycorrelationmatrix <- cor(${1:mymatrix})
	col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
	corrplot::corrplot(mycorrelationmatrix, method="shade", shade.col=NA, tl.col="black", tl.srt=45, col=col(200), addCoef.col="black")  

snippet myg_interactive_histogram
	# requires package ggvis
	library(ggvis)
	${1:mydataframe} %>% 
	  ggvis(x = ~${2:mydatacolumn}, fill := "gray") %>% 
	  layer_histograms(width =  input_slider(min=${3:mysliderMinValue}, max=${4:mysliderMax}, value= ${5:mysliderStartingValue}, label = "Bin Width")) 
	  
# end Computerworld ggplot2 code snippets

ggplot2 code snippets


Task

Snippet Name


Create basic bar graph myg_barplot_basic 
Create bar graph with bars in descending value order myg_barplot_basic_reorderXaxisByYvalue 
Create basic bar graph and set bars all to one color myg_barplot_basic_barsSetOneColor 
Create basic bar graph with bars all blue and outlined with black myg_barplot_basic_bars_blue 
Create bar graph with bars grouped and color-coded by one category myg_barplot_groupedByColor 
Create bar graph with bars grouped and color-coded by one category, using Dark2 RColorBrewer palette and adding plot headline myg_barplot_grouped_withColorPaletteAndHeadline 
Create bar graph where bars are labeled with y-value amounts myg_barplot_barslabeled 
Create bar graph where bars are labeled with y-value amounts and ordered by descending y value myg_barplot_barslabeled_reorderXaxisByYvalue 
Create basic scatterplot myg_scatterplot_basic 
Create basic scatterplot with specific point size myg_scatterplot_setPointSize 
Create scatterplot with points colored by another column's numeric value; set your own color palette from low to high myg_scatterplot_setColorByDataValue 
Create scatterplot with points colored by another column's categorical value; choose an RColorBrewer palette. myg_scatterplot_setColorBrewerPaletteByDataValue 
Create and auto-annotate scatterplot where dots are colored by another column's value (numeric or categorical); directlabels package must be installed and loaded myg_scatterplot_groupAndAutoAnnotateByColor 
Add "jitter" to scatterplot so points are less likely to be on top of each other myg_scatterplot_addJitter 
Create basic line graph with 1 line myg_line_basic 
Create line graph with a line for each category myg_line_linesByCategory 
Create and auto-annotate line graph with a line for each category; directlabels package required myg_line_goupAndAutoAnnotateByColor 
Add horizontal line to a graph by y intercept myg_add_horizontalLine 
Add vertical line to a graph by x intercept myg_add_vertictalLine 
Add headline and set point size myg_headline_setTextAndSize 
Make headline text bold myg_headline_makeBold 
Set x-axis title myg_Xaxis_setTitle 
Set y-axis title myg_Yaxis_setTitle 
Create new labels for x-axis values myg_Xaxis_setCatVariableLabels 
Set what y-axis values should appear as labels Yaxis_setContVariableLabels 
Set y-axis minimum and maximum display values Yaxis_setLimits 
Rotate x-axis labels 45 degrees myg_Xaxis_rotateLabels45degrees 
Rotate y-axis title to be horizontal myg_Yaxis_rotateTitle 
Turn off graph legend myg_legend_off 
Create multiple plots by an additional factor from an already existing plot, arranged in 2 columns myg_multiplots_1factor_2columns 
Create multiple plots by 2 additional factors from an already existing plot, arranged in 2 columns myg_multiplots_2factors_2columns 
Create multiple plots by 1 additional factor from an already existing plot, arranged in 2 rows myg_multiplots_1factor_2rows 
Create multiple plots by 2 additional factors from an already existing plot, arranged in 2 rows myg_multiplots_2factors_2rows 
Create grid of all permutations of multiple plots by 2 additional factors from an already existing plot myg_mutliplots_2factors 
Put multiple plots on one page/canvas, all in one column myg_multiplots_unrelatedPlots_1column 
Put multiple plots on one page/canvas, set number of columns myg_multiplots_unrelatedPlots_SetNumColumns 
Add text annotation to a plot by x,y coordinate myg_annotate_addByXYposition 
Create correlation matrix graphic from a matrix (or dataframe with all numerical columns) myg_corrplot 
Create interactive histogram where slider changes bin width; ggvis package required myg_interactive_histogram 

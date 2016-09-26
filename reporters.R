
require(rJava)
.jcall('java.lang.System','S','getProperty','java.version')

install.packages('ReporteRs')

require('devtools')
install_github('ReporteRsjars', 'davidgohel')
install_github('ReporteRs', 'davidgohel')

# Creation of mydoc, a mydocx object
mydoc <- docx( )


require(ggplot2)
doc = docx(title = 'My document')

doc = addTitle( doc , 'First 5 lines of iris', level = 1)
doc = addFlexTable( doc , vanilla.table(iris[1:5, ]), 
                    layout.properties= get.light.tableProperties())

doc = addTitle( doc , 'ggplot2 example', level = 1)
myggplot = qplot(Sepal.Length, Petal.Length, data = iris, color = Species, size = Petal.Width )
doc = addPlot( doc = doc , fun = print, x = myggplot )

doc = addTitle( doc , 'Text example', level = 1)
doc = addParagraph( doc, 'My tailor is rich.', stylename = 'Normal' )

writeDoc( doc, 'my_first_doc.docx' )

# ****************************
library( ReporteRs )

# Creation of mydoc, a mydocx object
mydoc <- docx( )

# add into mydoc first 10 lines of iris
mydoc <- addFlexTable( mydoc, FlexTable(iris[1:10,] ) )

# add a page break
mydoc <- addPageBreak( mydoc )

# add text with stylename "Normal" into mydoc 
mydoc <- addParagraph( mydoc, value = "Hello World!", stylename = "Normal" )

# add a plot into mydoc 
mydoc <- addPlot( mydoc, function() barplot( 1:8, col = 1:8 ) )

# write the doc 
writeDoc( mydoc, file = "word_simple_example.docx")


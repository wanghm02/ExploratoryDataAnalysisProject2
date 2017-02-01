# Exploratory Data Analysis - Assignment 2 - Q. #2
# Huamiao Wang Jan 31, 2017

# Load ggplot2
library(ggplot2)

# Loading provided datasets - loading from local machine
NEI <- readRDS("C:/Users/Huamiao/ExploratoryDataAnalysisProject2/data/summarySCC_PM25.rds")
SCC <- readRDS("C:/Users/Huamiao/ExploratoryDataAnalysisProject2/data/Source_Classification_Code.rds")

NEI$year <- factor(NEI$year, levels=c('1999', '2002', '2005', '2008'))

# Baltimore City, Maryland
# Los Angeles County, California
MD.onroad <- subset(NEI, fips == '24510' & type == 'ON-ROAD')
CA.onroad <- subset(NEI, fips == '06037' & type == 'ON-ROAD')

# Aggregate
MD.DF <- aggregate(MD.onroad[, 'Emissions'], by=list(MD.onroad$year), sum)
colnames(MD.DF) <- c('year', 'Emissions')
MD.DF$City <- paste(rep('MD', 4))

CA.DF <- aggregate(CA.onroad[, 'Emissions'], by=list(CA.onroad$year), sum)
colnames(CA.DF) <- c('year', 'Emissions')
CA.DF$City <- paste(rep('CA', 4))

DF <- as.data.frame(rbind(MD.DF, CA.DF))

# Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources 
# in Los Angeles County, California (fips == 06037). Which city has seen greater changes over time 
# in motor vehicle emissions?

# Generate the graph in the same directory as the source code
png('C:/Users/Huamiao/ExploratoryDataAnalysisProject2/plot6.png')

ggplot(data=DF, aes(x=year, y=Emissions)) + geom_bar(aes(fill=year),stat="identity") + guides(fill=F) + 
        ggtitle('Total Emissions of Motor Vehicle Sources\nLos Angeles County, California vs. Baltimore City, Maryland') + 
        ylab(expression('PM'[2.5])) + xlab('Year') + theme(legend.position='none') + facet_grid(. ~ City) + 
        geom_text(aes(label=round(Emissions,0), size=1, hjust=0.5, vjust=-1))

dev.off()
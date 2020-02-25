#### Portfolio - Clients - Online Revenue vs. Online Units Sold ####

setwd("Y:/Online Revenue Units Threshold")


library(ggiraph)
library(ggplot2)
library(lubridate)
library(tidyr)
library(dplyr)
library(htmlwidgets)

#### read in csv data pulled from relational database (SQL)
PortData <- read.csv('Portfolio Threshold.csv', stringsAsFactors = FALSE)

PrePostList <- read.csv('Last Year Current Year List.csv')

#### Windows font check ####

windowsFonts("Trebuchet MS" = windowsFont("Trebuchet MS"))

## update 1st col name
colnames(PortData)[1] <- "Client"
colnames(PrePostList)[1] <- "Client"

PortData2019 <- PortData[which(PortData$Year %in% 2019),]

#aggregate(PortData2019['Rounds','Revenue'], by=PortData2019['Course'], sum)
#pd19 = aggregate(cbind(Rounds, Revenue)~Course,PortData2019,FUN=sum)

##### Combine the duplicate course rows ####
PortData2019 = aggregate(cbind(Rounds, Revenue)~Client,PortData2019,FUN=sum)


#### Add the Portfolio Column using mergelist ####
PortData2019 = merge(PortData2019, PrePostList, by.x = c("Client"), 
                     by.y = c("Client"))

#### Remove apostraphes from Client names #### in order to not interfere with tooltip ####
PortData2019$Client <- gsub("'", "", PortData2019$Client)

#### Add Deficit/Surplus Column - Both numerics and factor ####
PortData2019 <- mutate(PortData2019, Deficit.Surplus = Revenue - 25000)
PortData2019 <- mutate(PortData2019, Def.Surp = if_else(Deficit.Surplus < 0, "Deficit", "Surplus"))
mutate(PortData2019, numclients = count(PortData2019$Portfolio.Entry))

#### Create tool tip that might not work ####
tooltip_css <- "background-color:gray;color:white;font-style:italic;padding:10px;border-radius:10px 20px 10px 20px;font-family:Trebuchet MS"


PortData2019$tooltipbookings = paste0("Client: #", rownames(PortData2019), "<br/>", "Online Revenue: $", formatC(PortData2019$Revenue, format="d", big.mark=","), "<br/>",
                                      "Online Units: ", formatC(PortData2019$Rounds, format="d", big.mark=","), "<br/>",
                                      "Threshold Balance: ", PortData2019$Def.Surp, " $", formatC(PortData2019$Deficit.Surplus, format="d", big.mark=","))


#### Revenue by Rounds Scatter Plots ####
pointplot <-ggplot(PortData2019, aes(Revenue, Rounds, data_id=Client, color=Portfolio.Entry, tooltip=tooltipbookings)) +
  scale_x_log10(labels = scales::dollar) +
  scale_y_log10(labels = scales::comma) +
  xlab("Online Revenue log10") +
  ylab("Online Units log10") +
  geom_vline(aes(xintercept = 25000), color="purple") +
  theme(text=element_text(family="Trebuchet MS", face="bold",size=10)) +
  annotate(geom="label",x=25000, y=(max(PortData2019$Rounds)), label="Threshold: $25,000", color="black", fill="purple", alpha=0.6, family="Trebuchet MS", fontface="bold", size=2.5, vjust=1, hjust=.2) +
  ggtitle("Client Category: Online Units Sold by Online Revenue") +
  geom_point_interactive() 


#### create svg image ####
pointploti <- ggiraph(code = print(pointplot), width_svg = 8, height_svg = 4, tooltip_extra_css = tooltip_css, hover_css = "cursor:pointer;fill:red;r:2pt;")

#### save as html widget ####
htmlwidgets::saveWidget(pointploti, "thresholdbalance_pointplot3.html", selfcontained = TRUE, libdir = NULL)
#htmlwidgets::saveWidget(pointploti, file="thresholdbalance_pointplot2.html", selfcontained = FALSE)


#### Revenue Density Plots - Last Year Clients vs. New Clients to Portfolio ####


#### Create topy variable to adjust the annotation height ####
td <- tapply(PortData2019$Revenue, PortData2019$Portfolio.Entry, density)$"Current Year"
maxy <- which.max(td$y)
#maxy <- which.max(density(PortData2019$Revenue)$y)
topy <- list(x=td$x[maxy], y=td$y[maxy])$y


#### Creating the numclients (number of clients per Portfolio.Entry) for the density tooltip#####
#LYN <- as.data.frame(table(PortData2019$Portfolio.Entry)[2])[1,]
#CYN <- as.numeric(as.data.frame(table(PortData2019$Portfolio.Entry)[1])[1,])

PortData2019$numclients[PortData2019$Portfolio.Entry == "Last Year"] <- as.data.frame(table(PortData2019$Portfolio.Entry)[2])[1,]
PortData2019$numclients[PortData2019$Portfolio.Entry == "Current Year"] <- as.numeric(as.data.frame(table(PortData2019$Portfolio.Entry)[1])[1,])



#### Tooltip for Density Charts ####

PortData2019$tooltipdenz = paste0(PortData2019$Portfolio.Entry, "</br>", "# of Clients: ", PortData2019$numclients)

#### Density Chart (normal) ####

portdenz <- ggplot(PortData2019, aes(Revenue, color= Portfolio.Entry, fill=Portfolio.Entry)) +
  scale_x_continuous(labels = scales::dollar) +
  ggtitle("Online Revenue Density Distribution by Clients: Last Year vs. Current Year") +
  xlab("Online Revenue") +
  ylab("Density") +
  scale_y_continuous(labels = scales::percent) +
  geom_vline(aes(xintercept = 25000), color="purple") +
  geom_density_interactive(aes(tooltip=tooltipdenz, data_id=Portfolio.Entry),alpha=0.2) +
  theme(text=element_text(family="Trebuchet MS", face="bold",size=10)) +
  annotate(geom="label",x=25000, y=((topy)*1), label="Threshold: $25,000", color="black", fill="purple", alpha=0.6, family="Trebuchet MS", fontface="bold", size=2.5, vjust=1, hjust=.2) 


portdenzi <- ggiraph(code = print(portdenz), width_svg = 8, height_svg = 4, tooltip_extra_css = tooltip_css, hover_css = "cursor:pointer;fill:chartreuse;r:2pt;")

htmlwidgets::saveWidget(portdenzi, "thresholdbalance_denzchart3.html", selfcontained = TRUE, libdir = NULL)



portdenzsqrt <- ggplot(PortData2019, aes(Revenue, color= Portfolio.Entry, fill = Portfolio.Entry)) +
  scale_x_sqrt(labels = scales::dollar) +
  ggtitle("Online Revenue Density Distribution by Clients: Last Year vs. Current Year") +
  labs(subtitle = "x-axis Square Root Transformation") +
  xlab("Online Revenue - sqrt") +
  ylab("Density") +
  scale_y_continuous(labels = scales::percent) +
  geom_vline(aes(xintercept = 25000), color="purple") +
  geom_density_interactive(aes(tooltip=tooltipdenz, data_id=Portfolio.Entry),alpha=0.2) +
  theme(text=element_text(family="Trebuchet MS", face="bold",size=10)) +
  annotate(geom="label",x=25000, y=(sqrt(topy)*1), label="Threshold: $25,000", color="black", fill="purple", alpha=0.6, family="Trebuchet MS", fontface="bold", size=2.5, vjust=1, hjust=.2) 

portdenzsqrti <- ggiraph(code = print(portdenzsqrt), width_svg = 8, height_svg = 4, tooltip_extra_css = tooltip_css, hover_css = "cursor:pointer;fill:chartreuse;r:2pt;")

htmlwidgets::saveWidget(portdenzsqrti, "thresholdbalance_denzchartsqrt3.html", selfcontained = TRUE, libdir = NULL)

#### Sum of Surpluses & Deficits for each portfolio entry category ####
PortData2019$entrybalnet[PortData2019$Portfolio.Entry == "Last Year"] <- aggregate(PortData2019$Deficit.Surplus~PortData2019$Portfolio.Entry, data=PortData2019, FUN="sum")[2,][2]
PortData2019$entrybalnet[PortData2019$Portfolio.Entry == "Current Year"] <- aggregate(PortData2019$Deficit.Surplus~PortData2019$Portfolio.Entry, data=PortData2019, FUN="sum")[1,][2]


#### Bar Chart tooltip ####
PortData2019$tooltipbarchart = paste0(PortData2019$Portfolio.Entry, "</br>","Net Online Revenue: $", formatC(PortData2019$entrybalnet, format="d", big.mark=","))


#### Bar Chart: # of Courses Last Year vs. Current Year ####

barchartt <- ggplot(PortData2019, aes(x=as.factor(Portfolio.Entry), fill=as.factor(Portfolio.Entry) )) + 
  geom_bar_interactive(aes(tooltip=tooltipbarchart, data_id=Portfolio.Entry),alpha=0.5) +
  scale_fill_brewer(palette = "Set1") +
  theme(legend.position="none") +
  ylab("Number of Courses") +
  xlab("Portfolio Entry") +
  geom_text(stat = 'count',aes(label =..count.., vjust = -0.3)) +
  ggtitle("Number of Clients by Portfolio Entry & Net Online Revenue (Surplus & Deficit Combined)") +
  theme(text=element_text(family="Trebuchet MS", face="bold",size=10))

barchartti <- ggiraph(code = print(barchartt), width_svg = 8, height_svg = 4, tooltip_extra_css = tooltip_css, hover_css = "cursor:pointer;fill:chartreuse;r:8pt;")

htmlwidgets::saveWidget(barchartti, "thresholdbalance_barchart3.html", selfcontained = TRUE, libdir = NULL)



#### Waterfall Charts of the Courses' Deficit.Surplus column - in ascending order ####

#sort the dataframe by the Deficit.Surplus column
PortData2019 <- PortData2019[order(PortData2019$Deficit.Surplus),]

#create an id for the waterfall plotting
PortData2019$id <- seq_along(PortData2019$Deficit.Surplus)

#convert course into factor
PortData2019$Client <- factor(PortData2019$Client, levels = PortData2019$Client)


##### Full Course List Waterfall Chart ####
### waterfall plotting
waterfall <- ggplot(PortData2019, aes(id, fill=Def.Surp)) +
  #theme(text=element_text(family="Trebuchet MS", face="bold",size=10)) +
  scale_y_continuous(labels = scales::dollar) +
  ggtitle("Waterfall Chart - Deficit or Surplus: (Online Revenue - $25,000)") +
  #scale_x_discrete("", breaks = levels(balance$desc) + labels = strwr(levels(balance$desc))) +
  geom_rect_interactive(aes(x = id, xmin=id - 0.45, xmax=id + 0.15, ymin=-0, ymax=Deficit.Surplus, tooltip=tooltipbookings)) +
  theme(text=element_text(family="Trebuchet MS", face="bold",size=10)) +
  theme(legend.position="right",legend.title=element_blank(),
        plot.title = element_text(hjust = 0.5, size=10), axis.text.x = element_text(angle=90, vjust=.5, size=4), axis.text.y=element_text(size=10)) +
  ylab("Net Balance") + xlab("Client (Ordered by Deficit/Surplus") 



waterfalli <- ggiraph(code = print(waterfall), width_svg = 8, height_svg = 4, tooltip_extra_css = tooltip_css, hover_css = "cursor:pointer;fill:chartreuse;r:2pt;")

htmlwidgets::saveWidget(waterfalli, "thresholdbalance_fullwaterfall.html", selfcontained = TRUE, libdir = NULL)


##### Partial List 1:50 Waterfall Chart #####

#reindex the dataframe rownames
rownames(PortData2019) <- 1:nrow(PortData2019)

PortData2019ss <- PortData2019[which(PortData2019$Deficit.Surplus < 40000),]


#top_n(PortData2019, 50, id)

waterfallsec <- ggplot(PortData2019ss, aes(id, fill=Def.Surp)) +
  #theme(text=element_text(family="Trebuchet MS", face="bold",size=10)) +
  scale_y_continuous(labels = scales::dollar) +
  #ylab("Deficit or Surplus: (Online Revenue - $18,000)") +
  ggtitle("Waterfall Chart - Deficit or Surplus: (Online Revenue - $25,000)") +
  #scale_x_discrete("", breaks = levels(balance$desc) + labels = strwr(levels(balance$desc))) +
  geom_rect_interactive(aes(x = id, xmin=id - 0.45, xmax=id + 0.15, ymin=-0, ymax=Deficit.Surplus, tooltip=tooltipbookings)) +
  theme(text=element_text(family="Trebuchet MS", face="bold",size=10),legend.title=element_blank(),axis.text.x = element_text(angle=90, vjust=.5, size=5),axis.text.y=element_text(size=10)) +
  labs(subtitle = "Net Balances Less Than $40,000") +
  ylab("Net Balance") +
  xlab("Client (Ordered by Deficit/Surplus") 


waterfallseci <- ggiraph(code = print(waterfallsec), width_svg = 8, height_svg = 4, tooltip_extra_css = tooltip_css, hover_css = "cursor:pointer;fill:chartreuse;r:2pt;")

htmlwidgets::saveWidget(waterfallseci, "thresholdbalance_40waterfall.html", selfcontained = TRUE, libdir = NULL)


##### Barchart/Waterfall chart for Total Deficits and Total Surplus - according to Def.Surp column ####

# take the table of the aggregated sum for each Def.Surp category and create a new column in the dataframe for a tooltip/barchart
PortData2019$totdefsurp[PortData2019$Def.Surp == "Deficit"] <- aggregate(PortData2019$Deficit.Surplus~PortData2019$Def.Surp, data=PortData2019, FUN="sum")[1,2]
PortData2019$totdefsurp[PortData2019$Def.Surp == "Surplus"] <- aggregate(PortData2019$Deficit.Surplus~PortData2019$Def.Surp, data=PortData2019, FUN="sum")[2,2]

#create dataframe for the waterfall chart?

#create tooltip
PortData2019$tooltipwaterchart2 = paste0(PortData2019$Def.Surp, "</br>","Cumulative Total: $", formatC(PortData2019$totdefsurp, format="d", big.mark=","))

### create the little data frame for the waterfall chart
dff1 <- rbind(data.frame(head(PortData2019, 1))) 
dff2 <- rbind(data.frame(tail(PortData2019, 1)))

watertotdf <- rbind(dff1, dff2)

#
watertotdf$id <- seq_along(watertotdf$totdefsurp)



### create waterfall chart
waterfalltotdefsur <- ggplot(watertotdf, aes(Def.Surp, fill=Def.Surp)) +
  #theme(text=element_text(family="Trebuchet MS", face="bold",size=10)) +
  scale_y_continuous(labels = scales::dollar) +
  #ylab("Deficit or Surplus: (Online Revenue - $18,000)") +
  ggtitle("Waterfall Chart - Online Revenue Total Deficit and Total Surplus") +
  geom_rect_interactive(aes(x=Def.Surp, xmin=id+0.15, xmax=id-0.15, ymin=-0, ymax=totdefsurp, tooltip=tooltipwaterchart2, data_id=id), alpha=0.7) +
  geom_text(aes(id, totdefsurp, label = scales::dollar(totdefsurp),family="Trebuchet MS", fontface="bold"), vjust = 1, size = 3) +
  theme(text=element_text(family="Trebuchet MS", face="bold",size=10),legend.title=element_blank(),axis.text.x = element_text(size=12),axis.text.y=element_text(size=10)) +
  #geom_text(aes(label=y, vjust = -0.3)) +
  #geom_text(aes(id, totdefsurp, label = scales::dollar(totdefsurp)), vjust = 1, size = 3) +
  ylab("Total") +
  xlab("") 


waterfalltotdefsuri <- ggiraph(code = print(waterfalltotdefsur), width_svg = 8, height_svg = 4, tooltip_extra_css = tooltip_css, hover_css = "cursor:pointer;fill:chartreuse;r:2pt;")


htmlwidgets::saveWidget(waterfalltotdefsuri, "thresholdbalance_todtdefsurwaterfall.html", selfcontained = TRUE, libdir = NULL)


##### List of interactive charts for document creation - rmarkdown####

thresholdbalance.graphs <- list(pointploti, portdenzi, barchartti, waterfalli, waterfallseci, waterfalltotdefsuri)

#htmlwidgets::saveWidget(pointploti, file="thresholdbalance_pointplot2.html", selfcontained = FALSE)




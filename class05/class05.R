#' ---
#' title: "Class 5 is really good"
#' output: github_document
#' ---

# baby weight data input
weight <- read.table("bimm143_05_rstats/weight_chart.txt",header=TRUE)


# make a custom plot
plot(weight,typ="b", pch=12, col="blue",main='baby')

#barplot
counts <- read.table("bimm143_05_rstats/feature_counts.txt",header=TRUE,sep = "\t")
barplot(counts$Count,width=2,space=0,horiz = TRUE,names.arg = counts$Feature,las=2,col="yellow",xlab="x",ylab="y")

# 2C histogramA
c(rnorm(10000),rnorm(10000)+4)
hist(c(rnorm(10000),rnorm(10000)+4),breaks=10,main = "lala",col = "red")

# Plot characters
plot(1:5,pch=1:10,cex=1:10,col=c("red","blue"))

#Boxplot


#PAR
par(mfrow=c(2,2))
plot(1,0)

#color vectors
male <- read.table("bimm143_05_rstats/male_female_counts.txt",header = T,sep = "\t")
barplot(male$Count,width=2,names.arg = male$Sample,col = rainbow(nrow(male)),las=2,horiz = T)
barplot(male$Count,width=2,names.arg = male$Sample,col = c("blue3","red1"),las=2,horiz = T)

#coloring by value
gene <- read.table("bimm143_05_rstats/up_down_expression.txt",header=T,sep="\t")
#how many gene in this data set
nrow(gene)
#how many are up, down and all around?
table(gene$State)
#let's plot this data
plot(gene$Condition1,gene$Condition2,col=gene$State)
#This is how we change palette
palette(c(rainbow(7)))
palette(c("red","green","yellow"))

#Coloring by point density
meth <- read.table("bimm143_05_rstats/expression_methylation.txt",header=T,sep="\t")

#better but still not very useful
mycols <- densCols(meth$gene.meth,meth$expression)
plot(meth$gene.meth,meth$expression,col=mycols)

# what we want to do
inds <- meth$expression>0

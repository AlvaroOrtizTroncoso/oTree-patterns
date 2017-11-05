library("ape")

# Load the data from the spreadsheet
sourcePath = "oTree_association_clean.csv"
sheet <- read.csv( sourcePath, header=TRUE )
sheet <- sheet[,2:ncol( sheet )] # remove the headers

# Compute a manhattan distance matrix
sheet <- t(sheet)
dm <- dist(sheet, method="manhattan")

# Estimate the tree using hierarchical clustering
phylo <- as.phylo(hclust(dm, method="average"))

# Test the correlation between tree and distance matrix
cophenetic <- cophenetic(phylo)
print( cor.test(cophenetic, as.matrix(dm)))

#Plot the tree
dev.new(width=5, height=4)
par(mar = rep(0, 4))
plot(phylo, use.edge.length=TRUE)

# Save the plot to file
#png( file="otree_examples_association.png", width=1000, height=800, res=96 )
#plot( phylo, use.edge.length=TRUE )
#dev.off()


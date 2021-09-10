#1. two most popular clustering approaches

#hierarchical agglomerative clustering
#each observation starts as its own cluster and then combined, two at a time, untial all clusters are merged into a single cluster

#partitioning clustering
#specify KL the number of clusters sought, and obeservations then are randomly divide into K groups and reshuffled to form cohesive clusters

#within each of these broad approaches, there are many clustering algorithms to choose from.

#for hierarchical clustering, the most popular are single linkage, complete linkage and averaege linkage, centroid and Ward's method

#for partitioning, the two most popular are k-means and partitioning around medoids(PAM)

#2. common steps in cluster analysis

#a. choose appropriate attributes: select variables that feel may be important for identifying and understanding differences among groups of observations within the data
#b. scale the data: most popular approach is to standardize each variable top a mean of 0 and a sd of 1, other alternatives include idividing each variable by its maximum value or sutracting the variable's mean and dividing by the variable's median absolute deviation

af1<-apply(mydata,2 ,function(x){(x-mean(x))/sd(x)})#subtracting mean 
df2 <-apply(mydata,2, function(x){x/max(x)}) #dividing each variable by its maximum
df3<-apply(mydata,2 function(x){(x-mean(x))/mad(x)})#dividing by the variable's median absolute deviation

#c. screen for outliers: many clustering are sensitive to outliers, we can screen for univariate outliers using fucntions from outliers package. The mvoutlier package contains functions that can be used to identify multivariate outliers. an alternative is to use a clustering method that is robust to the presence of outliers

#d. calculate distance: distance bewteen two observations is the Euclidean distance, but the Manhattan, Canberra, asymmetric binary, maximum and Minkowski distance measures are also available(ss ?dist for details)

#e. select a clustering algorithm: hierarchical clustering is useful for smaller problems(say, 150 observations or less) or partitioning method handle much larger problems but requires that the number of clusters be specified in advance, we need to choose a specific clustering algorithm

#f. obtain one or more cluster solutions: use the method at step e

#g. determine the number of clusters present: in order to obtain a final cluster solution, we must decide how many clusters to present in the data, this is a thorny problem but many approaches have been proposed
#it usually involves exracting various numbers of clusters(say, 2 to K) and comparing the quality of the solutions. The NbClust() function in the NBClust package provides 30 different indices to help 

#h. visualize the results. the results of a hierarchical clustering are usually presented as a dendrogram, partitioning results are typically visualized using a bivariate cluster plot

#i. interpret the clusters: by obtaining summary statistics for each variable by clusters. for continuous data, the mean or median for each variable within each cluster is calculated, for mixed data(data that contain categorical variables), the summary statistics will also include modes or categroy distributions

#j. validate the results: ask are these groupings in some sease real, and not a manifestation of unique aspects of this dataset or statistical technique?
#if a different cluster method or different sample is employed, would the same clusters be obtained? The fpc, clv and clValid packages each contain functions for evaluating the stability of a clustering solution

#3. calculating distances
#every cluster analysis begins with the calculation of a distance, dissimilarity or proximity between each entity to be clusetred
#the Euclidean distance between two observations is given by: dij=Sqrt(sum(xip-xjp)^2) where i and j are observations and p is the number of variables
install.packages("flexclust")
library(flexclust)
data(nutrient, package="flexclust")
head(nutrient,4)

#and the Euclidean distance between the first two(beef braised and hamburger) is 
#d=Sqrt((340-245)^3+(20-21)^2+(28-17)^2+(9-9)^2+(26-26)^2)=95.64
#dist() function in base R installation can be used to calculate the distances between all rows(observations) of a matrix or data frame.
#format: dist(x, method=)where x is the inpyt data and method="euclidean" by default, the function returns a lower triangle matrix by default but the as.matrix() function can be used to access the distances using standard bracket notation

d<-dist(nutrient)
as.matrix(d)[1:4,1:4]
#larger distances indicate larger dissimilarities between observations

#4. cluster analysis with mixed data types
#Euclidean distances are usually the distance measure of choice for continuous data
#daisy() function in the cluster package to obtain a dissimilarity matrix among observations that have a ny combination of binary, nominal, ordianl, and continuous attributes
#other functions in the cluster package can use too, for example, agnes() offers agglomerative hierarchical clustering, and pam() provides partitioning around medoids

#5. hierarchical cluster analysis

#first, define each obervation as a cluster
#calculate the distances between every cluster and every other cluster
#combine the two clusters that have the smallest distance. This reduces the number of clusters by one
#repeat steps 2 and 3 until all clusters have been merged into a single cluster containing all observations

#6. hierarchical clustering methods
#single linkage, shortest distance bewteen a point in one cluster and a point in the other cluster
#complete linkage, longest distance between a point in one cluster and a point in the other cluster
#average linkage, average distance between each point in one cluster and each point in the other cluster(also caleed UPGMA[unweighted pair group mean averaging]).
#centroid, distance between the centroids(vector of variable means) of the two clusters, for a single observation, the centroid is the variable's values
#Ward, the ANOVA sum of sqaures between the two clusters added up over all the variables

#single-linkage clustering tends to find elongated, cigar-shaped clusters. It also commonly displays a phenomenon called chaining- dissimilar observations are joined into the same cluster because they are simiar to intermediate observations between them
#complete-linkage clustering tends to find compact clusters of approximately equal diameter, it can also be sensitive to outlers
#average-linkage clustering offers compromise between the two, it is less likely to chain and is less susceptible to outliers, it also has a tendency to join clusters with small variances
#Ward's method tends to join clusters with small numbers of observations and tends to produce clusters with roughly equal numbers of observations, it can also be sensitive to outliers
#the centroid method offers an attractive alternative due to its simple and easily understood definition of cluster distances, it is also less sensitive to outliers than other hierarchical methods, but it may not perform as well as the average-linkage or Ward method

hclust(d, method=)#hierarchival clustering, d is a distance matrix produced by the dist() function and methods include "single", "complete","average", "centroid", and "ward"

data(nutrient, package="flexclust") #data imported
row.names(nutrient)<-tolower(row.names(nutrient))#rowname are set to lower case
nutrient.scaled<-scale(nutrient)#scale for range
d<-dist(nutrient.scaled)#Euclidean distances

fit.average<-hclust(d, method="average")
plot(fit.average, hang=-1, cex=.8, main="Average Linkage Clustering")
#hang option in the plot() function justifies the observation labels(causing them to hang down from 0)

#dendrogram displays how ietms are combined into clusters and is read from bottom up
#then the two observations that are closet(beef braised and smoked ham) are combined
#next,pork roast and pork simmered are combine followed by chicekn canned and tuna canned 
#further the beef braised/smoked ham cluster and the pork roast/pork simmered clusters are combined
#this continuous until all observations are combined into a single cluster

#the height dimension indicates the criterion value at whic hclusters are joined

#for average-linkage clustering, this criterion is the average distance between each point in one cluster and each point in the other cluster

install.packages("NbClust")#offers numerous indices for determining the best number of clusters in a cluster analysis
library(NbClust)
devAskNewPage((ask=T))
nc<- NbClust(nutrient.scaled, distance="euclidean",
             min.nc=2, max.nc=15, method="average")

table(nc$Best.n[1,])

barplot(table(nc$Best.n[1,]),
        xlab="Number of Clutsters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by 26 Criteria")
#here, four criteria each favor two clusters, four criteria facor three clusters, and so on.


#obtaining the final cluster solution

clusters<-cutree(fit.average,k=5)
table(clusters)

aggregate(nutrient, by=list(cluster=clusters), median)

aggregate(as.data.frame(nutrient.scaled), by=list(cluster=clusters), median)

plot(fit.average, hang=-1, cex=.8,
     main="Average Linkage Clustering\n5 Cluster Solution")

rect.hclust(fit.average, k=5)

#the cutree() function is used to cut the tree into five clusters
#the first cluster has 7 observations
#the second cluster has 16 observations and so on

#the aggregate() is then used to obtain the median profile for each cluster

#the results are reported in both the original metric and in standardized form

#the rect.hclust() is used to superimpose the five0cluster solution

#Sardines form their own cluster and are much higher in calcium than the other food groups
#beef heart is also a singleton and is high in protein and iron
#the clam cluster is low in protein and high in iron
#the ietms in the cluster containing beef roast to pork simmered are high in energy and fat, finally, the alrgest group is relatively low in iron

#hierarchical clustering can be particularly useful when you expect nested clustering and a meaningful hierarchy
#but tierarchical algorithms are greedy in the sense that once an observation is assigned to a cluster, it cant be reassigned later in the proces
#additionally, hierarchical clustering is difficult to apply in large samples

#7. partitioning cluster analysis

#in the partitioning approach, observations ar edivided into K groups and reshuffled to form the most cohesive clusters possible according to a given criterion
#this section considers two methods: k-means and partitioning around medoids(PAM)

#k-means clustering

#select K centroids(k rows chosen at random)
#assign each data point to its closest centroid
#recalculate the centroids as the average of all data points in a cluster(that is, the centroids are p-length mean vectors, where p is the number of variables)
#assign data points to their closest centroids
#continues step 3 and 4 until the observations arent reassigned or the maximum number of iterations(19 as a default) is reached

#R uses an efficient algorithm by Hartigan and Wong(1979) that partitions the observaions into k groups such that the sum of squares of the observations to their assigned cluster centers is a minimum

#k means clustering can handlelarger datasets than hierarchical cluster and observations arent permanently committed to a cluster
#they are moved when doing so improves the overall solution
#but the use of means implies that all variables mus tbe continuous, and the approach can be severely affected by outliers, it also performs poorly in the presence of non-convex clusters(for example ,U shaped)


#nstart option in kmeans() function attempts multiple initial configurations and reports on the best one. For example, adding nstart=25 generates 25 initial configurations, this approach often is recommended

#unlike hierarchical clustering, k means clustering requires that we specify in advance the number of clusters to extract
##additionally, a plot of the total within-groups sums of squares aginst the number of clusters in a k-means solution can be helpful

wssplot <-function(data, nc=15 , seed=1234){#data is the numeric dataset to be analyzed, nc is the maximum number of clusters
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for(i in 2:nc){
    set.seed(seed)#seed is a random-number seed
    wss[i]<-sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
  }
}
install.packages("rattle")
library(rattle)
data(wine, package="rattle")
head(wine)

df<-scale(wine[-1])#values vary in range, scaling here

wssplot(df)#number of clusters detrermined using the wssplot() and Nbclust()
#it indicates that there is a distinct drop in the within groups sum of squares when moving from one to three clusters. After three clusters, this decrease drops off, suggesting that a three cluster solution may be a good fit to the data 
library(NbClust)
set.seed(1234)
devAskNewPage(ask=T)
nc<-NbClust(df, min.nc=2, max.nc=15, method="kmeans")
table(nc$Best.n[1,])barplot(table(nc$Best.n[1,]),
                            xlab="Number of Clusters", ylab="Number of Criteria",
                            main="Number of Clusters Chosen by 26 Criteria")#14 of 24 criteria provided by the Nbclust package sugget a three cluster solution. Note that not all 30 criteria can be calculated for every dataset
set.seed(1234)
fit.km <-kmeans(df,3,nstart=25)
fit.km$size

fit.km$centers

aggregate(wine[-1], by=list(cluster=fit.km$cluster), mean)#used along with the cluster memberships to determine variable means for each cluster in the original metric

#how well did k means clustering uncover the actual structure of the data contained in the Type variable
ct.km <-table(wine$Type, fit.km$cluster)
ct.km

library(flexclust)
randIndex(ct.km)
#the adjusted rand index provides a measure of the agreement between two partitions, adjusted for chance
#it ranges from -1(no agreement) to 1(perfect agreement)


#8. partitioning around medoids
#since its based on means, the k-means clustering approach can be sensitive to outliers
#a more robust solution is provided by partitioning around medoids(PAM)
#rather than representing each cluster using a centroid(a vector of variable means), each cluster is identified by its most representative observation (called a medoid), whereas k means uses Euclidean distances, PAM can be based on any distance measure, it can therefore accommodata mixed data types and isnt limited to continuous variables

#9. PAM steps
#a. randomly select K observations(call each a medoid)
#b. calculate the distance/dissimilarity of evrery observation to each medoid
#c. assign each observation to its closest medoid
#e. calculate the sum of the distances of each observation from its medoid(total cost)
#f. select a point that isnt a medoid, and swap it with its medoid
#g. reassign every point to its closest medoid
#h. calculate the total cost
#i. if this total cost is smaller, keep the new point as a medod
#j. repeat steps f,j,h,i until the medoids dont change

pam(x,k, metric="euclidean", stand=F)#in the cluster package to partition around medoids

#x is a data matrix or data frame, k is the number of clusters, metric is the type of distance/dissimilarity measure to use, and stand is a logical value indicating whether the variables should be standardized before calculating this metric

library(cluster)
set.seed(1234)
fit.pam <- pam(wine[-1], k=3, stand=T)
fit.pam$medoids

clusplot(fit.pam, main="Bivariate Clusteer Plot")

ct.pam <-table(wine$Type, fit.pam$clustering)

randIndex(ct.pam)

#PAM didnt perform as well as k means in this instance
#0.7 <0.9 from k means

#9. avoiding nonexistent clusters
install.packages("fMultivar")
library(fMultivar)

set.seed(1234)
df<-rnorm2d(1000, rho=.5)
df<-as.data.frame(df)
plot(df, main="Bivariate Normal Distribution with rho=.5")
#the wssplot and NbClust() functions are then used to determine the number of clusters present
wssplot(df)
library(NbClust)
nc<-NbClust(df, min.nc=2, max.nc=15, method="kmeans")
dev.new()
barplot(tabe(nc$Beat.n[1,]),
        xlab="Number of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by 26 Criteria")

#wssplot suggest that there are three clusters, whereas many of the criteria returned by NbClust() suggest between two and three clusters

library(ggplot2)
library(cluster)
fit<-pam(df,k=2)
df$clustering <- factor(fit$clustering)
ggplot(data=df, aes(x=V1,y=V2, color=clustering, shape=clustering)) + geom_point()+ggtitle("Clustering of Bivariate Normal Data")
   #clearly the partitioning is artificial, there are no real clusters here
#Cubic Cluster Criteria(CCC) reported by NbClust can often help to uncove situations where no structure exsist
plot(nc$All.index[,4], type="o", ylab="CCC",
     xlab="Number of Clusters", col="blue")
#when the CCC values are all negative and decreasing for two or more clusters, the distribution is typically unimodal


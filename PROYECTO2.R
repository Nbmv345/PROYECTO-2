#EJERCICIO 1
uni.df = read.csv("Universities.csv")
str(uni.df)
summary(uni.df)

#a) Eliminacion de datos faltantes
sum(is.na(uni.df))

#eliminacion de datos nulos

uni.df<-uni.df[!is.na(uni.df$X..appli..rec.d),]
uni.df<-uni.df[!is.na(uni.df$X..appl..accepted),]
uni.df<-uni.df[!is.na(uni.df$X..new.stud..enrolled),]
uni.df<-uni.df[!is.na(uni.df$X..new.stud..from.top.10.),]
uni.df<-uni.df[!is.na(uni.df$X..new.stud..from.top.25.),]
uni.df<-uni.df[!is.na(uni.df$X..FT.undergrad),]
uni.df<-uni.df[!is.na(uni.df$X..PT.undergrad),]
uni.df<-uni.df[!is.na(uni.df$in.state.tuition),]
uni.df<-uni.df[!is.na(uni.df$out.of.state.tuition),]
uni.df<-uni.df[!is.na(uni.df$room),]
uni.df<-uni.df[!is.na(uni.df$board),]
uni.df<-uni.df[!is.na(uni.df$add..fees),]
uni.df<-uni.df[!is.na(uni.df$estim..book.costs),]
uni.df<-uni.df[!is.na(uni.df$estim..personal..),]
uni.df<-uni.df[!is.na(uni.df$X..fac..w.PHD),]
uni.df<-uni.df[!is.na(uni.df$stud..fac..ratio),]
uni.df<-uni.df[!is.na(uni.df$Graduation.rate),]

#verificacion de nuestra eliminacion
sum(is.na(uni.df))
summary(uni.df)

#b)Agrupacion, distancia euclidiana y dendograma

normalized_data <- scale(uni.df[,3:7]) #Se excluyen las dos primeras columnas ya que son de tipo caracter
View(normalized_data)
d <- dist(data, method = "euclidean")
h1 <- hclust(d, method = "complete" )
h2 <- hclust(d, method = "single" )
h3 <- hclust(d, method = "average" )
h4 <- hclust(d, method = "centroid" )
plot(h1)
plot1 <- plot(h1,hang=-1)
groups <- cutree(h1,k=4)
plot(h1)
rect.hclust(h1,plot(h1,hang=-1),k=4,border="red")

#c estadisticas resumidas
aggregate(uni.df[,-1],by=list(uni.df$Graduation.rate),median)
aggregate(uni.df[,-1],by=list(uni.df$estim..book.costs),median)
aggregate(uni.df[,-1],by=list(uni.df$X..appl..accepted),median)

#d

library(ggplot2) 
ggplot(uni.df, aes(x=reorder(Public..1...Private..2., Public..1...Private..2., function(x)-length(x)))) +
  geom_bar(fill='steelblue') +
  labs(x='Tipo de universidad (Privada (2) Publica(1))')



#f
categorical_variables <- names(select(uni.df,"State","Public..1...Private..2."))
print(categorical_variables)
continuous_variables <- setdiff(names(which(sapply(uni.df, is.numeric))),("Public..1...Private..2."))
uni.df_norm <- as.data.frame(scale(uni.df[,continuous_variables]))

dist_mat <- dist(uni.df_norm, method = 'euclidean')

hcluster <- hclust(dist_mat, method = "complete")

sub_grp <- cutree(hcluster, k = 6)
print(paste("No. of rows:", nrow(uni.df)))
print(paste("No. of cols:", ncol(uni.df)))
uni.df['class']<-as.factor(sub_grp)


cluster_table<-aggregate(uni.df[continuous_variables],by=uni.df['class'],mean)
point_new <- uni.df[uni.df['College.Name']=='Tufts University']
point_new[is.na(point_new)] <- 0
point_new<- as.integer(uni.df[-c(1:3)])

# fucntion to calculate the Euclidean Distance
Euclidean_Distance<-function(x1, x2) sqrt(sum((x1 - x2) ^ 2))

# calculating Euclidean distance of data from each cluster
for (i in 1:nrow(cluster_table)){
  point <- cluster_table[i,][-1]
  print(paste('Distance from cluster',i,'is', Euclidean_Distance(point_new,point) ))
}

# From the result we can see that the Tufts University data is closer to cluster 2.

# imputing the missing value by cluster average
uni.df$X..PT.undergrad[uni.df$College.Name == 'Tufts University']<-cluster_table$X..PT.undergrad[cluster_table$class == 2]



#F.2


punto <- uni.df[uni.df['College.Name']=='Tufts University']

punto<- scale(uni.df[,3:7])

d <- dist(punto,method="euclidean")
h2 <- hclust(d,method="complete")



plot(h2)

plot1 <- plot(h2,hang=-1) 

groups <- cutree(h2,k=4)

plot(h2)
rect.hclust(h2,plot(h2,hang=-1),k=4,border="blue")




#--------------------------------------------

#EJERCICIO 2
farma.df<- read.csv("Pharmaceuticals.csv", header=TRUE)

farma.df
pc<-farma.df[,2]
row.names(farma.df) <- farma.df[,2]
farma.df<- farma.df[, -c(1,2,12,13,14)]
farma.df.norm <- sapply(farma.df, scale)
set.seed(123)

#a
library(NbClust)
nc <- NbClust(farma.df.norm, min.nc=2, 
              max.nc=10, method="kmeans")
table(nc$Best.n[1,])

barplot(table(nc$Best.n[1,]), xlab="Numero de Clusters", ylab="Promedio", main="Numero de clusteres seleccionados por el promedio")

#checamos el numero numero de clusters
wssplot <- function(farma.df.norm, nc=10, seed=42) {
  wss <- (nrow(farma.df.norm)-1)*sum(apply(farma.df.norm, 2, var)) 
  for (i in 2:nc) {
    set.seed(123) 
    wss[i] <- sum(kmeans(farma.df.norm, centers=i)$withinss)
  } 
  plot(1:nc, wss, type="b", xlab="Numero de clusters", ylab="Suma de cuadrados")
}
wssplot(farma.df.norm,nc=10)

#b
#k-means cluster analisis
h.km <- kmeans(farma.df.norm, 4 , nstart=10)
h.km$size
h.km$centers


#c
# ver si un cluster forma algun patron
dist(h.km$centers)



#d
# gr�fico de dispersi�n
plot(c(0), xaxt = 'n', ylab = "", type = "l", ylim = c(min(h.km$centers), max(h.km$centers)), xlim = c(0, 9))
axis(1, at = c(1:9), labels = names(farma.df))
for (i in c(1:5))
  lines(h.km$centers[i,], lty = i, lwd = 2, 
        col = ifelse(i %in% c(1),"yellow",
                     (ifelse(i %in% c(2),"red",
                             (ifelse(i %in% c(3),"black",
                                     (ifelse(i %in% c(4),"purple","blue"))))))))

text(x = 0.5, y = h.km$centers[, 1], labels = paste("Grupos", c(1:4)))




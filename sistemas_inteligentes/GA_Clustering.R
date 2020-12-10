-------------------------------------------------------------------------------
  # Master of Science in Artificial Intelligence
  # Authors: Guillermo López
  # Date: 2020, November 3rd
------------------------------------------------------------------------------

# Required libraries
library(dplyr)

# Loading information. Should exists a sample_100.csv file created by the code <...>
db<-read.csv('sample_100.csv')
head(db)

# Data should be normalized to improve the k-means preformance. Mostly of our variables are boolean, although, there are three of them which are continuos: time in seconds and distance in kilometers.
variables_to_normalize <- names(select(db,contains('viaje')|contains('valor')|
                                         contains('tiempo_')|contains('distancia_')))
db2 <- db
min_D <- c() ; max_D <- c() ; dif_D <- c()
for(i in variables_to_normalize){
  min_D[i] <- min(db2[i],na.rm = T)
  max_D[i] <- max(db2[i],na.rm = T)
  dif_D[i] <- max_D[i] - min_D[i]
  db2[i] <- (db2[i]-min_D[i])/dif_D[i]

# Defining function to create an individual with k random centroids:
create_centroids <- function(k,db){
  db_names<-names(db)
  m <- length(db_names)
  centroids <- matrix(runif(m*k),nrow=k,ncol=m)
  return(centroids)
}

# Function to create an initial population with p individuals of k random centroids:
create_population<-function(p,k,db){
  population <- vector('list',p)
  for(i in 1:p){
    population[[i]] <- create_centroids(k,db)
  }
  return(population)
}

# Defining function to calculate intra-cluster distance:
inter_cluster_distance <- function(centroids){
  k <- nrow(centroids)
  inter_cluster <- matrix(Inf,nrow=k,ncol=k)
  for(i in 1:k){
    for(j in 1:k){
      if(i!=j){
        a <- centroids[i,]
        b <- centroids[j,]
        inter_cluster[i,j] <- sqrt(sum((a-b)^2))
      }
    }
  }
  return(inter_cluster)
}

# Defining function to calculate intra-cluster distance:
intra_cluster_distance <- function(db,centroids){
  db <- data.matrix(db)
  centroids <- data.matrix(centroids)
  n <- nrow(db)
  k <- nrow(centroids)
  distanceMatrix <- matrix(nrow=n,ncol=k)
  for(i in 1:k){
    distanceMatrix[,i] <- rowSums(t(t(db)-centroids[i,])^2,na.rm=T)
  }
  cluster <- apply(distanceMatrix,1,which.min)
  distance <- apply(distanceMatrix,1,min)
  intra <- c()
  for(i in 1:k){
    intra <- c(intra,sqrt(sum((cluster==i)*distance)/sum(cluster==i)))
  }
  intra <- ifelse(is.na(intra),1e+03,intra)
  intra_matrix <- matrix(nrow=k,ncol=k)
  for(i in 1:k){
    for(j in 1:k){
      intra_matrix[i,j] <- intra[i] + intra[j] 
    }
  }
  intra_cluster <- list(cluster=cluster,distance=distance,intra=intra,intra_matrix=intra_matrix)
  return(intra_cluster)
}

# function to calculate Davies-Bouldin index
davies_bouldin<-function(inter,intra){
  D_matrix <- intra$intra_matrix/inter
  D_max <- apply(D_matrix,1,max)
  db_index <- mean(D_max)
  return(db_index)
}

# function to update a selection matrix which include the fitness function and the odd to be selected
update_selection_matrix<-function(db,population,p,k){
  selection_matrix <- matrix(nrow=p,ncol=5)
  for(i in 1:p){
    intra <- intra_cluster_distance(db,population[[i]])
    inter <- inter_cluster_distance(population[[i]])
    selection_matrix[i,1] <- davies_bouldin(inter,intra)
  }
  selection_matrix[,2] <- 1/selection_matrix[,1]
  selection_matrix[,3] <- selection_matrix[,2]/sum(selection_matrix[,2])
  selection_matrix[,4] <- cumsum(selection_matrix[,3])
  selection_matrix[,5] <- selection_matrix[,4]
  return(selection_matrix)
}

# Genetic algorithm
genetic_algorithm<-function(db,k,p,gen,fm){
  m <- ncol(db)
  # create initial population
  population <- create_population(p,k,db)
  selection <- update_selection_matrix(db,population,p,k)
  evolution <- matrix(nrow=gen,ncol=2)
  evolution[1,1]<-1
  evolution[1,2]<-min(selection[,1])
  # evaluate fitness function for each individual
  for(i in 2:gen){
    # creating new_population
    new_population<-vector('list',p)
    for(l in 1:(p/2)){
      # parent selection
      a <- runif(2)
      b <- (t(selection[,4:5]) < a) * 1 + (t(selection[,4:5])-a)
      parent_A <- population[[which.min(b[1,])]]
      parent_B <- population[[which.min(b[2,])]]
      # reproduction
      #child_A<-child_B<-c()
      child<-vector('list',2)
      for(j in 1:k){
        c <- round(runif(1,min=2,max=m-1))
        #child_A <- c(child_A,parent_A[j,1:(c-1)],parent_B[j,c:m])
        child[[1]] <- c(child[[1]],parent_A[j,1:(c-1)],parent_B[j,c:m])
        #child_B <- c(child_B,parent_B[j,1:(c-1)],parent_A[j,c:m])
        child[[2]] <- c(child[[2]],parent_A[j,1:(c-1)],parent_B[j,c:m])
      }
      # mutation
      mutation_vector <- runif(2*k*m,min=0,max=2)
      mutation_vector <- ifelse(between(mutation_vector,2*(fm/2),2*(1-fm/2)),1,mutation_vector)
      mutation <- matrix(mutation_vector,nrow=2,ncol=k*m,byrow=T)
      new_population[[(l*2)-1]] <- matrix(child[[1]]*mutation[1,],nrow=k,ncol=m,byrow=T)
      new_population[[(l*2)-1]] <- ifelse(new_population[[(l*2)-1]]>1,1,
                                          new_population[[(l*2)-1]])
      new_population[[l*2]] <- matrix(child[[2]]*mutation[2,],nrow=k,ncol=m,byrow=T)
      new_population[[l*2]] <- ifelse(new_population[[l*2]]>1,1,new_population[[l*2]])
    }
    population <- new_population
    selection <- update_selection_matrix(db,population,p,k)
    evolution[i,1]<-i
    evolution[i,2]<-min(selection[,1])
  }
  best_subject<-population[[which.min(selection[,1])]]
  intra <- intra_cluster_distance(db,best_subject)
  inter <- inter_cluster_distance(best_subject)
  output<-list(evolution=evolution,best_subject=best_subject,intra=intra,inter=inter)
  return(output)
}

# setting experiment to find the best architecture
k_opt <- c(4,6,8,10)
p_opt <- c(10,20,30,50)
gen_opt <- c(25,50,75,100)
fm_opt <- c(0.01,0.02,0.03,0.05)
n<-0
res<-matrix(nrow=4^4*2,ncol=6)
for(i_k in k_opt){
  for(i_p in p_opt){
    for(i_g in gen_opt){
      for(i_f in fm_opt){
        for(i in 1:2){
          n<-n+1
          t <- system.time(ga <- genetic_algorithm(select(db2,-c('row_id')),i_k,i_p,i_g,i_f))
          res[n,1] <- i_k
          res[n,2] <- i_p
          res[n,3] <- i_g
          res[n,4] <- i_f
          res[n,5] <- min(ga$evolution[,2])
          res[n,6] <- t[[3]]
        }
      }
    }
  }
}


df_res <- data.frame(res)
names(df_res) <- c('centroids','population','generations','mutation_factor','db_index','time')

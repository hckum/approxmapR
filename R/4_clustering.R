#' get_weighted_sequence
#'
#' Calculates the inter-sequence distance matrix
#'
#' @param seqList A list of sequences
#' @export
#'
#' @return Returns the inter-sequence distance atrix of all the sequences
#'
#' @examples inter_seq_Distance(sequence_list)

#interStringLevenshteinDistance
inter_seq_Distance = function(seqList) {
  inter_Seq_Distance_Matrix = matrix(nrow = length(seqList),ncol = length(seqList))
  for (i in 1:nrow(inter_Seq_Distance_Matrix)) {
    for(j in 1:ncol(inter_Seq_Distance_Matrix)) {
      if(i==j) {
        inter_Seq_Distance_Matrix[i,j] = 0
      } else if(!(is.na(inter_Seq_Distance_Matrix[j,i]))) {
        inter_Seq_Distance_Matrix[i,j] = inter_Seq_Distance_Matrix[j,i]
      } else {
        dist = calculate_dist_btw_sequences(seqList[[i]],seqList[[j]])
        inter_Seq_Distance_Matrix[i,j] = dist$distance / max(dim(dist$distance_matrix))
      }
    }
  }
  #rownames(interStringDistanceMatrix) = stringList
  #colnames(interStringDistanceMatrix) = stringList
  return(inter_Seq_Distance_Matrix)
}



#' calculate_DensityInfo
#'
#' Calculates the density info of all the sequences
#'
#' @param inter_Seq_Distance_Matrix A matrix of inter sequence distances
#' @param k k nearest neighbours
#' @export
#'
#' @return Returns the density information for all the sequences
#'
#' @examples calculate_DensityInfo(sequence_list, 2)


calculate_DensityInfo = function(inter_seq_dist_mat, k) {
  results = list(nrow(inter_seq_dist_mat))
  for (i in 1:nrow(inter_seq_dist_mat)) {
    dist = inter_seq_dist_mat[i,]
    dist[i] = NA
    k_smallest_dist <- sort(dist,partial = k)[k]
    nearest_neighbours = dist<=k_smallest_dist
    #if(k_smallest_dist==0) k_smallest_dist = 1e-4
    n = sum(nearest_neighbours,na.rm = T)
    density = n/k_smallest_dist
    item = list(density = density,NearestSequences = nearest_neighbours,distances = dist)
    results[[i]] = item
  }
  return(results)
}



#' get_DensityArray
#'
#' Extracts the densities of all the sequences from the density info
#'
#' @param DensityInfo A list returned by calculate_DensityInfo
#' @export
#'
#' @return Returns the density information for all the sequences
#'
#' @examples get_DensityArray(Density_info)

#pass a density info object to the function and it extratcs all the densities
get_DensityArray = function(DensityInfo) {
  densityArray = numeric(length(DensityInfo))
  for (i in 1:length(DensityInfo)) {
    densityArray[i] = DensityInfo[[i]]$density
  }
  return(densityArray)
}




#' knnCluster
#'
#' Performs the knn clustering algorithm and returns clustering info
#'
#' @param seqList A list of sequences
#' @param k number of nearest neighbours to look at
#' @param id Id of every sequence in the list
#' @export
#'
#' @return Returns the id, cluster, cluster_density and the sequence_density of each sequence
#'
#' @examples knnCluster(seqList,k,id)

# #clustering
# knnCluster = function(seqList,k,id) {
#
#   densityInfo = calculate_DensityInfo(seqList,k)
#   densityArray = get_DensityArray(densityInfo)
#
#   #step 1 - initialize every sequence as a cluster
#   cluster = 1:length(seqList)
#   clusterDensity = densityArray
#
#   #step 2 - clustering based on criteria
#   cluster_change = 1
#   while(cluster_change>0) {
#     cluster_change = 0
#     for (i in 1:length(seqList)) {
#       nSeq = densityInfo[[i]]$NearestSequences
#       densityCheck = densityArray[i] < densityArray
#       prelimCriteria = nSeq & densityCheck
#       for (j in 1:length(seqList)) {
#         if(prelimCriteria[j]) {
#           if(densityInfo[[i]]$distances[j] == min(densityInfo[[i]]$distances[prelimCriteria])) {
#             if(cluster[i]!=cluster[j]) {
#               minCluster = min(cluster[i],cluster[j])
#               cluster[cluster==cluster[i]]=minCluster
#               cluster[cluster==cluster[j]]=minCluster
#               maxClusterDensity = max(clusterDensity[i],clusterDensity[j])
#               clusterDensity[cluster==minCluster] = maxClusterDensity
#               cluster_change = cluster_change + 1
#             }
#           }
#         }
#       }
#     }
#   }
#
#   #step 3 - clustering ties
#   cluster_change = 1
#   while(cluster_change>0) {
#     cluster_change = 0
#     for (i in 1:length(seqList)) {
#       nSeq = densityInfo[[i]]$NearestSequences
#       densityCheck = densityArray[i] < densityArray
#       noNeighbourWithGreaterDensity = rep(ifelse(sum(nSeq & densityCheck)==0,T,F),length(densityArray))
#       densityCheck = noNeighbourWithGreaterDensity & (densityArray[i] == densityArray)
#       prelimCriteria = nSeq & densityCheck
#       prelimCriteria[is.na(prelimCriteria)]=F
#       for (j in 1:length(seqList)) {
#         if (prelimCriteria[j]) {
#           if(clusterDensity[j]>clusterDensity[i]) {
#             minCluster = min(cluster[i],cluster[j])
#             cluster[cluster==cluster[i]]=minCluster
#             cluster[cluster==cluster[j]]=minCluster
#             #maxClusterDensity = max(clusterDensity[i],clusterDensity[j])
#             clusterDensity[cluster==minCluster] = clusterDensity[j]
#             cluster_change = cluster_change + 1
#           }
#         }
#       }
#     }
#   }
#
#   # cluster_unique = unique(cluster)
#   # for(cl in 1:length(cluster_unique)) {
#   #   cluster[cluster==cluster_unique[cl]] = cl
#   # }
#
#   res = data.frame("ID" = id, "Density" = round(densityArray,2), "Cluster" = cluster, "ClusterDensity" = round(clusterDensity,2))
#   return(res)
# }
#


#' cluster_knn
#'
#' Performs the knn clustering algorithm and returns clustering info
#'
#' @param sequences A list of sequences
#' @param k number of nearest neighbours to look at
#' @param id Id of every sequence in the list
#' @export
#'
#' @return Returns the id, cluster, cluster_density and the sequence_density of each sequence
#'
#' @examples cluster_knn(seqList,k,id)

cluster_knn <- function(sequences, k, id) {

  library(magrittr)
  library(dplyr)

  message("------------Clustering------------")
  message("Calculating distance matrix...")
  distance_matrix <- calculate_distance_bw_sequences_cpp(sequences)
  density_info <- calculate_DensityInfo(distance_matrix,k)
  (density_array <- get_DensityArray(density_info))

  #step 1 - initialize every *unique* sequence as a cluster
  message("Initializing clusters...")
  cluster <- 1:length(sequences)
  cluster_density <- density_array
  (cluster_info <- tibble::tibble(id,density_array,cluster,cluster_density))
  for(i in 1:length(sequences)) {
    distances <- distance_matrix[i,]
    cluster_info[distances==0,]$cluster = min(cluster_info[distances==0,]$cluster)
  }

  #step 2 - clustering based on criteria
  message("Clustering based on density...")
  for(i in 1:length(sequences)) {
    (current_cluster <- cluster_info$cluster[i])
    (cluster_to_merge <- as.integer(cluster_info %>%
                                      rename(id_new = id,density_array_new = density_array) %>%
                                        mutate(distances = distance_matrix[i,]) %>%
                                          filter(density_info[[i]]$NearestSequences) %>%
                                            filter(density_array_new > density_array[i]) %>%
                                              filter(cluster != current_cluster) %>%
                                                arrange(distances) %>%
                                                  slice(1) %>% select(cluster)))

    if(!is.na(cluster_to_merge)) {
      cluster_info[cluster_info$cluster == current_cluster,]$cluster <- cluster_to_merge
      cluster_info[cluster_info$cluster == cluster_to_merge,]$cluster_density <- max(cluster_info[cluster_info$cluster == cluster_to_merge,]$cluster_density)
    }
  }

  # print(length(unique(cluster_info$cluster)))
  # print(table(cluster_info$cluster))
  #
  #step 3
  message("Resolving ties...")
  for(i in 1:length(sequences)) {
    (current_cluster <- cluster_info$cluster[i])
    (current_cluster_density <- cluster_info$cluster_density[i])
    current_sequence_density <- cluster_info$density_array[i]
    nearest_neighbours <- cluster_info %>%
                                rename(id_new = id,density_array_new = density_array) %>%
                                  filter(density_info[[i]]$NearestSequences) %>%
                                    filter(cluster != current_cluster)
    neighbour_density_check <- nearest_neighbours %>% filter(density_array_new > current_sequence_density) %>% nrow()

    if(!neighbour_density_check) {
      cluster_to_merge <- as.integer(nearest_neighbours %>%
                            filter(near(density_array_new,current_sequence_density,0.1)) %>%
                              filter(cluster_density > current_cluster_density) %>%
                                  arrange(desc(cluster_density)) %>% slice(1) %>%
                                    select(cluster))
      #filter(near(density_array_new,density_array[i],0.001)) %>%
      if(!is.na(cluster_to_merge)) {
        cluster_info[cluster_info$cluster == current_cluster,]$cluster <- cluster_to_merge
        cluster_info[cluster_info$cluster == cluster_to_merge,]$cluster_density <- max(cluster_info[cluster_info$cluster == cluster_to_merge,]$cluster_density)
      }
    }
  }

  cluster2 <- cluster <- cluster_info$cluster
  cluster_unique = unique(cluster)
  for(cl in 1:length(cluster_unique)) {
    cluster2[cluster==cluster_unique[cl]] = cl
  }
  cluster_info$cluster <- cluster2

  res = data.frame("ID" = cluster_info$id , "Density" = cluster_info$density_array, "Cluster" = cluster_info$cluster, "ClusterDensity" = round(cluster_info$cluster_density,2))
  message("----------Done Clustering----------")
  return(res)
}

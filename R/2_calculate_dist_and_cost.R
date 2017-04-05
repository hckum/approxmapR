#' insert_blankItemset_before
#'
#' Inserts a blank space before the sequence given
#'
#' @param sequence A sequence
#'
#' @return Returns a sequence with a blank space inserted before
#'
#' @examples insert_blankItemset_before(sequence)

insert_blankItemset_before = function(sequence) {
     append(sequence,"_",0)
   }




#' insert_itemset_before
#'
#' Inserts the itemset before the given sequence
#'
#' @param itemset An itemset from a sequence
#' @param sequence A sequence
#'
#' @return Returns the sequence after inserting the itemset before the given sequence
#'
#' @examples insert_itemset_before(itemset,sequence)

insert_itemset_before = function(itemset,sequence) {
     append(sequence, itemset,0)
   }


#' insert_itemset_before_Wseq
#'
#' Inserts the specified weighted itemset before the given aligned weighted sequence
#'
#' @param weightedItemset A weighted itemset
#' @param alignedWSeq An aligned weighted sequence
#'
#' @return Returns the newly weighted sequence after inserting the specified weighted itemset before the given aligned weighted sequence
#'
#' @examples insert_itemset_before_Wseq(weighted_seq[[1]],alignedWSeq)

insert_itemset_before_Wseq = function(weightedItemset,alignedWSeq) {
  append(alignedWSeq, weightedItemset,0)
}




#' insert_blankItemset_before_Wseq
#'
#' Inserts a blank before the given weighted sequence
#'
#' @param Wseq A weighted Sequence
#'
#' @return Returns a weighted seq with a preceding underscore
#'
#' @examples insert_blankItemset_before_Wseq(weighted_seq)

insert_blankItemset_before_Wseq = function(Wseq) {
  Witemset = list(list(elements = "_",element_weights = 0, itemset_weight = 0))
  return(append(Wseq, Witemset,0))
}




#' calculate_sorenson_distance
#'
#' Calculates the sorenson distance between 2 itemsets based on the formula
#' @param itemset1 An itemset from a sequence
#' @param itemset2 An itemset from a sequence
#'
#' @return Returns the sorenson distance between 2 itemsets
#' @export
#'
#' @examples calculate_sorenson_distance(sequence1[[1]],sequence1[[2]])

calculate_sorenson_distance = function(itemset1, itemset2) {
  set1 = setdiff(itemset1,itemset2)
  set2 = setdiff(itemset2,itemset1)
  set_union = union(set1,set2)
  dist = length(set_union) / (length(itemset1) + length(itemset2))
  return(dist)
}




#' calculate_seq_soren_distance
#'
#' Calculates the sorenson distance between an itemsets in a sequence and an itemset in a weighted sequence
#'
#' @param seqItemset Itemset from a sequence
#' @param WseqItemset Itemset from a weighted sequence
#' @param n The number of sequences in the weighted sequence
#'
#' @return Retuns the sorenson distance between an itemsets in a sequence and an itemset in a weighted sequence
#' @export
#'
#' @examples calculate_seq_soren_distance(seq[[1]],Weighted_seq[[2]])

calculate_seq_soren_distance = function(seqItemset,WseqItemset,n) {
  v = WseqItemset$itemset_weight
  w = WseqItemset$element_weights
  x = WseqItemset$elements
  y = seqItemset
  y_no = length(seqItemset)
  eR = (sum(w) + (y_no * v) - (2 * sum(w[x %in% y]))) / (sum(w) + (y_no * v))
  repl = ((eR * v) + n - v) / n
  return(repl)
}




#' calculate_repl_btw_itemsets
#'
#' Calculates the replacement cost between an itemset in 2 sequences using the function supplied
#'
#' @param itemset1 Itemset from a sequence
#' @param itemset2 Itemset from a sequence
#' @param fun function to calculate the cost based on
#' @return Retuns the replacement cost between an itemset in 2 sequences using the function supplied
#' @export
#'
#' @examples calculate_repl_btw_itemsets(itemset1, itemset2, calculate_sorenson_distance)

calculate_repl_btw_itemsets = function(itemset1, itemset2, fun) {
  return(fun(itemset1,itemset2))
}



#' calculate_repl_btw_itemset_Witemset
#'
#' Calculates the replacement cost between an itemset in a sequence and an itemset in a weighted sequence using using the function supplied
#'
#' @param seqItemset Itemset from a sequence
#' @param WseqItemset Itemset from a weighted sequence
#' @param fun function to calculate the replacement cost
#' @param n the number of strings aligned in the weighted sequence
#' @return Retuns the replacement cost between an itemset in a sequence and an itemset in a weighted sequence using using the function supplied
#' @export
#'
#' @examples calculate_repl_btw_itemset_Witemset(seq[[1]],Weighted_seq[[2]],calculate_seq_soren_distance,n)

calculate_repl_btw_itemset_Witemset = function(itemset, Witemset, fun, n) {
  return(fun(itemset, Witemset,n))
}



#' calculate_indel
#'
#' Calculates the indel cost for an itemset based on the function supplied
#'
#' @param itemset Itemset from a sequence
#' @param fun function to calculate the indel cost
#' @return Retuns the indel cost for an itemset based on the function supplied
#' @export
#'
#' @examples calculate_indel(seq[[1]],calculate_seq_soren_distance)

calculate_indel = function(itemset,fun) {
  empty = c("")
  return(calculate_repl_btw_itemsets(itemset,empty,fun))
}




#' calculate_indel_wseq
#'
#' Calculates the indel cost for an itemset in a weighted sequence based on the function supplied
#'
#' @param itemset Itemset from a weighted sequence
#' @param fun function to calculate the indel cost
#' @return Retuns the indel cost for an itemset in a weighted sequence based on the function supplied
#' @export
#'
#' @examples calculate_indel(W-seq[[1]],calculate_seq_soren_distance)


calculate_indel_wseq = function(Witemset,fun) {
  empty = c("")
  return(calculate_repl_btw_itemsets(Witemset,empty,fun))
}




#' calculate_dist_btw_sequences
#'
#' Calculates the distance matrix between 2 sequences
#'
#' @param seq1 1st sequence
#' @param seq2 2nd sequence
#' @param fun function to calculate the costs
#' @return Retuns a list of the distance matrix and the distance
#' @export
#'
#' @examples calculate_dist_btw_sequences(seq[[1]],seq[[2]],calculate_sorenson_distance)

calculate_dist_btw_sequences = function(seq1, seq2,fun = calculate_sorenson_distance) {

  distance_matrix = matrix(nrow = length(seq1)+1,ncol = length(seq2)+1)

  distance_matrix[1,] = 0:length(seq2)
  distance_matrix[,1] = 0:length(seq1)

  for(i in 2:nrow(distance_matrix)) {
    for(j in 2:ncol(distance_matrix)) {
      repl = distance_matrix[i-1,j-1] + calculate_repl_btw_itemsets(seq1[[i-1]],seq2[[j-1]],fun)
      indel_r = distance_matrix[i,j-1] + calculate_indel(seq2[[j-1]],fun)
      indel_d = distance_matrix[i-1,j] + calculate_indel(seq1[[i-1]],fun)
      distance_matrix[i,j] = min(repl,indel_d,indel_r)
    }
  }

  results = list(distance_matrix = distance_matrix, distance = distance_matrix[nrow(distance_matrix),ncol(distance_matrix)])

  return(results)
}



#' calculate_dist_btw_seq_Wseq
#'
#' Calculates the distance matrix between a sequence and a weighted sequence
#'
#' @param seq A sequence
#' @param Wseq A weighted sequence
#' @param fun function to calculate the costs
#' @return Retuns a list of the distance matrix and the distance
#' @export
#'
#' @examples calculate_dist_btw_seq_Wseq(seq[[1]],w_seq[[2]],calculate_seq_soren_distance)



calculate_dist_btw_seq_Wseq = function(seq, Wseq,fun = calculate_seq_soren_distance) {

  n = Wseq$n
  Wseq$n = NULL
  distance_matrix = matrix(nrow = length(seq)+1,ncol = length(Wseq)+1)

  distance_matrix[1,] = 0:length(Wseq)
  distance_matrix[,1] = 0:length(seq)

  for(i in 2:nrow(distance_matrix)) {
    for(j in 2:ncol(distance_matrix)) {
      xxx = seq[[i-1]]
      yyy= Wseq[[j-1]]
      repl = distance_matrix[i-1,j-1] + calculate_seq_soren_distance(seqItemset = xxx, WseqItemset = yyy, n = n)
      indel_r = distance_matrix[i,j-1] + 1 #calculate_indel(seq2[[j-1]],fun,n)
      indel_d = distance_matrix[i-1,j] + 1 #calculate_indel(seq1[[i-1]],fun,n)
      distance_matrix[i,j] = min(repl,indel_d,indel_r)
    }
  }

  results = list(distance_matrix = distance_matrix, distance = distance_matrix[nrow(distance_matrix),ncol(distance_matrix)])

  return(results)
}


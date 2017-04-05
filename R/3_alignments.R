#' get_weighted_sequence
#'
#' Gets the weighted sequence when 2 alignments are passed
#'
#' @param seq1 Aligned Sequence 1
#' @param seq2 Aligned Sequence 2
#' @export
#'
#' @return Returns the weighted sequences for two alignments
#'
#' @examples get_weighted_sequence(seq1_after_aligning,seq2_after_aligning)

get_weighted_sequence = function(seq1, seq2) {

  weighted_seq = list(length(seq1))

  for(i in 1:length(seq1)) {
    all_elements = c(seq1[[i]],seq2[[i]])
    elements = union(seq1[[i]],seq2[[i]])
    elements = elements[elements != "_"]

    element_weights = numeric(length(elements))
    for(j in 1:length(elements)) {
      element_weights[j] = sum(all_elements==elements[j])
    }

    if(("_" %in% seq1[[i]])|("_" %in% seq2[[i]])) {
      itemset_weight = 1
    } else {
      itemset_weight = 2
    }
    weighted_seq[[i]] = list(elements = elements,element_weights = element_weights,itemset_weight = itemset_weight)
  }

  weighted_seq$n = 2

  return(weighted_seq)
}

#' add_seq_to_weighted_sequence
#'
#' Adds a sequence to a weighted sequence
#'
#' @param alignedSeq Aligned Sequence
#' @param alignedWSeq Aligned Weighted Sequence
#' @param n The number of sequences in the weighted sequence
#' @export
#'
#' @return Adds an aligned Sequence to a weighted sequence
#'
#' @examples add_seq_to_weighted_sequence(alignedSeq,alignedWSeq,n)

add_seq_to_weighted_sequence = function(alignedSeq,alignedWSeq,n) {

  for(i in 1:length(alignedWSeq)) {
    wItemset = alignedWSeq[[i]]
    itemset = alignedSeq[[i]]
    if("_" %in% itemset) {

      next

    } else if("_" %in% wItemset$elements) {

      alignedWSeq[[i]]$elements = unique(itemset)
      for(j in 1:length(alignedWSeq[[i]]$elements)) {
        alignedWSeq[[i]]$element_weights[j] = sum(alignedWSeq[[i]]$elements[j]==itemset)
      }
      #alignedWSeq[[i]]$element_weights =
      alignedWSeq[[i]]$itemset_weight = 1

    } else {

      all_elements = NULL
      for(j in 1:length(wItemset$elements)) {
        all_elements = c(all_elements,rep(wItemset$elements[j],wItemset$element_weights[j]))
      }
      all_elements = c(itemset,all_elements)
      unique_elements = unique(all_elements)

      alignedWSeq[[i]]$elements = unique_elements
      alignedWSeq[[i]]$element_weights = NULL
      for(j in 1:length(alignedWSeq[[i]]$elements)) {
        alignedWSeq[[i]]$element_weights[j] = sum(alignedWSeq[[i]]$elements[j]==all_elements)
      }
      alignedWSeq[[i]]$itemset_weight = wItemset$itemset_weight + 1
    }
  }

  alignedWSeq$n = n + 1
  return(alignedWSeq)
}


#' align_two_sequences
#'
#' Performs pairwise alignment between 2 sequences
#'
#' @param seq1 Aligned Sequence
#' @param seq2 Aligned Weighted Sequence
#' @param fun The function to calcuate the replacement sequence
#' @export
#'
#' @return Returns the weighted sequence of 2 sequences after performing pairwise alignment
#'
#' @examples align_two_sequences(seq1, seq2, calculate_sorenson_distance)

align_two_sequences = function(seq1, seq2, fun = calculate_sorenson_distance) {

  distance_matrix = calculate_dist_btw_sequences(seq1, seq2, fun)$distance_matrix
  alignedSeq1 = list()
  alignedSeq2 = list()

  i = length(seq1)+1
  j = length(seq2)+1



  backtrack = function(i,j,alignedSeq1,alignedSeq2,operations = "") {
    if((i==1) & (j==1))
    {
      # operations = rev(operations)
      # operations = operations[1:(length(operations)-1)]
      # opNo = 1:length(operations)
      # operations =  paste(operations[operations!="match"],"at position",opNo[operations!="match"])
      result = list(alignedSeq1=alignedSeq1, alignedSeq2=alignedSeq2, operations = operations)
      return(result)
    }

    if((i>1) & (j>1)) {
      #is left plus cost?
      if (distance_matrix[i,j]==distance_matrix[i,j-1] + 1)  {
        #left
        alignedSeq1 = insert_blankItemset_before(alignedSeq1)
        alignedSeq2 = insert_itemset_before(seq2[j-1][1],alignedSeq2)
        #operations[length(operations)+1] = "indel"
        backtrack(i,j-1,alignedSeq1,alignedSeq2,operations)
      } else if (distance_matrix[i,j]==distance_matrix[i-1,j]+1) {
        #is up plus cost
        alignedSeq2 = insert_blankItemset_before(alignedSeq2)
        alignedSeq1 = insert_itemset_before(seq1[i-1][1],alignedSeq1)
        #operations[length(operations)+1] = "indel"
        backtrack(i-1,j,alignedSeq1,alignedSeq2,operations)

      } else {
        #diag
        alignedSeq1 = insert_itemset_before(seq1[i-1][1],alignedSeq1)
        alignedSeq2 = insert_itemset_before(seq2[j-1][1],alignedSeq2)
        # if(distance_matrix[i,j] != distance_matrix[i-1,j-1]) {
        #   operations[length(operations)+1] = "repl"
        # } else {
        #   operations[length(operations)+1] = "match"
        # }
        backtrack(i-1,j-1,alignedSeq1,alignedSeq2,operations)
      }
    } else if ((i==1) & (j>1)) {
      alignedSeq1 = insert_blankItemset_before(alignedSeq1)
      alignedSeq2 = insert_itemset_before(seq2[j-1][1],alignedSeq2)
      #operations[length(operations)+1] = "indel"
      backtrack(i,j-1,alignedSeq1,alignedSeq2,operations)
    } else if((j==1) & (i>1)) {
      alignedSeq2 = insert_blankItemset_before(alignedSeq2)
      alignedSeq1 = insert_itemset_before(seq1[i-1][1],alignedSeq1)
      #operations[length(operations)+1] = "indel"
      backtrack(i-1,j,alignedSeq1,alignedSeq2,operations)
    }
  }
  #debug(backtrack)
  alignedSeqs = backtrack(i,j,alignedSeq1,alignedSeq2)
  weighted_seq = get_weighted_sequence(alignedSeqs$alignedSeq1,alignedSeqs$alignedSeq2)
  return(weighted_seq)
}

#' align_seq_to_Wseq
#'
#' Returns the new weighted sequence after aligning a sequence to a weighted sequence
#'
#' @param seq Sequence
#' @param weightedSeq Weighted Sequence
#' @param fun The function to calculate the replacements costs based on
#' @export
#'
#' @return Returns the new weighted sequence after aligning a sequence to a weighted sequence
#'
#' @examples align_two_sequences(seq1, seq2, calculate_sorenson_distance)

align_seq_to_Wseq = function(seq,weightedSeq,fun = calculate_seq_soren_distance()) {

  distance_matrix = calculate_dist_btw_seq_Wseq(seq, weightedSeq, fun)$distance_matrix
  n = weightedSeq$n
  weightedSeq$n = NULL
  alignedSeq = list()
  alignedWSeq = list()

  i = length(seq)+1
  j = length(weightedSeq)+1

  backtrack = function(i,j,alignedSeq,alignedWSeq,operations = "") {
    if((i==1) & (j==1))
    {
      # operations = rev(operations)
      # operations = operations[1:(length(operations)-1)]
      # opNo = 1:length(operations)
      # operations =  paste(operations[operations!="match"],"at position",opNo[operations!="match"])
      result = list(alignedSeq=alignedSeq, alignedWSeq=alignedWSeq, operations = operations)
      return(result)
    }

    if((i>1) & (j>1)) {
      h =  calculate_seq_soren_distance(seq[i-1],weightedSeq[[j-1]],n)
      if (distance_matrix[i,j]== (distance_matrix[i-1,j-1] + h)) {
        alignedSeq = insert_itemset_before(seq[i-1][1],alignedSeq)
        alignedWSeq = insert_itemset_before_Wseq(weightedSeq[j-1][1],alignedWSeq)
        backtrack(i-1,j-1,alignedSeq,alignedWSeq,operations)
      } else if (distance_matrix[i,j]==distance_matrix[i,j-1] + 1)  {
        #is left plus cost?
        alignedSeq = insert_blankItemset_before(alignedSeq)
        alignedWSeq = insert_itemset_before_Wseq(weightedSeq[j-1][1],alignedWSeq)
        #operations[length(operations)+1] = "indel"
        backtrack(i,j-1,alignedSeq,alignedWSeq,operations)
      } else if (distance_matrix[i,j]==distance_matrix[i-1,j]+1) {
        #is up plus cost
        alignedWSeq = insert_blankItemset_before_Wseq(alignedWSeq)
        alignedSeq = insert_itemset_before(seq[i-1][1],alignedSeq)
        #operations[length(operations)+1] = "indel"
        backtrack(i-1,j,alignedSeq,alignedWSeq,operations)

      } else {
        #diag
        alignedSeq = insert_itemset_before(seq[i-1][1],alignedSeq)
        alignedWSeq = insert_itemset_before_Wseq(weightedSeq[j-1][1],alignedWSeq)
        backtrack(i-1,j-1,alignedSeq,alignedWSeq,operations)
      }
    } else if ((i==1) & (j>1)) {
      alignedSeq = insert_blankItemset_before(alignedSeq)
      alignedWSeq = insert_itemset_before_Wseq(weightedSeq[j-1][1],alignedWSeq)
      #operations[length(operations)+1] = "indel"
      backtrack(i,j-1,alignedSeq,alignedWSeq,operations)
    } else if((j==1) & (i>1)) {
      alignedWSeq = insert_blankItemset_before_Wseq(alignedWSeq)
      alignedSeq = insert_itemset_before(seq[i-1][1],alignedSeq)
      #operations[length(operations)+1] = "indel"
      backtrack(i-1,j,alignedSeq,alignedWSeq,operations)
    }
  }
  #debug(backtrack)
  alignedSeqs = backtrack(i,j,alignedSeq,alignedWSeq)
  weighted_seq = add_seq_to_weighted_sequence(alignedSeqs$alignedSeq,alignedSeqs$alignedWSeq,n)
  return(weighted_seq)
}


#' align_multiple_sequences
#'
#' Aligns multiple sequences making use of the align functions
#'
#' @param seqList A list of sequences
#' @export
#'
#' @return Returns the weighted sequence after aligning all sequences in the list
#'
#' @examples align_multiple_sequences(seqList)

align_multiple_sequences = function(seqList) {


  if(length(seqList)==1) {
    sequence = seqList[[1]]
    Walign = vector(mode="list", length(sequence))
    for(i in 1:length(sequence)) {
      Walign[[i]]$elements = sequence[[i]]
      Walign[[i]]$itemset_weight = 1
      Walign[[i]]$element_weights = rep(1,length(sequence[[i]]))
    }
    Walign$n = 1
  } else {

    Walign = align_two_sequences(seqList[[1]],seqList[[2]])

    if(length(seqList)>2) {
      for(i in 3:length(seqList)) {
        Walign =  align_seq_to_Wseq(seqList[[i]],Walign)
      }
    }
  }

  return(Walign)
}


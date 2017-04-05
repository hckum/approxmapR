#' get_Itemset_Formatted
#'
#' Formats an weighted itemset for displaying on the console
#'
#' @param W_itemset A itemset from a weighted sequence
#' @param add_itemset_weight A boolean specifying whether or not to add the itemset weight
#'
#' @return Returns the formatted itemset as a string
#'
#' @examples get_Itemset_Formatted(W_itemset, add_itemset_weight = T)

get_Itemset_Formatted = function(W_itemset, add_itemset_weight = T) {
  with_weights = paste(W_itemset$elements,W_itemset$element_weights,sep = " : ")
  collapsed = paste(with_weights,collapse = ", ")
  result = ifelse(add_itemset_weight,paste("( ",collapsed," ) : ",W_itemset$itemset_weight,sep=""),paste("(",collapsed,")",sep=""))
  return(result)
}




#' get_C_Itemset_Formatted
#'
#' Formats an consensus itemset for displaying on the console
#'
#' @param C_itemset An itemset from the consensus pattern
#' @param no_white_space A boolean for specifying whether or not the output should be spaced
#'
#' @return Returns the formatted itemset as a string
#'
#' @examples get_C_Itemset_Formatted(C_itemset, no_white_space = T)

get_C_Itemset_Formatted = function(C_itemset, no_white_space = T) {

  collapsed = paste(C_itemset,collapse = " , ")
  result = paste("( ",collapsed," ) ")
  if(no_white_space) result =  gsub(" ","",result)
  return(result)
}





#' get_consensus_formatted
#'
#' Formats a consensus pattern for displaying on the console
#'
#' @param consensus_pattern A consensus pattern as a list
#' @param no_space_bw_Items A boolean for specifying whether or not the output should have space between the items
#' @param no_space_bw_Itemsets A boolean for specifying whether or not the output should have space between the itemsets
#' @return Returns the formatted consensus pattern as a string
#' @export
#' @examples get_consensus_formatted(consensus_pattern, no_space_bw_Items = T, no_space_bw_Itemsets = T)

get_consensus_formatted = function(consensus_pattern, no_space_bw_Items = T, no_space_bw_Itemsets = T) {
  #pattern_list = consensus_pattern[[2]]
  consensus_pattern$n = NULL
  consensus_pattern = lapply(consensus_pattern, function(x) x$elements)
  itemsets_formatted = lapply(consensus_pattern, get_C_Itemset_Formatted, no_space_bw_Items)
  result = character()
  if(no_space_bw_Itemsets) {
    result = paste(itemsets_formatted, collapse = "")
    result = paste("(",result,")",sep="")
  } else {
    result = paste(itemsets_formatted, collapse = " ")
    result = paste("( ",result," )",sep="")
  }
  return(result)
}





#' get_Wseq_Formatted
#'
#' Formats a weighted sequence for displaying on the console
#'
#' @param W_seq A consensus pattern as a list
#' @param add_itemset_weight A boolean for specifying whether or not the output should have itemset weights
#' @param no_white_space A boolean for specifying whether or not the output should be spaced
#' @return Returns the formatted sequence as a string
#' @export
#' @examples get_Wseq_Formatted(W_seq, add_itemset_weight = T, no_white_space = T)

get_Wseq_Formatted = function(W_seq, add_itemset_weight = T, no_white_space=T) {
  n = W_seq$n
  W_seq$n = NULL
  formatted_itemsets = lapply(W_seq,get_Itemset_Formatted, add_itemset_weight)
  formatted_itemsets = paste(formatted_itemsets, collapse = " ")
  result = paste("< ", formatted_itemsets," > : " , n,sep = "")
  if(no_white_space) result = gsub(" ","",result)
  return(result)
}



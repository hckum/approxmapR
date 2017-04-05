#' format_output
#'
#' @export


format_output = function(approxmap_obj) {
  clusters = approxmap_obj$clusters
  form_cons = approxmap_obj$formatted_results$consensus
  form_wseq = approxmap_obj$formatted_results$weighted_seq

  for(i in 1:length(clusters)) {

    cat(paste("Cluster ",i,":",sep = ""),"\n","Sequence IDs: ", clusters[[i]], "\n", "Weighted Sequence: ", form_wseq[[i]], "\n", "Consensus Pattern: ", form_cons[[i]],"\n\n")
  }

}


#' extract_freq
#'
#' @export

extract_freq = function(weighted_seq) {
  weighted_seq$n = NULL
  elements = unlist(lapply(weighted_seq, function(x) x$elements))
  element_weights = unlist(lapply(weighted_seq, function(x) x$element_weights))
  return(data.frame(elements = elements, element_weights = element_weights))
}


#' plot_frequency
#'
#' @export

plot_frequency = function(weighted_seq, cons_threshhold =0.5, noise_threshold = 0, variation_threshold = 0.2) {
  #n_thresh = threshhold * (length(weighted_seq)-1)
  n_thresh = cons_threshhold * weighted_seq$n
  v_thresh = variation_threshold * weighted_seq$n
  fq = extract_freq(weighted_seq)
  freq_plot <- fq %>% dplyr::mutate(element_number = 1:nrow(fq)) %>%  dplyr::filter(element_weights > noise_threshold) %>%
    ggplot2::ggplot(aes(x = element_number, y = element_weights, text = elements)) +
      ggplot2::geom_point(size = 0.75) +
        ggplot2::geom_path(group = 1, size=0.1) +
          ggplot2::geom_hline(yintercept = n_thresh, linetype = 2) +
            ggplot2::geom_hline(yintercept = v_thresh, linetype = 4) +
              ggplot2::theme(legend.position="none") +
                ggplot2::geom_label(aes(label = elements,size = element_weights))
  return(freq_plot)
}



#' get_Itemset_Formatted_HTML
#'
#' @export

get_Itemset_Formatted_HTML = function(W_itemset_html, add_itemset_weight = T) {
  with_weights = paste(W_itemset_html$elements,W_itemset_html$element_weights,sep = " : ")
  with_tags = paste0(W_itemset_html$start_tag,with_weights,W_itemset_html$end_tag)
  collapsed = paste(with_tags,collapse = ", ")
  result = ifelse(add_itemset_weight,paste("( ",collapsed," ) : ",W_itemset_html$itemset_weight,sep=""),paste("(",collapsed,")",sep=""))
  return(result)
}



#' get_Wseq_Formatted_HTML
#'
#' @export

get_Wseq_Formatted_HTML = function(W_seq, add_itemset_weight = T, no_white_space=T) {
  n = W_seq$n
  W_seq$n = NULL
  W_seq_html = get_tagged_itemsets_from_wseq(W_seq)
  formatted_itemsets = lapply(W_seq_html,get_Itemset_Formatted_HTML, add_itemset_weight)
  formatted_itemsets = paste(formatted_itemsets, collapse = " ")
  result = paste("< ", formatted_itemsets," > : " , n,sep = "")
  if(no_white_space) result = gsub(" ","",result)
  return(result)
}


#' get_tagged_itemsets_from_wseq
#'
#' @export

get_tagged_itemsets_from_wseq = function(wseq) {
  wseq$n = NULL
  weights = unlist(lapply(wseq, function(x) x$element_weights))
  range = range(weights)
  if(range[1] != range[2]){
    block_size = diff(range)/5
    blocks = seq(range[1],range[2],by = block_size)
    wseq = lapply(wseq, function(w_itemset) {
      block_no = cut(w_itemset$element_weights,breaks = blocks, labels = F, include.lowest = T)
      w_itemset$start_tag = paste0("<priority",block_no,">")
      w_itemset$end_tag = paste0("</priority",block_no,">")
      return(w_itemset)
    })
  } else {
    wseq = lapply(wseq, function(w_itemset) {
      w_itemset$start_tag = rep("<priority5>",length(w_itemset$elements))
      w_itemset$end_tag = rep("</priority5>",length(w_itemset$elements))
      return(w_itemset)
    })
  }

  return(wseq)
}

#' tag_items
#'
#' @export

tag_items = function(itemset_with_tags) {
  itemset_with_tags$elements = paste0(itemset_with_tags$start_tag,itemset_with_tags$elements,itemset_with_tags$end_tag)
  return(itemset_with_tags)
}


#' get_consensus_formatted_HTML_tablerow
#'
#' @export

get_consensus_formatted_HTML_tablerow <- function(weighted_seq, strength,blank_if_absent = F, pattern = "Consensus") {
  n = weighted_seq$n
  weighted_seq$n = NULL
  W_seq_html = get_tagged_itemsets_from_wseq(weighted_seq)
  tagged_items_wseq <- lapply(W_seq_html, tag_items)
  tagged_items_wseq$n = n
  cons_tagged = get_consensus_pattern(tagged_items_wseq,strength,blank_if_absent)
  cons_tagged$n = NULL
  consensus_pattern = unlist(lapply(cons_tagged, function(items) {
      items = items$elements
      item = paste(items,collapse = ",")
      if(item != "") item = paste0("(",item,")")
      paste0("<td>",item,"</td>")
    }))
  consensus_pattern = paste0(consensus_pattern,collapse = "")
  consensus_pattern = paste0("<tr><td width = 35%>",pattern,":</td>",consensus_pattern,"</tr>")
  return(consensus_pattern)
}

#' get_cons_var_table
#'
#' @export

get_cons_var_table <- function (weighted_seq, var_cutoff, cons_cutoff) {
  var_table = get_consensus_formatted_HTML_tablerow(weighted_seq,var_cutoff,blank_if_absent = F,pattern = "Variation")
  var_pat = get_consensus_pattern(weighted_seq,var_cutoff,blank_if_absent = F)
  cons_table = get_consensus_formatted_HTML_tablerow(var_pat,cons_cutoff,blank_if_absent = T)
  table = paste0("<table>",cons_table,var_table,"</table>")
  return(table)
}





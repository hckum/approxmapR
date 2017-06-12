#' get_consensus_pattern
#'
#' Gets the consensus pattern from the weighted sequence
#'
#' @param weighted_seq A weighted sequence
#' @param strength The cutoff to extract the consensus pattern from the weighted sequence
#' @export
#'
#' @return Returns the consensus pattern if you give the weighted sequence
#'
#' @examples get_consensus_pattern(weighted_seq, 0.4)


get_consensus_pattern = function(weighted_seq, strength, blank_if_absent = F) {

  n = weighted_seq$n
  weighted_seq$n = NULL
  min_occurences = n * strength
  consensus_pattern = list()

  for(i in 1:length(weighted_seq)) {
    itemset = weighted_seq[[i]]
    strength_test = itemset$element_weights > min_occurences
    elements = (itemset$elements[strength_test])
    element_weights = itemset$element_weights[strength_test]
    #consensus_pattern = append(consensus_pattern,elements,i-1)
    if(length(elements)>0) consensus_pattern[[length(consensus_pattern)+1]] = list(elements = elements, element_weights = element_weights)
    if(blank_if_absent) {
      if(length(elements) == 0) consensus_pattern[[length(consensus_pattern)+1]] = list(elements = "", element_weights = NULL)
    }
  }

  consensus_pattern$n = n

  return(consensus_pattern)
}



#' get_approxMap
#'
#' Does all the steps in approxmap algorithm
#'
#' @param seqList A list of sequences
#' @param k The number of nearest neighbours to look at
#' @param strength The cutoff to extract the consensus pattern from the weighted sequence
#' @export
#'
#' @return Clusters and gives the consensus pattern from the sequence list
#'
#' @examples get_approxMap(sequences_list, 2, 0.4)

get_approxMap = function(seqList,k,strength, id = 1) {

  if(id==1) id = 1:length(seqList)

  cluster_info = cluster_knn(seqList,k,id)
  clusters = cluster_info$Cluster
  consensus_patterns = list()
  cluster_ids = list()
  weighted_seqs = list()

  message("Aligning sequences in each cluster...")
  for(i in 1:length(unique(clusters))) {
    current_cluster = clusters == unique(clusters)[i]
    cluster_ids[[i]] = id[current_cluster]
    current_density = cluster_info$Density[current_cluster]
    current_seqs = seqList[current_cluster]
    current_seqs = current_seqs[order(-current_density)]
    weighted_seqs[[i]] = align_multiple_sequences(current_seqs)
    consensus_patterns[[i]] = get_consensus_pattern(weighted_seqs[[i]],strength)
    #cluster_pattern = list(ID = current_id, consensus_pattern = consensus_pattern,weighted_alignment = weighted_alignment)
    #consensus_patterns[[i]] = cluster_pattern
  }

  message("Getting consensus patterns...")
  formatted_con_pat = lapply(consensus_patterns, get_consensus_formatted)
  formatted_weighted_seqs = lapply(weighted_seqs, get_Wseq_Formatted)
  formatted_results = list(weighted_seq = formatted_weighted_seqs, consensus = formatted_con_pat)
  results = list(clusters = cluster_ids, weighted_seqs = weighted_seqs, consensus_patterns = consensus_patterns, formatted_results = formatted_results)
  return(results)
}

#' swap
#'
#' @export

swap= function (x, y)
{
  x.sub <- substitute(x)
  y.sub <- substitute(y)
  x.val <- x
  e <- parent.frame()
  do.call("<-", list(x.sub, y.sub), envir = e)
  do.call("<-", list(y.sub, x.val), envir = e)
}

#' approxmap
#'
#' @export

approxmap = function(file, k, cons_cutoff = 0.5, var_thresh = 0.2, noise_thresh = 0, period1 = "1 Week", st.date = "Mon", results_directory = "~"){

  data = read.csv(file, header=T)
  if(!all(is.na(as.Date(as.character(data[1,2]),format="%m/%d/%Y")))) {

    colnames(data) <- c("ID", "Date", "Event")
    unique_items = nrow(data.frame(data %>% select(Event) %>%
                                     unique()))


    inp <- ord_date(data, get.pd(period1), get.st.date(st.date))

    # change col names
    colnames(inp) <- c("ID", "Period", "Event")
    suppressMessages( inp <- inp %>% group_by(ID, Period) %>%
                        select(Event) %>% unique() )
    inp <- na.omit(inp)

    message(noquote(sprintf("\n")))
    message(noquote(sprintf("\nSummary Statistics for the data is as follows:")))
    message(noquote(sprintf("\n")))
    # check what is getting displayed

    # noquote(sprintf("Number of Unique Items: %i", nrow(unique_items)))
    # message(noquote(sprintf("\nNumbers of Unique Items: %i", nrow(unique_items))))
    print(noquote(sprintf("Numbers of Unique Items: %i", unique_items)))
    # nrow(unique_items)

  } else {
    inp <- data
    colnames(inp) <- c("ID", "Period", "Event")
    unique_items = nrow(data.frame(inp %>% select(Event) %>% unique()))

    message(noquote(sprintf("\n")))
    message(noquote(sprintf("Summary Statistics for the data is as follows:")))
    # noquote(sprintf("Numbers of Unique Items: %i", nrow(unique_items)))
    message(noquote(sprintf("\n")))
    print(noquote(sprintf("Numbers of Unique Items: %i", unique_items)))

    suppressMessages( inp <- inp %>% group_by(ID, Period) %>% select(Event) %>% unique())
    inp <- na.omit(inp)
  }

  file_name <- strsplit(basename(file), split = "\\.")[[1]][1]
  param_string <- paste0(file_name,"_k",k)
  results_directory = paste0(results_directory,"/approxmap_results/",param_string,"/")

  aggregated_data <- inp

  ###### check names


  tab1 = inp %>% group_by(ID) %>%
                      select(Period) %>%
                      unique()  %>%
                      summarize(count=n())
  tab2 = tab1[,2]

  cat(sprintf("\n[2] Statistics for Number of Sets per Sequence:\n
              Mean: %f \n
              Std. Dev.: %f \n
              Max.: %f \n
              Min.: %f \n
              Quantiles: \n",
              mean(tab2[[1]]), sd(tab2[[1]]), max(tab2[[1]]), min(tab2[[1]]) ))
  print(noquote( c("         ", quantile(tab2[[1]]) )))

  # check names
  tab3 = inp %>% group_by(ID, Period) %>% summarize(Items=n())
  tab4 = tab3[,3]


  cat(sprintf("\n[3] Statistics for Number of Items per Set:\n
              Mean: %f \n
              Std. Dev.: %f \n
              Max.: %f \n
              Min.: %f \n
              Quantiles: \n",
              mean(tab4[[1]]), sd(tab4[[1]]), max(tab4[[1]]), min(tab4[[1]]) ))
  print(noquote( c("         ", quantile(tab4[[1]]) )))


  cat(noquote("\n\n"))





  inp = cvt_seq(inp)
  results = get_approxMap(inp, k, cons_cutoff)


  frmt_wt_seq_html <- lapply(results$weighted_seqs
                             , get_Wseq_Formatted_HTML)
  frmt_wt_seq_html <- strsplit(unlist(frmt_wt_seq_html), " |>:")

  for (i in 1:length(frmt_wt_seq_html)){
    swap(frmt_wt_seq_html[[i]][1],frmt_wt_seq_html[[i]][2])
  }

  frmt_wt_seq_html <- strsplit(unlist(frmt_wt_seq_html), "\\(")

  for (i in seq(2,length(frmt_wt_seq_html),2)){
    frmt_wt_seq_html[[i]][1] = " "
  }

  seq_length = matrix(0, length(frmt_wt_seq_html)/2, 1)
  for (i in seq(2,length(frmt_wt_seq_html),2) ){
    seq_length[i/2]   =  length(frmt_wt_seq_html[[i]])
    max_seq_length = max(seq_length)
  }

  w_seq_table = matrix("()",length(frmt_wt_seq_html)/2
                       , max_seq_length)

  for (i in seq(2,length(frmt_wt_seq_html),2)){
    for (k in 1:length((frmt_wt_seq_html[[i]]))){
      w_seq_table[i/2,k] = paste(c("(",frmt_wt_seq_html[[i]][k])
                                 ,collapse = "")
      w_seq_table[i/2,1] = frmt_wt_seq_html[[i-1]][1]
    }
  }

  tempReport4 <- system.file("rmd/markdown_weighted_seq.Rmd", package="approxmapR")
  params4 <- list(input = w_seq_table
                  , get_title = "Weighted Sequences")
  wseq_file_name <- paste0(param_string,"_wseq.html")
  rmarkdown::render(tempReport4, quiet = TRUE
                    , output_file = wseq_file_name
                    , params = params4
                    , envir = new.env(parent = globalenv()), output_dir = results_directory)

  data_file_name <- paste0(results_directory,file_name,"_aggregated.csv")
  write.csv(aggregated_data, data_file_name, row.names = F)

  tempReport3 <- system.file("rmd/markdown_plot.Rmd", package="approxmapR")
  plot_file_name <- paste0(param_string,"_freq_plots.html")
  params3 <- list(w_seq= results$weighted_seqs,
                  n_cutoff= 0.4, cons_cutoff= cons_cutoff,
                  noise_thresh = noise_thresh,
                  var_thresh = var_thresh,
                  get_title = "Plot")
  rmarkdown::render(tempReport3, quiet = TRUE
                    , output_file = plot_file_name
                    , params = params3
                    , envir = new.env(parent = globalenv()), output_dir = results_directory)



  frmt_cons_html <- lapply(results$consensus_patterns
                           , get_consensus_formatted_HTML_tablerow
                           , cons_cutoff)

  cons_var_table = matrix(0,2*length(frmt_cons_html),1)

  for ( i in seq(1,length(frmt_cons_html)) ){
    cons_var_table[2*(i-1)+1,1] <- paste(c("Cluster no.", i)
                                         , collapse = "")

    cons_var_table[2*(i-1)+2,1] <- get_cons_var_table(results$weighted_seqs[[i]]
                                                      , var_thresh, cons_cutoff)
  }

  tempReport2 <- system.file("rmd/markdown_consensus_pat.Rmd", package="approxmapR")
  patterns_file_name <- paste0(param_string,"_cons_var_patterns.html")
  params2 <- list(input = cons_var_table
                  , get_title = "Patterns")
  rmarkdown::render(tempReport2, quiet = TRUE,
                    output_file = patterns_file_name,
                    params = params2,
                    envir = new.env(parent = globalenv()), output_dir = results_directory)

  txt_file <- lapply(results$weighted_seqs, function(x) {
    cons_pat <- get_consensus_pattern(x,cons_cutoff)
    cons_pat <- get_consensus_formatted(cons_pat)
    var_pat <- get_consensus_pattern(x,var_thresh)
    var_pat <- get_consensus_formatted(var_pat)
    return(list(cons_pat,var_pat))
  })

  sink(paste0(results_directory,param_string,"_cons_var_patterns.txt"))
  print(txt_file)
  sink()

  message(paste("Results printed out to ",results_directory))
  return(invisible(results))

}

Getting Started With ApproxmapR
================
Gurudev Ilangovan
2017-11-20

Approxmap is an algorithm used for exploratory data analysis of sequential data. When we have longitudinal data and we want to find out the underlying patterns, we use approxmap. `approxmapR` aims to provide a consistent and tidy api for using the algorithm in R. This vignette aims to demonstrate the basic workflow of approxmapR.

Installation is simple.

    install.packages("devtools")
    devtools::install_github("ilangurudev/approxmapR")

Setting Up
==========

To load the package we use,

``` r
library(approxmapR)
```

Though, it is not required, we strongly encourage you to use the `tidyverse` package. The `approxmapR` was designed with the same paradigm and hence works cohesively with `tidyverse`. To install and load,

    install.packages("tidyverse")

To load,

``` r
library(tidyverse)
```

    ## -- Attaching packages ----------------------------------------- tidyverse 1.2.1 --

    ## v ggplot2 2.2.1     v purrr   0.2.4
    ## v tibble  1.3.4     v dplyr   0.7.4
    ## v tidyr   0.7.2     v stringr 1.2.0
    ## v readr   1.1.1     v forcats 0.2.0

    ## -- Conflicts -------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

Motivation
==========

Now that we have loaded and installed everything we need, let's jump into the problem and motivation for using approxmap. Let's say you have a dataset that looks like this:

    ## # A tibble: 51,264 x 3
    ##       id     period       event
    ##    <int>      <chr>       <chr>
    ##  1     1 1993-07-01    training
    ##  2     2 1993-07-01 joblessness
    ##  3     3 1993-07-01 joblessness
    ##  4     4 1993-07-01    training
    ##  5     5 1993-07-01 joblessness
    ##  6     6 1993-07-01 joblessness
    ##  7     7 1993-07-01 joblessness
    ##  8     8 1993-07-01  employment
    ##  9     9 1993-07-01 joblessness
    ## 10    10 1993-07-01  employment
    ## # ... with 51,254 more rows

The base data is from the package `TraMineR` (it has been tweaked a little for our problem) and provides the employment status of 712 individuals for each month from 1993 to 1998. Now you are interested in answering the question, "**What are the general sequences of employment that people go through?**". Since there are `n_people` people, it is not possible for us to decipher what the patterns are by visual inspection. Looking for exact sequences that are present in *all* these people will get us nowhere.

So we need to methodically formulate the solution. Let's look at some key terms necessary for doing that:

1.  **Item**: An item is an event that belongs to an ID. We are actually interested in seeing what the pattern of the items are.
2.  **Itemset**: An itemset is a collection of items within which the order of items doesn't matter. A typical example of an itemset is *bread and butter*. Itemsets are used when the aggregation (more on that later) is typically such that it includes several items.
3.  **Sequence**: A sequence is an ordered collection of itemsets. This means that the way they are ordered matters.
4.  **Weighted Sequence**: Several sequences put together after multiple alignment. Multiple alignment is beyond the scope of this vignette but please check [this small example](https://en.wikipedia.org/wiki/Levenshtein_distance) to get an intuition.

Hence, every person (id) in our dataset represents a sequence. We therefore have 712 sequences. Though we can extract a general pattern from all these sequences, a better idea would be to

1.  Group similar sequences into clusters
2.  Create a weighted sequence for each cluster
3.  Extract the items that are present in a position a specified percent of the time.

The above steps are the crux of the approxmap algorithm.

Workflow
========

Let us go through the sequence of steps involved in analyzing the mvad dataset using approxmap. Please note that this version of approxmap only supports unique items within an itemset.

General Instructions
--------------------

1.  Any time you need more information on a particular function, you could, as always, use `?function_name` to get detailed help.
2.  The package uses the `%>%` operator (Ctrl/Cmd + Shift + M). This means you can move from one function to another seamlessly.

1. Aggregate the data
---------------------

For the algorithm to create sequences, it needs data in the form:

    ## # A tibble: 350 x 3
    ##       id period event
    ##    <int>  <int> <int>
    ##  1     1      1    63
    ##  2     1      2    20
    ##  3     1      2    22
    ##  4     1      2    23
    ##  5     1      2    50
    ##  6     1      2    66
    ##  7     1      2    96
    ##  8     1      3    16
    ##  9     1      3    50
    ## 10     1      4    51
    ## # ... with 340 more rows

So basically we need to aggregate the dataset i.e. go from dates to aggregations (called as period) in the package. For this we use the `aggregate_sequences()` function. The aggregate sequences takes in a number of parameters. `format` is used to specify the date format, `unit` is used to specify the unit of aggregation - day, week, month and so on, `n_units` is used to specify the number of units to aggregate. So if unit is "week" and n\_units is 4, 4 weeks becomes the unit of aggregation. For more information please refer to the function documentation.

The function also displays some useful statistics about the sequences.

``` r
mvad %>%
  aggregate_sequences(format = "%Y-%m-%d", unit = "month", n_units = 1)
```

    ## Generating summary statistics of aggregated data...

    ## The number of unique items is 6
    ## 
    ## Statistics for the number of sets per sequence:
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   1.000   2.000   3.000   2.751   3.000   5.000 
    ## 
    ## Statistics for the number of items in a set
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##       1       1       1       1       1       1 
    ## 
    ## Frequencies of items
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   182.0   229.2   299.5   326.5   347.2   609.0

    ## # A tibble: 1,959 x 3
    ## # Groups:   id, event [1,959]
    ##       id period       event
    ##    <int>  <int>       <chr>
    ##  1     1      1    training
    ##  2     1      3  employment
    ##  3     2      1 joblessness
    ##  4     2      3          FE
    ##  5     2     39          HE
    ##  6     3      1 joblessness
    ##  7     3      3    training
    ##  8     3     27          FE
    ##  9     3     61  employment
    ## 10     4      1    training
    ## # ... with 1,949 more rows

`approxmapR` also allows for pre-aggregated data through the `pre_aggregated()` function. This function ensures all the right classes are aplied to the data before moving on to the next steps.

    pre_aggregated_df %>%
      pre_aggregated()

2. Cluster the data
-------------------

The next step involves one of the more computationally intensive steps of the algorithm - clustering. To cluster we simply need to pass an aggregated dataframe and the `k` parameter, which refers to the number of nearest neighbours to consider while clustering. In essense, lower the `k` value, higher the number of clusters and vice-versa. Selecting the right value of k is a judgement call that is very specific to the data.

Since it is a heavy task, we have used caching to store the results. What caching does is compares the aggregated dataframe to the one in memory and if it is identical, then uses the previously computed results to cluster. For turning caching off, use the parameter `use_cache=FALSE`

``` r
mvad %>%
  aggregate_sequences(format = "%Y-%m-%d", unit = "month", n_units = 3, summary_stats=FALSE) %>%
    cluster_knn(k = 15)
```

    ## # A tibble: 15 x 3
    ##    cluster       df_sequences     n
    ##      <int>             <list> <int>
    ##  1       1 <tibble [142 x 2]>   142
    ##  2       2  <tibble [88 x 2]>    88
    ##  3       3  <tibble [64 x 2]>    64
    ##  4       4  <tibble [63 x 2]>    63
    ##  5       5  <tibble [59 x 2]>    59
    ##  6       6  <tibble [56 x 2]>    56
    ##  7       7  <tibble [55 x 2]>    55
    ##  8       8  <tibble [50 x 2]>    50
    ##  9       9  <tibble [47 x 2]>    47
    ## 10      10  <tibble [32 x 2]>    32
    ## 11      11  <tibble [24 x 2]>    24
    ## 12      12  <tibble [24 x 2]>    24
    ## 13      13  <tibble [23 x 2]>    23
    ## 14      14  <tibble [21 x 2]>    21
    ## 15      15  <tibble [19 x 2]>    19

The output of the cluster\_knn function is a dataframe with 3 columns -

1.  cluster (cluster\_id)
2.  df\_sequences (dataframes of id and sequences corresponding to the cluster)
3.  n which refers to the number of sequences in the cluster and is used to sort the dataframe

3. Extract the patterns
-----------------------

Now that we have clustered, the next step is to calculate a weighted sequence for each cluster. We can do this using the `get_weighted_sequence()` function. However, the `filter_pattern()` function automatically does this for us. So all we need to do is call the `filter_pattern()` with the required threshold and an optional pattern name (default is consensus).

The `threshold` parameter is used to specify the specify the proportion of sequeneces the item must have been present in.

``` r
mvad %>%
  aggregate_sequences(format = "%Y-%m-%d", unit = "month", n_units = 1, summary_stats=FALSE) %>%
    cluster_knn(k = 15) %>%
      filter_pattern(threshold = 0.3, pattern_name = "variation")
```

    ## Clustering...

    ## Calculating distance matrix...

    ## Caching distance matrix...

    ## # A tibble: 10 x 5
    ##    cluster     n variation_pattern       df_sequences weighted_sequence
    ##      <int> <int>            <list>             <list>            <list>
    ##  1       1   162  <S3: W_Sequence> <tibble [162 x 2]>  <S3: W_Sequence>
    ##  2       2   114  <S3: W_Sequence> <tibble [114 x 2]>  <S3: W_Sequence>
    ##  3       3   112  <S3: W_Sequence> <tibble [112 x 2]>  <S3: W_Sequence>
    ##  4       4    75  <S3: W_Sequence>  <tibble [75 x 2]>  <S3: W_Sequence>
    ##  5       5    64  <S3: W_Sequence>  <tibble [64 x 2]>  <S3: W_Sequence>
    ##  6       6    63  <S3: W_Sequence>  <tibble [63 x 2]>  <S3: W_Sequence>
    ##  7       7    40  <S3: W_Sequence>  <tibble [40 x 2]>  <S3: W_Sequence>
    ##  8       8    37  <S3: W_Sequence>  <tibble [37 x 2]>  <S3: W_Sequence>
    ##  9       9    29  <S3: W_Sequence>  <tibble [29 x 2]>  <S3: W_Sequence>
    ## 10      10    16  <S3: W_Sequence>  <tibble [16 x 2]>  <S3: W_Sequence>

We can also chain multiple `filter_pattern()` functions to keep adding patterns.

``` r
mvad %>%
  aggregate_sequences(format = "%Y-%m-%d", unit = "month", n_units = 3, summary_stats=FALSE) %>%
    cluster_knn(k = 15) %>%
      filter_pattern(threshold = 0.3, pattern_name = "variation") %>%
        filter_pattern(threshold = 0.4, pattern_name = "consensus") 
```

    ## Clustering...

    ## Calculating distance matrix...

    ## Caching distance matrix...

    ## # A tibble: 15 x 6
    ##    cluster     n consensus_pattern variation_pattern       df_sequences
    ##      <int> <int>            <list>            <list>             <list>
    ##  1       1   142  <S3: W_Sequence>  <S3: W_Sequence> <tibble [142 x 2]>
    ##  2       2    88  <S3: W_Sequence>  <S3: W_Sequence>  <tibble [88 x 2]>
    ##  3       3    64  <S3: W_Sequence>  <S3: W_Sequence>  <tibble [64 x 2]>
    ##  4       4    63  <S3: W_Sequence>  <S3: W_Sequence>  <tibble [63 x 2]>
    ##  5       5    59  <S3: W_Sequence>  <S3: W_Sequence>  <tibble [59 x 2]>
    ##  6       6    56  <S3: W_Sequence>  <S3: W_Sequence>  <tibble [56 x 2]>
    ##  7       7    55  <S3: W_Sequence>  <S3: W_Sequence>  <tibble [55 x 2]>
    ##  8       8    50  <S3: W_Sequence>  <S3: W_Sequence>  <tibble [50 x 2]>
    ##  9       9    47  <S3: W_Sequence>  <S3: W_Sequence>  <tibble [47 x 2]>
    ## 10      10    32  <S3: W_Sequence>  <S3: W_Sequence>  <tibble [32 x 2]>
    ## 11      11    24  <S3: W_Sequence>  <S3: W_Sequence>  <tibble [24 x 2]>
    ## 12      12    24  <S3: W_Sequence>  <S3: W_Sequence>  <tibble [24 x 2]>
    ## 13      13    23  <S3: W_Sequence>  <S3: W_Sequence>  <tibble [23 x 2]>
    ## 14      14    21  <S3: W_Sequence>  <S3: W_Sequence>  <tibble [21 x 2]>
    ## 15      15    19  <S3: W_Sequence>  <S3: W_Sequence>  <tibble [19 x 2]>
    ## # ... with 1 more variables: weighted_sequence <list>

4. Formatted output
-------------------

Though all the algorithmic work is done, the output is hardly readable as the objects of interest are all present as classes. We have not "prettified" the output by design because concealing it would really inhibit additional explaratory possibilities. Instead, we have a simple function that can be called for this - `format_sequence()`

``` r
mvad %>%
  aggregate_sequences(format = "%Y-%m-%d", unit = "month", n_units = 3, summary_stats=FALSE) %>%
    cluster_knn(k = 15) %>%
      filter_pattern(threshold = 0.3, pattern_name = "variation") %>%
        filter_pattern(threshold = 0.4, pattern_name = "consensus") %>%
          format_sequence() 
```

    ## Clustering...

    ## Using cached distance matrix...

    ## # A tibble: 15 x 5
    ##    cluster     n
    ##      <int> <dbl>
    ##  1       1 18.51
    ##  2       2 11.47
    ##  3       3  8.34
    ##  4       4  8.21
    ##  5       5  7.69
    ##  6       6  7.30
    ##  7       7  7.17
    ##  8       8  6.52
    ##  9       9  6.13
    ## 10      10  4.17
    ## 11      11  3.13
    ## 12      12  3.13
    ## 13      13  3.00
    ## 14      14  2.74
    ## 15      15  2.48
    ## # ... with 3 more variables: consensus_pattern <chr>,
    ## #   variation_pattern <chr>, weighted_sequence <chr>

Since markdown by default limits the screen content the important output gets truncated. So I have used another paramter called `kable` which can be safely ignored if it doesn't make sense. The format\_sequence also has a parameter called `compare` which when TRUE lists the patterns within a cluster row-by-row. The r function `View()` can be chained to opened the output dataframe in the built-in viewer but sometimes the output strings are too large to be viewed there. So we can chain the readr function `write_csv()` to save the output and explore the results in a text editor or excel.

``` r
(approxmap_results <- 
  mvad %>%
    aggregate_sequences(format = "%Y-%m-%d", unit = "month", n_units = 3, summary_stats=FALSE) %>%
      cluster_knn(k = 15) %>%
        filter_pattern(threshold = 0.3, pattern_name = "variation") %>%
          filter_pattern(threshold = 0.4, pattern_name = "consensus") %>%
            format_sequence(compare=TRUE))%>%
            kable()
```

    ## Clustering...

    ## Using cached distance matrix...

<table>
<thead>
<tr>
<th style="text-align:right;">
cluster
</th>
<th style="text-align:right;">
n
</th>
<th style="text-align:left;">
pattern
</th>
<th style="text-align:left;">
w\_sequence
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
18.51
</td>
<td style="text-align:left;">
consensus\_pattern
</td>
<td style="text-align:left;">
&lt;(training:82):120 (employment:80):134&gt; : 142
</td>
</tr>
<tr>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
18.51
</td>
<td style="text-align:left;">
variation\_pattern
</td>
<td style="text-align:left;">
&lt;(training:82):120 (employment:80, joblessness:50):134&gt; : 142
</td>
</tr>
<tr>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
18.51
</td>
<td style="text-align:left;">
weighted\_sequence
</td>
<td style="text-align:left;">
&lt;(employment:18, FE:6, joblessness:9, school:36):48 (joblessness:1):1 (employment:25, FE:18, HE:1, joblessness:4, school:6, training:82):120 (employment:80, FE:4, joblessness:50, training:4):134&gt; : 142
</td>
</tr>
<tr>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
11.47
</td>
<td style="text-align:left;">
consensus\_pattern
</td>
<td style="text-align:left;">
&lt;(FE:61, joblessness:59):88 (employment:83):86&gt; : 88
</td>
</tr>
<tr>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
11.47
</td>
<td style="text-align:left;">
variation\_pattern
</td>
<td style="text-align:left;">
&lt;(FE:61, joblessness:59):88 (HE:33):38 (employment:83):86&gt; : 88
</td>
</tr>
<tr>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
11.47
</td>
<td style="text-align:left;">
weighted\_sequence
</td>
<td style="text-align:left;">
&lt;(joblessness:1, school:1):1 (employment:2, FE:61, joblessness:59, school:23, training:2):88 (HE:33, school:5):38 (employment:83, HE:1, joblessness:1, training:3):86 (employment:1, FE:1, joblessness:2, training:1):5&gt; : 88
</td>
</tr>
<tr>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
8.34
</td>
<td style="text-align:left;">
consensus\_pattern
</td>
<td style="text-align:left;">
&lt;(training:33):64 (FE:26, joblessness:35):64 (employment:63):64&gt; : 64
</td>
</tr>
<tr>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
8.34
</td>
<td style="text-align:left;">
variation\_pattern
</td>
<td style="text-align:left;">
&lt;(training:33):64 (FE:26, joblessness:35):64 (employment:63):64&gt; : 64
</td>
</tr>
<tr>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
8.34
</td>
<td style="text-align:left;">
weighted\_sequence
</td>
<td style="text-align:left;">
&lt;(FE:3, training:5):8 (FE:10, HE:1, joblessness:15, school:19, training:33):64 (FE:26, HE:1, joblessness:35, school:2):64 (employment:63, FE:1):64&gt; : 64
</td>
</tr>
<tr>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
8.21
</td>
<td style="text-align:left;">
consensus\_pattern
</td>
<td style="text-align:left;">
&lt;(school:58):60 (HE:61):63&gt; : 63
</td>
</tr>
<tr>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
8.21
</td>
<td style="text-align:left;">
variation\_pattern
</td>
<td style="text-align:left;">
&lt;(school:58):60 (HE:61):63&gt; : 63
</td>
</tr>
<tr>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
8.21
</td>
<td style="text-align:left;">
weighted\_sequence
</td>
<td style="text-align:left;">
&lt;(employment:1, FE:2, joblessness:5, school:58, training:1):60 (employment:18, FE:13, joblessness:6, school:1):35 (FE:3, joblessness:2, school:3):8 (employment:2, FE:1, HE:61, training:1):63 (HE:1, training:1):2&gt; : 63
</td>
</tr>
<tr>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
7.69
</td>
<td style="text-align:left;">
consensus\_pattern
</td>
<td style="text-align:left;">
&lt;(training:26):56 (employment:57):57 (joblessness:44):59&gt; : 59
</td>
</tr>
<tr>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
7.69
</td>
<td style="text-align:left;">
variation\_pattern
</td>
<td style="text-align:left;">
&lt;(FE:21, training:26):56 (employment:57):57 (joblessness:44):59&gt; : 59
</td>
</tr>
<tr>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
7.69
</td>
<td style="text-align:left;">
weighted\_sequence
</td>
<td style="text-align:left;">
&lt;(FE:2, school:3):5 (FE:21, joblessness:9, school:13, training:26):56 (employment:57, FE:2, joblessness:1, school:2, training:1):57 (FE:2, HE:3, school:1, training:1):7 (FE:6, HE:1, joblessness:44, training:8):59&gt; : 59
</td>
</tr>
<tr>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
7.30
</td>
<td style="text-align:left;">
consensus\_pattern
</td>
<td style="text-align:left;">
&lt;(employment:54):56&gt; : 56
</td>
</tr>
<tr>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
7.30
</td>
<td style="text-align:left;">
variation\_pattern
</td>
<td style="text-align:left;">
&lt;(employment:54):56&gt; : 56
</td>
</tr>
<tr>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
7.30
</td>
<td style="text-align:left;">
weighted\_sequence
</td>
<td style="text-align:left;">
&lt;(employment:54, joblessness:3, school:4, training:12):56&gt; : 56
</td>
</tr>
<tr>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
7.17
</td>
<td style="text-align:left;">
consensus\_pattern
</td>
<td style="text-align:left;">
&lt;(employment:43):55 (joblessness:41):55&gt; : 55
</td>
</tr>
<tr>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
7.17
</td>
<td style="text-align:left;">
variation\_pattern
</td>
<td style="text-align:left;">
&lt;(employment:43, school:17):55 (joblessness:41):55&gt; : 55
</td>
</tr>
<tr>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
7.17
</td>
<td style="text-align:left;">
weighted\_sequence
</td>
<td style="text-align:left;">
&lt;(employment:43, FE:15, joblessness:4, school:17, training:4):55 (HE:1, joblessness:5, training:7):13 (FE:11, joblessness:41, training:3):55 (training:1):1&gt; : 55
</td>
</tr>
<tr>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
6.52
</td>
<td style="text-align:left;">
consensus\_pattern
</td>
<td style="text-align:left;">
&lt;(FE:50):50 (employment:41):50&gt; : 50
</td>
</tr>
<tr>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
6.52
</td>
<td style="text-align:left;">
variation\_pattern
</td>
<td style="text-align:left;">
&lt;(FE:50):50 (employment:41):50&gt; : 50
</td>
</tr>
<tr>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
6.52
</td>
<td style="text-align:left;">
weighted\_sequence
</td>
<td style="text-align:left;">
&lt;(FE:50, school:8):50 (employment:41, HE:12, joblessness:1):50&gt; : 50
</td>
</tr>
<tr>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
6.13
</td>
<td style="text-align:left;">
consensus\_pattern
</td>
<td style="text-align:left;">
&lt;(employment:43, FE:37):47&gt; : 47
</td>
</tr>
<tr>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
6.13
</td>
<td style="text-align:left;">
variation\_pattern
</td>
<td style="text-align:left;">
&lt;(employment:43, FE:37):47&gt; : 47
</td>
</tr>
<tr>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
6.13
</td>
<td style="text-align:left;">
weighted\_sequence
</td>
<td style="text-align:left;">
&lt;(employment:43, FE:37, joblessness:14):47&gt; : 47
</td>
</tr>
<tr>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
4.17
</td>
<td style="text-align:left;">
consensus\_pattern
</td>
<td style="text-align:left;">
&lt;(FE:24, joblessness:19):32 (training:32):32 (employment:32):32&gt; : 32
</td>
</tr>
<tr>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
4.17
</td>
<td style="text-align:left;">
variation\_pattern
</td>
<td style="text-align:left;">
&lt;(FE:24, joblessness:19):32 (training:32):32 (employment:32):32&gt; : 32
</td>
</tr>
<tr>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
4.17
</td>
<td style="text-align:left;">
weighted\_sequence
</td>
<td style="text-align:left;">
&lt;(school:2):2 (joblessness:1):1 (FE:24, joblessness:19, school:4):32 (joblessness:1, training:32):32 (employment:32):32 (FE:1):1&gt; : 32
</td>
</tr>
<tr>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
3.13
</td>
<td style="text-align:left;">
consensus\_pattern
</td>
<td style="text-align:left;">
&lt;(employment:17, school:24):24 (HE:24):24&gt; : 24
</td>
</tr>
<tr>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
3.13
</td>
<td style="text-align:left;">
variation\_pattern
</td>
<td style="text-align:left;">
&lt;(employment:17, joblessness:9, school:24):24 (HE:24):24&gt; : 24
</td>
</tr>
<tr>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
3.13
</td>
<td style="text-align:left;">
weighted\_sequence
</td>
<td style="text-align:left;">
&lt;(employment:17, joblessness:9, school:24):24 (HE:24):24&gt; : 24
</td>
</tr>
<tr>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
3.13
</td>
<td style="text-align:left;">
consensus\_pattern
</td>
<td style="text-align:left;">
&lt;(employment:12, FE:24, joblessness:13):24 (HE:24):24&gt; : 24
</td>
</tr>
<tr>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
3.13
</td>
<td style="text-align:left;">
variation\_pattern
</td>
<td style="text-align:left;">
&lt;(employment:12, FE:24, joblessness:13):24 (HE:24):24&gt; : 24
</td>
</tr>
<tr>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
3.13
</td>
<td style="text-align:left;">
weighted\_sequence
</td>
<td style="text-align:left;">
&lt;(employment:12, FE:24, joblessness:13):24 (HE:24, school:1):24&gt; : 24
</td>
</tr>
<tr>
<td style="text-align:right;">
13
</td>
<td style="text-align:right;">
3.00
</td>
<td style="text-align:left;">
consensus\_pattern
</td>
<td style="text-align:left;">
&lt;(joblessness:23, training:23):23 (employment:23):23&gt; : 23
</td>
</tr>
<tr>
<td style="text-align:right;">
13
</td>
<td style="text-align:right;">
3.00
</td>
<td style="text-align:left;">
variation\_pattern
</td>
<td style="text-align:left;">
&lt;(joblessness:23, training:23):23 (employment:23):23&gt; : 23
</td>
</tr>
<tr>
<td style="text-align:right;">
13
</td>
<td style="text-align:right;">
3.00
</td>
<td style="text-align:left;">
weighted\_sequence
</td>
<td style="text-align:left;">
&lt;(joblessness:23, training:23):23 (employment:23):23&gt; : 23
</td>
</tr>
<tr>
<td style="text-align:right;">
14
</td>
<td style="text-align:right;">
2.74
</td>
<td style="text-align:left;">
consensus\_pattern
</td>
<td style="text-align:left;">
&lt;(employment:18, FE:13):21 (training:21):21&gt; : 21
</td>
</tr>
<tr>
<td style="text-align:right;">
14
</td>
<td style="text-align:right;">
2.74
</td>
<td style="text-align:left;">
variation\_pattern
</td>
<td style="text-align:left;">
&lt;(employment:18, FE:13):21 (training:21):21&gt; : 21
</td>
</tr>
<tr>
<td style="text-align:right;">
14
</td>
<td style="text-align:right;">
2.74
</td>
<td style="text-align:left;">
weighted\_sequence
</td>
<td style="text-align:left;">
&lt;(employment:18, FE:13, joblessness:5, school:1):21 (school:1):1 (training:21):21&gt; : 21
</td>
</tr>
<tr>
<td style="text-align:right;">
15
</td>
<td style="text-align:right;">
2.48
</td>
<td style="text-align:left;">
consensus\_pattern
</td>
<td style="text-align:left;">
&lt;(FE:18, joblessness:9):19 (employment:19):19 (HE:19):19&gt; : 19
</td>
</tr>
<tr>
<td style="text-align:right;">
15
</td>
<td style="text-align:right;">
2.48
</td>
<td style="text-align:left;">
variation\_pattern
</td>
<td style="text-align:left;">
&lt;(FE:18, joblessness:9):19 (employment:19):19 (HE:19):19&gt; : 19
</td>
</tr>
<tr>
<td style="text-align:right;">
15
</td>
<td style="text-align:right;">
2.48
</td>
<td style="text-align:left;">
weighted\_sequence
</td>
<td style="text-align:left;">
&lt;(FE:18, joblessness:9):19 (employment:19, school:2):19 (HE:19):19&gt; : 19
</td>
</tr>
</tbody>
</table>
    approxmap_results %>% write_csv("approxmap_results.csv")

Using `tidyverse` to fully exploit approxmapR
=============================================

The output is what is called as a tibble (a supercharged dataframe) that makes it possible to do things like storing a list of tibbles (df\_sequences) in a column. To inspect the say the first 2 rows, we can use standard `dplyr` commands.

``` r
df_sequences <- 
  mvad %>%
  aggregate_sequences(format = "%Y-%m-%d", unit = "month", n_units = 1, summary_stats=FALSE) %>%
    cluster_knn(k = 15) %>%
      top_n(2) %>%
        pull(df_sequences)
```

    ## Clustering...

    ## Calculating distance matrix...

    ## Caching distance matrix...

    ## Selecting by n

``` r
df_sequences
```

    ## [[1]]
    ## # A tibble: 162 x 2
    ##       id       sequence
    ##    <int>         <list>
    ##  1     7 <S3: Sequence>
    ##  2    14 <S3: Sequence>
    ##  3    64 <S3: Sequence>
    ##  4    74 <S3: Sequence>
    ##  5   125 <S3: Sequence>
    ##  6   167 <S3: Sequence>
    ##  7   168 <S3: Sequence>
    ##  8   173 <S3: Sequence>
    ##  9   176 <S3: Sequence>
    ## 10   188 <S3: Sequence>
    ## # ... with 152 more rows
    ## 
    ## [[2]]
    ## # A tibble: 114 x 2
    ##       id       sequence
    ##    <int>         <list>
    ##  1     8 <S3: Sequence>
    ##  2    30 <S3: Sequence>
    ##  3   134 <S3: Sequence>
    ##  4   157 <S3: Sequence>
    ##  5   163 <S3: Sequence>
    ##  6   180 <S3: Sequence>
    ##  7   182 <S3: Sequence>
    ##  8   183 <S3: Sequence>
    ##  9   186 <S3: Sequence>
    ## 10   192 <S3: Sequence>
    ## # ... with 104 more rows

To explore these sequences, we also have tidy print methods. The functional programming toolkit for R, `purrr` provides an efficient means to fully exploit such outputs.

``` r
df_sequences %>%
          map(function(df_cluster){
            df_cluster %>%
              mutate(sequence = map_chr(sequence, format_sequence))
          })
```

    ## [[1]]
    ## # A tibble: 162 x 2
    ##       id                          sequence
    ##    <int>                             <chr>
    ##  1     7 <(joblessness) (FE) (employment)>
    ##  2    14 <(joblessness) (FE) (employment)>
    ##  3    64 <(joblessness) (FE) (employment)>
    ##  4    74 <(joblessness) (FE) (employment)>
    ##  5   125 <(joblessness) (FE) (employment)>
    ##  6   167 <(joblessness) (FE) (employment)>
    ##  7   168 <(joblessness) (FE) (employment)>
    ##  8   173 <(joblessness) (FE) (employment)>
    ##  9   176 <(joblessness) (FE) (employment)>
    ## 10   188 <(joblessness) (FE) (employment)>
    ## # ... with 152 more rows
    ## 
    ## [[2]]
    ## # A tibble: 114 x 2
    ##       id            sequence
    ##    <int>               <chr>
    ##  1     8 <(employment) (FE)>
    ##  2    30 <(employment) (FE)>
    ##  3   134 <(employment) (FE)>
    ##  4   157 <(employment) (FE)>
    ##  5   163 <(employment) (FE)>
    ##  6   180 <(employment) (FE)>
    ##  7   182 <(employment) (FE)>
    ##  8   183 <(employment) (FE)>
    ##  9   186 <(employment) (FE)>
    ## 10   192 <(employment) (FE)>
    ## # ... with 104 more rows

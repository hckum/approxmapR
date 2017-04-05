Approxmap
=========

This is an implementation of the ApproxMAP algorithm by Dr. Hye-Chung Kum. The algorithm is explained [here](http://web.cs.ucla.edu/~weiwang/paper/SDM03_2.pdf). To learn more about the algorithm and for additional resources, click [here](http://www.unc.edu/~kum/approxMAP/).

Installation instructions
-------------------------

To install and load Approxmap, use the following code snippet:

    install.packages("devtools")
    devtools::install_github("hckum/approxmapR")
    library(approxmapR)

Approxmap modes
---------------

The R Approxmap package can be run in 2 modes:

1.  Batch mode (script or console)
2.  GUI

Approxmap Batch Mode
====================

Once the package is loaded, we can use the following commands to get our output:

    data_path = "./data/demo1.csv" #present in the data directory of the package
    approxmap(data_path, k = 2, cons_cutoff = 0.5, var_thresh = 0.2, noise_thresh = 0, period1 = "1 Week", st.date = "Mon")

-   The approxmap function takes in the path of the *csv* input file as the first argument. The data path variable should hence point to the whole address of the csv file.
-   The k value refers to the number of nearest neighbours to consider for clustering.
-   cons\_cutoff argument takes in a value between 0 and 1 to calculate consensus pattern
-   var\_thresh argument takes in a value between 0 and 1 to calculate consensus pattern but it should be lower than cons\_cutoff as it is a superset of the consensus pattern.
-   noise\_thresh refers to the min number of occurences to appear in the plots.
-   period1 value refers to the period of aggregation.
-   st.date value refers to when the algorithm should start aggregating from.
-   Given these arguments (many of which have defaults), the function generates html outputs within a folder titled **approxmap\_results** at the address specified by the results\_directory argument. There are 4 outputs that get generated. **consensus\_var\_pattern** prints out the aligned consensus and variation patterns for each cluster. **Plot** has all the plots in terms of element weights. **time\_period** has the aggregated input. **weighted\_sequences** has the weighted sequence for each cluster. -The weighted sequences and consensus/variation patterns are formatted in such a way that the elements with the most strong presence appear big and dark and the relatively weaker elements appear light and small. The outputs can be opened by excel also to aid further analysis.
-   The function also returns the complete results which are not printed in the console by defualt. If assigned `x <- approxmap(data_path, k = 2)`, it can be extracted or printed using that variable. You can even use the $ symbol to access parts of the results as the output is a list (as in `x$formatted_results$consensus`)

Approxmap GUI
=============

A GUI implementation of the Approxmap algorithm. To invoke type `gui_approxmap()` in the console. A browser window will open. The left pane of the app is where you give the inputs (upload data, set the cutoff, etc.) and the right pane is where the results are displayed. The steps for using the applet are discussed below (The points correspond to the input steps):

### Input Panel

1.  **File Upload**: This is the data file that the user wants to analyze. The input must be a csv file with 3 columns:

-   ID
-   Date
-   Item

1.  **Data Aggregation**: This panel deals with aggregating the data into sets. For instance, if the aggregation level is "Calendar Month", all items in January are grouped as a set (and so on for Feb, Mar, Apr..). The start date is a panel that will appear for certain kinds of periods where you can specify when the program can start aggregating.
2.  **Hierarchy File Upload**: This is an optional panel. You can upload a file for further aggregation at the item level. You can choose to, for example, group treatments into physiological, mental and others. Once the file is uploaded, the aggregation levels are populated according to the number of columns. The default aggregation is 1 which means, the data will be analyzed at the most granular level. There are certain standards that the hierarchical data file must adhere to:

-   It must be a csv file
-   The *first* column of the file must contain the elements in the "Item" column of the original data.
-   *All the elements* in the item column of the original data *must* be present in the hierarchical table
-   The agrregation moves from left to right

1.  **Clustering**: The clustering algorithm used is kNN clustering. The k (number of nearest neighbours to look at) can be specified here. k defaults to 2.
2.  **Cutoffs**: Cutoffs can be specified here. After the approxmap button is clicked, these are the only inputs that work simultaneously as they are changed There are 3 cutoffs used:
3.  Noise cutoff: Given in terms of actual frequencies. Affects the frequency chart that will be displayed. It basically clears all the points that do not meet the cutoff in terms of te number of occurences.
4.  Variation cutoff: Mines the variation pattern (a superset of the consensus pattern). Since it is not as stringent as the consensus pattern, by definition the cutoff for variation pattern must be lower than the consensus pattern. This is automatically taken care in the program.
5.  Consensus cutoff: Mines the consensus pattern for the clusters. Specifies the consensus pattern threshold
6.  **Get Approxmap**: Once the parameters are specified, you can click on this to get the patterns. You can play with the cutoffs after clicking on them but *for any other change in input (aggregation, hierarchy or knn) the button has to be clicked again to get the new approxmap*. This is because, the patterns are calcualted from the weighted sequence and hence do need a lot of computation whereas making the other inputs interactive would have slowed the program down as the user is just determining the parameters.

### Output panel

1.  **Data Tab**: Displays the input data before (left) and after (right) date and hierarchy aggregation.
2.  **Cluster Tab**: Empty for now.
3.  **Consensus Patterns Tab**:

-   Has c tabs (where c is the number of clusters)
-   For each cluster, the consensus and variation patterns, the frequency plot (to reevaulate the thresholds) and the weighted sequence are displayed.
-   All the patterns are color-coded (and size-coded) according to the strength of the signal. This means that items that have occured many times appear more prominently than the ones that haven't.

### Demo

1.  Go to \[<http://ilangurudev.shinyapps.io/ApproxMap_Shiny/>\]
2.  For the input data file, use "demo1.csv" found in the data folder of this github project (Click on \[<http://github.com/hckum/approxmapR/tree/master/>\] and navigate to the "data" folder. Then, right-click and click on save link/target as)
3.  For the hierarchical data, use "Tree.csv" in the same folder using the same method. Please ensure that you do not select "Tree - Copy.csv" as it is a hierarchical file with a few entries missing (to be used for testing).
4.  Use any input parameters that you desire.

Feel free to leave your suggestions at \[<kum@tamu.edu>\] or \[<ilan50_guru@tamu.edu>\]

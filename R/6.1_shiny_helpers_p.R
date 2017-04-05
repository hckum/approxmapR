#convert period and start date in numbers
#' get.pd
#'
#' @export



get.pd = function(period1){
  period1 = tolower(period1)
  if(period1  == '1 week'){ pd = 1 }
  else if (period1 == 'calender month'){ pd = 2 }
  else if (period1 == 'quarterly'){ pd = 3 }
  else if (period1 == '6 month'){ pd = 4 }
  else if (period1 == '1 year'){ pd = 5 }
}


#' get.st.date
#'
#' @export

get.st.date = function(days){
  days = tolower(days)
  if(days == "mon"){st.date = 2}
  else if (days == "tue") {st.date = 3}
  else if (days == "wed") {st.date = 4}
  else if (days == "thur") {st.date = 5}
  else if (days == "fri") {st.date = 6}
  else if (days == "sat") {st.date = 7}
  else if (days == "sun") {st.date = 1}
  else if (days == "jan"){st.date = 1}
  else if (days == "feb") {st.date = 2}
  else if (days == "mar") {st.date = 3}
  else if (days == "apr") {st.date = 4}
  else if (days == "may") {st.date = 5}
  else if (days == "jun") {st.date = 6}
  else if (days == "july") {st.date = 7}
  else if (days == "aug") {st.date = 8}
  else if (days == "sep") {st.date = 9}
  else if (days == "oct") {st.date = 10}
  else if (days == "nov") {st.date = 11}
  else {st.date = 12}

}


#function to order according to heirarchy

#' ord.heir
#'
#' @export

ord.heir = function(data2, level){

  data2 = data.frame(data2,stringsAsFactors=FALSE) #form the data as data frame format
  colnames(data2) = c("ID", "Period", "Item")

  if(level == 1){
    return(data2)
  }

  if(level == 2){
    # data2temp = data2
    # data2temp[,3] = "L2a"    #assuming A,B,C,D is L2a
    # data2temp[data2[,3]=="I",,3] = "L2b"    #Assuming I,K,L,M is L2b
    #data2temp[] = "L2c"   #assuming X,Y,Z is L2c


    # return(data2temp)
    #data2$Item = as.character(data$Item)
    data2 = within(data2, Item[Item == "A" | Item == "B"] <- 'L2a')
    #data2$Item = as.factor(data$Item)

    return(data2)
  }

  if(level == 3){

  }
}


#function to order according to level

#' aggregate_hierarchy
#'
#' @export

aggregate_hierarchy <- function(input, tree, level) {
  names(input) <-c("id","date",names(tree)[1])
  # if(length(setdiff(input[,3],tree[,1]))>0)
  # {
  #   return("Matching rows not found for every item in original dataset")
  # } else {
    (df <- left_join(input,tree, by = names(input)[3]))
    return(df[,c(1,2,(level+2))])
  #}


}

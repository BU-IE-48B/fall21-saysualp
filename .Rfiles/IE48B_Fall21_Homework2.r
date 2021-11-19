install.packages("genlasso")
install.packages("e1071")
install.packages("rattle")

require(repr)
options(scipen=999)
options(repr.plot.width=15, repr.plot.height=8)

require(genlasso)
require(data.table)

# assuming you have the CBF in your working directory in the following format:
# 'working_directory/'
current_folder=getwd()
dataset='CBF'

cbf_path = sprintf('%s/%s_TRAIN.txt', current_folder, dataset)
cbf = fread(cbf_path)
setnames(cbf, "V1", "class")
cbf[,id:=1:.N]
head(cbf)

fusedlasso_representation_list = list()
min_lambda = list()

for (series in c(1:30)) {
    
    # apply fusedlasso for each series
    repr = fusedlasso1d(as.matrix(cbf[id==series, 2:129]))
    fusedlasso_representation_list[[series]] = repr
    
    # make crossvalidation, and take the lambda which gives the least error. The default is k = 10 folds.
    cv = cv.trendfilter(fusedlasso_representation_list[[series]])
    min_lambda[[series]] = cv$lambda.min
   
}

# sample representation with lambda obtained from cross-validation
plot(fusedlasso_representation_list[[1]], lambda=min_lambda[[1]], main="Minimal CV error")

require(e1071)
require(rpart)
require(rattle)
require(ggplot2)

# melt the data for long format
long_cbf = melt(cbf,id.vars=c('id','class'))

# need to get numerical part of the variable (represents time)
# using gsub to set the nonnumerical part to zero length
long_cbf[,time:=as.numeric(gsub("\\D", "", variable))-1]

# remove variable
long_cbf=long_cbf[,list(id,class,time,value)]
long_cbf=long_cbf[order(id,time)]

# check
head(long_cbf)

tree_representation_list = list()
best_max_depth = list()

rpart.ranges = list(minsplit=20, cp=0, minbucket=10, maxdepth=1:10)

for (series in c(1:30)) {
    
    # make crossvalidation, and take the best maxdepth which gives the least error. The default is k = 10 folds.
    tree_tune = tune(rpart, value~time, data=long_cbf[id==series], ranges=rpart.ranges)
    best_max_depth[[series]] = tree_tune
    
    # apply regression tree for each series
    repr = rpart(value~time, long_cbf[id==series], control=rpart.control(cp=0, maxdepth=best_max_depth[[series]]$best.parameters[,4], minsplit=20, minbucket=10))
    tree_representation_list[[series]] = repr
     
}

# e.g.tuned max_depth for series1 is 3 and for series2 is 2
best_max_depth[[1]]
best_max_depth[[2]]

# plot results of series1
sample_series = long_cbf[id==1]
fancyRpartPlot(tree_representation_list[[1]])
sample_series[,tree_rep:=predict(tree_representation_list[[1]], sample_series)]

data_plot=melt(sample_series, id.vars='time', measure.vars=c('value','tree_rep'))

ggplot(data_plot,aes(x=time,y=value,color=variable))+
    geom_line()

fitted_values = list()

for (series in c(1:30)) {
    
    selected_series = long_cbf[id==series] 
    
    # fit values with regression tree
    selected_series[,tree_rep:=predict(tree_representation_list[[series]], selected_series)]

    # fit values with fusedlasso
    selected_series[,fusedlasso_rep:=predict(fusedlasso_representation_list[[series]],  lambda=min_lambda[[series]], selected_series)$fit]
    
    #list fitted values
    fitted_values[[series]] = selected_series
    
}

head(fitted_values[[1]])

fitted_values_dt = do.call(rbind, fitted_values)

mse = melt(fitted_values_dt, id.vars=c("id", "class","time"), measure.vars=c("value","tree_rep","fusedlasso_rep"))
mse = merge(mse, long_cbf[,list(id, class, time, obs=value)], by=c("id",  "class", "time"))

head(mse)

mse_final = mse[!(variable=="value"), list(mse=mean((value-obs)^2)),list(id, class, variable)]
head(mse_final)

ggplot(mse_final, aes(x=variable,y=mse)) + facet_wrap(~class) + geom_boxplot()

install.packages("shiny", type="binary")
install.packages("manipulateWidget")
install.packages("TSclust")

require(TSclust)

# raw time series as input
matrix_cbf = as.matrix(cbf[,2:130])
rownames(matrix_cbf) = matrix_cbf[,129]

# calculate Euclidean distance between series
distance_matrix = as.matrix(diss(matrix_cbf, METHOD="EUCL"))

# make distance from i to i infinitely big number 
distance_matrix[distance_matrix==0] = 9999

l1 <- apply(distance_matrix, 2, which.min)
l2 <- apply(distance_matrix, 2, min)

result_raw = data.frame(series = colnames(distance_matrix), 
                        nearest_neighbour = rownames(distance_matrix)[l1], 
                        distance = l2)

result_raw = merge(result_raw, cbf[,list(nearest_neighbour = id, predicted  = class)], by="nearest_neighbour")
result_raw = merge(result_raw, cbf[,list(series = id, actual = class)], by="series")
result_raw$correctly_classified = ifelse(result_raw$predicted==result_raw$actual, 1, 0)

result_raw

# change long format to wide format to calculate distances 
tree_rep = reshape(fitted_values_dt[,c("id","time", "tree_rep")], idvar = "id", timevar = "time", direction = "wide")
colnames(tree_rep)=gsub("tree_rep.", "V", names(tree_rep))

head(tree_rep)

# tree representation as input
matrix_cbf = as.matrix(tree_rep)
rownames(matrix_cbf) = matrix_cbf[,1]

# calculate Euclidean distance between series
distance_matrix = as.matrix(diss(matrix_cbf, METHOD="EUCL"))

# make distance from i to i infinitely big number 
distance_matrix[distance_matrix==0] = 9999

l1 <- apply(distance_matrix, 2, which.min)
l2 <- apply(distance_matrix, 2, min)

result_tree = data.frame(series = colnames(distance_matrix), 
                        nearest_neighbour = rownames(distance_matrix)[l1], 
                        distance = l2)

result_tree = merge(result_tree, cbf[,list(nearest_neighbour = id, predicted  = class)], by="nearest_neighbour")
result_tree = merge(result_tree, cbf[,list(series = id, actual = class)], by="series")
result_tree$correctly_classified = ifelse(result_tree$predicted==result_tree$actual, 1, 0)

result_tree

# change long format to wide format to calculate distances 
fusedlasso_rep = reshape(fitted_values_dt[,c("id","time", "fusedlasso_rep")], idvar = "id", timevar = "time", direction = "wide")
colnames(fusedlasso_rep)=gsub("fusedlasso_rep.", "V", names(fusedlasso_rep))

head(fusedlasso_rep)

# fusedlasso representation as input
matrix_cbf = as.matrix(fusedlasso_rep)
rownames(matrix_cbf) = matrix_cbf[,1]

# calculate Euclidean distance between series
distance_matrix = as.matrix(diss(matrix_cbf, METHOD="EUCL"))

# make distance from i to i infinitely big number 
distance_matrix[distance_matrix==0] = 9999

l1 <- apply(distance_matrix, 2, which.min)
l2 <- apply(distance_matrix, 2, min)

result_lasso = data.frame(series = colnames(distance_matrix), 
                          nearest_neighbour = rownames(distance_matrix)[l1], 
                          distance = l2)

result_lasso = merge(result_lasso, cbf[,list(nearest_neighbour = id, predicted  = class)], by="nearest_neighbour")
result_lasso = merge(result_lasso, cbf[,list(series = id, actual = class)], by="series")
result_lasso$correctly_classified = ifelse(result_lasso$predicted==result_lasso$actual, 1, 0)

result_lasso

# accuracy when raw time series as input
sum(result_raw$correctly_classified)/nrow(result_raw)

# accuracy when tree representation as input
sum(result_tree$correctly_classified)/nrow(result_tree)

# accuracy when fusedlasso representation as input
sum(result_lasso$correctly_classified)/nrow(result_lasso)

require(data.table)
require(ggplot2)
require(repr)
require(rpart)
require(rattle)
require(TSrepr)
require(TSdist)
require(dtw)
require(zoo)
require(e1071)

options(scipen=999)
options(repr.plot.width=15, repr.plot.height=8)
#memory.limit(size = 9999999999)

# assuming you have the data in your working directory in the following format:
# 'working_directory/'
current_folder = getwd()

# funtion to read & preprocess the data 
preprocess = function(dataset, type, file_format, to_long=FALSE){
    # read the data
    data_path = sprintf("%s/%s_%s.%s", current_folder, dataset, type, file_format)
    data = fread(data_path)
    # first add id column 
    data[,id:=1:.N]   
    # second rename class column name as "class" then sort based on class 
    setnames(data,'V1','class')
    data = data[order(class)]
    data[,class:=as.character(class)]
    # change data to long format for easier visualization and analysis 
    if (to_long==TRUE){
        long = melt(data, id.vars=c('id','class'))
        # need to get numerical part of the variable (represents time)
        # using gsub to set the nonnumerical part to zero length
        long[,time:=as.numeric(gsub("\\D", "", variable))-1]
        long=long[,list(id,class,time,value)]
        long=long[order(id,time)]
        return(long)
    } else {
        return(data) 
    }
      
}

# table showing the performance (mean/std of accuracy) of best algorithm settings on each train set 
bests_for_each_train_dataset = data.table()
# table showing the performance (mean/std of accuracy) of best algorithm settings on each test set 
bests_for_each_test_dataset = data.table()

# funtion to create representations for proposed types and if desired plot a sample with last_series_id 
representation = function(data, long_data, last_series_id = 1, plot_represenations = TRUE){
    all_id_paa_rep = list() 
    paa_rep = list()
    paa_rep_param = list()
    segment_length = c(5, 20)
    
    all_id_tree_rep = list()
    tree_rep = list()
    rpart.ranges =  list(minsplit=15:20, cp=0, minbucket=5:7, maxdepth=25:30)
    
    # calculate paa representation
    for (i in 1:last_series_id){
        iter = 1
        # calculate paa representation of one instance with proposed segment lenghts
        for (l in segment_length){
            # create table contains all time labels from the original data
            time_expanded = data.table(time = 1:(ncol(data)-1))
            # filter long_data for one instance
            data_ts = long_data[id==i]$value
            # calculate representation
            paa_rep_current = repr_paa(data_ts, l, meanC)
            # create dummy time to plot this representation
            dummy_time = c(1:(length(paa_rep_current)-1))*l 
            dummy_time = c(dummy_time, (ncol(data)-1))
            # create table that contains the paa representations 
            dt_paa = data.table(dummy_time, paa_rep_current)
            # speficy column name as representation name
            representation_name = paste0('paa_rep_sl=', l)
            names(dt_paa) = c("time", representation_name)
            representation = merge(time_expanded, dt_paa, by="time", all.x=T)
            # fill na values
            representation[,(representation_name):=nafill(get(representation_name),'nocb')]
            # store all paa representation of one instance with proposed settings
            paa_rep_param[[iter]] = representation
            iter=iter+1
        }
        # columnwise bind paa representation of one instance with all parameters setting
        id_representation = do.call(cbind, c(list(paa_rep_param[[1]][,.(time)]), lapply(paa_rep_param, `[`,,!c("time"))))
        id_representation[,id:=i]
        # change order of columns to make id first
        id_representation = id_representation[,c(4,1,2,3)]
        paa_rep[[i]] = id_representation
        
        default_rep = list()  
        tuned_rep = list()
        tuned_models = list()
        
        # calculate tree representation
        selected_instance = long_data[id==i]
    
        # apply regression tree for each series with default model
        repr = rpart(value~time, long_data[id==i], control=rpart.control(minsplit=20, 
                                                                         cp=0.01, 
                                                                         minbucket=7, 
                                                                         maxdepth=30))
        default_rep[[i]] = repr
        # fit values with default model for instance i
        selected_instance[,tree_rep_default:=predict(default_rep[[i]], selected_instance)]     
        # make crossvalidation, and take the best parameteres which gives the least error. The default is k = 10 folds.
        tree_tune = tune(rpart, value~time, data=long_data[id==i], ranges=rpart.ranges)
        tuned_models[[i]] = tree_tune       
        # apply regression tree for each series with tuned model
        repr = rpart(value~time, long_data[id==i], control=rpart.control(minsplit=tuned_models[[i]]$best.parameters[,1], 
                                                                         cp=0, 
                                                                         minbucket=tuned_models[[i]]$best.parameters[,3]), 
                                                                         maxdepth=tuned_models[[i]]$best.parameters[,4])
        tuned_rep[[i]] = repr
        # fit values with regression tree with tuned model
        selected_instance[,tree_rep_tuned:=predict(tuned_rep[[i]], selected_instance)]
        # construct tree representation of instance i and add to list
        tree_rep[[i]] = selected_instance  
         
    }
    
    # bind all paa representations of all instances with proposed settings
    all_id_paa_rep = do.call(rbind, paa_rep)
    
    # bind all tree representations of all instances with proposed settings
    all_id_tree_rep = do.call(rbind, tree_rep)
    
    # all representations
    all_rep = merge(all_id_tree_rep,
                    all_id_paa_rep,
                    by=c("id", "time"))
    
    if (plot_represenations==TRUE){
        data_plot = all_rep[id==last_series_id]
        data_plot = melt(data_plot,id.vars='time', measure.vars=c('value','tree_rep_default','tree_rep_tuned','paa_rep_sl=5', 'paa_rep_sl=20'))
        ggplot(data_plot,aes(x=time,y=value,color=variable)) + geom_line(size=1)
            
    } else {
        return(all_rep)
    }
    
}

# Example run of the function:
# Function will calculate the all representations for series1 and return plot of them since the plot_represenations=TRUE
representation(data=train_data, long_data=train_data_long, last_series_id = 1, plot_represenations = TRUE)

# Example run of the function:
# One can change last_series_id as nrow(input_data) <or any id that wants to calculate last select and plot_represenations = FALSE 
# then function will return a list containing all representations of all series until last_series_id in long format
reps = representation(data=train_data, long_data=train_data_long, last_series_id = 2, plot_represenations = FALSE)
head(reps)

large_number=10000

# function to calculate proposed distances of raw time series and proposed representations   
distance = function(dataset, reps, rep_names, dist_names, type){
    iter = 1
    for (i in rep_names){
        dt = as.data.table(reps[iter])
        if ("euc" %in% dist_names) {
        # remove id variable from the dt
        dist_euc = as.matrix(dist(dt[,2:ncol(dt)]))
        diag(dist_euc) = large_number
        fwrite(dist_euc, sprintf('hw3_%s_%s_euc_%s_dist.csv', type, dataset, i), col.names=F)
        }
        if ("dtw" %in% dist_names){
        dist_dtw = as.matrix(dtwDist(dt[,2:ncol(dt)]))
        diag(dist_dtw) = large_number
        fwrite(dist_dtw, sprintf('hw3_%s_%s_dtw_%s_dist.csv', type, dataset, i), col.names=F)
        }
        if ("lcss" %in% dist_names){
        dist_lcss = TSDatabaseDistances(dt[,2:ncol(dt)], distance='lcss', epsilon=0.05)
        dist_lcss = as.matrix(dist_lcss)
        diag(dist_lcss) = large_number
        fwrite(dist_lcss, sprintf('hw3_%s_%s_lcss_%s_dist.csv', type, dataset, i), col.names=F)  
        }
        if ("erp" %in% dist_names){
        dist_erp = TSDatabaseDistances(dt[,2:ncol(dt)], distance='erp', g=0.5)
        dist_erp = as.matrix(dist_erp)
        diag(dist_erp) = large_number
        fwrite(dist_erp, sprintf('hw3_%s_%s_erp_%s_dist.csv', type, dataset, i), col.names=F)  
        }
        iter = iter + 1
    }
            
}


# function to find nearest neighbours
nn_classify = function(dist_matrix, train_classes, test_indices, k=1){
    
    test_distances_to_train = dist_matrix[test_indices,]
    test_distances_to_train = test_distances_to_train[,-test_indices]
    train_classes = train_classes[-test_indices]
    # order the distances of each instance to another
    ordered_neighbours = apply(test_distances_to_train, 1, order)
    if(k==1){
        nearest_class = as.numeric(ordered_neighbours[as.numeric(ordered_neighbours[1,])])
        nearest_class = data.table(id=test_indices, nearest_class)
    } else {
        # returns each classes' k nearest neighbours 
        nearest_class = apply(ordered_neighbours[1:k,], 2, function(x) {train_classes[x]})
        nearest_class = data.table(id=test_indices, t(nearest_class))
    }
    
    long_nn_class = melt(nearest_class,'id')
    
    # count the classes of nearest neighbours for each instance
    class_counts = long_nn_class[,.N,list(id,value)]
    # calculate the frequency of each neigbour
    class_counts[,predicted_prob:=N/k]
    wide_class_prob_predictions = dcast(class_counts, id~value, value.var='predicted_prob')
    wide_class_prob_predictions[is.na(wide_class_prob_predictions)]=0
    # returns class which has higher probability (i.e most frequent class) as predicted value
    class_predictions = class_counts[,list(predicted = value[which.max(N)]), by=list(id)]
    
    return(list(prediction = class_predictions, prob_estimates = wide_class_prob_predictions))
    
}

dataset = "PowerCons"
type = "TRAIN"
file_format = "txt"

train_data = preprocess(dataset=dataset, type=type, file_format=file_format, to_long=FALSE)
train_data_long = preprocess(dataset=dataset, type=type, file_format=file_format, to_long=TRUE)

# visualize time series based on class
ggplot(train_data_long, aes(time, value)) + geom_line(aes(color = as.character(id)), show.legend = FALSE) +
     facet_wrap(~class)

representation(data=train_data, long_data=train_data_long, last_series_id = 1, plot_represenations = TRUE)

# calculate representations for whole dataset
reps = representation(data=train_data, long_data=train_data_long, last_series_id = nrow(train_data), plot_represenations = FALSE)
fwrite(reps, sprintf('hw3_reps_%s_%s.csv', type, dataset), col.names=TRUE)

# Create a overall accuracy table to find performance of representations and select the best setting among each representations
reps = fread(sprintf('hw3_reps_%s_%s.csv', type, dataset))
mse = melt(reps, id.vars=c("id", "class", "time"), measure.vars=c("value","tree_rep_default","tree_rep_tuned","paa_rep_sl=5", "paa_rep_sl=20"))
mse[,class:=as.character(class)]
mse = merge(mse, train_data_long[,list(id, class, time, obs=value)], by=c("id",  "class", "time"))
mse[!(variable=="value"), list(mse=mean((value-obs)^2)),list(variable)]

# change selected raw_series and selected representations to long format and store it in a list
raw_series = dcast(reps[,c(1,2,3,4)], id ~ time, value.var = "value")
tree_rep_tuned = dcast(reps[,c(1,2,3,6)], id ~ time, value.var = "tree_rep_tuned")
paa_rep_sl_5 = dcast(reps[,c(1,2,3,7)], id ~ time, value.var = "paa_rep_sl=5")
train_representation_list = list(raw_series, tree_rep_tuned, paa_rep_sl_5)

distance(dataset = dataset, reps = train_representation_list, rep_names = c("raw", "tree_rep_tuned", "paa_rep_sl=5"), dist_names = c("euc", "dtw", "lcss", "erp"),  type = type)

dist_files = list.files(pattern = paste0("hw3_TRAIN_", dataset))
dist_files

require(TunePareto)

k_levels = c(1, 3, 5)
set.seed(2021)
# 5 repeats of stratified 10-fold cross-validation
nof_rep = 5
n_fold = 10

result = vector('list',length(dist_files)*nof_rep*n_fold*length(k_levels))

# create test indices to be used in cross-validation
train_classes = train_data$class
cv_indices = generateCVRuns(train_classes, ntimes = nof_rep, nfold = n_fold, leaveOneOut = FALSE, stratified = TRUE)

iter=1
# loop to calculate best k value for nearest neighbour with 10-fold-cross-validation, returns accuracy of each run in each fold
for(m in 1:length(dist_files)){ 
    dist_mat = as.matrix(fread(dist_files[m], header=FALSE))
    
    for(i in 1:nof_rep){
        this_fold = cv_indices[[i]]
        
        for(j in 1:n_fold){
            test_indices = this_fold[[j]]
            
            for(k in 1:length(k_levels)){
                current_k = k_levels[k]
                current_fold = nn_classify(dist_mat, train_classes, test_indices, k = current_k)
                accuracy = sum(train_classes[test_indices]==current_fold$prediction$predicted)/length(test_indices)
                distance_representation_name =  gsub("hw3_TRAIN_PowerCons_", "", dist_files[m])
                tmp = data.table(representation = gsub("hw3_TRAIN_PowerCons_", "", dist_files[m]), distance_measure = gsub("hw3_TRAIN_PowerCons_", "", dist_files[m]), repid = i, foldid = j, k = current_k, acc = accuracy)
                result[[iter]] = tmp
                iter=iter+1
                
            }
            
        }
    
    }   
    
}

overall_results = rbindlist(result)
summary = overall_results[,list(avg_accuracy = mean(acc), sdev_accuracy=sd(acc)), by=list(distance_measure, k)]
summary[order(-avg_accuracy)]

overall_summary_table = data.table(dataset=dataset, type=type, distance_measure = "ERP", representation = "Raw", k="5", accuracy = summary[order(-avg_accuracy)][1,3])
bests_for_each_train_dataset = rbind(bests_for_each_train_dataset, overall_summary_table)
fwrite(bests_for_each_train_dataset, "hw3_bests_for_each_train_dataset.csv")

type = "TEST"
test_data = preprocess(dataset=dataset, type=type, file_format=file_format, to_long=FALSE)
test_data_long = preprocess(dataset=dataset, type=type, file_format=file_format, to_long=TRUE)
test_classes = test_data$class

# calculate representations for whole dataset
reps = representation(data=test_data, long_data=test_data_long, last_series_id = nrow(test_data), plot_represenations = FALSE)
fwrite(reps, sprintf('hw3_reps_%s_%s.csv', type, dataset), col.names=TRUE)
reps = fread(sprintf('hw3_reps_%s_%s.csv', type, dataset))

# since raw series performed the best in train set, only use this representation
raw_series = dcast(reps[,c(1,2,3,4)], id ~ time, value.var = "value")
test_representation_list = list(raw_series)

# since erp distance performed the best in train set, only use this distance measure
distance(dataset = dataset, reps = test_representation_list, rep_names = c("raw"), dist_names = c("erp"), type = type)

best_k_levels = 5
dist_files = list.files(pattern = paste0("hw3_TEST_", dataset))

print(dist_files[1])
dist_mat = as.matrix(fread(dist_files[1], header=FALSE))
    
best_k_level_for_current_approach = best_k_levels
ordered_neighbours = apply(dist_mat, 1, order)
nearest_class = apply(ordered_neighbours[1:best_k_level_for_current_approach,], 2, function(x) {test_classes[x]})
nearest_class = data.table(id=1:nrow(dist_mat), t(nearest_class))

long_nn_class = melt(nearest_class,'id')
class_counts = long_nn_class[,.N,list(id,value)]
class_counts[,predicted_prob:=N/best_k_level_for_current_approach]
wide_class_prob_predictions = dcast(class_counts, id~value, value.var='predicted_prob')
wide_class_prob_predictions[is.na(wide_class_prob_predictions)]=0
class_predictions = class_counts[,list(predicted = value[which.max(N)]), by=list(id)]
        
# create confusion matrix and report accuracy
true_status = test_classes
confusion_matrix = table(true_status, prediction=class_predictions$predicted)
print(confusion_matrix)
        
accuracy = sum(true_status==class_predictions$predicted)/length(class_predictions$predicted)
print(accuracy)

overall_summary_table = data.table(dataset=dataset, type=type, distance_measure = "ERP", representation = "Raw", k="5", accuracy = accuracy)
bests_for_each_test_dataset = rbind(bests_for_each_test_dataset, overall_summary_table)
# store the results 
fwrite(bests_for_each_test_dataset, 'hw3_bests_for_each_test_dataset.csv')

 # for example; 5-nearest neighbours of first instances are instances 122, 132, 74, 147 and 85
ordered_neighbours[1:5]

# they belong to the 2,2,1,2,1 clasess respectively
nearest_class[id==1]

# Among 5 neighbours, 3 of them belong to class 2, 2 of them belongs to class 1. Since the max occurance is 2 among these 5, one can predict the instance 1 as 2
class_counts[id==1]

dataset = "Earthquakes"
type = "TRAIN"
file_format = "txt"

train_data = preprocess(dataset=dataset, type=type, file_format=file_format, to_long=FALSE)
train_data_long = preprocess(dataset=dataset, type=type, file_format=file_format, to_long=TRUE)

# visualize time series based on class
ggplot(train_data_long, aes(time, value)) + geom_line(aes(color = as.character(id)), show.legend = FALSE) +
     facet_grid(class~.)

representation(data=train_data, long_data=train_data_long, last_series_id = 1, plot_represenations = TRUE)

# calculate representations for whole dataset
reps = representation(data=train_data, long_data=train_data_long, last_series_id = nrow(train_data), plot_represenations = FALSE)
fwrite(reps, sprintf('hw3_reps_%s_%s.csv', type, dataset), col.names=TRUE)

# Create a overall accuracy table to find performance of representations and select the best setting among each representations
reps = fread(sprintf('hw3_reps_%s_%s.csv', type, dataset))
mse = melt(reps, id.vars=c("id", "class", "time"), measure.vars=c("value","tree_rep_default","tree_rep_tuned","paa_rep_sl=5", "paa_rep_sl=20"))
mse[,class:=as.character(class)]
mse = merge(mse, train_data_long[,list(id, class, time, obs=value)], by=c("id",  "class", "time"))
mse[!(variable=="value"), list(mse=mean((value-obs)^2)),list(variable)]

# change selected raw_series and selected representations to long format and store it in a list
raw_series = dcast(reps[,c(1,2,3,4)], id ~ time, value.var = "value")
tree_rep_tuned = dcast(reps[,c(1,2,3,6)], id ~ time, value.var = "tree_rep_tuned")
paa_rep_sl_5 = dcast(reps[,c(1,2,3,7)], id ~ time, value.var = "paa_rep_sl=5")
train_representation_list = list(raw_series, tree_rep_tuned, paa_rep_sl_5)

distance(dataset = dataset, reps = train_representation_list, rep_names = c("raw", "tree_rep_tuned", "paa_rep_sl=5"), dist_names = c("euc", "dtw", "lcss", "erp"),  type = type)

dist_files = list.files(pattern = paste0("hw3_TRAIN_", dataset))
dist_files

require(TunePareto)

k_levels = c(1, 3, 5)
set.seed(2021)
# 5 repeats of stratified 10-fold cross-validation
nof_rep = 5
n_fold = 10

result = vector('list',length(dist_files)*nof_rep*n_fold*length(k_levels))

# create test indices to be used in cross-validation
train_classes = train_data$class
cv_indices = generateCVRuns(train_classes, ntimes = nof_rep, nfold = n_fold, leaveOneOut = FALSE, stratified = TRUE)

iter=1
# loop to calculate best k value for nearest neighbour with 10-fold-cross-validation, returns accuracy of each run in each fold
for(m in 1:length(dist_files)){ 
    dist_mat = as.matrix(fread(dist_files[m], header=FALSE))
    
    for(i in 1:nof_rep){
        this_fold = cv_indices[[i]]
        
        for(j in 1:n_fold){
            test_indices = this_fold[[j]]
            
            for(k in 1:length(k_levels)){
                current_k = k_levels[k]
                current_fold = nn_classify(dist_mat, train_classes, test_indices, k = current_k)
                accuracy = sum(train_classes[test_indices]==current_fold$prediction$predicted)/length(test_indices)
                distance_representation_name =  gsub("hw3_TRAIN_Earthquakes_", "", dist_files[m])
                tmp = data.table(representation = gsub("hw3_TRAIN_Earthquakes_", "", dist_files[m]), distance_measure = gsub("hw3_TRAIN_Earthquakes_", "", dist_files[m]), repid = i, foldid = j, k = current_k, acc = accuracy)
                result[[iter]] = tmp
                iter=iter+1
                
            }
            
        }
    
    }   
    
}

overall_results = rbindlist(result)
summary = overall_results[,list(avg_accuracy = mean(acc), sdev_accuracy=sd(acc)), by=list(distance_measure, k)]
summary[order(-avg_accuracy)]

overall_summary_table = data.table(dataset=dataset, type=type, distance_measure = "LCSS", representation = "Raw", k="5", accuracy = summary[order(-avg_accuracy)][1,3])
bests_for_each_train_dataset = fread("hw3_bests_for_each_train_dataset.csv", header=TRUE)
bests_for_each_train_dataset = rbind(bests_for_each_train_dataset, overall_summary_table)
fwrite(bests_for_each_train_dataset, "hw3_bests_for_each_train_dataset.csv")

type = "TEST"
test_data = preprocess(dataset=dataset, type=type, file_format=file_format, to_long=FALSE)
test_data_long = preprocess(dataset=dataset, type=type, file_format=file_format, to_long=TRUE)
test_classes = test_data$class

# calculate representations for whole dataset
reps = representation(data=test_data, long_data=test_data_long, last_series_id = nrow(test_data), plot_represenations = FALSE)
fwrite(reps, sprintf('hw3_reps_%s_%s.csv', type, dataset), col.names=TRUE)
reps = fread(sprintf('hw3_reps_%s_%s.csv', type, dataset))

# since raw series performed the best in train set, only use this representation
raw_series = dcast(reps[,c(1,2,3,4)], id ~ time, value.var = "value")
test_representation_list = list(raw_series)

# since lcss distance performed the best in train set, only use this distance measure
distance(dataset = dataset, reps = test_representation_list, rep_names = c("raw"), dist_names = c("lcss"), type = type)

best_k_levels = 5
dist_files = list.files(pattern = paste0("hw3_TEST_", dataset))

print(dist_files[1])
dist_mat = as.matrix(fread(dist_files[1], header=FALSE))
    
best_k_level_for_current_approach = best_k_levels
ordered_neighbours = apply(dist_mat, 1, order)
nearest_class = apply(ordered_neighbours[1:best_k_level_for_current_approach,], 2, function(x) {test_classes[x]})
nearest_class = data.table(id=1:nrow(dist_mat), t(nearest_class))

long_nn_class = melt(nearest_class,'id')
class_counts = long_nn_class[,.N,list(id,value)]
class_counts[,predicted_prob:=N/best_k_level_for_current_approach]
wide_class_prob_predictions = dcast(class_counts, id~value, value.var='predicted_prob')
wide_class_prob_predictions[is.na(wide_class_prob_predictions)]=0
class_predictions = class_counts[,list(predicted = value[which.max(N)]), by=list(id)]
        
# create confusion matrix and report accuracy
true_status = test_classes
confusion_matrix = table(true_status, prediction=class_predictions$predicted)
print(confusion_matrix)
        
accuracy = sum(true_status==class_predictions$predicted)/length(class_predictions$predicted)
print(accuracy)

overall_summary_table = data.table(dataset=dataset, type=type, distance_measure = "LCSS", representation = "Raw", k="5", accuracy = accuracy)
bests_for_each_test_dataset = fread("hw3_bests_for_each_test_dataset.csv", header=TRUE)
bests_for_each_test_dataset = rbind(bests_for_each_test_dataset, overall_summary_table)
fwrite(bests_for_each_test_dataset, "hw3_bests_for_each_test_dataset.csv")

dataset = "ECG200"
type = "TRAIN"
file_format = "txt"

train_data = preprocess(dataset=dataset, type=type, file_format=file_format, to_long=FALSE)
train_data_long = preprocess(dataset=dataset, type=type, file_format=file_format, to_long=TRUE)

# visualize time series based on class
ggplot(train_data_long, aes(time, value)) + geom_line(aes(color = as.character(id)), show.legend = FALSE) +
     facet_grid(class~.)

representation(data=train_data, long_data=train_data_long, last_series_id = 1, plot_represenations = TRUE)

# calculate representations for whole dataset
reps = representation(data=train_data, long_data=train_data_long, last_series_id = nrow(train_data), plot_represenations = FALSE)
fwrite(reps, sprintf('hw3_reps_%s_%s.csv', type, dataset), col.names=TRUE)

# Create a overall accuracy table to find performance of representations and select the best setting among each representations
reps = fread(sprintf('hw3_reps_%s_%s.csv', type, dataset))
mse = melt(reps, id.vars=c("id", "class", "time"), measure.vars=c("value","tree_rep_default","tree_rep_tuned","paa_rep_sl=5", "paa_rep_sl=20"))
mse[,class:=as.character(class)]
mse = merge(mse, train_data_long[,list(id, class, time, obs=value)], by=c("id",  "class", "time"))
mse[!(variable=="value"), list(mse=mean((value-obs)^2)),list(variable)]

# change selected raw_series and selected representations to long format and store it in a list
raw_series = dcast(reps[,c(1,2,3,4)], id ~ time, value.var = "value")
tree_rep_tuned = dcast(reps[,c(1,2,3,6)], id ~ time, value.var = "tree_rep_tuned")
paa_rep_sl_5 = dcast(reps[,c(1,2,3,7)], id ~ time, value.var = "paa_rep_sl=5")
train_representation_list = list(raw_series, tree_rep_tuned, paa_rep_sl_5)

distance(dataset = dataset, reps = train_representation_list, rep_names = c("raw", "tree_rep_tuned", "paa_rep_sl=5"), dist_names = c("euc", "dtw", "lcss", "erp"),  type = type)

dist_files = list.files(pattern = paste0("hw3_TRAIN_", dataset))
dist_files

require(TunePareto)

k_levels = c(1, 3, 5)
set.seed(2021)
# 5 repeats of stratified 10-fold cross-validation
nof_rep = 5
n_fold = 10

result = vector('list',length(dist_files)*nof_rep*n_fold*length(k_levels))

# create test indices to be used in cross-validation
train_classes = train_data$class
cv_indices = generateCVRuns(train_classes, ntimes = nof_rep, nfold = n_fold, leaveOneOut = FALSE, stratified = TRUE)

iter=1
# loop to calculate best k value for nearest neighbour with 10-fold-cross-validation, returns accuracy of each run in each fold
for(m in 1:length(dist_files)){ 
    dist_mat = as.matrix(fread(dist_files[m], header=FALSE))
    
    for(i in 1:nof_rep){
        this_fold = cv_indices[[i]]
        
        for(j in 1:n_fold){
            test_indices = this_fold[[j]]
            
            for(k in 1:length(k_levels)){
                current_k = k_levels[k]
                current_fold = nn_classify(dist_mat, train_classes, test_indices, k = current_k)
                accuracy = sum(train_classes[test_indices]==current_fold$prediction$predicted)/length(test_indices)
                distance_representation_name =  gsub("hw3_TRAIN_ECG200_", "", dist_files[m])
                tmp = data.table(representation = gsub("hw3_TRAIN_ECG200_", "", dist_files[m]), distance_measure = gsub("hw3_TRAIN_ECG200_", "", dist_files[m]), repid = i, foldid = j, k = current_k, acc = accuracy)
                result[[iter]] = tmp
                iter=iter+1
                
            }
            
        }
    
    }   
    
}

overall_results = rbindlist(result)
summary = overall_results[,list(avg_accuracy = mean(acc), sdev_accuracy=sd(acc)), by=list(distance_measure, k)]
summary[order(-avg_accuracy)]

overall_summary_table = data.table(dataset=dataset, type=type, distance_measure = "LCSS", representation = "Raw", k="3", accuracy = summary[order(-avg_accuracy)][1,3])
bests_for_each_train_dataset = fread("hw3_bests_for_each_train_dataset.csv", header=TRUE)
bests_for_each_train_dataset = rbind(bests_for_each_train_dataset, overall_summary_table)
fwrite(bests_for_each_train_dataset, "hw3_bests_for_each_train_dataset.csv")

type = "TEST"
test_data = preprocess(dataset=dataset, type=type, file_format=file_format, to_long=FALSE)
test_data_long = preprocess(dataset=dataset, type=type, file_format=file_format, to_long=TRUE)
test_classes = test_data$class

# calculate representations for whole dataset
reps = representation(data=test_data, long_data=test_data_long, last_series_id = nrow(test_data), plot_represenations = FALSE)
fwrite(reps, sprintf('hw3_reps_%s_%s.csv', type, dataset), col.names=TRUE)
reps = fread(sprintf('hw3_reps_%s_%s.csv', type, dataset))

# since raw series performed the best in train set, only use this representation
raw_series = dcast(reps[,c(1,2,3,4)], id ~ time, value.var = "value")
test_representation_list = list(raw_series)

# since lcss distance performed the best in train set, only use this distance measure
distance(dataset = dataset, reps = test_representation_list, rep_names = c("raw"), dist_names = c("lcss"), type = type)

best_k_levels = 3
dist_files = list.files(pattern = paste0("hw3_TEST_", dataset))

print(dist_files[1])
dist_mat = as.matrix(fread(dist_files[1], header=FALSE))
    
best_k_level_for_current_approach = best_k_levels
ordered_neighbours = apply(dist_mat, 1, order)
nearest_class = apply(ordered_neighbours[1:best_k_level_for_current_approach,], 2, function(x) {test_classes[x]})
nearest_class = data.table(id=1:nrow(dist_mat), t(nearest_class))

long_nn_class = melt(nearest_class,'id')
class_counts = long_nn_class[,.N,list(id,value)]
class_counts[,predicted_prob:=N/best_k_level_for_current_approach]
wide_class_prob_predictions = dcast(class_counts, id~value, value.var='predicted_prob')
wide_class_prob_predictions[is.na(wide_class_prob_predictions)]=0
class_predictions = class_counts[,list(predicted = value[which.max(N)]), by=list(id)]
        
# create confusion matrix and report accuracy
true_status = test_classes
confusion_matrix = table(true_status, prediction=class_predictions$predicted)
print(confusion_matrix)
        
accuracy = sum(true_status==class_predictions$predicted)/length(class_predictions$predicted)
print(accuracy)

overall_summary_table = data.table(dataset=dataset, type=type, distance_measure = "LCSS", representation = "Raw", k="3", accuracy = accuracy)
bests_for_each_test_dataset = fread("hw3_bests_for_each_test_dataset.csv", header=TRUE)
bests_for_each_test_dataset = rbind(bests_for_each_test_dataset, overall_summary_table)
fwrite(bests_for_each_test_dataset, "hw3_bests_for_each_test_dataset.csv")

dataset = "Plane"
type = "TRAIN"
file_format = "txt"

train_data = preprocess(dataset=dataset, type=type, file_format=file_format, to_long=FALSE)
train_data_long = preprocess(dataset=dataset, type=type, file_format=file_format, to_long=TRUE)

# visualize time series based on class
ggplot(train_data_long, aes(time, value)) + geom_line(aes(color = as.character(id)), show.legend = FALSE) +
     facet_wrap(class~.)

representation(data=train_data, long_data=train_data_long, last_series_id = 1, plot_represenations = TRUE)

# calculate representations for whole dataset
reps = representation(data=train_data, long_data=train_data_long, last_series_id = nrow(train_data), plot_represenations = FALSE)
fwrite(reps, sprintf('hw3_reps_%s_%s.csv', type, dataset), col.names=TRUE)

# Create a overall accuracy table to find performance of representations and select the best setting among each representations
reps = fread(sprintf('hw3_reps_%s_%s.csv', type, dataset))
mse = melt(reps, id.vars=c("id", "class", "time"), measure.vars=c("value","tree_rep_default","tree_rep_tuned","paa_rep_sl=5", "paa_rep_sl=20"))
mse[,class:=as.character(class)]
mse = merge(mse, train_data_long[,list(id, class, time, obs=value)], by=c("id",  "class", "time"))
mse[!(variable=="value"), list(mse=mean((value-obs)^2)),list(variable)]

# change selected raw_series and selected representations to long format and store it in a list
raw_series = dcast(reps[,c(1,2,3,4)], id ~ time, value.var = "value")
tree_rep_tuned = dcast(reps[,c(1,2,3,6)], id ~ time, value.var = "tree_rep_tuned")
paa_rep_sl_5 = dcast(reps[,c(1,2,3,7)], id ~ time, value.var = "paa_rep_sl=5")
train_representation_list = list(raw_series, tree_rep_tuned, paa_rep_sl_5)

distance(dataset = dataset, reps = train_representation_list, rep_names = c("raw", "tree_rep_tuned", "paa_rep_sl=5"), dist_names = c("euc", "dtw", "lcss", "erp"),  type = type)

dist_files = list.files(pattern = paste0("hw3_TRAIN_", dataset))
dist_files

require(TunePareto)

k_levels = c(1, 3, 5)
set.seed(2021)
# 5 repeats of stratified 10-fold cross-validation
nof_rep = 5
n_fold = 10

result = vector('list',length(dist_files)*nof_rep*n_fold*length(k_levels))

# create test indices to be used in cross-validation
train_classes = train_data$class
cv_indices = generateCVRuns(train_classes, ntimes = nof_rep, nfold = n_fold, leaveOneOut = FALSE, stratified = TRUE)

iter=1
# loop to calculate best k value for nearest neighbour with 10-fold-cross-validation, returns accuracy of each run in each fold
for(m in 1:length(dist_files)){ 
    dist_mat = as.matrix(fread(dist_files[m], header=FALSE))
    
    for(i in 1:nof_rep){
        this_fold = cv_indices[[i]]
        
        for(j in 1:n_fold){
            test_indices = this_fold[[j]]
            
            for(k in 1:length(k_levels)){
                current_k = k_levels[k]
                current_fold = nn_classify(dist_mat, train_classes, test_indices, k = current_k)
                accuracy = sum(train_classes[test_indices]==current_fold$prediction$predicted)/length(test_indices)
                distance_representation_name =  gsub("hw3_TRAIN_Plane_", "", dist_files[m])
                tmp = data.table(representation = gsub("hw3_TRAIN_Plane_", "", dist_files[m]), distance_measure = gsub("hw3_TRAIN_Plane_", "", dist_files[m]), repid = i, foldid = j, k = current_k, acc = accuracy)
                result[[iter]] = tmp
                iter=iter+1
                
            }
            
        }
    
    }   
    
}

overall_results = rbindlist(result)
summary = overall_results[,list(avg_accuracy = mean(acc), sdev_accuracy=sd(acc)), by=list(distance_measure, k)]
summary[order(-avg_accuracy)]

overall_summary_table = data.table(dataset=dataset, type=type, distance_measure = "LCSS", representation = "Raw", k="5", accuracy = summary[order(-avg_accuracy)][1,3])
bests_for_each_train_dataset = fread("hw3_bests_for_each_train_dataset.csv", header=TRUE)
bests_for_each_train_dataset = rbind(bests_for_each_train_dataset, overall_summary_table)
fwrite(bests_for_each_train_dataset, "hw3_bests_for_each_train_dataset.csv")

type = "TEST"
test_data = preprocess(dataset=dataset, type=type, file_format=file_format, to_long=FALSE)
test_data_long = preprocess(dataset=dataset, type=type, file_format=file_format, to_long=TRUE)
test_classes = test_data$class

# calculate representations for whole dataset
reps = representation(data=test_data, long_data=test_data_long, last_series_id = nrow(test_data), plot_represenations = FALSE)
fwrite(reps, sprintf('hw3_reps_%s_%s.csv', type, dataset), col.names=TRUE)
reps = fread(sprintf('hw3_reps_%s_%s.csv', type, dataset))

# since raw series performed the best in train set, only use this representation
raw_series = dcast(reps[,c(1,2,3,4)], id ~ time, value.var = "value")
test_representation_list = list(raw_series)

# since lcss distance performed the best in train set, only use this distance measure
distance(dataset = dataset, reps = test_representation_list, rep_names = c("raw"), dist_names = c("lcss"), type = type)

best_k_levels = 5
dist_files = list.files(pattern = paste0("hw3_TEST_", dataset))

print(dist_files[1])
dist_mat = as.matrix(fread(dist_files[1], header=FALSE))
    
best_k_level_for_current_approach = best_k_levels
ordered_neighbours = apply(dist_mat, 1, order)
nearest_class = apply(ordered_neighbours[1:best_k_level_for_current_approach,], 2, function(x) {test_classes[x]})
nearest_class = data.table(id=1:nrow(dist_mat), t(nearest_class))

long_nn_class = melt(nearest_class,'id')
class_counts = long_nn_class[,.N,list(id,value)]
class_counts[,predicted_prob:=N/best_k_level_for_current_approach]
wide_class_prob_predictions = dcast(class_counts, id~value, value.var='predicted_prob')
wide_class_prob_predictions[is.na(wide_class_prob_predictions)]=0
class_predictions = class_counts[,list(predicted = value[which.max(N)]), by=list(id)]
        
# create confusion matrix and report accuracy
true_status = test_classes
confusion_matrix = table(true_status, prediction=class_predictions$predicted)
print(confusion_matrix)
        
accuracy = sum(true_status==class_predictions$predicted)/length(class_predictions$predicted)
print(accuracy)

overall_summary_table = data.table(dataset=dataset, type=type, distance_measure = "LCSS", representation = "Raw", k="5", accuracy = accuracy)
bests_for_each_test_dataset = fread("hw3_bests_for_each_test_dataset.csv", header=TRUE)
bests_for_each_test_dataset = rbind(bests_for_each_test_dataset, overall_summary_table)
fwrite(bests_for_each_test_dataset, "hw3_bests_for_each_test_dataset.csv")

dataset = "SyntheticControl"
type = "TRAIN"
file_format = "txt"

train_data = preprocess(dataset=dataset, type=type, file_format=file_format, to_long=FALSE)
train_data_long = preprocess(dataset=dataset, type=type, file_format=file_format, to_long=TRUE)

# visualize time series based on class
ggplot(train_data_long, aes(time, value)) + geom_line(aes(color = as.character(id)), show.legend = FALSE) +
     facet_wrap(class~.)

representation(data=train_data, long_data=train_data_long, last_series_id = 1, plot_represenations = TRUE)

# calculate representations for whole dataset
reps = representation(data=train_data, long_data=train_data_long, last_series_id = nrow(train_data), plot_represenations = FALSE)
fwrite(reps, sprintf('hw3_reps_%s_%s.csv', type, dataset), col.names=TRUE)

# Create a overall accuracy table to find performance of representations and select the best setting among each representations
reps = fread(sprintf('hw3_reps_%s_%s.csv', type, dataset))
reps[,value:=as.numeric(value)]
mse = melt(reps, id.vars=c("id", "class", "time"), measure.vars=c("value","tree_rep_default","tree_rep_tuned","paa_rep_sl=5", "paa_rep_sl=20"))
mse[,class:=as.character(class)]
mse = merge(mse, train_data_long[,list(id, class, time, obs=value)], by=c("id",  "class", "time"))
mse[!(variable=="value"), list(mse=mean((value-obs)^2)),list(variable)]

# change selected raw_series and selected representations to long format and store it in a list
raw_series = dcast(reps[,c(1,2,3,4)], id ~ time, value.var = "value")
tree_rep_tuned = dcast(reps[,c(1,2,3,6)], id ~ time, value.var = "tree_rep_tuned")
paa_rep_sl_5 = dcast(reps[,c(1,2,3,7)], id ~ time, value.var = "paa_rep_sl=5")
train_representation_list = list(raw_series, tree_rep_tuned, paa_rep_sl_5)

distance(dataset = dataset, reps = train_representation_list, rep_names = c("raw", "tree_rep_tuned", "paa_rep_sl=5"), dist_names = c("euc", "dtw", "lcss", "erp"),  type = type)

dist_files = list.files(pattern = paste0("hw3_TRAIN_", dataset))
dist_files

require(TunePareto)

k_levels = c(1, 3, 5)
set.seed(2021)
# 5 repeats of stratified 10-fold cross-validation
nof_rep = 5
n_fold = 10

result = vector('list',length(dist_files)*nof_rep*n_fold*length(k_levels))

# create test indices to be used in cross-validation
train_classes = train_data$class
cv_indices = generateCVRuns(train_classes, ntimes = nof_rep, nfold = n_fold, leaveOneOut = FALSE, stratified = TRUE)

iter=1
# loop to calculate best k value for nearest neighbour with 10-fold-cross-validation, returns accuracy of each run in each fold
for(m in 1:length(dist_files)){ 
    dist_mat = as.matrix(fread(dist_files[m], header=FALSE))
    
    for(i in 1:nof_rep){
        this_fold = cv_indices[[i]]
        
        for(j in 1:n_fold){
            test_indices = this_fold[[j]]
            
            for(k in 1:length(k_levels)){
                current_k = k_levels[k]
                current_fold = nn_classify(dist_mat, train_classes, test_indices, k = current_k)
                accuracy = sum(train_classes[test_indices]==current_fold$prediction$predicted)/length(test_indices)
                distance_representation_name =  gsub("hw3_TRAIN_SyntheticControl_", "", dist_files[m])
                tmp = data.table(representation = gsub("hw3_TRAIN_SyntheticControl_", "", dist_files[m]), distance_measure = gsub("hw3_TRAIN_SyntheticControl_", "", dist_files[m]), repid = i, foldid = j, k = current_k, acc = accuracy)
                result[[iter]] = tmp
                iter=iter+1
                
            }
            
        }
    
    }   
    
}

overall_results = rbindlist(result)
summary = overall_results[,list(avg_accuracy = mean(acc), sdev_accuracy=sd(acc)), by=list(distance_measure, k)]
summary[order(-avg_accuracy)]

overall_summary_table = data.table(dataset=dataset, type=type, distance_measure = "EUC", representation = "PAA", k="3", accuracy = summary[order(-avg_accuracy)][1,3])
bests_for_each_train_dataset = fread("hw3_bests_for_each_train_dataset.csv", header=TRUE)
bests_for_each_train_dataset = rbind(bests_for_each_train_dataset, overall_summary_table)
fwrite(bests_for_each_train_dataset, "hw3_bests_for_each_train_dataset.csv")

type = "TEST"
test_data = preprocess(dataset=dataset, type=type, file_format=file_format, to_long=FALSE)
test_data_long = preprocess(dataset=dataset, type=type, file_format=file_format, to_long=TRUE)
test_classes = test_data$class

# calculate representations for whole dataset
reps = representation(data=test_data, long_data=test_data_long, last_series_id = nrow(test_data), plot_represenations = FALSE)
fwrite(reps, sprintf('hw3_reps_%s_%s.csv', type, dataset), col.names=TRUE)
reps = fread(sprintf('hw3_reps_%s_%s.csv', type, dataset))

# since paa performed the best in train set, only use this representation
paa_rep_sl_5 = dcast(reps[,c(1,2,3,7)], id ~ time, value.var = "paa_rep_sl=5")
test_representation_list = list(paa_rep_sl_5)

# since euc distance performed the best in train set, only use this distance measure
distance(dataset = dataset, reps = test_representation_list, rep_names = c("paa"), dist_names = c("euc"), type = type)

best_k_levels = 3
dist_files = list.files(pattern = paste0("hw3_TEST_", dataset))

print(dist_files[1])
dist_mat = as.matrix(fread(dist_files[1], header=FALSE))
    
best_k_level_for_current_approach = best_k_levels
ordered_neighbours = apply(dist_mat, 1, order)
nearest_class = apply(ordered_neighbours[1:best_k_level_for_current_approach,], 2, function(x) {test_classes[x]})
nearest_class = data.table(id=1:nrow(dist_mat), t(nearest_class))

long_nn_class = melt(nearest_class,'id')
class_counts = long_nn_class[,.N,list(id,value)]
class_counts[,predicted_prob:=N/best_k_level_for_current_approach]
wide_class_prob_predictions = dcast(class_counts, id~value, value.var='predicted_prob')
wide_class_prob_predictions[is.na(wide_class_prob_predictions)]=0
class_predictions = class_counts[,list(predicted = value[which.max(N)]), by=list(id)]
        
# create confusion matrix and report accuracy
true_status = test_classes
confusion_matrix = table(true_status, prediction=class_predictions$predicted)
print(confusion_matrix)
        
accuracy = sum(true_status==class_predictions$predicted)/length(class_predictions$predicted)
print(accuracy)

overall_summary_table = data.table(dataset=dataset, type=type, distance_measure = "EUC", representation = "PAA", k="3", accuracy = accuracy)
bests_for_each_test_dataset = fread("hw3_bests_for_each_test_dataset.csv", header=TRUE)
bests_for_each_test_dataset = rbind(bests_for_each_test_dataset, overall_summary_table)
fwrite(bests_for_each_test_dataset, "hw3_bests_for_each_test_dataset.csv")

bests_for_each_test_dataset = fread("hw3_bests_for_each_test_dataset.csv", header=TRUE)
bests_for_each_test_dataset

bests_for_each_train_dataset = fread("hw3_bests_for_each_train_dataset.csv", header=TRUE)
bests_for_each_train_dataset

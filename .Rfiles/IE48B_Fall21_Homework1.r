options(scipen=999)

require(data.table)

# assuming you have the XYZ cordinates in your working directory in the following format:
# 'working_directory/'
current_folder = getwd()

file_names = list.files(pattern = "uWaveGestureLibrary")
list_files = lapply(file_names, read.table)

list_files = lapply(seq_along(list_files), function(x) {
  list_files[[x]]$V0 = 1:nrow(list_files[[x]]) # column VO is created to specify the sample instance
  list_files[[x]]
})

x <- as.data.table(list_files[1])
y <- as.data.table(list_files[2])
z <- as.data.table(list_files[3])

    
# melt X cordinates for long format
setnames(x,"V1","class")
setnames(x,"V0","instance")
x = melt(x,id.vars=c('class','instance'))
x[,time:=as.numeric(gsub("\\D", "", variable))-1]
x = x[,list(class,instance, time, value)]
x = x[order(instance, class,time)]
setnames(x, "value", "x_cordinate")


# melt Y cordinates for long format
setnames(y,"V1","class")
setnames(y,"V0","instance")
y = melt(y,id.vars=c('class','instance'))
y[,time:=as.numeric(gsub("\\D", "", variable))-1]
y = y[,list(class,instance, time, value)]
y = y[order(instance, class, time)]
setnames(y, "value", "y_cordinate")

# melt Z cordinates  for long format
setnames(z,"V1","class")
setnames(z,"V0","instance")
z = melt(z,id.vars=c('class','instance'))
z[,time:=as.numeric(gsub("\\D", "", variable))-1]
z = z[,list(class, instance, time, value)]
z = z[order(instance, class, time)]
setnames(z, "value", "z_cordinate")


xy = merge(x,
            y,
            by=c("class","instance","time"))
            

xyz = merge(xy,
            z,
            by=c("class","instance","time"))

# shows x,y,z cordinates of one instance belongs to class 1
head(xyz)

require(lattice)

# instances belong to class 1, 2, 3, 4, 5, 6, 7, 8 respectively
samples <- xyz[instance %in% c(11, 15, 4, 5, 2, 1, 7, 6),]

velocity_samples = samples[, list(x_cordinate=cumsum(x_cordinate),
                                  y_cordinate=cumsum(y_cordinate),
                                  z_cordinate=cumsum(z_cordinate)), list(class, instance)]

cloud(z_cordinate~x_cordinate*y_cordinate|as.factor(class), data=velocity_samples, xlab="x", ylab="y", zlab="z")

require(ggplot2)
require(repr)

options(repr.plot.width=15, repr.plot.height=8)

ggplot(samples, aes(time)) +  geom_line(aes(y = x_cordinate,  colour = "x"), size=1) + geom_line(aes(y = y_cordinate, colour = "y"), size=1) +  geom_line(aes(y = z_cordinate, colour = "z"), size=1) + scale_color_discrete(name = "Cordinates") + facet_wrap(~class) + ylab("cordinates")

xyz[, interval_index:=as.numeric(cut(time, 2, ordered_result=T), list(instance))]
interval_statistic = xyz[, list(x_median=median(x_cordinate), y_median=median(y_cordinate), z_median=median(z_cordinate)), list(class,instance,interval_index)]
head(interval_statistic)

# represent each series with the interval means
interval_statistic_x = dcast(interval_statistic, instance+class~paste0('interval_',interval_index),value.var='x_median')
interval_statistic_x$statistic <- "x"

interval_statistic_y = dcast(interval_statistic, instance+class~paste0('interval_',interval_index),value.var='y_median')
interval_statistic_y$statistic <- "y"

interval_statistic_z = dcast(interval_statistic, instance+class~paste0('interval_',interval_index),value.var='z_median')
interval_statistic_z$statistic <- "z"

summary = rbind(interval_statistic_x, interval_statistic_y, interval_statistic_z)
summary[, class:=as.factor(class)]

ggplot(summary, aes(x = interval_1, y = interval_2, color = class)) + geom_point(size = 3) + facet_wrap(~statistic) 

ar_xyz = copy(xyz)
ar_xyz = ar_xyz[order(instance,time)]

ar_xyz[,x_lag1:=shift(x_cordinate,1), list(instance)]
ar_xyz[,x_lag2:=shift(x_cordinate,2), list(instance)]
ar_xyz[,y_lag1:=shift(y_cordinate,1), list(instance)]
ar_xyz[,y_lag2:=shift(y_cordinate,2), list(instance)]
ar_xyz[,z_lag1:=shift(z_cordinate,1), list(instance)]
ar_xyz[,z_lag2:=shift(z_cordinate,2), list(instance)]

fit_ar2 = function(xyz){
    
    fit = lm(x_cordinate~x_lag1+x_lag2+y_lag1+y_lag2+z_lag1+z_lag2, xyz)
    return(fit)
}

class_type=unique(ar_xyz$class)
fitted_models=lapply(class_type, function(x) fit_ar2(ar_xyz[class==x]))

ar_xyz_residuals = copy(ar_xyz)   
                     
for(i in 1:length(class_type)){
    
    current_class=class_type[i]
    ar_xyz_residuals[,paste0('residual_',current_class):=x_cordinate-predict(fitted_models[[i]], ar_xyz_residuals)] 
}

residual_statistics = ar_xyz_residuals[,list(mean_residual_1=mean(residual_1, na.rm=T),
                                             mean_residual_2=mean(residual_2, na.rm=T),
                                             mean_residual_3=mean(residual_3, na.rm=T),
                                             mean_residual_4=mean(residual_4, na.rm=T),
                                             mean_residual_5=mean(residual_5, na.rm=T),
                                             mean_residual_6=mean(residual_6, na.rm=T),
                                             mean_residual_7=mean(residual_7, na.rm=T),
                                             mean_residual_7=mean(residual_8, na.rm=T)), list(instance, class)]

residual_statistics = melt(residual_statistics, id.vars=c('instance','class'))

ggplot(residual_statistics, aes(x=variable, y=value, color=variable)) + geom_boxplot() + facet_wrap(~class) +  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

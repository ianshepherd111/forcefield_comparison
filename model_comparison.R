#!/usr/bin/env Rscript --vanilla

# rm(list = ls())


main_dir <- "~/Desktop"
in_dir <- "csv_files"
out_dir <- "output"

#read-in data from csv files
setwd(file.path(main_dir, in_dir))

for (i in 1:7){
  assign(  paste0("df",i), read.csv(paste0("set_",i,".csv"),
                                    header=F,

                                    col.names=c("experiment", "calc_1", "calc_2", "calc_3")
  )
  )
}

#create output directory and set as wd
ifelse(
  !dir.exists(file.path(main_dir, out_dir)),
  dir.create(file.path(main_dir, out_dir)), FALSE
)

setwd(file.path(main_dir, out_dir))


average_deviation <- character()
average_sq_deviation <- character()
# create column of means, calculate deviation of mean from each experimental value
# calculate deviation squared, take average of both over all 47 molecules
for (i in 1:7){
  datacols<-eval(parse(text = paste0("df",i,"[2:4]") ))
  assign(  "calc_mean", rowMeans(datacols)  )
  assign(
    "dat",
    eval(parse(text = paste0("df",i,"[1]") ))
  )
  assign(  "deviation", abs(calc_mean-dat)  )
  assign(  "dev_sq", deviation^2  )
  colnames(deviation)[colnames(deviation) == 'experiment'] <- 'absolute_deviation'
  colnames(dev_sq)[colnames(dev_sq) == 'experiment'] <- 'deviation_squared'

  assign(
    paste0("df",i),
    cbind( eval(parse(text = paste0("df",i) )),  calc_mean, deviation, dev_sq)
  )

  average_deviation <- c(average_deviation, (sum(deviation))/(nrow(deviation)) )
  average_sq_deviation <- c(average_sq_deviation, (sum(dev_sq))/(nrow(deviation)) )

}

av_deviations<-cbind(average_deviation,average_sq_deviation)


#
# Merge datasets for ANOVA
#

# get the length of the dataset(s)
df_curr<-t(eval(parse(text=paste0("df",i,"[1]"))) )
datalen<-length(df_curr)


# create "num" vector containing set number (group number) then append as column to data
for (i in 1:7){

  num <- rep( paste0(i) ,datalen)

  name<-paste0("df",i)

  assign(
    name,
    data.frame( eval(parse(text=name)) , group_no = num)
  )

}

# merge all the datasets together
merged_data<-do.call("rbind", list(df1, df2, df3, df4, df5, df6, df7))



# SUBGROUPS
# generate sub datasets and then merge them
# index numbers become non-sequential so that indexes dont overlap



# subgroup 1
for (i in 1:7){

  partname_1<-paste0("df",i,"[1:4,]")
  partname_2<-paste0("df",i,"[22:27,]")
  subname<-paste0("df",i,"_s1")

  assign(
    subname,
    data.frame(rbind(eval(parse(text=partname_1)), eval(parse(text=partname_2)) ) )
  )

}
s1<-do.call("rbind", list(df1_s1, df2_s1, df3_s1, df4_s1, df5_s1, df6_s1, df7_s1))



# subgroup 2
for (i in 1:7){

  partname_1<-paste0("df",i,"[5:9,]")
  partname_2<-paste0("df",i,"[28:35,]")
  subname<-paste0("df",i,"_s2")

  assign(
    subname,
    data.frame(rbind(eval(parse(text=partname_1)), eval(parse(text=partname_2)) ) )
  )

}
s2<-do.call("rbind", list(df1_s2, df2_s2, df3_s2, df4_s2, df5_s2, df6_s2, df7_s2))



# subgroup 3
for (i in 1:7){

  partname_1<-paste0("df",i,"[10:15,]")
  partname_2<-paste0("df",i,"[36:39,]")
  subname<-paste0("df",i,"_s3")

  assign(
    subname,
    data.frame(rbind(eval(parse(text=partname_1)), eval(parse(text=partname_2)) ) )
  )

}
s3<-do.call("rbind", list(df1_s3, df2_s3, df3_s3, df4_s3, df5_s3, df6_s3, df7_s3))



# subgroup 4
for (i in 1:7){

  partname_1<-paste0("df",i,"[16,]")
  partname_2<-paste0("df",i,"[40:46,]")
  subname<-paste0("df",i,"_s4")

  assign(
    subname,
    data.frame(rbind(eval(parse(text=partname_1)), eval(parse(text=partname_2)) ) )
  )

}
s4<-do.call("rbind", list(df1_s4, df2_s4, df3_s4, df4_s4, df5_s4, df6_s4, df7_s4))



# subgroup 5
for (i in 1:7){

  partname_1<-paste0("df",i,"[17:21,]")
  partname_2<-paste0("df",i,"[47,]")
  subname<-paste0("df",i,"_s5")

  assign(
    subname,
    data.frame(rbind(eval(parse(text=partname_1)), eval(parse(text=partname_2)) ) )
  )

}
s5<-do.call("rbind", list(df1_s5, df2_s5, df3_s5, df4_s5, df5_s5, df6_s5, df7_s5))


#
# Output graphs of deviations
#

# print out averages
sink('average_deviations.txt')
cat(sprintf ("Protocol no. \x20\x20\x20 Deviation \x20\x20\x20 Squared deviation \n") )
for (i in 1:7){
  cat(sprintf("%d %s %s\n", i, av_deviations[i,1], av_deviations[i,2]) )
}
sink()


devplot_a<-as.numeric(av_deviations[,1])
devplot_b<-as.numeric(av_deviations[,2])
devplot<-cbind(devplot_a,devplot_b)
devplot<-t(devplot)

colnames(devplot) <- c("1","2","3","4","5","6","7")
colours <- c("red", "blue")
leg<-c("Absolute", "Squared")

pdf("deviations.pdf")
barplot(devplot,
        main="Averaged absolute deviations and squared deviations by protocol",
        xlab="Protocol number",
        ylab="Deviation",
        beside=TRUE,
        col=colours,
        legend=leg
)
graphics.off()


pdf("absolute_deviations.pdf")
devplot_a<-t(devplot_a)
colnames(devplot_a) <- c("1","2","3","4","5","6","7")
barplot(devplot_a,
        main="Averaged absolute deviation by protocol",
        xlab="Protocol number",
        ylab="Deviation",
        beside=TRUE,
        col="red"
)
graphics.off()


pdf("squared_deviations.pdf")
devplot_b<-t(devplot_b)
colnames(devplot_b) <- c("1","2","3","4","5","6","7")
barplot(devplot_b,
        main="Averaged squared deviation by protocol",
        xlab="Protocol number",
        ylab="Deviation",
        beside=TRUE,
        col="blue"
)
graphics.off()



#
# ANOVA on datasets and subsets
#

# All data
groupFactor = as.factor(merged_data$group_no)

pdf("all_data.pdf")
plot(
  merged_data$absolute_deviation ~ groupFactor,
  main="Deviation of the calculated mean from the \nexperimental value for each compound",
  xlab="Protocol number",
  ylab="Absolute deviation"
  )
graphics.off()

sink('all_data_ANOVA.txt')
cat(sprintf ("ANOVA output: \n") )
dataAnova = aov(merged_data$absolute_deviation ~ groupFactor)
summary(dataAnova)
cat(sprintf ("\n\n\nTukeyHSD output: \n") )
TukeyHSD(dataAnova)
sink()


# subgroup 1
groupFactor = as.factor(s1$group_no)

pdf("sub1.pdf")
plot(
  s1$absolute_deviation ~ groupFactor,
  main="Deviation of the calculated mean from the \nexperimental value for compounds in subset 1",
  xlab="Protocol number",
  ylab="Absolute deviation"
)
graphics.off()

sink('subset_1_ANOVA.txt')
cat(sprintf ("ANOVA output: \n") )
dataAnova = aov(s1$absolute_deviation ~ groupFactor)
summary(dataAnova)
cat(sprintf ("\n\n\nTukeyHSD output: \n") )
TukeyHSD(dataAnova)
sink()


# subgroup 2
groupFactor = as.factor(s2$group_no)

pdf("sub2.pdf")
plot(
  s2$absolute_deviation ~ groupFactor,
  main="Deviation of the calculated mean from the \nexperimental value for compounds in subset 2",
  xlab="Protocol number",
  ylab="Absolute deviation"
)
graphics.off()

sink('subset_2_ANOVA.txt')
cat(sprintf ("ANOVA output: \n") )
dataAnova = aov(s2$absolute_deviation ~ groupFactor)
summary(dataAnova)
cat(sprintf ("\n\n\nTukeyHSD output: \n") )
TukeyHSD(dataAnova)
sink()


# subgroup 3
groupFactor = as.factor(s3$group_no)

pdf("sub3.pdf")
plot(
  s3$absolute_deviation ~ groupFactor,
  main="Deviation of the calculated mean from the \nexperimental value for compounds in subset 3",
  xlab="Protocol number",
  ylab="Absolute deviation"
)
graphics.off()

sink('subset_3_ANOVA.txt')
cat(sprintf ("ANOVA output: \n") )
dataAnova = aov(s3$absolute_deviation ~ groupFactor)
summary(dataAnova)
cat(sprintf ("\n\n\nTukeyHSD output: \n") )
TukeyHSD(dataAnova)
sink()



# subgroup 4
groupFactor = as.factor(s4$group_no)

pdf("sub4.pdf")
plot(
  s4$absolute_deviation ~ groupFactor,
  main="Deviation of the calculated mean from the \nexperimental value for compounds in subset 4",
  xlab="Protocol number",
  ylab="Absolute deviation"
)
graphics.off()

sink('subset_4_ANOVA.txt')
cat(sprintf ("ANOVA output: \n") )
dataAnova = aov(s4$absolute_deviation ~ groupFactor)
summary(dataAnova)
cat(sprintf ("\n\n\nTukeyHSD output: \n") )
TukeyHSD(dataAnova)
sink()


# subgroup 5
groupFactor = as.factor(s5$group_no)

pdf("sub5.pdf")
plot(
  s5$absolute_deviation ~ groupFactor,
  main="Deviation of the calculated mean from the \nexperimental value for compounds in subset 5",
  xlab="Protocol number",
  ylab="Absolute deviation"
)
graphics.off()

sink('subset_5_ANOVA.txt')
cat(sprintf ("ANOVA output: \n") )
dataAnova = aov(s5$absolute_deviation ~ groupFactor)
summary(dataAnova)
cat(sprintf ("\n\n\nTukeyHSD output: \n") )
TukeyHSD(dataAnova)
sink()

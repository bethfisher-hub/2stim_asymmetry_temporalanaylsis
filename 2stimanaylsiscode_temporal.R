# Code from Ariel and Yusuke 

### Filename we're going to play with
filename = "/Users/bethfisher/Downloads/color_eccen_2stim_during_20_10_04.csv"

stimuli_number = 9
response_type_list = c('during')
response_names = c('During')
trial_types = c('during')
catch_trial_number = 20

trace_cutoff = 2 # mean dissimilarity for physically identical colours must be below this
antitrace_cutoff = 3.5 # mean dissimilarity accepted for maximally physically different colours must be above this
rt_cutoff = 700 # mean reaction times must be above this

exclude_noncompliant = FALSE

plotsubjects = FALSE
plot_within_between = FALSE
plotexpsummary = FALSE
across = FALSE
population = FALSE

# live dangerously, get rid of pesky warnings
oldw <- getOption("warn")
options(warn = -1)

shhh <- suppressPackageStartupMessages # stops annoying warnings when loading libraries
library(tidyr)
library(plyr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(MASS)
library(Matrix)
library(reshape2)
library(ape) # stats
library(vegan) # stats
library(RColorBrewer)
library(cocor)
library(DescTools)
library(reshape2)
library(grid)
library(ggplotify)


# rainbowcloud theme for plotting, stolen from: 
# https://micahallen.org/2018/03/15/introducing-raincloud-plots/?utm_campaign=News&utm_medium=Community&utm_source=DataCamp.com
raincloud_theme = theme(
text = element_text(size = 10),
axis.title.x = element_text(size = 16),
axis.title.y = element_text(size = 16),
axis.text = element_text(size = 14),
axis.text.x = element_text(angle = 45, vjust = 0.5),
legend.title=element_text(size=16),
legend.text=element_text(size=16),
legend.position = "right",
plot.title = element_text(lineheight=.8, face="bold", size = 16),
panel.border = element_blank(),
panel.grid.minor = element_blank(),
panel.grid.major = element_blank(),
axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))

# stealing ability to make flat violin plots
source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")


# Plot a dissimilarity matrix
dissimplot <- function(datadf,colors,dependent='color'){
    
    plot <- ggplot(datadf, aes(x = Color_1, y = Color_2)) +
      theme(axis.text.x = element_text(colour = colors), axis.text.y = element_text(colour = colors),
                      axis.title.x = element_blank(), axis.title.y = element_blank(),
                      plot.title = element_text(hjust = 0.5))
    
    # stuff that's standard across plot types
        plot <- plot + geom_raster(aes(fill = similarity)) +
                labs(title = 'Temporal Plot') +
                scale_fill_gradientn(colours = c("white","black")) +
                guides(fill=guide_legend(title="Dissimilarity"))
    return(plot)
}


# Similarity judgment histogram
simhistplot <- function(datadf){
    
   plot <- ggplot(datadf, aes(x = similarity)) + geom_bar(aes(y = ..prop..)) +
    scale_x_discrete(limits=c(0,1,2,3,4,5,6,7), name = 'Dissimilarity') +
    ylab('Frequency') + ylim(0,0.8)
    return(plot)
}

simhistplot_summary <- function(datadf){
    
    datadf$subject <- as.character(datadf$subject) # necessary for visualisation
    
    plot <- ggplot(datadf, aes(x = similarity)) + 
    geom_line(stat='count',aes(y = ..prop..,group = subject),color='#CC9933') +
    geom_line(stat='count',aes(y = ..prop..),size=1.5) +
    scale_x_discrete(limits=c(0,1,2,3,4,5,6,7), name = 'Dissimilarity') +
    ylab('Frequency') + ylim(0,0.8)
    return(plot)
    
}

# reaction time for each similarity
rsplot <- function(datadf){
    
    plot <- ggplot(datadf, aes(x= similarity, y=response_time)) + 
    stat_summary(fun.y = mean, geom = "bar") + 
    stat_summary(fun.data = mean_se, geom = "errorbar", size =0.5, aes(width=0.5)) +
    scale_x_discrete(limits=c(0,1,2,3,4,5,6,7), name = 'Dissimilarity') + ylab('Reaction Time (ms)') +
    theme(legend.position = "none") +
    ylim(0,4000) # anyone taking more than 4 seconds has probably mindwandered
    
    return(plot)
}

rsplot_summary <- function(datadf){
    
    datadf$subject <- as.character(datadf$subject) # necessary for visualisation
    
    plot <- ggplot(datadf, aes(x= similarity, y=response_time,group = subject, color = subject)) + 
    stat_summary(fun.y = mean, geom = "line", size=0.8) + 
    #stat_summary(fun.data = mean_se, geom = "errorbar", size =0.5, aes(width=0.5)) +
    scale_x_discrete(limits=c(0,1,2,3,4,5,6,7), name = 'Dissimilarity') + ylab('Mean Reaction Time (ms)') +
    theme(legend.position = "none") +
    ylim(0,4000) # anyone taking more than 4 seconds has probably mindwandered 
    
    return(plot)
    
}

# reaction time raincloud plot
rsplot_raincloud <- function(datadf,xtype='linear'){
    
    datadf$subject <- as.character(datadf$subject) # necessary for visualisation  
    datadf$similarity <- as.character(datadf$similarity) # necessary for visualisation
    
    ylabtext = 'Reaction Time (ms)'
    
    plot <- ggplot(datadf, aes(y = response_time, x = similarity, fill = similarity)) +
            geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
            geom_point(aes(y = response_time, color = similarity),
                   position = position_jitter(width = .15), size = .5, alpha = 0.8) +
            geom_boxplot(width = .1, guides = FALSE, outlier.shape = NA, alpha = 0.5) +
            expand_limits(x = 5.25) +
            guides(fill = FALSE) +
            guides(color = FALSE) +
            scale_color_brewer(palette = "Spectral") +
            scale_fill_brewer(palette = "Spectral") +
            xlab('Dissimilarity') + ylab("Reaction Time (ms)")
            # coord_flip() +
            theme_bw() +
            raincloud_theme
    
    if(xtype == 'log'){
        plot <- plot + scale_y_continuous(trans='log10')
    } else{
        plot <- plot + ylim(0,5000)
    }
    
    return(plot)
}

rsplot_raincloud(datadf,xtype='linear')

# correlation between reaction times and similarity judgements
# grouping at individual trial, individual participant, experiment or entire population level
rt_similarity_cor <- function(datadf,level='participant'){
        
    if(level=="participant"){
        datadf <- datadf %>% 
                group_by(subject) %>% 
                mutate(rt_similarity_correlation = cor(similarity,response_time))
        datadf <- aggregate(datadf, by=list(datadf$subject), FUN = mean)

                
    }
    return(datadf)
    
}


rt_similarity_plot <- function(datadf,xlabel='BLANK'){
    
    datadf <- rt_similarity_cor(datadf)
    
    datadf[xlabel] = xlabel
    
    plot <- ggplot(datadf,aes(x=xlabel,y=rt_similarity_correlation)) + 
                geom_boxplot() + 
                geom_dotplot(binaxis='y',stackdir='center',dotsize=0.75) +
                theme(text = element_text(size=15)) + xlab("")
                ggtitle(title)
    
    plot <- plot + ylab("Correlation (Spearman)") + ylim(-1,1)
    plot <- plot + geom_hline(yintercept=0, linetype="dashed", color = "blue")
    return(plot)
}


# subject info
sumplot <- function(datadf){
    
    # change ms to s, add the delay for each trial
    datadf$response_time <- ((datadf$response_time + 0.125*nrow(datadf)) / 1000)
    
    plot <- ggplot(datadf, aes(x=subject, y = response_time)) + 
    stat_summary(fun.y = sum, geom = "bar") + ylim(0,1000) +
    ylab('Response Time Total') + theme(axis.title.x=element_blank(), axis.text.x = element_text(size=20))
    
    return(plot)
}


# get median reaction time
rt_avg <- function(datadf){
    return(median(datadf$response_time))
}


# function to aggregate everyone's data together
aggregate_df <- function(datadf,dependent='color'){

    # aggregate everyone's data together for the matrices
    everyonedata <- aggregate(datadf, by=list(
        datadf$Color_1,
        datadf$Color_2,
        datadf$Circle_1,
        datadf$Circle_2,
        datadf$bin1,
        datadf$bin2
        ), FUN=mean, 
    )

    # correct the column names
    everyonedata$Color_1 <- everyonedata$Group.1
    everyonedata$Color_2 <- everyonedata$Group.2
    everyonedata$Circle_1 <- everyonedata$Group.3
    everyonedata$Circle_2 <- everyonedata$Group.4
    everyonedata$bin1 <- everyonedata$Group.5
    everyonedata$bin2 <- everyonedata$Group.6
    
    return(everyonedata)
}

# Data analysis 
datadf = read.csv(filename)
savestr <- substr(filename,1,nchar(filename)-4) # for saving related files later

# Remove practice trial data
datadf <- subset(datadf, trial_number != 0)
# changing color values from RGB to hex for graphing purpose
datadf$Color_1 <- as.character(datadf$Color_1)
datadf$Color_1 <- revalue(datadf$Color_1, 
                                                    c(  "1" = '#FF0000',
                                                        "2" = '#FFAA00',
                                                        "3" = '#AAFF00',
                                                        "4" = '#00FF00',
                                                        "5" = '#00FFA9',
                                                        "6" = '#00A9FF',
                                                        "7" = '#0000FF',
                                                        "8" = '#AA00FF',
                                                        "9" = '#FF00AA'))
datadf$Color_2 <- as.character(datadf$Color_2)
datadf$Color_2 <- revalue(datadf$Color_2, 
                                                    c(  "1" = '#FF0000',
                                                        "2" = '#FFAA00',
                                                        "3" = '#AAFF00',
                                                        "4" = '#00FF00',
                                                        "5" = '#00FFA9',
                                                        "6" = '#00A9FF',
                                                        "7" = '#0000FF',
                                                        "8" = '#AA00FF',
                                                        "9" = '#FF00AA'))

# colors for the labels
# red, orange, yellow, green, cyan, cyan-blue, blue, purple, pink
colors <- c('#FF0000','#FFAA00','#AAFF00','#00FF00','#00FFA9','#00A9FF','#0000FF','#AA00FF','#FF00AA')
# can change the way the plot line up
# red, pink, orange, purple, yellow, blue, green, cyan-blue, cyan
#colors <- c('#FF0000','#FF00AA','#FFAA00','#AA00FF','#AAFF00','#0000FF','#00FF00','#00A9FF','#00FFA9')
abcolors <- sort(colors) # this was messing up the asymmetry plot, maybe useful for some other stuff

# changing from int indicators in the .csv file to more readable labels for eccentricity
foveal = -1
peripheral = 1

# set the maximum and minimum dissimilarity values for later analysis
min_val = 0
max_val = 6

# calculate the catch trial score for a subject
catch_score <- function(datadf){
  datadf <- subset(datadf, trial_type == 'catch')
  datadf$correct <- ifelse(datadf$similarity == datadf$catch_vals, 1, 0) # determine whether they got the catch trials right
  score <- sum(datadf$correct)/nrow(datadf) # get the score
  return(score)
}


# catch trial checker
catch_trial_checker <- function(datadf){
  
  subjectlist <- sort(unique(datadf$subject))
  print("Catch scores")
  for (subjectid in subjectlist){
    subjectdf = subset(datadf, subject == subjectid)
    
    catch_trials <- subset(subjectdf, trial_type == 'catch')
    catch_num = nrow(catch_trials)
    catch_correct = nrow(subset(catch_trials, catch_vals == similarity))
    
    print(paste("Subject",subjectid,":",catch_correct,"/",catch_num))
  }
}

# Remove catch trials 
datadf <- subset(datadf, trial_type != 'catch')

# screen parameters
screen_parameters <- function(datadf,individual=FALSE){
  
  subjectlist <- sort(unique(datadf$subject))
  print("Screen Parameters")
  screen_fail = 0
  viewing_fail = 0
  for (subjectid in subjectlist){
    subjectdf = subset(datadf, subject == subjectid)
    
    screen_size <- round(screen_size(subjectdf)/10,1)
    viewing_distance <- round(view_distance(subjectdf)/10,1)
    
    if(screen_size < 20){screen_fail = screen_fail + 1}
    if(viewing_distance < 30){viewing_fail = viewing_fail + 1}
    
    if(individual){
      print(paste("Subject",subjectid,":"))
      print(paste("Screen size:",screen_size,"cm"))
      print(paste("Viewing distance:",viewing_distance,"cm"))
      print("")
    }
    
    
  }
  print("")
  print(paste("Screen size issues:",screen_fail,"/",length(subjectlist)))
  print(paste("Viewing distance issues:",viewing_fail,"/",length(subjectlist)))
}


# factor the dataframes for the plot function
dissimdata2 <- function(datadf, colors){
    
    # refactor the levels so they can be plotted properly later if need be
    datadf$Color_1 <- with(datadf, factor(Color_1, levels = colors))
    datadf$Color_2 <- with(datadf, factor(Color_2, levels = colors))
    
    return(datadf)
}

quantify_asymmetry <- function(datadf){
    
    datadf <- dissimdata2(datadf, colors)
    
    # aggregate over the remaining columns of interest
    nmdsdata <- aggregate(datadf, by = list(datadf$Color_1, datadf$Color_2),FUN=mean)
    nmdsdata$Color_1 <- nmdsdata$Group.1
    nmdsdata$Color_2 <- nmdsdata$Group.2

    nmdsdata = subset(nmdsdata, select = c("Color_1","Color_2","similarity"))  # get rid of unnecessary columns
    
    nmdsmatrix <- spread(nmdsdata, Color_1, similarity) # convert the dataframe to a matrix
    nmdsmatrix <- data.matrix(nmdsmatrix) # change first column from colour to number (just some label stuff) 
    nmdsmatrix <- nmdsmatrix[,-1] # get rid of the labels in the first column, it messes up the code
    nmdsmatrix[is.na(nmdsmatrix)] <- 0  # change NA to 0 so sum can be calculated.
    
    matdf <- as.data.frame(as.vector(abs(nmdsmatrix - t(nmdsmatrix)))) # calculate the asymmetry
    asymmery_value <- sum(matdf)/2 # need to divide by 2 to get rid of the duplicates

    return(asymmery_value)
}


# return a list of the asymmetrical values for each subject
asymValues_list2 <- function(datadf){
    
    subjectlist <- sort(unique(datadf$subject)) # obtain a list of all the subjects
    
    asymValues_list <- vector() # array to store the values in
    
    for (ID in subjectlist){ # go through subject by subject
        subjectdf = subset(datadf, subject == ID) # select the ID for subject of interest
        asymValues_list <- c(asymValues_list, quantify_asymmetry(subjectdf))
    }
    return(asymValues_list)
}


# Dissimplot for all data 
dissimplot_temporal <- function(datadf,colors,dependent='color'){
    
    # refine data using function "dissimdata2 "
    datatemp <- dissimdata2(datadf, colors)

    plot <- ggplot(datatemp, aes(x = Color_1, y = Color_2)) +
    theme(axis.text.x = element_text(colour = colors), axis.text.y = element_text(colour = colors),
                      axis.title.x = element_blank(), axis.title.y = element_blank(),
                      plot.title = element_text(hjust = 0.5))
    
    # stuff that's standard across plot types
        plot <- plot + geom_raster(aes(fill = similarity)) +
                labs(title = 'Presented - Response Screen') +
                scale_fill_gradientn(colours = c("white","black")) +
                guides(fill=guide_legend(title="Dissimilarity"))
    return(plot)
}


# Plot a dismiliarity matrix for each subject manually 
dissimplot_temporal_subject <- function(datadf, colors, ID){
  
  #Subset data for the subject 
  subjectdf = subset(datadf, subject == ID) 
  
  # labeling the types
  label1 <- "Subject ID:"
  label2 <- ID
  
  # refine data using function "dissimdata2 "
  datatemp <- dissimdata2(subjectdf, colors)
  
  plot <- ggplot(datatemp, aes(x = Color_1, y = Color_2)) +
    theme(axis.text.x = element_text(colour = colors), axis.text.y = element_text(colour = colors),
          axis.title.x = element_blank(), axis.title.y = element_blank(),
          plot.title = element_text(hjust = 0.5))
  
  # stuff that's standard across plot types
  plot <- plot + geom_raster(aes(fill = similarity)) +
    labs(title = label1, label2, sep = " to ") +
    scale_fill_gradientn(colours = c("white","black")) +
    guides(fill=guide_legend(title="Dissimilarity"))
  return(plot)
}

# Plot a dissimilarity matrix for each subject by going through a list 

temp_dissim <- function(datadf){
  subjectlist <- sort(unique(datadf$subject)) # obtain a list of all the subjects
  plot_list <- list()
  k = 0
  
  for (ID in subjectlist){ # go through subject by subject
    k = k + 1
    subjectdf = subset(datadf, subject == ID) # select the ID for subject of interest
    plot <- dissimplot_temporal_subject(subjectdf,colors,ID)
    plot_list[[k]] <- as.grob(plot) # add it to the plot_list
    
  }
  g <- marrangeGrob(plot_list, nrow=2,ncol=2) # need to manually change layout
  return(g)
}

# Asymmtery matrix temporal

df2mat_asymmetry_temporal <- function(datadf){
    
    datatemp <- dissimdata2(datadf, colors)
    
    # aggregate over the remaining columns of interest
    nmdsdata <- aggregate(datatemp, by = list(datatemp$Color_1, datatemp$Color_2),FUN=mean)
    nmdsdata$Color_1 <- nmdsdata$Group.1
    nmdsdata$Color_2 <- nmdsdata$Group.2

    nmdsdata = subset(nmdsdata, select = c("Color_1","Color_2","similarity"))  # get rid of unnecessary columns
    nmdsmatrix <- spread(nmdsdata, Color_1, similarity) # convert the dataframe to a matrix
    #nmdsmatrix[is.na(nmdsmatrix)] <- 0  # change NA to 0 
    nmdsmatrix <- data.matrix(nmdsmatrix) # change first column from colour to number(just some label stuff) 
    nmdsmatrix <- nmdsmatrix[,-1] # get rid of the labels in the first column, it messes up the code
    matdf <- as.data.frame(nmdsmatrix - t(nmdsmatrix)) # calculate the asymmetry
    #matdf$colorset <- c(abcolors) # adding additional column "colorset"
    matdf$colorset <- c(colors) # adding additional column "colorset"
    num_colors <- length(colors)
    matdf <- matdf %>% gather(othercolor,asymmetry ,1:num_colors) # convert the matrix back to the data frame which has the 
                                                                  # column "colortset", "othercolor", "asymmetry"
    return(matdf)
}



# plot an asymmetry matrix for all data
asymmetry_plot_temporal <- function(datadf, colors){

  datatemp <- df2mat_asymmetry_temporal(datadf)
  
  # refactor the levels so they can be plotted properly later if need be
  datatemp$colorset <- with(datatemp, factor(colorset, levels = colors))
  datatemp$othercolor <- with(datatemp, factor(othercolor, levels = colors))
  
  plot <- ggplot(datatemp, aes(x = colorset, y = othercolor)) +
    theme(axis.text.x = element_text(colour = colors), axis.text.y = element_text(colour = colors),
          axis.title.x = element_blank(), axis.title.y = element_blank(),
          #axis.title.x = element_text("left"), axis.title.y = element_text("right"),
          plot.title = element_text(hjust = 0.5))
  
  # stuff that's standard across plot types
  plot <- plot + geom_raster(aes(fill = asymmetry)) +
    labs(title = 'Presented - Response Screen') +
    scale_fill_gradientn(colours = c("blue","white","red"), limits = c(-4,4)) +
    guides(fill=guide_legend(title="Dissimilarity\nAsymmetry"))
  return(plot)
}


# Plot an asymmetry matrix for each subject manually 
asymmetry_plot_temporal_subject <- function(datadf, colors, ID){
  
  subjectdf = subset(datadf, subject == ID) 
  
  # labeling the types
  label1 <- "Subject ID:"
  label2 <- ID
  
  datatemp <- df2mat_asymmetry_temporal(subjectdf)
  
  # refactor the levels so they can be plotted properly later if need be
  datatemp$colorset <- with(datatemp, factor(colorset, levels = colors))
  datatemp$othercolor <- with(datatemp, factor(othercolor, levels = colors))
  
  plot <- ggplot(datatemp, aes(x = colorset, y = othercolor)) +
    theme(axis.text.x = element_text(colour = colors), axis.text.y = element_text(colour = colors),
          axis.title.x = element_blank(), axis.title.y = element_blank(),
          #axis.title.x = element_text("left"), axis.title.y = element_text("right"),
          plot.title = element_text(hjust = 0.5))
  
  # stuff that's standard across plot types
  plot <- plot + geom_raster(aes(fill = asymmetry)) +
    labs(title = paste(label1, label2, sep = " to ")) +
    scale_fill_gradientn(colours = c("blue","white","red"), limits = c(-4,4)) +
    guides(fill=guide_legend(title="Dissimilarity\nAsymmetry"))
  return(plot)
}
  
# Plot asymmetry plot for each subject by going through list 

temp_asymplot <- function(datadf){
  # matrix to use to define the plot layout, specified manually for now
  subjectlist <- sort(unique(datadf$subject)) # obtain a list of all the subjects
  plot_list <- list()
  k = 0
    
    for (ID in subjectlist){ # go through subject by subject
      k = k + 1
      subjectdf = subset(datadf, subject == ID) # select the ID for subject of interest
      plot <- asymmetry_plot_temporal_subject(subjectdf,colors,ID)
      plot_list[[k]] <- as.grob(plot) # add it to the plot_list
      
    }
    g <- marrangeGrob(plot_list, nrow=2,ncol=2)
    return(g)
  }












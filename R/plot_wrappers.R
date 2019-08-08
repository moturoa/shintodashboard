stacked_barplot <- function(data, 
                            group, 
                            title="", 
                            xlab="",
                            ylab="",
                            order=FALSE, 
                            rotate_xlabs = FALSE, 
                            print=TRUE,
                            return_levs=FALSE){
  
  data$group <- data[,group]
  
  if(order){
    data <- mutate(data, Group = reorder_within(group, gender, "F"))
  } else {
    data <- mutate(data, Group = factor(group))
  }
  
  p <- ggplot(data, aes(x=Group, fill=gender)) +
    geom_bar(stat="count", position=position_fill(reverse=TRUE)) +
    theme_sage() +
    labs(x=xlab, y="Gender Balance", title=title) +
    scale_fill_manual(values=sage_cols) +
    scale_y_continuous(labels = scales::percent, 
                       breaks=c(0,0.25,0.5,0.75,1), 
                       limits=c(0,1.01), expand=c(0, 0))
  if(rotate_xlabs){
    p <- p + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
  }
  
  if(print){
    print(p)
    if(return_levs)return(levels(data$Group))
  } else {
    return(p)
  }
}


grouped_barplot <- function(data, group, xlab="", ylab="Number of persons", 
                            title="", order=FALSE, rotate_xlabs = FALSE, print=TRUE){
  
  data$group <- data[,group]
  
  if(order){
    data <- mutate(data, Group = reorder_within(group, gender, "F"))
  } else {
    data <- mutate(data, Group = group)
  }
  
  p <- ggplot(data, aes(x=Group, fill=gender)) +
    geom_bar(stat="count", position=position_dodge()) +
    theme_sage() +
    labs(x=xlab, y=ylab, title=title) +
    scale_fill_manual(values=sage_cols)
  
  if(rotate_xlabs){
    p <- p + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
  }
  
  if(print){
    print(p)
  } else {
    return(p)
  }
}

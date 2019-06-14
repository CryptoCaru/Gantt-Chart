library(reshape2)
library(ggplot2)
library(RColorBrewer)

gantt_chart <- function(d,sol_vector,names){
  solution_matrix <- d[,sol_vector]
  gantt_1 <- solution_matrix[1,]
  gantt_2 <- cumsum(gantt_1)
  
  len <- ncol(solution_matrix) 
  x <- list()
  
  # build job 1
  x[[1]] <- data.frame("Start"=0, "End"=gantt_2[1])
  col <- solution_matrix[,1]
  for(k in 2:nrow(solution_matrix)){
    x[[1]] <- rbind(x[[1]], data.frame("Start"=x[[1]][k-1,2], "End"=(x[[1]][k-1,2] + col[k])))
  }
  
  # build job 2 to job n
  for(i in 2:len){
    x[[i]] <- data.frame("Start"=gantt_2[i-1], "End"=gantt_2[i])
    col <- solution_matrix[,i]
    for(k in 2:nrow(solution_matrix)){
      Max <- max(x[[i]][k-1,2], x[[i-1]][k,2])
      x[[i]] <- rbind(x[[i]], data.frame("Start"=Max, "End"=(Max + col[k])))
    }
  }
  
  # vector of machine names
  n_machine <- c()
  for(i in 1:nrow(x[[1]])){
    n_machine[i] <-  paste0("Machine ",i)
  }
  
  machine <- data.frame("Machine" = n_machine)
  
  # vector of jobs
  for(i in 1:length(x)){
    x[[i]] <- cbind(machine,x[[i]])
    jobby <- paste0("Job ", best_solution[i])
    Job <- c(1:nrow(x[[i]]))
    for(k in 1:nrow(x[[k]]))
      Job[k] <- jobby
    x[[i]] <- cbind(x[[i]], Job)
  }
  
  # from list to dataframe
  data <- x[[1]]
  for(i in 2:length(x)){
    data <- rbind(data,x[[i]])
  }
  
  cl <- colors(distinct = TRUE)
  set.seed(2603) # to set random generator seed
  colors_gantt <- sample(cl, len)
  
  data$Machine <- factor(data$Machine, levels = rev(n_machine))
  
  dfr <- data.frame(
    name        = factor(rev(data$Machine)), levels = rev(data$Machine),
    start.date  = rev(data$Start),
    end.date    = rev(data$End),
    Jobs = as.factor(rev(data$Job))
  )
  mdfr <- melt(dfr, measure.vars = c("start.date", "end.date"))
  grafico <- (ggplot(mdfr, aes(value, name, colour = Jobs)) + 
                geom_line(size = 6) +
                ylab(NULL) + 
                xlab(names)  + theme_bw() + theme(legend.position="top") + scale_colour_manual(values = colors_gantt))
  plot(grafico)
}        

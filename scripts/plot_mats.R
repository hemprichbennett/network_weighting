plot_t_list <- function(t_list){
  plot_list <- list()
  z <- 1
  for(i in 2:length(t_list)){
    chosen_mat <- t_list[[i]]
    
    g <- graph_from_data_frame(chosen_mat, directed = T)
    
    V(g)$guild <- ifelse(V(g)$name %in% chosen_mat$upper_sp, 'upper', 'lower')
    
    plot_list[[z]] <- ggraph(g) +
      geom_edge_link(alpha = .25, 
                     aes(width = 'weighting')) +
      geom_node_point(aes(color = guild, size = 2)) + 
      #geom_node_text(aes(label = name),  repel = TRUE)+
      theme_graph() +
      theme(legend.position = 'none')+ 
      ggtitle(names(t_list)[i])
    z <- z + 1
  }
  
  
  out_plot <- gridExtra::grid.arrange(plot_list[[1]], plot_list[[2]], plot_list[[3]], plot_list[[4]])
  return(out_plot)
}

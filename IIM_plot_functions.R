###############################################################################
##############  TECHNICAL COEFFICIENT REPRESENTATIONS    ######################
###############################################################################

#HEATMAP

set_A_heatmap <- function(IO_net_){
  IO_net_heat_ <- get.adjacency(IO_net_, attr="weight", sparse=F)
  colnames(IO_net_heat_) <- V(IO_net_)$name
  rownames(IO_net_heat_) <- V(IO_net_)$name
  return(IO_net_heat_)
}


plot_A_heatmap <- function(IO_net_){
  col_ <- colorRampPalette(brewer.pal(9,"YlOrRd"))(50)
  heatmap(IO_net_[,nrow(IO_net_):1],cexRow = 1, cexCol = 1,
          col = col_,
          Rowv = NA, Colv = NA, margins=c(4,4))
}


plot_A_pheatmap <- function(IO_net_, clust_ = 'cols'){
  fontsize_row = 10 - nrow(IO_net_) / 15
  #fontsize_row = 20
  if(clust_ == 'cols'){
    pheatmap(IO_net_, cluster_rows=F,
             color = colorRampPalette(brewer.pal(9,"YlOrRd"))(50),
             fontsize_row=fontsize_row, fontsize_col = fontsize_row, border_color=NA)
  }else if(clust_ == "rows"){
    pheatmap(IO_net_, cluster_cols=F,
             color = colorRampPalette(brewer.pal(9,"YlOrRd"))(50),
             fontsize_row=fontsize_row, fontsize_col = fontsize_row, border_color=NA)
    
  }else if(clust_ == "none"){
    pheatmap(IO_net_, cluster_cols=F,cluster_rows=F,
             color = colorRampPalette(brewer.pal(9,"YlOrRd"))(50),
             fontsize_row=fontsize_row, fontsize_col = fontsize_row, border_color=NA)
    
  }
}



###############################################################################
###################       DYNAMIC INOPERABILITY    ############################
###############################################################################



#DYNAMIC INOPERABILITY
#INPUT: q_dyn; OUTPUT: time plot for all sectors

plot_qdyn <- function(q_dyn_, length_ = -1){
  #Prepare ad-hoc dataframe for plotting time series
  if(length_==-1){
  df_ <- data.frame(x=1:length(q_dyn_[,1]),q_dyn_[,]) %>%
    gather(key = "variable", value = "value", -x)
  }else{
    df_ <- data.frame(x=1:length_,q_dyn_[1:length_,]) %>%
      gather(key = "variable", value = "value", -x)
  }
  #Plot sectors time series
  return(ggplot(df_, aes(x = x, y = value)) + 
    geom_line(aes(color = variable))+
    labs(color="Sectors")+
    xlab("time")+
    ylab("Inoperability q"))
}


plot_qdyn3D <- function(q_dyn_){
  fig_ <- plot_ly(z = ~q_dyn_)
  fig_ <- fig_ %>% add_surface(contours = list(
    z = list(
      show=TRUE,
      usecolormap=TRUE,
      highlightcolor="#ff0000",
      project=list(z=TRUE)
    )
  ))
  
  fig_ <- fig_ %>% layout(
    scene = list(
      camera=list(
        eye = list(x=1.87, y=0.88, z=-0.64)
      ),
      xaxis = list(title = "NACE r2"),
      yaxis = list(title = "Time"),
      zaxis = list(title = "Inoperability q"),
      yaxis = list(
        type = "category"
      )
    )
  )
  return(fig_)
}

plot_qdyn3D_mod <- function(q_dyn_){
  # reversing column order to have A on the left side
  fig_ <- plot_ly(z = ~q_dyn_[,order(colnames(q_dyn_), decreasing = TRUE)], colorbar = list(title = ""), width = 1200, height = 1000)
  fig_ <- fig_ %>% add_surface(contours = list(
    z = list(
      show=TRUE,
      usecolormap=TRUE,
      highlightcolor="#ff0000",
      project=list(z=TRUE)
    )
  ))
  
  fig_ <- fig_ %>% layout(
    scene = list(
      camera=list(
        #eye = list(x=1.87, y=0.88, z=-0.64)
        # view form the front (width = x = naces from left to right, depth = y = time, height = z = q)
        eye = list(x=0.4, y=2.2, z=0.8)
      ),
      legend = l,
      xaxis = list(title = "NACE",
                   tickvals = seq(1:65),
                   # column order reversed
                   ticktext = rev(colnames(q_dyn_)),
                   tickfont = list(size = 10)),
      yaxis = list(title = "Time"),
      zaxis = list(title = "Inoperability q")
    )
  )
  return(fig_)
}

# plot_2D <- function(q_dyn_){
#   df_ <- data.frame(t=1:nrow(q_dyn_),q_dyn_[,]) %>%
#     gather(key = "Sector", value = "y", -t)
#   fig_ <- plot_ly(df_, x = ~t, y = ~y, name= ~Sector, type = 'scatter', mode = 'lines')
#   return(fig_)
# }

# modified function with y-axis label
plot_2D <- function(q_dyn_, y_label_ = "y", length_ = -1, colors_ = -1,legend_=TRUE){
  # build data-frame

  
  if(length_==-1){
    df_ <- data.frame(t=1:nrow(q_dyn_),q_dyn_[,]) %>%
      gather(key = "Sector", value = "y", -t)
  }else{
    df_ <- data.frame(t=1:length_,q_dyn_[1:length_,]) %>%
      gather(key = "Sector", value = "y", -t)
  }
  
  # axis labels
  # the commented part is to customize the appearance
  #
  # f <- list(
  #   family = "Courier New, monospace",
  #   size = 18,
  #   color = "#7f7f7f"
  # )

  
  x_ <-  list(
    title = "Time [days]",
    visible = TRUE,
    #titlefont = f
    #mirror = TRUE,
    #tickmode = "auto",
    #nticks = 8,
    tickmode = "linear",
    tick0 = 0,
    dtick = 25
  )
  y_ <- list(
    title = y_label_
  )
  
  # plot
  
  if(colors_!=-1){
    fig_ <- plot_ly(df_, x = ~t, y = ~y, name= ~Sector, type = 'scatter', mode = 'lines',
                    color = ~Sector, colors = colors_)%>% 
      layout(xaxis = x_, yaxis = y_,showlegend = legend_)
  }else{
    fig_ <- plot_ly(df_, x = ~t, y = ~y, name= ~Sector, type = 'scatter', mode = 'lines')
    fig_ <- fig_ %>% layout(xaxis = x_, yaxis = y_,showlegend = legend_)
  }
  

  return(fig_)
}


###############################################################################
#########################       ECONOMIC LOSS     #############################
###############################################################################

#ECONOMIC LOSS - STEP BY STEP
plot_Qdyn <- function(Q_dyn_, time_def_ = "day"){
  #Prepare ad-hoc dataframe for plotting time series
  df_ <- data.frame(x=1:length(Q_dyn_[,1]),Q_dyn_[,]) %>%
    gather(key = "variable", value = "value", -x)
  
  #Plot sectors time series
  return(ggplot(df_, aes(x = x, y = value)) + 
           geom_line(aes(color = variable))+
           labs(color="Sectors")+
           xlab(paste("time [",time_def_,"]"))+
           ylab("Inoperability q"))
}


#CUMULATIVE ECONOMIC LOSS - STEP BY STEP
plot_Qdyn_cumulative <- function(Q_dyn_, time_def_ = "day"){
  #Prepare ad-hoc dataframe for plotting time series
  df_ <- data.frame(x=1:length(Q_dyn_[,1]),Q_dyn_[,]) %>%
    gather(key = "variable", value = "value", -x)
  
  #Plot sectors time series
  return(ggplot(df_, aes(x = x, y = value)) + 
           geom_line(aes(color = variable))+
           labs(color="Sectors")+
           xlab(paste("time [",time_def_,"]"))+
           ylab("Inoperability q"))
}


#TOTAL ECONOMIC LOSS - HISTOGRAM
plot_QTot_hist <- function(Q_){
  df_ <- data.frame(rownames(Q_),Q_)
  colnames(df_) <- c("NACE","Q")
  
  ggplot(data=df_, aes(x=NACE, y=Q))+
    geom_bar(stat="identity")+
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5,size=12))
}


plot_qperc_hist <- function(q_){
  df_ <- data.frame(rownames(q_),q_)
  colnames(df_) <- c("NACE","q")
  
  ggplot(data=df_, aes(x=NACE, y=q))+
    geom_bar(stat="identity")+
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5,size=12))
}

plot_qperc_hist_with_errorbar <- function(q_,y_label_ = "Total inoperability"){
  ggplot(data=q_)+
    geom_bar(aes(x=NACE, y=ymed),stat="identity")+
    geom_errorbar(aes(x=NACE,ymin=ymin,ymax=ymax),
                  width=0.4, colour="orange", alpha=0.9)+
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5,size=12))+
    labs(y = y_label_)
}

plot_QTot_hist_with_errorbar <- function(Q_, y_label_ = "Economic losses [mln euro]"){
  ggplot(data=Q_)+
    geom_bar(aes(x=NACE, y=ymed),stat="identity")+
    geom_errorbar(aes(x=NACE,ymin=ymin,ymax=ymax),
                  width=0.4, colour="orange", alpha=0.9)+
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5,size=12))+
    labs(y = y_label_)
}



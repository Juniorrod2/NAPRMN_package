Plot_ropls_scores <- function(model,groups=vector(),comp=c(1,2),group_colors=NULL,show_labels=F,point_size=4,plot_theme=ggpubr::theme_pubr()){

  if(model@typeC=="PCA"){

    PCA_data <- extract_ropls_data(model)


    if(length(groups)==0){

      if(show_labels==T){
        score_plot <- ggscatter(PCA_data$Scores,x=colnames(PCA_data$Scores[comp[1]+1]),
                                y=colnames(PCA_data$Scores[comp[2]+1]),color = "blue",
                                size = point_size,label = "Samples")
      }

      if(show_labels==F){
        score_plot <- ggscatter(PCA_data$Scores,x=colnames(PCA_data$Scores[comp[1]+1]),
                                y=colnames(PCA_data$Scores[comp[2]+1]),color = "red",
                                size = point_size)
      }


      score_plot <- score_plot+labs(x=paste("PC",comp[1]," (",PCA@modelDF$R2X[comp[1]]*100,"%)"),
                                    y=paste("PC",comp[2]," (",PCA@modelDF$R2X[comp[2]]*100,"%)"))
    }


    if(length(groups)!=0){

      PCA_data$Scores$Group <- groups

      if(show_labels==T){
        score_plot <- ggscatter(PCA_data$Scores,x=colnames(PCA_data$Scores[comp[1]+1]),
                                y=colnames(PCA_data$Scores[comp[2]+1]),color="Group",
                                size = point_size,label = "Samples")
      }

      if(show_labels==F){
        score_plot <- ggscatter(PCA_data$Scores,x=colnames(PCA_data$Scores[comp[1]+1]),
                                y=colnames(PCA_data$Scores[comp[2]+1]),color="Group",
                                size = point_size)
      }

      score_plot <- score_plot+
        stat_ellipse(aes(color=Group,fill=Group),geom = "polygon",alpha=0.3)+
        labs(x=paste("PC",comp[1]," (",PCA@modelDF$R2X[comp[1]]*100,"%)"),
             y=paste("PC",comp[2]," (",PCA@modelDF$R2X[comp[2]]*100,"%)"))

      if(!is.null(group_colors)){
        score_plot <-  score_plot+scale_color_manual(values = group_colors)+
          scale_fill_manual(values = group_colors)
      }
    }


    return(score_plot+plot_theme)

  }
}


Plot_scores <- function(model,groups=NULL,comp=c(1,2),point_size=4,ellipse=T,labels=T,color_labels=F,point=T,repel_labels=F,theme=ggpubr::theme_pubr()){

  if(model@typeC=="PCA"){

    PCA_data <- extract_ropls_data(model)
    if(!is.null(groups)){
      PCA_data$Scores$Group <- groups
      Group <- "Group"
      col_group <- Group
      group_alpha=0.3
      Legend=T
    }else{
      col_group = "black"
      Group <- NULL
      group_alpha=0
      Legend=F
    }

    if(ellipse==F){
      group_alpha <- 0
    }

    if(length(labels)==1&&labels==T){
      plot_label="Samples"
    }else{
      if(length(labels)==1&&labels==F){
        plot_label=NULL
      }else{
        PCA_data$Scores$Samples <- labels
        plot_label="Samples"
      }
    }
    if(point==F){
      color_labels=T
    }

    scores_plot <- ggscatter(PCA_data$Scores,x=paste("p",comp[1],sep = ""),y=paste("p",comp[2],sep = ""),color=col_group,
              size = point_size,label = plot_label,point = point,
              font.label = if(color_labels==F) c(14,"plain","black"),repel = repel_labels)+
      stat_ellipse(aes(color=if(ellipse==T) eval(Group),fill=if(ellipse==T) eval(Group)),geom = "polygon",alpha=group_alpha,show.legend = F)+
      labs(x=paste("PC",comp[1]," (",model@modelDF$R2X[comp[1]]*100,"%)"),
           y=paste("PC",comp[2]," (",model@modelDF$R2X[comp[2]]*100,"%)"),
           title = "PCA")+
      theme_bw()+theme(legend.text = element_text(size = 14),
                       axis.text = element_text(size=14),
                       axis.title = element_text(size = 14),
                       legend.position = "bottom")

  }

  if(model@typeC=="PLS-DA"){

    PLS_data <- extract_ropls_data(model)
    Group <- "Group"
    col_group <- Group
    group_alpha=0.3
    Legend=T

    if(ellipse==F){
      group_alpha <- 0
    }

    if(length(labels)==1&&labels==T){
      plot_label="Samples"
    }else{
      if(length(labels)==1&&labels==F){
        plot_label=NULL
      }else{
        PLS_data$Scores$Samples <- labels
        plot_label="Samples"
      }
    }
    if(point==F){
      color_labels=T
    }

    scores_plot <- ggscatter(PLS_data$Scores,x=paste("p",comp[1],sep = ""),y=paste("p",comp[2],sep = ""),color=col_group,
                             size = point_size,label = plot_label,point = point,
                             font.label = if(color_labels==F) c(14,"plain","black"),repel = repel_labels)+
      stat_ellipse(aes(color=if(ellipse==T) eval(Group),fill=if(ellipse==T) eval(Group)),geom = "polygon",alpha=group_alpha,show.legend = F)+
      labs(x=paste("Comp",comp[1]," (",model@modelDF$R2X[comp[1]]*100,"%)"),
           y=paste("Comp",comp[2]," (",model@modelDF$R2X[comp[2]]*100,"%)"),
           title = "PLS-DA")+
      theme_bw()+theme(legend.text = element_text(size = 14),
                       axis.text = element_text(size=14),
                       axis.title = element_text(size = 14),
                       legend.position = "bottom")

  }

  return(scores_plot+theme)
}

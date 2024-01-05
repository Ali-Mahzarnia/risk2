#install.packages("https://github.com/Ali-Mahzarnia/brainconn2/archive/master.tar.gz", repos = NULL, type="source")

library(brainconn2)
# t=which(connectivitvals!=0, arr.ind=TRUE)
# t_correct = t
# for (i in 1:(dim(t_correct)[1]*dim(t_correct)[2])) {
#         for (j in noreadcsf) {
#                 if (j <= t_correct[i]) {
#                         t_correct[i] = t_correct[i] + 1
#                 }
#         }
#         #t[i] = t[i] - 1
# }
# connectivitvals_new = connectivity [,,1] *0
# for (i in 1:dim(t)[1]) {
#         temindex_new = t_correct[i,]
#         temindex_old = t[i,]
#         connectivitvals_new[temindex_new[1], temindex_new[2]] = connectivitvals[temindex_old[1], temindex_old[2]] 
# }
connectivitvals_new = connectivitvals + t(connectivitvals)
isSymmetric(connectivitvals_new)

save(connectivitvals_new, file="human_scca_results84.rda")

brainconn(atlas ="Desikan84", conmat=connectivitvals_new, 
          view="left", node.size =2, 
          node.color = "black", 
          edge.width = 4, edge.color="red", 
          edge.alpha = 0.65,
          edge.color.weighted = T,
          scale.edge.width=T,
          labels = T,
          all.nodes =F, 
          show.legend = T, 
          label.size=7, background.alpha=1, 
          label.edge.weight=F, background = "ICBM152")  

library(ggplot2)
ggsave("glassleft.png", plot = last_plot(), 
       device='png', 
       scale=1, width=20, 
       height=20, unit=c("in"), dpi=400)




brainconn(atlas ="Desikan84", conmat=connectivitvals_new, 
          view="top", node.size =2, 
          node.color = "black", 
          edge.width = 4, edge.color="red", 
          edge.alpha = 0.65,
          edge.color.weighted = T,
          scale.edge.width=T,
          labels = T,
          all.nodes =F, 
          show.legend = T, 
          label.size=7, background.alpha=1, 
          label.edge.weight=F, background = "ICBM152")  

library(ggplot2)
ggsave("glasstop.png", plot = last_plot(), 
       device='png', 
       scale=1, width=20, 
       height=20, unit=c("in"), dpi=400)


brainconn(atlas ="Desikan84", conmat=connectivitvals_new, 
          view="front", node.size =2, 
          node.color = "black", 
          edge.width = 4, edge.color="red", 
          edge.alpha = 0.65,
          edge.color.weighted = T,
          scale.edge.width=T,
          labels = T,
          all.nodes =F, 
          show.legend = T, 
          label.size=5, background.alpha=1, 
          label.edge.weight=F, background = "ICBM152")  

library(ggplot2)
ggsave("glassfront.png", plot = last_plot(), 
       device='png', 
       scale=1, width=20, 
       height=20, unit=c("in"), dpi=400)

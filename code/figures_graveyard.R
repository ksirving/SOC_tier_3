# ## plot with thresholds
# labels <- c(MAX_depth_cm_Slice1 = "Slice 1", MAX_depth_cm_Slice2 = "Slice 2",
#             MAX_depth_cm_Slice3 = "Slice 3", MAX_depth_cm_Slice4 = "Slice 4")
# 
# png("figures/02_willow_seedling_Depth_Q.png", width = 500, height = 600)
# 
ggplot(all_data, aes(x = Q, y=prob_fit)) +
  geom_line(aes( group = variable, lty = variable)) +
  # scale_linetype_manual(values= c("dotted", "solid", "dashed"),
  #                       breaks=c("MAX_depth_cm_Slice1", "MAX_depth_cm_Slice2", "MAX_depth_cm_Slice3", "MAX_depth_cm_Slice4"))+
  facet_wrap(~variable, scales="free_x", nrow=3, labeller=labeller(variable = labels)) +

  # geom_point(data = subset(new_data, variable =="MAX_depth_cm_Slice1"), aes(y=50, x=newx1), color="green") +
  # geom_point(data = subset(new_data, variable =="MAX_depth_cm_Slice2"), aes(y=50, x=newx2), color="green") +
  # geom_point(data = subset(new_data, variable =="MAX_depth_cm_Slice3"), aes(y=50, x=newx3), color="green") +
  # geom_point(data = subset(new_data, variable =="MAX_depth_cm_Slice4"), aes(y=50, x=newx4), color="green") +

  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none") +
  labs(title = "Willow Seedling: Depth ~ Q",
       y = "Mortality (%)",
       x = "Q (cms)") #+ theme_bw(base_size = 15)

# dev.off()
### plot discharge over time
## bind dfs back together
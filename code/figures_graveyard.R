

# ## plot with thresholds
labels <- c(av.depth.m_slice2 = "Overbank")
# 
# png("figures/02_willow_seedling_Depth_Q.png", width = 500, height = 600)

new_data <- read.csv("output_data/02_example_node_for_figures.csv")
head(new_data)
new_data <- filter(new_data, Q < 60)

png("figures/Example_node_prob_v_Q.png", width = 500, height = 650)

ggplot(new_data, aes(x = Q, y=prob_fit)) +
  geom_line(aes( group = variable, lty = variable)) +
  # scale_linetype_manual(values= c("dotted", "solid", "dashed"),
  #                       breaks=c("MAX_depth_cm_Slice1", "MAX_depth_cm_Slice2", "MAX_depth_cm_Slice3", "MAX_depth_cm_Slice4"))+
  facet_wrap(~variable, scales="free_x", nrow=3, labeller=labeller(variable = labels)) +
 
  # geom_point(data = subset(new_data, variable =="MAX_depth_cm_Slice1"), aes(y=50, x=newx1), color="green") +
  # geom_point(data = subset(new_data, variable =="MAX_depth_cm_Slice2"), aes(y=50, x=newx2), color="green") +
  # geom_point(data = subset(new_data, variable =="MAX_depth_cm_Slice3"), aes(y=50, x=newx3), color="green") +
  # geom_point(data = subset(new_data, variable =="MAX_depth_cm_Slice4"), aes(y=50, x=newx4), color="green") +

  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none") +
  theme(text = element_text(size=20),
        axis.text.x = element_text(size = 20))+
  labs(title = "Example: Mortality ~ Q (Depth)",
       y = "Mortality (%)",
       x = "Q (cms)") #+ theme_bw(base_size = 15)

dev.off()
### plot discharge over time
## bind dfs back together

png("figures/Example_node_prob_v_Q.png", width = 500, height = 600)

ggplot(new_data, aes(x = subset(new_data, Q<100), y=prob_fit)) +
  geom_line(aes( group = variable, lty = variable)) +
  # scale_linetype_manual(values= c("dotted", "solid", "dashed"),
  #                       breaks=c("depth_cm_LOB", "depth_cm_MC", "depth_cm_ROB"))+
  # facet_wrap(~variable, scales="free_x", nrow=3, labeller=labeller(variable = labels)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none") +
  labs(title = "Example: Probability of Mortality Vs Q",
       y = "Probability of Mortality (%)",
       x = "Q (cms)") #+ theme_bw(base_size = 15)
dev.off()
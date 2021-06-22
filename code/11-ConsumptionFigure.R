# Supplementall Figure- consumption 

# plotting it (on the x axis) against the (our region-level) estimates (on the y axis) 
# of pre- and post-vaccination non-susceptibility.

data <- read_excel("/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/data/consumption.xlsx")
head(data)
names(data)[2] <- "regs"
names(data)[6] <- "mac"
names(data)[9] <- "pen"

data <- data %>% 
  dplyr::select(super_region, regs, country, mac, pen)

#merge data with pre-post estimates from supplement 
load("/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/data/pen_inv_consump.Rdata")
load("/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/data/pen_ninv_consump.Rdata")
load("/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/data/mac_inv_consump.Rdata")
load("/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/data/mac_ninv_consump.Rdata")

pen_inv_cons <- left_join(data, pen_inv, by = "regs") %>% 
  dplyr::select(regs, EE, era, pen, country)
pen_inv_cons <- na.omit(pen_inv_cons)
pen_inv_cons <- pen_inv_cons %>% 
  filter(EE != "NaN")
pen_inv_cons

pen_ninv_cons <- left_join(data, pen_ninv, by = "regs") %>% 
  dplyr::select(regs, EE, era, pen, country)
pen_ninv_cons <- na.omit(pen_ninv_cons)
pen_ninv_cons <- pen_ninv_cons %>% 
  filter(EE != "NaN")
pen_ninv_cons

mac_inv_cons <- left_join(data, mac_inv, by = "regs") %>% 
  dplyr::select(regs, EE, era, mac, country)
mac_inv_cons <- na.omit(mac_inv_cons)
mac_inv_cons <- mac_inv_cons %>% 
  filter(EE != "NaN")
names(mac_inv_cons)[4] <- "pen"
mac_inv_cons

mac_ninv_cons <- left_join(data, mac_ninv, by = "regs") %>% 
  dplyr::select(regs, EE, era, mac, country)
mac_ninv_cons <- na.omit(mac_ninv_cons)
mac_ninv_cons <- mac_ninv_cons %>% 
  filter(EE != "NaN")
names(mac_ninv_cons)[4] <- "pen"
mac_ninv_cons

#conert EE to numeric
pen_inv_cons$EE <- as.numeric(as.character(pen_inv_cons$EE)) * 100
pen_ninv_cons$EE <- as.numeric(as.character(pen_ninv_cons$EE)) * 100
mac_inv_cons$EE <- as.numeric(as.character(mac_inv_cons$EE)) * 100
mac_ninv_cons$EE <- as.numeric(as.character(mac_ninv_cons$EE)) * 100

library(ggtext)

#15 is fine for penicillin, 7.5 for mac 
# plot_function_pen <- function(data, ylab) {
#   require(ggrepel)
#   set.seed(42)
#   cor <- cor.test(data$pen, data$EE, method = "spearman", conf.level = 0.95,exact=FALSE)
#   pValue <- round(cor$p.value, 2)
#   pValue2 <- ifelse(pValue == 0, 0.001, pValue)
# 
# plot <-  ggplot(data, aes(x = pen, y = EE, label = country)) + 
#   geom_point(aes(color = country)) + 
#   #geom_cor(aes(x = pen, y = EE)) + 
#   #geom_line(aes(group = country)) + #can remove line
#   ylim(0, 100) +  
#   #geom_text_repel(aes(label = country), size = 3.5) + 
#   theme_minimal() + 
#   xlab("DDD/1000 inhabitants/day") + 
#   ylab(ylab) + #"Regional estimate of nonsusceptibility to invasive Penicillin isolates"
#   theme(axis.title.x = element_text(size = 12), 
#         axis.title.y = element_text(size = 12)) + 
#   guides(size = FALSE) +  
#   labs(color = "Country")  
#   annotate(geom="text", x=13, y=100, 
#            label= paste0("R = ", " ", round(cor$estimate, 2)),
#              color="black") 
#   # annotate(geom="text", x=13, y=95,
#   #          label= paste0("p = ", " ", round(cor$p.value, 3)),
#   #          color="black", fontface = 'italic')
#   # annotate(geom = "text", x=13, y=94,
#   #          label = paste0("italic(p) == ", pValue2, parse = TRUE))
#  # scale_shape(name = "PCV Implementation Status", labels = c("Post PCV", "Pre PCV") )
# 
# return(plot) 
# }

plot_function_pen <- function(data, ylab) {
  require(ggrepel)
  set.seed(42)
  cor <- cor.test(data$pen, data$EE, method = "spearman", conf.level = 0.95,exact=FALSE)
  pValue <- round(cor$p.value, 2)
  pValue2 <- ifelse(pValue == 0, 0.001, pValue)
  
  plot <-  ggplot(data, aes(x = pen, y = EE, label = country)) + 
    geom_point(aes(color = country)) + 
    #geom_cor(aes(x = pen, y = EE)) + 
    #geom_line(aes(group = country)) + #can remove line
    ylim(0, 100) +  
    #geom_text_repel(aes(label = country), size = 3.5) + 
    theme_minimal() + 
    xlab("DDD/1000 inhabitants/day") + 
    ylab(ylab) + #"Regional estimate of nonsusceptibility to invasive Penicillin isolates"
    theme(axis.title.x = element_text(size = 12), 
          axis.title.y = element_text(size = 12)) + 
    guides(size = FALSE) +  
    labs(color = "Country")  + 
    annotate(geom="text", x=13, y=100, 
             label= paste0("R = ", " ", round(cor$estimate, 2)),
             color="black") 
  # annotate(geom="text", x=17, y=98, 
  #          label= paste0("p = ", " ", round(cor$p.value, 2)),
  #          color="black", fontface = 'italic')
  # annotate(geom = "text", x=7, y=94,
  #          label = paste0("italic(p) == ", pValue2, parse = TRUE))
  # annotate(geom = "text", x=7, y=95,
  #          label = paste0("italic(p) == ", round(cor$p.value, 3)), parse = TRUE)
  # scale_shape(name = "PCV Implementation Status", labels = c("Post PCV", "Pre PCV") )
  
  return(plot) 
}


plot_function_mac <- function(data, ylab) {
  require(ggrepel)
  set.seed(42)
  cor <- cor.test(data$pen, data$EE, method = "spearman", conf.level = 0.95,exact=FALSE)
  pValue <- round(cor$p.value, 2)
  pValue2 <- ifelse(pValue == 0, 0.001, pValue)
  
  plot <-  ggplot(data, aes(x = pen, y = EE, label = country)) + 
    geom_point(aes(color = country)) + 
    #geom_cor(aes(x = pen, y = EE)) + 
    #geom_line(aes(group = country)) + #can remove line
    ylim(0, 100) +  
    #geom_text_repel(aes(label = country), size = 3.5) + 
    theme_minimal() + 
    xlab("DDD/1000 inhabitants/day") + 
    ylab(ylab) + #"Regional estimate of nonsusceptibility to invasive Penicillin isolates"
    theme(axis.title.x = element_text(size = 12), 
          axis.title.y = element_text(size = 12)) + 
    guides(size = FALSE) +  
    labs(color = "Country")  + 
    annotate(geom="text", x=7, y=100, 
             label= paste0("R = ", " ", round(cor$estimate, 2)),
             color="black") 
    # annotate(geom="text", x=17, y=98, 
    #          label= paste0("p = ", " ", round(cor$p.value, 2)),
    #          color="black", fontface = 'italic')
    # annotate(geom = "text", x=7, y=94,
    #          label = paste0("italic(p) == ", pValue2, parse = TRUE))
    # annotate(geom = "text", x=7, y=95,
    #          label = paste0("italic(p) == ", round(cor$p.value, 3)), parse = TRUE)
  # scale_shape(name = "PCV Implementation Status", labels = c("Post PCV", "Pre PCV") )
  
  return(plot) 
}


plot_functionDiff_pen <- function(data, ylab) {
  set.seed(42)
  cor <- cor.test(data$pen, data$EE, method = "spearman", conf.level = 0.95,exact=FALSE)
  pValue <- round(cor$p.value, 2)
  pValue2 <- ifelse(pValue == 0, 0.001, pValue)
  
  plot <-  ggplot(data, aes(x = pen, y = EE, label = country)) + 
    geom_point(aes(color = country)) + 
    #geom_line(aes(group = country)) + #can remove line
    ylim(-40, 100) +  
    #geom_text_repel(aes(label = country), size = 3.5) + 
    theme_minimal() + 
    xlab("DDD/1000 inhabitants/day") + 
    ylab(ylab) + #"Regional estimate of nonsusceptibility to invasive Penicillin isolates"
    theme(axis.title.x = element_text(size = 12), 
          axis.title.y = element_text(size = 12)) + 
    guides(size = FALSE) + 
    labs(color = "Country") + 
    annotate(geom="text", x=13, y=100, 
             label= paste0("R = ", " ", round(cor$estimate, 2)),
             color="black") 
    # annotate(geom = "text", x=7, y=94,
    #          label = paste0("italic(p) == ", pValue2, parse = TRUE))
    # annotate(geom = "text", x=13, y=95,
    #          label = paste0("italic(p) == ", round(cor$p.value, 3)), parse = TRUE)
  # scale_shape(name = "PCV Implementation Status", labels = c("Post PCV", "Pre PCV") )
  return(plot) 
}

plot_functionDiff_mac <- function(data, ylab) {
  set.seed(42)
  cor <- cor.test(data$pen, data$EE, method = "spearman", conf.level = 0.95,exact=FALSE)
  pValue <- round(cor$p.value, 2)
  pValue2 <- ifelse(pValue == 0, 0.001, pValue)
  
  plot <-  ggplot(data, aes(x = pen, y = EE, label = country)) + 
    geom_point(aes(color = country)) + 
    #geom_line(aes(group = country)) + #can remove line
    ylim(-40, 100) +  
    #geom_text_repel(aes(label = country), size = 3.5) + 
    theme_minimal() + 
    xlab("DDD/1000 inhabitants/day") + 
    ylab(ylab) + #"Regional estimate of nonsusceptibility to invasive Penicillin isolates"
    theme(axis.title.x = element_text(size = 12), 
          axis.title.y = element_text(size = 12)) + 
    guides(size = FALSE) + 
    labs(color = "Country") + 
    annotate(geom="text", x=7, y=100, 
             label= paste0("R = ", " ", round(cor$estimate, 2)),
             color="black") 
    # annotate(geom = "text", x=7, y=95,
    #          label = paste0("italic(p) == ", round(cor$p.value, 3)), parse = TRUE)
  # annotate(geom = "text", x=7, y=94,
  #          label = paste0("italic(p) == ", pValue2, parse = TRUE))
  # scale_shape(name = "PCV Implementation Status", labels = c("Post PCV", "Pre PCV") )
  return(plot) 
}

pen_inv_consPre <- pen_inv_cons %>% filter(era == "pre")
pen_inv_consPost <- pen_inv_cons %>% filter(era == "post")
pen_ninv_consPre <- pen_ninv_cons %>% filter(era == "pre")
pen_ninv_consPost <- pen_ninv_cons %>% filter(era == "post")

mac_inv_consPre <-  mac_inv_cons %>% filter(era == "pre")
mac_inv_consPost <- mac_inv_cons %>% filter(era == "post")
mac_ninv_consPre <- mac_ninv_cons %>% filter(era == "pre")
mac_ninv_consPost <- mac_ninv_cons %>% filter(era == "post")


cor <- cor.test(mac_ninv_consPre$pen, mac_ninv_consPre$EE, method = "spearman", conf.level = 0.95)

plotPenInvPre <- plot_function_pen(pen_inv_consPre, ylab = "Proportion of invasive isolates non-susceptible to penicillin \n before PCV implementation, %")
plotPenInvPost <- plot_function_pen(pen_inv_consPost, ylab = "Proportion of invasive isolates non-susceptible to penicillin \n after PCV implementation, %")
    pen_inv_cons_wide <- pen_inv_cons %>%  pivot_wider(names_from = era, values_from =EE ) %>%  mutate(EE = pre-post)
plotPenInvDiff <- plot_functionDiff_pen(pen_inv_cons_wide, ylab = "Change in proportion of invasive isolates non-susceptible to penicillin \n following PCV implementation, %")

plotPenNInvPre <-plot_function_pen(pen_ninv_cons, ylab = "Proportion of non-invasive isolates non-susceptible to penicillin \n before PCV implementation, %")
plotPenNInvPost <- plot_function_pen(pen_ninv_consPost, ylab = "Proportion of non-invasive isolates non-susceptible to penicillin \n after PCV implementation, %")
    pen_ninv_cons_wide <- pen_ninv_cons %>%  pivot_wider(names_from = era, values_from =EE ) %>%  mutate(EE = pre-post)
plotPenNInvDiff <- plot_functionDiff_pen(pen_ninv_cons_wide, ylab = "Change in proportion of non-invasive isolates non-susceptible to penicillin \n following PCV implementation, %")

plotMacInvPre <-plot_function_mac(mac_inv_cons, ylab = "Proportion of invasive isolates non-susceptible to macrolides \n before PCV implementation, %")
plotMacInvPost <- plot_function_mac(mac_inv_consPost, ylab = "Proportion of invasive isolates non-susceptible to macrolides \n after PCV implementation, %")
    mac_inv_cons_wide <- mac_inv_cons %>%  pivot_wider(names_from = era, values_from =EE ) %>%  mutate(EE = pre-post)
plotMacInvDiff <- plot_functionDiff_mac(mac_inv_cons_wide, ylab = "Change in proportion of invasive isolates non-susceptible to macrolides \n following PCV implementation, %")

plotMacNInvPre <-plot_function_mac(mac_ninv_cons, ylab = "Proportion of non-invasive isolates non-susceptible to macrolides \n before PCV implementation, %")
plotMacNInvPost <- plot_function_mac(mac_ninv_consPost, ylab = "Proportion of non-invasive isolates non-susceptible to macrolides \n after PCV implementation, %")
    mac_ninv_cons_wide <- mac_ninv_cons %>%  pivot_wider(names_from = era, values_from =EE ) %>%  mutate(EE = pre-post)
plotMacNInvDiff <- plot_functionDiff_mac(mac_ninv_cons_wide, ylab = "Change in proportion of non-invasive isolates non-susceptible to macrolides \n following PCV implementation, %")

library(cowplot)

# Penicillin Invasive 
prowPenInv <- plot_grid(
  plotPenInvPre + theme(legend.position="none"),
  plotPenInvPost + theme(legend.position="none"),
  plotPenInvDiff + theme(legend.position="none"),
  align = 'vh',
  #hjust = -1,
  nrow = 1, 
  rel_widths = c(1, 1, 1))

legend_PenInv <- get_legend( ## extract a legend that is laid out horizontally
  plotPenInvPre +
    guides(color = guide_legend(nrow = 5)) +
    theme(legend.position = "bottom")
)

figPenInv <- plot_grid(prowPenInv, legend_PenInv, ncol = 1, rel_heights = c(1, .3))

#Penicillin Noninvasive 
prowPenNInv <- plot_grid(
  plotPenNInvPre + theme(legend.position="none"),
  plotPenNInvPost + theme(legend.position="none"),
  plotPenNInvDiff + theme(legend.position="none"),
  align = 'vh',
  nrow = 1, 
  rel_widths = c(1, 1, 1))

legend_PenNInv <- get_legend( ## extract a legend that is laid out horizontally
  plotPenNInvPre +
    guides(color = guide_legend(nrow = 5)) +
    theme(legend.position = "bottom")
)

figPenNInv <- plot_grid(prowPenNInv, legend_PenNInv, ncol = 1, rel_heights = c(1, .3))

#Macrolide Invasive 
prowMacInv <- plot_grid(
  plotMacInvPre + theme(legend.position="none"),
  plotMacInvPost + theme(legend.position="none"),
  plotMacInvDiff + theme(legend.position="none"),
  align = 'vh',
  #hjust = -1,
  nrow = 1, 
  rel_widths = c(1, 1, 1))

legend_MacInv <- get_legend( ## extract a legend that is laid out horizontally
  plotMacInvPre +
    guides(color = guide_legend(nrow = 5)) +
    theme(legend.position = "bottom")
)

figMacInv <- plot_grid(prowMacInv, legend_MacInv, ncol = 1, rel_heights = c(1, .3))

#Macrolide Noninvasive 
prowMacNInv <- plot_grid(
  plotMacNInvPre + theme(legend.position="none"),
  plotMacNInvPost + theme(legend.position="none"),
  plotMacNInvDiff + theme(legend.position="none"),
  align = 'vh',
  #hjust = -1,
  nrow = 1, 
  rel_widths = c(1, 1, 1))

legend_MacNInv <- get_legend( ## extract a legend that is laid out horizontally
  plotMacNInvPre +
    guides(color = guide_legend(nrow = 5)) +
    theme(legend.position = "bottom")
)

figMacNInv <- plot_grid(prowMacNInv, legend_MacNInv, ncol = 1, rel_heights = c(1, .3))

ggsave(plot = figPenInv, 
       filename ="/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/plots/figConsPenInv.pdf", 
       width = 15, height= 8)

ggsave(plot = figPenNInv, 
       filename ="/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/plots/figConsPenNInv.pdf", 
       width = 15, height= 8)


ggsave(plot = figMacInv, 
       filename ="/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/plots/figConsMacInv.pdf", 
       width = 15, height= 8)

ggsave(plot = figMacNInv, 
       filename ="/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/plots/figConsMacNInv.pdf", 
       width = 15, height= 8)

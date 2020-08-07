## TEP correlation analysis and plots (8/4/2020)

# Upload libraries
library(ggplot2)
library(ggpubr)
library(RColorBrewer)


# Read in .csv file of weekly environmental factors and TEP measurements
weekly_TEP <- read.csv('weekly_TEP_factors.csv', header = TRUE, check.names = F)


# Run a normality test for each data variable from weekly series
norm_week <-apply(weekly_TEP[,4:25], 2, function(x) shapiro.test(x)$p.value)
norm_week <- as.data.frame(norm_week)


# Check out a plot of the normality data (in this case Chlorophyll)
ggqqplot(weekly_TEP$Chl, ylab = "Chl")


# Estimate Spearman values and p-values between TEP and every other factor in weekly dataset
rho <- pval <- NULL
for(i in 2:ncol(weekly_TEP)){
  rho[i-1]  <- cor.test(weekly_TEP[,1],weekly_TEP[,i], method = "spearman")$estimate # Change to [,2] for correlations with TEP pro and [,3] for correlations with TEP:Chl ratio
  pval[i-1] <- cor.test(weekly_TEP[,1],weekly_TEP[,i], method = "spearman")$p.value
}
week_TEP <- cbind(rho,pval)
rownames(week_TEP) <- paste(colnames(weekly_TEP)[1],"vs",colnames(weekly_TEP)[2:ncol(weekly_TEP)]) # Change to [2] or [3] for labeling with TEP pro or TEP:Chl ratio
week_TEP
week_TEP_corr <-as.data.frame(week_TEP)


# Re-run correlation analysis for tidal data
tidal_TEP <- read.csv('tidal_TEP_factors.csv', header = TRUE, check.names = F)


# Estimate Spearman values and p-values between TEP and every other factor in tidal dataset
rho <- pval <- NULL
for(i in 2:ncol(tidal_TEP)){
  rho[i-1]  <- cor.test(tidal_TEP[,1],tidal_TEP[,i], method = "spearman")$estimate # Follow above instructions to estimate correlations with TEP pro and TEP:Chl 
  pval[i-1] <- cor.test(tidal_TEP[,1],tidal_TEP[,i], method = "spearman")$p.value
}
tide_TEP <- cbind(rho,pval)
rownames(tide_TEP) <- paste(colnames(tidal_TEP)[1],"vs",colnames(tidal_TEP)[2:ncol(tidal_TEP)])
tide_TEP
tide_TEP_corr <-as.data.frame(tide_TEP)


# Re-run correlation analysis for diel data
daily_TEP <- read.csv('diel_TEP_factors.csv', header = TRUE, check.names = F)


# Estimate Spearman values and p-values between TEP and every other factor in the diel dataset
rho <- pval <- NULL
for(i in 2:ncol(daily_TEP)){
  rho[i-1]  <- cor.test(daily_TEP[,1],daily_TEP[,i], method = "spearman")$estimate # Follow above instructions to estimate correlations with TEP pro and TEP:Chl 
  pval[i-1] <- cor.test(daily_TEP[,1],daily_TEP[,i], method = "spearman")$p.value
}
diel_TEP <- cbind(rho,pval)
rownames(diel_TEP) <- paste(colnames(daily_TEP)[1],"vs",colnames(daily_TEP)[2:ncol(daily_TEP)])
diel_TEP
diel_TEP_corr <-as.data.frame(diel_TEP)


# Plot Spearman correlation for TEP vs. Chlorophyll across all sampling time points (diel to weekly)
TEP_vs_Chl <- read.csv('Tep_chl_corr.csv', header = TRUE, check.names = F, fileEncoding="UTF-8-BOM")
p <- ggscatter(TEP_vs_Chl, x = "TEP", y = "Chl", cor.method = "spearman", cor.coef =T, size = 4, add.params = list(color = "black", fill = "lightgray"), add = "reg.line", conf.int = TRUE, xlab = "TEP (µg XG eq. L-1)", ylab = "Chl (µg L-1)") + geom_point(shape = 21, stroke = 2)+geom_point(aes(fill=Time),size = 5, shape = 21, colour = "black") + scale_color_viridis(discrete = TRUE, option = "D")+scale_fill_viridis(discrete = TRUE) 
pnew <- ggpar(p, xlim = c(0, 1500), ylim = c(0, 20)) # Adjust axes scales
plot(pnew)
ggsave(filename = "TEP_vs_CHl.tiff", plot = last_plot(), device = "tiff", path = NULL, scale = 1, width = 8, height = 5, units = c("in", "cm", "mm"), dpi = 600) # Example of exporting high quality figures from R


# Upload Spearman values and build time scale heatmaps for all TEP measurements
install.packages("pheatmap")
library(pheatmap)


# Seasonal heatmaps
seasonal <- read.csv('TEP_weekly_correlations.csv', header=T, row.names = 1, check.names=F)
mat_breaks <- seq(min(-0.6), max(0.6), length.out = 10) # Number of breaks (based on Spearman values) to have in your color gradient and associated legend (will be called in core pheatmap function)
color <- colorRampPalette(rev(brewer.pal(n = 7, name ="RdYlBu")))(10) # Color palette to apply to the heatmap (will be called in core pheatmap function)
pheatmap(seasonal, treeheight_row =0, treeheight_col =0, cellwidth = 50, cellheight = 20, border_color = "black", breaks = mat_breaks, cluster_rows = F, cluster_cols = F, legend_breaks = c(-0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6), legend_labels = c("-0.6", "-0.4", "-0.2", "0", "0.2", "0.4", "0.6"), color= color, main = "Seasonal", filename = "Seasonal_TEP.pdf") # Here is where you can customize your heatmap (e.g. cell sizes, color scales and gradients, legend, etc.). Heatmap is saved as a pdf file output. 


# Tidal heatmaps
tidal <- read.csv('TEP_tidal_correlations.csv', header=T, row.names = 1, check.names=F)
pheatmap(tidal, treeheight_row =0, treeheight_col =0, cellwidth = 50,cellheight = 20,border_color = "black", breaks = mat_breaks,  cluster_cols=F, cluster_rows = F, legend_breaks = c(-0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6), legend_labels = c("-0.6", "-0.4", "-0.2", "0", "0.2", "0.4", "0.6"), color= color, main = "Tidal", filename = "Tidal_TEP.pdf")


# Diel heatmaps
mat_breaks_diel <- seq(min(-1), max(1), length.out = 10) # Diel data has different breaks based on Spearman values
diel <- read.csv('TEP_diel_correlations.csv', header=T, row.names = 1, check.names=F)
pheatmap(diel, treeheight_row =0, treeheight_col =0, cellwidth = 50, cellheight = 20, border_color = "black", breaks = mat_breaks_diel, cluster_cols=F, cluster_rows = F, legend_breaks = c(-1, -0.5, 0, 0.5, 1), legend_labels = c("-1", "-0.5",  "0", "0.5",  "1"), color= color, main = "Diel", filename = "Diel_TEP.pdf")


# Plot TEP vs. TEP production rates across all sampling points
TEP_VS_TEPpro <- read.csv('Tep_vs_teppro.csv', header = TRUE, check.names = F, fileEncoding="UTF-8-BOM")
p <- ggscatter(TEP_VS_TEPpro, x = "TEP", y = "TEPpro", cor.method = "spearman", cor.coef = F, size = 4, xlab = "TEP (µg XG eq. L-1)", ylab = "TEP pro (µg XG eq. L-1 d-1)")+ geom_errorbar(data=TEP_VS_TEPpro, mapping=aes(x=TEP, xmin=TEP-Error1, xmax=TEP+Error1), width=0.1, size=0.5, color="black")+ geom_errorbar(data=TEP_VS_TEPpro, mapping=aes(y=TEPpro, ymin=TEPpro-Error2, ymax=TEPpro+Error2), width=20, size=0.5, color="black")+ geom_point(shape = 21, stroke = 2)+geom_point(aes(fill=Time),size = 5, shape = 21, colour = "black") + scale_color_viridis(discrete = TRUE, option = "D")+scale_fill_viridis(discrete = TRUE) + geom_hline(yintercept=0, linetype="dashed")
pnew <- ggpar(p, xlim = c(0, 1500), ylim = c(-1, 2))
plot(pnew)
ggsave(filename = "TEP_vs_TEPpro.tiff", plot = last_plot(), device = "tiff", path = NULL, scale = 1, width = 8, height = 5, units = c("in", "cm", "mm"), dpi = 600) # Example of exporting high quality figures from R

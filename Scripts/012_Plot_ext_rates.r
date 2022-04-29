# clean environment
rm(list=ls())

##############################################################
# Load libraries                                          ####
##############################################################

library(ggplot2)
library(dplyr)
library(ggpubr)
library(Cairo)

#Set theme
theme_set(theme_light(base_size = 15))

##################################################################
# Load data and plot ext rates from Mio; time unit = 250 ka   ####
# iterations = 100 M; grid_plot default                       ####
##################################################################

data_ext_from_Mio_default <- read.csv('Results/PyRate_ext_rates/Plots_ext_dates/Ext_rates_from_Mio_grid_plot_default.csv')

data_ext_from_Mio_default <- mutate(data_ext_from_Mio_default, time_ext_Ma = time_ext/4) #Convert x axis in Ma

plot_ext_from_Mio_default <- ggplot(data_ext_from_Mio_default, aes(x= time_ext_Ma, y = rate_ext)) +
  geom_line(data = data_ext_from_Mio_default, aes(x = time_ext_Ma, y = rate_ext), inherit.aes = FALSE, stat = 'identity', colour = "#c6392f", size = 1) +
  geom_ribbon(data = data_ext_from_Mio_default, aes(x = time_ext_Ma, ymin = minHPD_ext, ymax = maxHPD_ext), inherit.aes = FALSE, fill = "#c6392f", alpha = .1)+
  xlab("Time (Ma)") + ylab("Extinction rate")+
  scale_y_continuous(breaks=c(0,2,4,6,8))+
  scale_x_continuous(breaks=c(0,-5,-10,-15,-20))+
  theme(axis.title = element_text(family = "Arial", size = 18, colour = "grey40"),
        axis.text.x = element_text(family = "Arial", size = 14),
        axis.text.y = element_text(family = "Arial", size = 14),
        panel.grid = element_blank()) 

plot_ext_from_Mio_default

#####################################################################
# Load data and plot frequency of ext rate shifts from Mio;      ####
# time unit = 250 ka; iterations = 100 M; grid_plot default      ####
#####################################################################

bf2_Mio = 0.03327601624453699
bf6_Mio = 0.20276915504541185

data_ext_shift_from_Mio_default <- read.csv('Results/PyRate_ext_rates/Plots_ext_dates/Ext_rate_shifts_from_Mio_grid_plot_default.csv')

data_ext_shift_from_Mio_default <- mutate(data_ext_shift_from_Mio_default, mids_Ma = mids/4) #Convert x axis in Ma

plot_ext_shift_from_Mio_default <- ggplot(data_ext_shift_from_Mio_default, aes(x= mids_Ma, y = counts)) +
  geom_segment(aes(x=mids_Ma, xend=mids_Ma, y=0, yend=counts), color="#c6392f", size = .5) +
  geom_point( color="#c6392f", size=1) +
  xlab("Time (Ma)") + ylab("Frequency of rate shift")+
  scale_y_continuous(breaks=c(0,0.2,0.4,0.6,0.8,1.0))+
  scale_x_continuous(breaks=c(0,-5,-10,-15,-20))+
  geom_hline(yintercept=bf2_Mio, linetype="dashed", color = "grey40", size=.3)+
  geom_hline(yintercept=bf6_Mio, linetype="dashed", color = "grey40", size=.3)+
  theme(axis.title = element_text(family = "Arial", size = 18, colour = "grey40"),
        axis.text.x = element_text(family = "Arial", size = 14),
        axis.text.y = element_text(family = "Arial", size = 14),
        panel.grid = element_blank())

plot_ext_shift_from_Mio_default

#####################################################################
# Load data and plot ext rates from Mio; time unit = 250 ka      ####
# iterations = 100 M; grid_plot 0.05                             ####
#####################################################################

data_ext_from_Mio_0.05 <- read.csv('Results/PyRate_ext_rates/Plots_ext_dates/Ext_rates_from_Mio_grid_plot_0.05.csv')

data_ext_from_Mio_0.05 <- mutate(data_ext_from_Mio_0.05, time_ext_Ma = time_ext/4) #Convert x axis in Ma

plot_ext_from_Mio_0.05 <- ggplot(data_ext_from_Mio_0.05, aes(x= time_ext_Ma, y = rate_ext)) +
  geom_line(data = data_ext_from_Mio_0.05, aes(x = time_ext_Ma, y = rate_ext), inherit.aes = FALSE, stat = 'identity', colour = "#c6392f", size = 1) +
  geom_ribbon(data = data_ext_from_Mio_0.05, aes(x = time_ext_Ma, ymin = minHPD_ext, ymax = maxHPD_ext), inherit.aes = FALSE, fill = "#c6392f", alpha = .1)+
  xlab("Time (Ma)") + ylab("Extinction rate")+
  #  scale_y_sqrt(breaks=c(0,2,4,6,8))+
  scale_y_continuous(breaks=c(0,2,4,6,8))+
  scale_x_continuous(breaks=c(0,-5,-10,-15,-20))+
  theme(axis.title = element_text(family = "Arial", size = 18, colour = "grey40"),
        axis.text.x = element_text(family = "Arial", size = 14),
        axis.text.y = element_text(family = "Arial", size = 14),
        panel.grid = element_blank()) 

plot_ext_from_Mio_0.05

#####################################################################
# Load data and plot frequency of ext rate shifts from Mio;      ####
# time unit = 250 ka; iterations = 100 M; grid_plot 0.05         ####
#####################################################################

bf2_Mio = 0.002234713719297753
bf6_Mio = 0.01627998412337174

data_ext_shift_from_Mio_0.05 <- read.csv('Results/PyRate_ext_rates/Plots_ext_dates/Ext_rate_shifts_from_Mio_grid_plot_0.05.csv')

data_ext_shift_from_Mio_0.05 <- mutate(data_ext_shift_from_Mio_0.05, mids_Ma = mids/4) #Convert x axis in Ma

plot_ext_shift_from_Mio_0.05 <- ggplot(data_ext_shift_from_Mio_0.05, aes(x= mids_Ma, y = counts)) +
  geom_segment(aes(x=mids_Ma, xend=mids_Ma, y=0, yend=counts), color="#c6392f", size = .05) +
  geom_point( color="#c6392f", size=.3) +
  xlab("Time (Ma)") + ylab("Frequency of rate shift")+
  scale_y_continuous(breaks=c(0,0.2,0.4,0.6,0.8,1.0))+
  scale_x_continuous(breaks=c(0,-5,-10,-15,-20))+
  geom_hline(yintercept=bf2_Mio, linetype="dashed", color = "grey40", size=.3)+
  geom_hline(yintercept=bf6_Mio, linetype="dashed", color = "grey40", size=.3)+
  theme(axis.title = element_text(family = "Arial", size = 18, colour = "grey40"),
        axis.text.x = element_text(family = "Arial", size = 14),
        axis.text.y = element_text(family = "Arial", size = 14),
        panel.grid = element_blank())

plot_ext_shift_from_Mio_0.05

#########################################################################
# Plot magnitude of extinction rate increase relative to the base value #
#########################################################################

data_ext_from_Mio_0.05 <- mutate(data_ext_from_Mio_0.05, ext_rate_increase = rate_ext/0.3750480)

plot_ext_increase_from_Mio_0.05 <- ggplot(data_ext_from_Mio_0.05, aes(x= time_ext_Ma, y = ext_rate_increase)) +
  geom_line(data = data_ext_from_Mio_0.05, aes(x = time_ext_Ma, y = ext_rate_increase), inherit.aes = FALSE, stat = 'identity', colour = "#c6392f", size = 1) +
  xlab("Time (Ma)") + ylab("Magnitude of rate increase")+
  scale_y_continuous(breaks=c(0,1,2,3,4))+
  scale_x_continuous(breaks=c(0,-5,-10,-15,-20))+
  theme(axis.title = element_text(family = "Arial", size = 18, colour = "grey40"),
        axis.text.x = element_text(family = "Arial", size = 14),
        axis.text.y = element_text(family = "Arial", size = 14),
        panel.grid = element_blank()) 

plot_ext_increase_from_Mio_0.05

#####################################################################
# Load data and plot ext rates from Pleisto; time unit = 100 ka  ####
# iterations = 100 M; grid_plot 0.05                             ####
#####################################################################

data_ext_from_Pleisto_0.05 <- read.csv('Results/PyRate_ext_rates/Plots_ext_dates/Ext_rates_from_Pleisto_grid_plot_0.05.csv')

data_ext_from_Pleisto_0.05 <- mutate(data_ext_from_Pleisto_0.05, time_ext_Ma = time_ext/10) #Convert x axis in Ma

plot_ext_from_Pleisto_0.05 <- ggplot(data_ext_from_Pleisto_0.05, aes(x= time_ext_Ma, y = rate_ext)) +
  geom_line(data = data_ext_from_Pleisto_0.05, aes(x = time_ext_Ma, y = rate_ext), inherit.aes = FALSE, stat = 'identity', colour = "#c6392f", size = 1) +
  geom_ribbon(data = data_ext_from_Pleisto_0.05, aes(x = time_ext_Ma, ymin = minHPD_ext, ymax = maxHPD_ext), inherit.aes = FALSE, fill = "#c6392f", alpha = .1)+
  xlab("Time (Ma)") + ylab("Extinction rate")+
  scale_y_continuous(breaks=c(0,0.2,0.4,0.6,0.8,1.0))+
  scale_x_continuous(breaks=c(0,-0.5,-1,-1.5,-2,-2.5))+
  theme(axis.title = element_text(family = "Arial", size = 18, colour = "grey40"),
        axis.text.x = element_text(family = "Arial", size = 14),
        axis.text.y = element_text(family = "Arial", size = 14),
        panel.grid = element_blank()) 

plot_ext_from_Pleisto_0.05

######################################################################
# Load data and plot frequency of ext rate shifts from Pleisto;   ####
# time unit = 100 ka; iterations = 100 M; grid_plot 0.05          ####
######################################################################

bf2_Pleisto = 0.005482758681601927
bf6_Pleisto = 0.03914130514609744

data_ext_shift_from_Pleisto_0.05 <- read.csv('Results/PyRate_ext_rates/Plots_ext_dates/Ext_rate_shifts_from_Pleisto_grid_plot_0.05.csv')

data_ext_shift_from_Pleisto_0.05 <- mutate(data_ext_shift_from_Pleisto_0.05, mids_Ma = mids/10) #Convert x axis in Ma

plot_ext_shift_from_Pleisto_0.05 <- ggplot(data_ext_shift_from_Pleisto_0.05, aes(x= mids_Ma, y = counts)) +
  geom_segment(aes(x=mids_Ma, xend=mids_Ma, y=0, yend=counts), color="#c6392f", size = .05) +
  geom_point( color="#c6392f", size=.3) +
  xlab("Time (Ma)") + ylab("Frequency of rate shift")+
  scale_y_continuous(breaks=c(0,0.2,0.4,0.6,0.8,1.0))+
  scale_x_continuous(breaks=c(0,-0.5,-1,-1.5,-2,-2.5))+
  geom_hline(yintercept=bf2_Pleisto, linetype="dashed", color = "grey40", size=.3)+
  geom_hline(yintercept=bf6_Pleisto, linetype="dashed", color = "grey40", size=.3)+
  theme(axis.title = element_text(family = "Arial", size = 18, colour = "grey40"),
        axis.text.x = element_text(family = "Arial", size = 14),
        axis.text.y = element_text(family = "Arial", size = 14),
        panel.grid = element_blank())

plot_ext_shift_from_Pleisto_0.05

#########################################################################
# Plot magnitude of extinction rate increase relative to the base value #
#########################################################################

data_ext_from_Pleisto_0.05 <- mutate(data_ext_from_Pleisto_0.05, ext_rate_increase = rate_ext/0.06208176)

plot_ext_increase_from_Pleisto_0.05 <- ggplot(data_ext_from_Pleisto_0.05, aes(x= time_ext_Ma, y = ext_rate_increase)) +
  geom_line(data = data_ext_from_Pleisto_0.05, aes(x = time_ext_Ma, y = ext_rate_increase), inherit.aes = FALSE, stat = 'identity', colour = "#c6392f", size = 1) +
  xlab("Time (Ma)") + ylab("Magnitude of rate increase")+
  scale_y_continuous(breaks=c(1,3,6,9,12))+
  scale_x_continuous(breaks=c(0,-0.5,-1,-1.5,-2,-2.5))+
  theme(axis.title = element_text(family = "Arial", size = 18, colour = "grey40"),
        axis.text.x = element_text(family = "Arial", size = 14),
        axis.text.y = element_text(family = "Arial", size = 14),
        panel.grid = element_blank()) 

plot_ext_increase_from_Pleisto_0.05

#########################################################################
# Load data and plot ext rates from Late Pleisto; time unit = 10 ka  ####
# iterations = 100 M; grid_plot 0.05; min_dt 0.1                     ####
#########################################################################

data_ext_from_Late_Pleisto_0.05_0.1 <- read.csv('Results/PyRate_ext_rates/Plots_ext_dates/Ext_rates_from_Late_Pleisto_grid_plot_0.05_min_dt_0.1.csv')

data_ext_from_Late_Pleisto_0.05_0.1 <- mutate(data_ext_from_Late_Pleisto_0.05_0.1, time_ext_Ma = time_ext/100) #Convert x axis in Ma

plot_ext_from_Late_Pleisto_0.05_0.1 <- ggplot(data_ext_from_Late_Pleisto_0.05_0.1, aes(x= time_ext_Ma, y = rate_ext)) +
  geom_line(data = data_ext_from_Late_Pleisto_0.05_0.1, aes(x = time_ext_Ma, y = rate_ext), inherit.aes = FALSE, stat = 'identity', colour = "#c6392f", size = 1) +
  geom_ribbon(data = data_ext_from_Late_Pleisto_0.05_0.1, aes(x = time_ext_Ma, ymin = minHPD_ext, ymax = maxHPD_ext), inherit.aes = FALSE, fill = "#c6392f", alpha = .1)+
  xlab("Time (Ma)") + ylab("Extinction rate")+
  scale_y_continuous(breaks=c(0,0.5,1,1.5,2))+
  scale_x_continuous(breaks=c(0,-0.025,-0.05,-0.075,-0.1,-0.125))+
  theme(axis.title = element_text(family = "Arial", size = 18, colour = "grey40"),
        axis.text.x = element_text(family = "Arial", size = 14),
        axis.text.y = element_text(family = "Arial", size = 14),
        panel.grid = element_blank()) 

plot_ext_from_Late_Pleisto_0.05_0.1

###########################################################################
# Load data and plot frequency of ext rate shifts from Late Pleisto;   ####
# time unit = 10 ka; iterations = 100 M; grid_plot 0.05; min_dt 0.1    ####
###########################################################################

bf2_Late_Pleisto_0.05_0.1 = 0.008848983002501302
bf6_Late_Pleisto_0.05_0.1 = 0.061886761710444826

data_ext_shift_from_Late_Pleisto_0.05_0.1 <- read.csv('Results/PyRate_ext_rates/Plots_ext_dates/Ext_rate_shifts_from_Late_Pleisto_grid_plot_0.05_min_dt_0.1.csv')

data_ext_shift_from_Late_Pleisto_0.05_0.1 <- mutate(data_ext_shift_from_Late_Pleisto_0.05_0.1, mids_Ma = mids/100) #Convert x axis in Ma

plot_ext_shift_from_Late_Pleisto_0.05_0.1 <- ggplot(data_ext_shift_from_Late_Pleisto_0.05_0.1, aes(x= mids_Ma, y = counts)) +
  geom_segment(aes(x=mids_Ma, xend=mids_Ma, y=0, yend=counts), color="#c6392f", size = .05) +
  geom_point( color="#c6392f", size=.3) +
  xlab("Time (Ma)") + ylab("Frequency of rate shift")+
  scale_y_continuous(breaks=c(0,0.2,0.4,0.6,0.8,1.0))+
  scale_x_continuous(breaks=c(0,-0.025,-0.05,-0.075,-0.1,-0.125))+
  geom_hline(yintercept=bf2_Late_Pleisto_0.05_0.1, linetype="dashed", color = "grey40", size=.3)+
  geom_hline(yintercept=bf6_Late_Pleisto_0.05_0.1, linetype="dashed", color = "grey40", size=.3)+
  theme(axis.title = element_text(family = "Arial", size = 18, colour = "grey40"),
        axis.text.x = element_text(family = "Arial", size = 14),
        axis.text.y = element_text(family = "Arial", size = 14),
        panel.grid = element_blank())

plot_ext_shift_from_Late_Pleisto_0.05_0.1

#########################################################################
# Plot magnitude of extinction rate increase relative to the base value #
#########################################################################

data_ext_from_Late_Pleisto_0.05_0.1 <- mutate(data_ext_from_Late_Pleisto_0.05_0.1, ext_rate_increase = rate_ext/0.02082706)

plot_ext_increase_from_Late_Pleisto_0.05_0.1 <- ggplot(data_ext_from_Late_Pleisto_0.05_0.1, aes(x= time_ext_Ma, y = ext_rate_increase)) +
  geom_line(data = data_ext_from_Late_Pleisto_0.05_0.1, aes(x = time_ext_Ma, y = ext_rate_increase), inherit.aes = FALSE, stat = 'identity', colour = "#c6392f", size = 1) +
  xlab("Time (Ma)") + ylab("Magnitude of rate increase")+
  scale_y_continuous(breaks=c(1,25,50,75))+
  scale_x_continuous(breaks=c(0,-0.025,-0.05,-0.075,-0.1,-0.125))+
  theme(axis.title = element_text(family = "Arial", size = 18, colour = "grey40"),
        axis.text.x = element_text(family = "Arial", size = 14),
        axis.text.y = element_text(family = "Arial", size = 14),
        panel.grid = element_blank()) 

plot_ext_increase_from_Late_Pleisto_0.05_0.1

################################################
# Combine all 9 plots for Extended Data Fig. 8 #
################################################

figure_1 <- ggarrange(plot_ext_from_Mio_0.05, plot_ext_shift_from_Mio_0.05, plot_ext_increase_from_Mio_0.05,
                      plot_ext_from_Pleisto_0.05, plot_ext_shift_from_Pleisto_0.05, plot_ext_increase_from_Pleisto_0.05,
                      plot_ext_from_Late_Pleisto_0.05_0.1, plot_ext_shift_from_Late_Pleisto_0.05_0.1,plot_ext_increase_from_Late_Pleisto_0.05_0.1,
                      ncol = 1, nrow = 9)
figure_1

ggsave(figure_1, filename = "Ext_rates_from_Mio_Pl_and_LP.pdf", path = "Results/PyRate_ext_rates/Plots_ext_dates", width = 10, height = 18, device = cairo_pdf) #save the plot in pdf with legend and axis names with Roboto font

################################################
# Plot ext rate and frequency of               #
# ext rate sifts from Miocene in the same plot #
################################################

Plot_Mio <- ggplot(data_ext_from_Mio_default, aes(x= time_ext_Ma, y = rate_ext)) +
  geom_line(data = data_ext_from_Mio_default, aes(x = time_ext_Ma, y = rate_ext), inherit.aes = FALSE, stat = 'identity', colour = "#c6392f", size = 1) +
  geom_ribbon(data = data_ext_from_Mio_default, aes(x = time_ext_Ma, ymin = minHPD_ext, ymax = maxHPD_ext), inherit.aes = FALSE, fill = "#c6392f", alpha = .1)+
  geom_tile(data = data_ext_shift_from_Mio_default, aes(x = mids_Ma, y = -0.2, fill = counts, height=0.25)) +
  scale_fill_gradient2(low="white", high="#c6392f", name = "Frequency of rate shift",limits = c(0,1.0), breaks = c(0, 0.25, 0.50, 0.75, 1.0))+
  xlab("Time (Ma)") + ylab("Extinction rate")+
  scale_y_continuous(breaks=c(0,2,4,6,8))+
  scale_x_continuous(breaks=c(0,-5,-10,-15,-20))+
        theme(axis.title = element_text(size = 16, colour = "grey40"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
    #    legend.title= element_text(size = 8), 
    #    legend.text=element_text(size = 8),
        legend.position = "none",
        panel.grid = element_blank()) 

Plot_Mio

ggsave(Plot_Mio, filename = "Ext_rates_and_shifts_from_Mio.pdf", path = "Results/PyRate_ext_rates/Plots_ext_dates", width = 5, height = 2, device = cairo_pdf) #save the plot in pdf with legend and axis names with Roboto font

################################################
# Plot ext rate and frequency of               #
# ext rate sifts from Pleisto in the same plot #
################################################

data_ext_shift_from_Pleisto_default <- read.csv('Results/PyRate_ext_rates/Plots_ext_dates/Ext_rate_shifts_from_Pleisto_grid_plot_default.csv')

data_ext_shift_from_Pleisto_default <- mutate(data_ext_shift_from_Pleisto_default, mids_Ma = mids/10) #Convert x axis in Ma

Plot_Pleisto <- ggplot(data_ext_from_Pleisto_0.05, aes(x= time_ext_Ma, y = rate_ext)) +
  geom_vline(xintercept = -0.130, linetype = "dotted", colour = "#dc9e31", size = 0.4)+
  geom_vline(xintercept = -0.073, colour = "#dc9e31", size = 0.4)+
  geom_line(data = data_ext_from_Pleisto_0.05, aes(x = time_ext_Ma, y = rate_ext), inherit.aes = FALSE, stat = 'identity', colour = "#c6392f", size = 1) +
  geom_ribbon(data = data_ext_from_Pleisto_0.05, aes(x = time_ext_Ma, ymin = minHPD_ext, ymax = maxHPD_ext), inherit.aes = FALSE, fill = "#c6392f", alpha = .1)+
  geom_tile(data = data_ext_shift_from_Pleisto_default, aes(x = mids_Ma, y = -0.03, fill = counts, height=0.04)) +
  scale_fill_gradient2(low="white", high="#c6392f", name = "Frequency of rate shift",limits = c(0,1.0), breaks = c(0, 0.25, 0.50, 0.75, 1.0))+
  xlab("Time (Ma)") + ylab("Extinction rate")+
  scale_y_continuous(breaks=c(0,0.5,1,1.5,2), limits = c(-0.07,1))+
  scale_x_continuous(breaks=c(0,-0.5,-1,-1.5,-2,-2.5), limits = c(-2.54,0))+
  theme(axis.title = element_text(size = 16, colour = "grey40"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.position = "none",
   #     legend.text=element_text(size = 8),
        panel.grid = element_blank()) 

Plot_Pleisto

ggsave(Plot_Pleisto, filename = "Ext_rates_and_shifts_from_Pleisto.pdf", path = "Results/PyRate_ext_rates/Plots_ext_dates", width = 5, height = 2, device = cairo_pdf) #save the plot in pdf with legend and axis names with Roboto font

#####################################################
# Plot ext rate and frequency of                    #
# ext rate sifts from Late Pleisto in the same plot #
#####################################################

data_ext_shift_from_Late_Pleisto_default_0.1 <- read.csv('Results/PyRate_ext_rates/Plots_ext_dates/Ext_rate_shifts_from_Late_Pleisto_grid_plot_default_min_dt_0.1.csv')

data_ext_shift_from_Late_Pleisto_default_0.1 <- mutate(data_ext_shift_from_Late_Pleisto_default_0.1, mids_Ma = mids/100) #Convert x axis in Ma

Plot_Late_Pleisto <- ggplot(data_ext_from_Late_Pleisto_0.05_0.1, aes(x= time_ext_Ma, y = rate_ext)) +
  geom_line(data = data_ext_from_Late_Pleisto_0.05_0.1, aes(x = time_ext_Ma, y = rate_ext), inherit.aes = FALSE, stat = 'identity', colour = "#c6392f", size = 1) +
  geom_ribbon(data = data_ext_from_Late_Pleisto_0.05_0.1, aes(x = time_ext_Ma, ymin = minHPD_ext, ymax = maxHPD_ext), inherit.aes = FALSE, fill = "#c6392f", alpha = .1)+
  geom_tile(data = data_ext_shift_from_Late_Pleisto_default_0.1, aes(x = mids_Ma, y = -0.1, fill = counts, height=0.1)) +
  scale_fill_gradient2(low="white", high="#c6392f", name = "Frequency of rate shift",limits = c(0,1.0), breaks = c(0, 0.25, 0.50, 0.75, 1.0))+
  guides(fill = guide_colourbar(barwidth = 10, barheight = 0.5))+
  xlab("Time (Ma)") + ylab("Extinction rate")+
  scale_y_continuous(breaks=c(0,1,2))+
  scale_x_continuous(breaks=c(0,-0.025,-0.05,-0.075,-0.1,-0.125), limits = c(-0.129,0))+
    theme(axis.title = element_text(size = 16, colour = "grey40"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.title= element_text(size = 12, colour = "grey40"), 
        legend.text=element_text(size = 12, colour = "grey40"),
        legend.position="bottom",
        panel.grid = element_blank()) 

Plot_Late_Pleisto

ggsave(Plot_Late_Pleisto, filename = "Ext_rates_and_shifts_from_Late_Pleisto_min_dt_0.1.pdf", path = "Results/PyRate_ext_rates/Plots_ext_dates", width = 5, height = 2, device = cairo_pdf) #save the plot in pdf with legend and axis names with Roboto font

#####################################
# Combine all 3 plots for Figure 3A #
#####################################

get_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

LP_legend <- get_legend(Plot_Late_Pleisto)

figure_2 <- ggarrange(Plot_Mio, Plot_Pleisto, Plot_Late_Pleisto,
                      ncol = 3, nrow = 1,
                      common.legend = TRUE,
                      legend.grob = LP_legend,
                      legend = "bottom")

figure_2

ggsave(figure_2, filename = "Figure_3A.pdf", path = "Results/PyRate_ext_rates/Plots_ext_dates", width = 10, height = 3, device = cairo_pdf) #save the plot in pdf with legend and axis names with Roboto font

###################################################################################
# Load data and plot ext rates from Late Pleisto; time unit = 10 ka            ####
# iterations = 100 M; grid_plot 0.05; min_dt 0.1; Madagascar only direct dates ####
###################################################################################

data_ext_from_Late_Pleisto_0.05_0.1_M <- read.csv('Results/PyRate_ext_rates/Plots_ext_dates/Ext_rates_from_LP_Madag_only_direct.csv')

data_ext_from_Late_Pleisto_0.05_0.1_M <- mutate(data_ext_from_Late_Pleisto_0.05_0.1_M, time_ext_Ma = time_ext/100) #Convert x axis in Ma

plot_ext_from_Late_Pleisto_0.05_0.1_M <- ggplot(data_ext_from_Late_Pleisto_0.05_0.1_M, aes(x= time_ext_Ma, y = rate_ext)) +
  geom_line(data = data_ext_from_Late_Pleisto_0.05_0.1_M, aes(x = time_ext_Ma, y = rate_ext), inherit.aes = FALSE, stat = 'identity', colour = "#c6392f", size = 1) +
  geom_ribbon(data = data_ext_from_Late_Pleisto_0.05_0.1_M, aes(x = time_ext_Ma, ymin = minHPD_ext, ymax = maxHPD_ext), inherit.aes = FALSE, fill = "#c6392f", alpha = .1)+
  xlab("Time (Ma)") + ylab("Extinction rate")+
  scale_y_continuous(breaks=c(0,0.5,1,1.5,2))+
  scale_x_continuous(breaks=c(0,-0.025,-0.05,-0.075,-0.1,-0.125))+
  theme(axis.title = element_text(family = "Arial", size = 18, colour = "grey40"),
        axis.text.x = element_text(family = "Arial", size = 14),
        axis.text.y = element_text(family = "Arial", size = 14),
        panel.grid = element_blank()) 

plot_ext_from_Late_Pleisto_0.05_0.1_M

###########################################################################
# Load data and plot frequency of ext rate shifts from Late Pleisto;   ####
# time unit = 10 ka; iterations = 100 M; grid_plot 0.05; min_dt 0.1;   ####
# Madagascar only direct dates                                         ####
###########################################################################

bf2_Late_Pleisto_0.05_0.1_M = 0.008940946764048881
bf6_Late_Pleisto_0.05_0.1_M = 0.062495169975149416

data_ext_shift_from_Late_Pleisto_0.05_0.1_M <- read.csv('Results/PyRate_ext_rates/Plots_ext_dates/Ext_rate_shifts_from_LP_Madag_only_direct.csv')

data_ext_shift_from_Late_Pleisto_0.05_0.1_M <- mutate(data_ext_shift_from_Late_Pleisto_0.05_0.1_M, mids_Ma = mids/100) #Convert x axis in Ma

plot_ext_shift_from_Late_Pleisto_0.05_0.1_M <- ggplot(data_ext_shift_from_Late_Pleisto_0.05_0.1_M, aes(x= mids_Ma, y = counts)) +
  geom_segment(aes(x=mids_Ma, xend=mids_Ma, y=0, yend=counts), color="#c6392f", size = .05) +
  geom_point( color="#c6392f", size=.3) +
  xlab("Time (Ma)") + ylab("Frequency of rate shift")+
  scale_y_continuous(breaks=c(0,0.2,0.4,0.6,0.8,1.0))+
  scale_x_continuous(breaks=c(0,-0.025,-0.05,-0.075,-0.1,-0.125))+
  geom_hline(yintercept=bf2_Late_Pleisto_0.05_0.1_M, linetype="dashed", color = "grey40", size=.3)+
  geom_hline(yintercept=bf6_Late_Pleisto_0.05_0.1_M, linetype="dashed", color = "grey40", size=.3)+
  theme(axis.title = element_text(family = "Arial", size = 18, colour = "grey40"),
        axis.text.x = element_text(family = "Arial", size = 14),
        axis.text.y = element_text(family = "Arial", size = 14),
        panel.grid = element_blank())

plot_ext_shift_from_Late_Pleisto_0.05_0.1_M

#########################################################################
# Plot magnitude of extinction rate increase relative to the base value #
#########################################################################

data_ext_from_Late_Pleisto_0.05_0.1_M <- mutate(data_ext_from_Late_Pleisto_0.05_0.1_M, ext_rate_increase = rate_ext/0.02163634)

plot_ext_increase_from_Late_Pleisto_0.05_0.1_M <- ggplot(data_ext_from_Late_Pleisto_0.05_0.1_M, aes(x= time_ext_Ma, y = ext_rate_increase)) +
  geom_line(data = data_ext_from_Late_Pleisto_0.05_0.1, aes(x = time_ext_Ma, y = ext_rate_increase), inherit.aes = FALSE, stat = 'identity', colour = "#c6392f", size = 1) +
  xlab("Time (Ma)") + ylab("Magnitude of rate increase")+
  scale_y_continuous(breaks=c(1,25,50,75))+
  scale_x_continuous(breaks=c(0,-0.025,-0.05,-0.075,-0.1,-0.125))+
  theme(axis.title = element_text(family = "Arial", size = 18, colour = "grey40"),
        axis.text.x = element_text(family = "Arial", size = 14),
        axis.text.y = element_text(family = "Arial", size = 14),
        panel.grid = element_blank()) 

plot_ext_increase_from_Late_Pleisto_0.05_0.1_M

#############################################################################################
# Load data and plot ext rates from Miocene; time unit = 250 ka                          ####
# iterations = 100 M; grid_plot 0.05; only final representatives of anagenetic lineages  ####
#############################################################################################

data_ext_from_Mio_0.05_ANAG <- read.csv('Results/PyRate_ext_rates/Plots_ext_dates/Ext_rates_from_Mio_anagen_cut.csv')

data_ext_from_Mio_0.05_ANAG <- mutate(data_ext_from_Mio_0.05_ANAG, time_ext_Ma = time_ext/4) #Convert x axis in Ma

plot_ext_from_Mio_0.05_ANAG <- ggplot(data_ext_from_Mio_0.05_ANAG, aes(x= time_ext_Ma, y = rate_ext)) +
  geom_line(data = data_ext_from_Mio_0.05_ANAG, aes(x = time_ext_Ma, y = rate_ext), inherit.aes = FALSE, stat = 'identity', colour = "#c6392f", size = 1) +
  geom_ribbon(data = data_ext_from_Mio_0.05_ANAG, aes(x = time_ext_Ma, ymin = minHPD_ext, ymax = maxHPD_ext), inherit.aes = FALSE, fill = "#c6392f", alpha = .1)+
  xlab("Time (Ma)") + ylab("Extinction rate")+
  #  scale_y_sqrt(breaks=c(0,2,4,6,8))+
  scale_y_continuous(breaks=c(0,2,4,6,8))+
  scale_x_continuous(breaks=c(0,-5,-10,-15,-20))+
  theme(axis.title = element_text(family = "Arial", size = 18, colour = "grey40"),
        axis.text.x = element_text(family = "Arial", size = 14),
        axis.text.y = element_text(family = "Arial", size = 14),
        panel.grid = element_blank()) 

plot_ext_from_Mio_0.05_ANAG

######################################################################
# Load data and plot frequency of ext rate shifts from Miocene;   ####
# time unit = 250 ka; iterations = 100 M; grid_plot 0.05;         ####
# only final representatives of anagenetic lineages               ####
######################################################################

bf2_Mio_ANAG = 0.0022049707868768607
bf6_Mio_ANAG = 0.016066315693469845

data_ext_shift_from_Mio_0.05_ANAG <- read.csv('Results/PyRate_ext_rates/Plots_ext_dates/Ext_rate_shifts_from_Mio_anagen_cut.csv')

data_ext_shift_from_Mio_0.05_ANAG <- mutate(data_ext_shift_from_Mio_0.05_ANAG, mids_Ma = mids/4) #Convert x axis in Ma

plot_ext_shift_from_Mio_0.05_ANAG <- ggplot(data_ext_shift_from_Mio_0.05_ANAG, aes(x= mids_Ma, y = counts)) +
  geom_segment(aes(x=mids_Ma, xend=mids_Ma, y=0, yend=counts), color="#c6392f", size = .05) +
  geom_point( color="#c6392f", size=.3) +
  xlab("Time (Ma)") + ylab("Frequency of rate shift")+
  scale_y_continuous(breaks=c(0,0.2,0.4,0.6,0.8,1.0))+
  scale_x_continuous(breaks=c(0,-5,-10,-15,-20))+
  geom_hline(yintercept=bf2_Mio_ANAG, linetype="dashed", color = "grey40", size=.3)+
  geom_hline(yintercept=bf6_Mio_ANAG, linetype="dashed", color = "grey40", size=.3)+
  theme(axis.title = element_text(family = "Arial", size = 18, colour = "grey40"),
        axis.text.x = element_text(family = "Arial", size = 14),
        axis.text.y = element_text(family = "Arial", size = 14),
        panel.grid = element_blank())

plot_ext_shift_from_Mio_0.05_ANAG

#########################################################################
# Plot magnitude of extinction rate increase relative to the base value #
#########################################################################

data_ext_from_Mio_0.05_ANAG <- mutate(data_ext_from_Mio_0.05_ANAG, ext_rate_increase = rate_ext/0.8285775)

plot_ext_increase_from_Mio_0.05_ANAG <- ggplot(data_ext_from_Mio_0.05_ANAG, aes(x= time_ext_Ma, y = ext_rate_increase)) +
  geom_line(data = data_ext_from_Mio_0.05_ANAG, aes(x = time_ext_Ma, y = ext_rate_increase), inherit.aes = FALSE, stat = 'identity', colour = "#c6392f", size = 1) +
  xlab("Time (Ma)") + ylab("Magnitude of rate increase")+
  #  scale_y_sqrt(breaks=c(0,2,4,6,8))+
  scale_y_continuous(breaks=c(-0.5,0,0.5,1,1.5,2))+
  scale_x_continuous(breaks=c(0,-5,-10,-15,-20))+
  theme(axis.title = element_text(family = "Arial", size = 18, colour = "grey40"),
        axis.text.x = element_text(family = "Arial", size = 14),
        axis.text.y = element_text(family = "Arial", size = 14),
        panel.grid = element_blank()) 

plot_ext_increase_from_Mio_0.05_ANAG

#############################################################################################
# Load data and plot ext rates from Pleisto; time unit = 100 ka                          ####
# iterations = 100 M; grid_plot 0.05; only final representatives of anagenetic lineages  ####
#############################################################################################

data_ext_from_Pleisto_0.05_ANAG <- read.csv('Results/PyRate_ext_rates/Plots_ext_dates/Ext_rates_from_Pleisto_anagen_cut.csv')

data_ext_from_Pleisto_0.05_ANAG <- mutate(data_ext_from_Pleisto_0.05_ANAG, time_ext_Ma = time_ext/10) #Convert x axis in Ma

plot_ext_from_Pleisto_0.05_ANAG <- ggplot(data_ext_from_Pleisto_0.05_ANAG, aes(x= time_ext_Ma, y = rate_ext)) +
  geom_line(data = data_ext_from_Pleisto_0.05_ANAG, aes(x = time_ext_Ma, y = rate_ext), inherit.aes = FALSE, stat = 'identity', colour = "#c6392f", size = 1) +
  geom_ribbon(data = data_ext_from_Pleisto_0.05_ANAG, aes(x = time_ext_Ma, ymin = minHPD_ext, ymax = maxHPD_ext), inherit.aes = FALSE, fill = "#c6392f", alpha = .1)+
  xlab("Time (Ma)") + ylab("Extinction rate")+
  scale_y_continuous(breaks=c(0,0.2,0.4,0.6,0.8,1.0))+
  scale_x_continuous(breaks=c(0,-0.5,-1,-1.5,-2,-2.5))+
  theme(axis.title = element_text(family = "Arial", size = 18, colour = "grey40"),
        axis.text.x = element_text(family = "Arial", size = 14),
        axis.text.y = element_text(family = "Arial", size = 14),
        panel.grid = element_blank()) 

plot_ext_from_Pleisto_0.05_ANAG

######################################################################
# Load data and plot frequency of ext rate shifts from Pleisto;   ####
# time unit = 100 ka; iterations = 100 M; grid_plot 0.05;         ####
# only final representatives of anagenetic lineages               ####
######################################################################

bf2_Pleisto_ANAG = 0.005514684894930311
bf6_Pleisto_ANAG = 0.039361468590275485

data_ext_shift_from_Pleisto_0.05_ANAG <- read.csv('Results/PyRate_ext_rates/Plots_ext_dates/Ext_rate_shifts_from_Pleisto_anagen_cut.csv')

data_ext_shift_from_Pleisto_0.05_ANAG <- mutate(data_ext_shift_from_Pleisto_0.05_ANAG, mids_Ma = mids/10) #Convert x axis in Ma

plot_ext_shift_from_Pleisto_0.05_ANAG <- ggplot(data_ext_shift_from_Pleisto_0.05_ANAG, aes(x= mids_Ma, y = counts)) +
  geom_segment(aes(x=mids_Ma, xend=mids_Ma, y=0, yend=counts), color="#c6392f", size = .05) +
  geom_point( color="#c6392f", size=.3) +
  xlab("Time (Ma)") + ylab("Frequency of rate shift")+
  scale_y_continuous(breaks=c(0,0.2,0.4,0.6,0.8,1.0))+
  scale_x_continuous(breaks=c(0,-0.5,-1,-1.5,-2,-2.5))+
  geom_hline(yintercept=bf2_Pleisto_ANAG, linetype="dashed", color = "grey40", size=.3)+
  geom_hline(yintercept=bf6_Pleisto_ANAG, linetype="dashed", color = "grey40", size=.3)+
  theme(axis.title = element_text(family = "Arial", size = 18, colour = "grey40"),
        axis.text.x = element_text(family = "Arial", size = 14),
        axis.text.y = element_text(family = "Arial", size = 14),
        panel.grid = element_blank())

plot_ext_shift_from_Pleisto_0.05_ANAG

#########################################################################
# Plot magnitude of extinction rate increase relative to the base value #
#########################################################################

data_ext_from_Pleisto_0.05_ANAG <- mutate(data_ext_from_Pleisto_0.05_ANAG, ext_rate_increase = rate_ext/0.05864778)

plot_ext_increase_from_Pleisto_0.05_ANAG <- ggplot(data_ext_from_Pleisto_0.05_ANAG, aes(x= time_ext_Ma, y = ext_rate_increase)) +
  geom_line(data = data_ext_from_Pleisto_0.05_ANAG, aes(x = time_ext_Ma, y = ext_rate_increase), inherit.aes = FALSE, stat = 'identity', colour = "#c6392f", size = 1) +
  xlab("Time (Ma)") + ylab("Magnitude of rate increase")+
  scale_y_continuous(breaks=c(1,3,6,9,12))+
  scale_x_continuous(breaks=c(0,-0.5,-1,-1.5,-2,-2.5))+
  theme(axis.title = element_text(family = "Arial", size = 18, colour = "grey40"),
        axis.text.x = element_text(family = "Arial", size = 14),
        axis.text.y = element_text(family = "Arial", size = 14),
        panel.grid = element_blank()) 

plot_ext_increase_from_Pleisto_0.05_ANAG

##########################################################################################################
# Load data and plot ext rates from Late Pleisto; time unit = 10 ka                                   ####
# iterations = 100 M; grid_plot 0.05; mind_dt 0.1; only final representatives of anagenetic lineages  ####
##########################################################################################################

data_ext_from_Late_Pleisto_0.05_0.1_ANAG <- read.csv('Results/PyRate_ext_rates/Plots_ext_dates/Ext_rates_from_LP_anagen_cut.csv')

data_ext_from_Late_Pleisto_0.05_0.1_ANAG <- mutate(data_ext_from_Late_Pleisto_0.05_0.1_ANAG, time_ext_Ma = time_ext/100) #Convert x axis in Ma

plot_ext_from_Late_Pleisto_0.05_0.1_ANAG <- ggplot(data_ext_from_Late_Pleisto_0.05_0.1_ANAG, aes(x= time_ext_Ma, y = rate_ext)) +
  geom_line(data = data_ext_from_Late_Pleisto_0.05_0.1_ANAG, aes(x = time_ext_Ma, y = rate_ext), inherit.aes = FALSE, stat = 'identity', colour = "#c6392f", size = 1) +
  geom_ribbon(data = data_ext_from_Late_Pleisto_0.05_0.1_ANAG, aes(x = time_ext_Ma, ymin = minHPD_ext, ymax = maxHPD_ext), inherit.aes = FALSE, fill = "#c6392f", alpha = .1)+
  xlab("Time (Ma)") + ylab("Extinction rate")+
  scale_y_continuous(breaks=c(0,0.5,1,1.5,2))+
  scale_x_continuous(breaks=c(0,-0.025,-0.05,-0.075,-0.1,-0.125))+
  theme(axis.title = element_text(family = "Arial", size = 18, colour = "grey40"),
        axis.text.x = element_text(family = "Arial", size = 14),
        axis.text.y = element_text(family = "Arial", size = 14),
        panel.grid = element_blank()) 

plot_ext_from_Late_Pleisto_0.05_0.1_ANAG

###########################################################################
# Load data and plot frequency of ext rate shifts from Late Pleisto;   ####
# time unit = 10 ka; iterations = 100 M; grid_plot 0.05; min_dt 0.1;   ####
# only final representatives of anagenetic lineages                    ####
###########################################################################

bf2_Late_Pleisto_0.05_0.1_ANAG = 0.008938985400863235
bf6_Late_Pleisto_0.05_0.1_ANAG = 0.06248220116618506

data_ext_shift_from_Late_Pleisto_0.05_0.1_ANAG <- read.csv('Results/PyRate_ext_rates/Plots_ext_dates/Ext_rate_shifts_from_LP_anagen_cut.csv')

data_ext_shift_from_Late_Pleisto_0.05_0.1_ANAG <- mutate(data_ext_shift_from_Late_Pleisto_0.05_0.1_ANAG, mids_Ma = mids/100) #Convert x axis in Ma

plot_ext_shift_from_Late_Pleisto_0.05_0.1_ANAG <- ggplot(data_ext_shift_from_Late_Pleisto_0.05_0.1_ANAG, aes(x= mids_Ma, y = counts)) +
  geom_segment(aes(x=mids_Ma, xend=mids_Ma, y=0, yend=counts), color="#c6392f", size = .05) +
  geom_point( color="#c6392f", size=.3) +
  xlab("Time (Ma)") + ylab("Frequency of rate shift")+
  scale_y_continuous(breaks=c(0,0.2,0.4,0.6,0.8,1.0))+
  scale_x_continuous(breaks=c(0,-0.025,-0.05,-0.075,-0.1,-0.125))+
  geom_hline(yintercept=bf2_Late_Pleisto_0.05_0.1_ANAG, linetype="dashed", color = "grey40", size=.3)+
  geom_hline(yintercept=bf6_Late_Pleisto_0.05_0.1_ANAG, linetype="dashed", color = "grey40", size=.3)+
  theme(axis.title = element_text(family = "Arial", size = 18, colour = "grey40"),
        axis.text.x = element_text(family = "Arial", size = 14),
        axis.text.y = element_text(family = "Arial", size = 14),
        panel.grid = element_blank())

plot_ext_shift_from_Late_Pleisto_0.05_0.1_ANAG

#########################################################################
# Plot magnitude of extinction rate increase relative to the base value #
#########################################################################

data_ext_from_Late_Pleisto_0.05_0.1_ANAG <- mutate(data_ext_from_Late_Pleisto_0.05_0.1_ANAG, ext_rate_increase = rate_ext/0.02094461)

plot_ext_increase_from_Late_Pleisto_0.05_0.1_ANAG <- ggplot(data_ext_from_Late_Pleisto_0.05_0.1_ANAG, aes(x= time_ext_Ma, y = ext_rate_increase)) +
  geom_line(data = data_ext_from_Late_Pleisto_0.05_0.1_ANAG, aes(x = time_ext_Ma, y = ext_rate_increase), inherit.aes = FALSE, stat = 'identity', colour = "#c6392f", size = 1) +
  xlab("Time (Ma)") + ylab("Magnitude of rate increase")+
  scale_y_continuous(breaks=c(1,25,50,75))+
  scale_x_continuous(breaks=c(0,-0.025,-0.05,-0.075,-0.1,-0.125))+
  theme(axis.title = element_text(family = "Arial", size = 18, colour = "grey40"),
        axis.text.x = element_text(family = "Arial", size = 14),
        axis.text.y = element_text(family = "Arial", size = 14),
        panel.grid = element_blank()) 

plot_ext_increase_from_Late_Pleisto_0.05_0.1_ANAG

###########################################################################
# Combine all 9 plots for Extended Data Fig. 9                            #
# Sensitivity analysis Madagascar only direct dates + anagenetic lineages # 
###########################################################################

figure_3 <- ggarrange(plot_ext_from_Late_Pleisto_0.05_0.1_M, plot_ext_shift_from_Late_Pleisto_0.05_0.1_M,
                      plot_ext_from_Mio_0.05_ANAG, plot_ext_shift_from_Mio_0.05_ANAG,
                      plot_ext_from_Pleisto_0.05_ANAG, plot_ext_shift_from_Pleisto_0.05_ANAG,
                      plot_ext_from_Late_Pleisto_0.05_0.1_ANAG, plot_ext_shift_from_Late_Pleisto_0.05_0.1_ANAG,
                      ncol = 1, nrow = 8)
figure_3

ggsave(figure_3, filename = "Ext_rates_from_Mio_Pl_and_LP_MAD&ANAGEN_CUT.pdf", path = "Results/PyRate_ext_rates/Plots_ext_dates", width = 10, height = 16, device = cairo_pdf) #save the plot in pdf with legend and axis names with Roboto font

#End of script

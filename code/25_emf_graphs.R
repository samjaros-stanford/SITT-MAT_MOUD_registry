library(extrafont)
#font_import()
#loadfonts(device="win") # NOTE: May need to change this to have it work on Mac
library(ggplot2)
library(grid)
library(lubridate)
library(scales)
library(tidyverse)

############
# Settings #
############
# QRIR Data assembled by code/09_combine_for_emf.R
report_data = read.csv(here::here("data/current_QRIR.csv")) %>%
  mutate(date = ymd(date))
# Colors for most plots
SITTMAT_colors = c("#007ea7", "#ff9f1c", "#5d9818", "#e71d36", "#9900ff", "#00ffff")
# Colors for linkage plots
red_yellow_green = c("#e71d36", "#ff9f1c", "#5d9818")
font = "Century Gothic"
month_for_filenames = "July23"
reaim_dims = c(5.4,2.12) # in inches, normally 5.49, 1.3
imat_size = "short" #tall or short
imat_dates = c("Sep-22", "Apr-23")

##########
# RE-AIM #
##########

# Line plot for percent patients diagnosed with OUD on MOUD
make_MOUDplot = function(id, save=F){
  plot_data = report_data %>%
    filter(program_id==id, variable %in% c("reaim_b1", "reaim_b2", "reaim_b4", 
                                           "reaim_b4p", "reaim_b5", "reaim_b5p")) %>%
    # Presence of the new variable "reaim_b5p" indices this site is using the new measures
    # If they are using the new measures, b2, b5, and b5p should be used for the graph
    group_by(program_id) %>%
    mutate(new_measures = any(variable=="reaim_b5p")) %>%
    ungroup() %>%
    mutate(graph_var = case_when(
      new_measures & variable=="reaim_b2"  ~ "denom",
      new_measures & variable=="reaim_b5"  ~ "numer",
      new_measures & variable=="reaim_b5p" ~ "percent",
      !new_measures & variable=="reaim_b1"  ~ "denom",
      !new_measures & variable=="reaim_b4"  ~ "numer",
      !new_measures & variable=="reaim_b4p" ~ "percent",
      T ~ NA_character_)) %>%
    filter(!is.na(graph_var)) %>%
    # Pivot wider for graphing
    pivot_wider(id_cols = c(program_id, date),
                names_from = graph_var,
                values_from = value)
  
  targetLabel_x = max(plot_data$date)
  targetLabel_y = ifelse(plot_data[plot_data$date==max(plot_data$date),"percent"]>75,65,85)
  # For manual adjustment of the target label, when needed
  #targetLabel_x = ymd("2022-10-01")
  #targetLabel_y = 90
  plot = ggplot(plot_data, aes(x=date, y=percent)) +
    geom_hline(yintercept=75, linetype="dashed", color=SITTMAT_colors[3]) +
    geom_text(aes(x=targetLabel_x,y=targetLabel_y,label="SITT-MAT Target"), 
              color=SITTMAT_colors[3], size=3, family=font, hjust=0.8) +
    geom_point(aes(color=program_id), size=3, show.legend=F) +
    geom_line(aes(color=program_id), linewidth=1, show.legend=F) +
    geom_label(aes(label=paste0(numer, "/", denom)), size=3, family=font) +
    scale_color_manual(values=SITTMAT_colors) +
    scale_x_date(date_breaks="month", date_labels="%b-%y") +
    scale_y_continuous(breaks=seq(0,100,25), labels=function(x) paste0(x,"%"), limits=c(0,100)) +
    theme_minimal() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.title = element_blank(),
          axis.text = element_text(size=10, family=font, color="black"),
          plot.title = element_text(size=13, hjust=0.5)) +
    coord_cartesian(clip="off")
  
  if(save){
    filepath = paste0("figures/QRIR_figures_",month_for_filenames,"/", id, "/", id, "_MOUDplot_", month_for_filenames, ".png")
    ggsave(here::here(filepath), plot, width=reaim_dims[1], height=reaim_dims[2], units="in")
  }
  
  return(plot)
}

# Line plot for percent new patients diagnosed with OUD put on MOUD within 72h
make_72Access = function(id, save=F){
  plot_data = report_data %>%
    filter(program_id==id, variable %in% c("reaim_c1_n", "reaim_b2")) %>%
    # Flip to get variables as columns
    pivot_wider(id_cols=c(program_id, date),
                names_from=variable,
                values_from=value) %>%
    # Calculate % OUD patients given MOUD within 72h
    mutate(percent72h = if_else(reaim_b2==0 | is.na(reaim_c1_n), 0, 
                                if_else(reaim_c1_n>reaim_b2, 100, reaim_c1_n/reaim_b2*100)))
    
  plot = ggplot(plot_data, aes(x=date, y=percent72h)) +
    geom_point(aes(color=program_id), size=3, show.legend=F) +
    geom_line(aes(color=program_id), linewidth=1, show.legend=F) +
    geom_label(aes(label=paste0(reaim_c1_n,"/",reaim_b2)), size=3, family=font) +
    scale_color_manual(values=SITTMAT_colors) +
    scale_x_date(date_breaks="month", date_labels="%b-%y") +
    scale_y_continuous(breaks=seq(0,100,25), labels=function(x) paste0(x,"%"), limits=c(0,100)) +
    theme_minimal() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.title = element_blank(),
          axis.text = element_text(size=10, family=font, color="black")) +
    coord_cartesian(clip="off")
  
  if(save){
    filepath = paste0("figures/QRIR_figures_",month_for_filenames,"/", id, "/", id, "_72Access_", month_for_filenames, ".png")
    ggsave(here::here(filepath), plot, width=reaim_dims[1], height=reaim_dims[2], units="in")
  }
  
  return(plot)
}

# Bar plot for percent of patients discharged by linkage status
make_referralLinkage = function(id, save=F){
  referral_vars = c("reaim_c6", "reaim_c7", "reaim_c8")
  referral_labels = list("reaim_c6"="Referred and linkage confirmed",
                         "reaim_c7"="Referred but did not confirm linkage",
                         "reaim_c8"="No referral")
  
  plot_data = report_data %>%
    filter(program_id==id, variable %in% referral_vars) %>%
    mutate(variable = factor(variable, levels=rev(referral_vars), ordered=T))
  
  plot = ggplot(plot_data, aes(x=date, y=value)) +
    geom_col(aes(fill=variable), width=10) +
    scale_fill_manual(labels=~referral_labels[.x], values=red_yellow_green, guide=guide_legend(reverse=T, override.aes=list(shape=15))) +
    scale_x_date(date_breaks="month", date_labels="%b-%y") +
    scale_y_continuous(labels=~paste0(.x,"%")) +
    theme_minimal() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.title = element_blank(),
          axis.text = element_text(size=10, family=font, color="black"),
          legend.position = "bottom",
          legend.title = element_blank(),
          legend.key.size = unit(3, "pt"),
          legend.text = element_text(size=8, family=font, color="black"),
          legend.box.margin = margin(-12,-10,-8,-10)) +
    coord_cartesian(clip="off")
  
  if(save){
    filepath = paste0("figures/QRIR_figures_",month_for_filenames,"/", id, "/", id, "_referralLinkage_", month_for_filenames, ".png")
    ggsave(here::here(filepath), plot, width=reaim_dims[1], height=reaim_dims[2], units="in")
  }
  
  return(plot)
}

########
# IMAT #
########

# Line plot for IMAT subscale & total by month
make_imat = function(id, save=F){
  # Need to set sizing based on tall/short desired size
  #   imat_dims specifies final plot size in inches (width, height)
  #   imat_text_size specifies text sizes (all other text, x axis labels)
  if(imat_size=="short"){
    imat_dims = c(7.5,2.24)
    imat_text_size = c(8,7.8)
  } else {
    imat_dims = c(7.5,5)
    imat_text_size = c(11, 8.2)
  }
  
  imat_labels = list("imat_d1" = "Infrastructure",
                     "imat_d2" = "Clinic\nCulture &\nEnvironment",
                     "imat_d3" = "Patient\nIdentification\n& Initiating\nCare",
                     "imat_d4" = "Care Delivery\n& Treatment\nResponse\nMonitoring",
                     "imat_d5" = "Care\nCoordination",
                     "imat_d6" = "Workforce",
                     "imat_d7" = "Staff Training\n& Development",
                     "imat_s1" = "Low Barrier\nCare",
                     "imat_total" = "Total")
  imat_scale_labels = c("1: Not\nIntegrated", "2", "3: Partially\nIntegrated",
                        "4", "5: Fully\nIntegrated")

  if (id=="all") {
    plot_data = report_data %>%
      filter(startsWith(variable, prefix="imat")) %>%
      group_by(date, variable) %>%
      summarize(value = mean(value, na.rm=T), .groups="keep") %>%
      # Dates need to be factors for legend
      mutate(date = factor(format(date, "%b-%y"), levels=imat_dates, ordered=T))
  } else {
    plot_data = report_data %>%
      filter(program_id==id, startsWith(variable, "imat")) %>%
      # Dates need to be factors for legend
      mutate(date = factor(format(date, "%b-%y"), levels=imat_dates, ordered=T))
  }
  
  plot = ggplot(plot_data, aes(x=variable, y=value, group=date)) +
    geom_line(aes(color=date), linewidth=2, show.legend=F) +
    geom_point(aes(color=date), size=4) +
    scale_color_manual(breaks=imat_dates, values=SITTMAT_colors) +
    scale_x_discrete(labels=~imat_labels[.x]) +
    scale_y_continuous(limits=c(1,5), breaks=1:5, labels=imat_scale_labels) +
    theme_minimal() +
    theme(axis.title = element_blank(),
          axis.text.x = element_text(size=imat_text_size[2]),
          axis.text.y = element_text(size=imat_text_size[1]),
          axis.text = element_text(family=font, color="black"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          plot.margin = margin(-10,2,20,2),
          legend.box.margin = margin(5,-10,-10,-10),
          legend.position = "top",
          legend.text = element_text(size=imat_text_size[1], family=font, color="black"),
          legend.title = element_blank()) +
    coord_cartesian(clip="off")
  
  # Annotate plot to specify which variables are dimensions
  dim_line = linesGrob(x=c(.4,10.5), y=c(-2,-2), gp=gpar(col="black", lwd=1))
  dim_text = textGrob(label="DIMENSIONS", hjust=0.5, gp=gpar(fontfamily=font, fontsize=imat_text_size[1]))
  sub_line = linesGrob(x=c(14.25,15.9), y=c(-2,-2), gp=gpar(col="black", lwd=1))
  sub_text = textGrob(label="SUBSCALE", hjust=0.5, gp=gpar(fontfamily=font, fontsize=imat_text_size[1]))
  total_line = linesGrob(x=c(16.2,17.85), y=c(-2,-2), gp=gpar(col="black", lwd=1))
  total_text = textGrob(label="TOTAL", hjust=0.5, gp=gpar(fontfamily=font, fontsize=imat_text_size[1]))
  
  if(imat_size=="short"){
    plot = plot +
      annotation_custom(dim_line, ymin=-1.33, ymax=-1.33, xmin=.2, xmax=.9) +
      annotation_custom(dim_text, ymin=-2.1, ymax=-1.1, xmin=1.5, xmax=6.5) +
      annotation_custom(sub_line, ymin=-1.33, ymax=-1.33, xmin=.5, xmax=.999) +
      annotation_custom(sub_text, ymin=-2.1, ymax=-1.1, xmin=7.51, xmax=8.51) +
      annotation_custom(total_line, ymin=-1.33, ymax=-1.33, xmin=.5, xmax=.999) +
      annotation_custom(total_text, ymin=-2.1, ymax=-1.1, xmin=8.51, xmax=9.51)
  } else {
    plot = plot +
      annotation_custom(dim_line, ymin=0.12, ymax=0.12, xmin=.2, xmax=.9) +
      annotation_custom(dim_text, ymin=-0.5, ymax=0.5, xmin=1.5, xmax=6.5) +
      annotation_custom(sub_line, ymin=0.12, ymax=0.12, xmin=.5, xmax=.999) +
      annotation_custom(sub_text, ymin=-0.5, ymax=0.5, xmin=7.51, xmax=8.51) +
      annotation_custom(total_line, ymin=0.12, ymax=0.12, xmin=.5, xmax=.999) +
      annotation_custom(total_text, ymin=-0.5, ymax=0.5, xmin=8.51, xmax=9.51)
  }
  
  if(save){
    filepath = here::here(paste0("figures/QRIR_figures_",month_for_filenames,"/", id, "/", id, "_imat_", month_for_filenames, ".png"))
    ggsave(here::here(filepath), plot, width=imat_dims[1], height=imat_dims[2], units="in")
  }
  
  return(plot)
}

##########################
# | USE THIS FOR PLOTS | #
# v  Runner functions  v #
##########################

# Produce all plots
#   If show_plots=T, it will ask for user input before showing next plot
make_allPlots = function(id, save_plots=F, show_plots=T){
  print(paste0("Creating plots for ", id))
  p = make_MOUDplot(id, save=save_plots)
  if(show_plots){
    print(p)
    invisible(readline(prompt="Press [enter] to continue"))
  }
  p = make_72Access(id, save=save_plots)
  if(show_plots){
    print(p)
    invisible(readline(prompt="Press [enter] to continue"))
  }
  p = make_referralLinkage(id, save=save_plots)
  if(show_plots){
    print(p)
    invisible(readline(prompt="Press [enter] to continue"))
  }
  p = make_imat(id, save=save_plots)
  if(show_plots) print(p)
}
# Define which sites you want to produce plots for
#programs = sort(unique(report_data$program_id))
programs = c("id01","id15","id17","id34","id35","id44")
# Iterate through programs & produce all plots
for(program in programs){
  make_allPlots(program, save_plots=T, show_plots=F)
}


# Produce only IMAT (for sites that don't have REAIM data)
#imat_programs = sort(unique(report_data %>% filter(startsWith(variable,"imat")) %>% pull(program_id)))
imat_programs = c("id51")
for(program in imat_programs){
  print(paste0("Creating IMAT plot for ", program))
  make_imat(program, save=T)
}

library(extrafont)
#font_import()
loadfonts(device="win")
library(ggplot2)
library(grid)
library(lubridate)
library(scales)
library(tidyverse)

############
# Settings #
############
report_data = read.csv(here::here("data/current_QRIR.csv")) %>%
  mutate(date = ymd(date))
SITTMAT_colors = c("#007ea7", "#ff9f1c", "#5d9818", "#e71d36", "#9900ff", "#00ffff")
red_yellow_green = c("#e71d36", "#ff9f1c", "#5d9818")
font = "Century Gothic"
month_for_filenames = "Apr23"
reaim_dims = c(5.49,1.3) # in inches
imat_dims = c(7.5,2.24) # in inches
imat_dates = c("Sep-22", "Apr-23")

##########
# RE-AIM #
##########

# Line plot for percent patients diagnosed with OUD on MOUD
make_MOUDplot = function(id, save=F){
  plot_data = report_data %>%
    filter(program_id==id, variable%in%c("reaim_b1", "reaim_b4", "reaim_b4p")) %>%
    pivot_wider(id_cols = c(program_id, date),
                names_from = variable,
                values_from = value)
  
  plot = ggplot(plot_data, aes(x=date, y=reaim_b4p)) +
    geom_point(aes(color=program_id), size=3, show.legend=F) +
    geom_line(aes(color=program_id), linewidth=1, show.legend=F) +
    geom_label(aes(label=paste0(reaim_b4, "/", reaim_b1)), size=3, family=font) +
    scale_color_manual(values=SITTMAT_colors) +
    scale_x_date(date_labels="%b-%y") +
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
    filepath = paste0("figures/QRIR_figures/", id, "/", id, "_MOUDplot_", month_for_filenames, ".png")
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
    scale_x_date(date_labels="%b-%y") +
    scale_y_continuous(breaks=seq(0,100,25), labels=function(x) paste0(x,"%"), limits=c(0,100)) +
    theme_minimal() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.title = element_blank(),
          axis.text = element_text(size=10, family=font, color="black")) +
    coord_cartesian(clip="off")
  
  if(save){
    filepath = paste0("figures/QRIR_figures/", id, "/", id, "_72Access_", month_for_filenames, ".png")
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
    scale_x_date(date_labels="%b-%y") +
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
    filepath = paste0("figures/QRIR_figures/", id, "/", id, "_referralLinkage_", month_for_filenames, ".png")
    ggsave(here::here(filepath), plot, width=reaim_dims[1], height=reaim_dims[2], units="in")
  }
  
  return(plot)
}

########
# IMAT #
########

# Line plot for IMAT subscale & total by month
make_imat = function(id, save=F){
  imat_labels = list("imat_d1" = "Infrastructure",
                     "imat_d2" = "Clinic Culture\n& Environment",
                     "imat_d3" = "Patient\nIdentification\n& Initiating Care",
                     "imat_d4" = "Care Delivery &\nTreatment\nResponse\nMonitoring",
                     "imat_d5" = "Care\nCoordination",
                     "imat_d6" = "Workforce",
                     "imat_d7" = "Staff Training &\nDevelopment",
                     "imat_total" = "Total")
  imat_scale_labels = c("1: Not\nIntegrated", "2", "3: Partially\nIntegrated",
                        "4", "5: Fully\nIntegrated")

  plot_data = report_data %>%
    filter(program_id==id, startsWith(variable, "imat")) %>%
    # Dates need to be factors for legend
    mutate(date = factor(format(date, "%b-%y"), levels=imat_dates, ordered=T))
  
  plot = ggplot(plot_data, aes(x=variable, y=value, group=date)) +
    geom_point(aes(color=date), size=4) +
    geom_line(aes(color=date), linewidth=2, show.legend=F) +
    scale_color_manual(values=SITTMAT_colors) +
    scale_x_discrete(labels=~imat_labels[.x]) +
    scale_y_continuous(limits=c(1,5), breaks=1:5, labels=imat_scale_labels) +
    theme_minimal() +
    theme(axis.title = element_blank(),
          axis.text.x = element_text(size=8), 
          axis.text = element_text(family=font, color="black"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          plot.margin = margin(-10,2,15,2),
          legend.box.margin = margin(5,-10,-10,-10),
          legend.position = "top",
          legend.text = element_text(size=9, family=font, color="black"),
          legend.title = element_blank()) +
    coord_cartesian(clip="off")
  
  # Annotate plot to specify which variables are dimensions
  dim_line = linesGrob(x=c(.37,10.5), y=c(-2,-2), gp=gpar(col="black", lwd=1))
  dim_text = textGrob(label="DIMENSIONS", hjust=0.5, gp=gpar(fontfamily=font, fontsize=9))
  total_line = linesGrob(x=c(14.42,15.62), y=c(-2,-2), gp=gpar(col="black", lwd=1))
  total_text = textGrob(label="TOTAL", hjust=0.5, gp=gpar(fontfamily=font, fontsize=9))
  
  plot = plot +
    annotation_custom(dim_line, ymin=-1.3, ymax=-1.3, xmin=.2, xmax=.9) +
    annotation_custom(dim_text, ymin=-2.1, ymax=-1.1, xmin=1.5, xmax=6.5) +
    annotation_custom(total_line, ymin=-1.3, ymax=-1.3, xmin=.5, xmax=.999) +
    annotation_custom(total_text, ymin=-2.1, ymax=-1.1, xmin=7.51, xmax=8.51)
  
  if(save){
    filepath = paste0("figures/QRIR_figures/", id, "/", id, "_imat_", month_for_filenames, ".png")
    ggsave(here::here(filepath), plot, width=imat_dims[1], height=imat_dims[2], units="in")
  }
  
  return(plot)
}

########
# Main #
########

# Function to run through all plots
#   If show_plots=T, it will ask for user input before showing next plot
make_allPlots = function(id, save_plots=F, show_plots=T){
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

#programs = sort(unique(report_data$program_id))
all_programs = c("id01", "id02", "id15", "id17",  "id34", "id35", "id38", 
                 "id39", "id40", "id63")

for(program in all_programs){
  make_allPlots(program, save_plots=T, show_plots=F)
}

# Programs that do not have new REAIM scores
imat_programs = c("id26", "id51", "id53")

for(program in imat_programs){
  make_imat(program, save=T)
}

# functions used in dust_observation tool

library(ggplot2)
library(tidyverse)

# do list and vector intersect?
list_vec_int <- function(a_list, b_vec){
    if (is.null(b_vec)){
        return(TRUE)
    } else{
        return(length(intersect(unlist(a_list), b_vec)) > 0)
    }
}

# make plume source id legend
id_legend_plot <- ggplot(data.frame(x=.1, y=1, label="#"), aes(x=x, y=y, label=label)) +
    geom_label(size=3) +
    geom_text(mapping=aes(x=0.13, y=1, label="Plume Source ID"), hjust=0, 
              size=3) +
    xlim(0, .5) +
    theme(panel.background=element_blank(), 
          axis.ticks=element_blank(), 
          axis.text=element_blank(), 
          axis.title=element_blank())
id_legend_grob <- ggplotGrob(id_legend_plot)
id_legend_grob$vp <- grid::viewport(width=unit(.5, "npc"), height=unit(.1, "npc"))

source_data_query <- function(date_begin, date_end){
    paste0("SELECT p1.*, p4.behavior ", 
               "FROM (", 
               "SELECT po.plume_observation_id AS id, po.datetime, ",
               "po.lat_long AS observer_position, ", 
               "po.wind_speed, ps.acres, wd.wind_direction_cardinal, ",
               "pd.opacity, pc.confidence, psh.crosses_shoreline, ",
               "po.met_conditions, po.observation_duration, po.comments, ",
               "po.temp_ambient, po.wind_direction_id, po.plume_density_id, ", 
               "po.plume_confidence_id, po.plume_shoreline_id, u.full_name, ", 
               "ST_X(ST_CENTROID(ST_TRANSFORM(ps.area::geometry, 26911))) ", 
               "AS centroid_x, ",
               "ST_Y(ST_CENTROID(ST_TRANSFORM(ps.area::geometry, 26911))) ", 
               "AS centroid_y ",
               "FROM field_data.plume_observations po ", 
               "LEFT JOIN auth.users u ",
               "ON po.added_by=u.user_id ", 
               "LEFT JOIN field_data.plume_source ps ", 
               "ON ps.plume_observation_id=po.plume_observation_id ",
               "LEFT JOIN field_data.wind_directions wd ",
               "ON po.wind_direction_id=wd.wind_direction_id ",
               "LEFT JOIN field_data.plume_density pd ",
               "ON po.plume_density_id=pd.plume_density_id ",
               "LEFT JOIN field_data.plume_shoreline psh ",
               "ON po.plume_shoreline_id=psh.plume_shoreline_id ",
               "LEFT JOIN field_data.plume_confidence pc ",
               "ON po.plume_confidence_id=pc.plume_confidence_id ",
               "WHERE po.datetime::date BETWEEN '", date_begin, "' AND '", 
               date_end, "') p1 ", 
               "LEFT JOIN (",
               "SELECT p3.plume_observation_id AS id, ",
               "STRING_AGG(p2.behavior_characteristic, ', ') AS behavior ", 
               "FROM field_data.plume_behavior p2 ", 
               "JOIN field_data.plume_observations p3 ", 
               "ON p2.plume_behavior_id=ANY(p3.plume_behavior_id) ", 
               "GROUP BY p3.plume_observation_id) p4 ", 
               "ON p1.id=p4.id;")
}

source_poly_query <- function(date_begin, date_end){
    paste0("SELECT ps.plume_observation_id AS id, ",
           "ST_X(ST_TRANSFORM((ST_DUMPPOINTS(ps.area::geometry)).geom, 26911)) ",
           "AS x, ",
           "ST_Y(ST_TRANSFORM((ST_DUMPPOINTS(ps.area::geometry)).geom, 26911)) ",
           "AS y ", 
           "FROM field_data.plume_source ps ", 
           "JOIN field_data.plume_observations po ", 
           "ON ps.plume_observation_id=po.plume_observation_id ",
           "WHERE po.datetime::date BETWEEN '", date_begin, "' AND '", 
           date_end, "';") 
}

traj_query <- function(date_begin, date_end){
    paste0("SELECT pt.plume_observation_id AS id, pt.plume_trajectory_id, ",
           "ST_X(ST_TRANSFORM((ST_DUMPPOINTS(pt.trajectory::geometry)).geom, ",
           "26911)) AS x, ", 
           "ST_Y(ST_TRANSFORM((ST_DUMPPOINTS(pt.trajectory::geometry)).geom, ",
           "26911)) AS y ", 
           "FROM field_data.plume_trajectory pt ", 
           "JOIN field_data.plume_observations po ", 
           "ON pt.plume_observation_id=po.plume_observation_id ",
           "WHERE po.datetime::date BETWEEN '", date_begin, "' AND '", 
           date_end, "';") 
}

img <- png::readPNG("./data/logo.png")
logo_grob <- grid::rasterGrob(img, interpolate=TRUE)

wind_index <- c("W"=0*pi/16, "WSW"=2*pi/16, "SW"=4*pi/16, "SSW"=6*pi/16, 
                "S"=8*pi/16, "SSE"=10*pi/16, "SE"=12*pi/16, "ESE"=14*pi/16, 
                "E"=16*pi/16, "ENE"=18*pi/16, "NE"=20*pi/16, "NNE"=22*pi/16, 
                "N"=24*pi/16, "NNW"=26*pi/16, "NW"=28*pi/16, "WNW"=30*pi/16)


# plot map background
plot_dust_background <- function(polys=all_polys){
    p1 <- polys %>% 
    ggplot(aes(x=x, y=y)) +
    geom_path(data=dcas, mapping=aes(group=objectid), color='grey') +
    geom_path(data=offlake, mapping=aes(group=objectid), color='white') +
    geom_path(data=shoreline, mapping=aes(group=area_name), color='blue') +
    coord_equal() +
    geom_rect(xmin=406000, xmax=407000, ymin=4013000, ymax=4013500, fill="black", 
              color="black") + 
    geom_rect(xmin=407000, xmax=408000, ymin=4013000, ymax=4013500, fill="white", 
              color="black") + 
    geom_rect(xmin=408000, xmax=409000, ymin=4013000, ymax=4013500, fill="black", 
              color="black") + 
    geom_rect(xmin=409000, xmax=410000, ymin=4013000, ymax=4013500, fill="white", 
              color="black") + 
    geom_rect(xmin=410000, xmax=411000, ymin=4013000, ymax=4013500, fill="black", 
              color="black") + 
    geom_text(data=data.frame(x=c(406000, 411000), y=c(4012500, 4012500), 
                              label=c("0km", "5km")), 
              mapping=aes(x=x, y=y, label=label)) +
    geom_segment(data=data.frame(x=408500, y=4015100), 
                 mapping=aes(x=x, xend=x, y=y, yend=y+1), 
                 color="black", size=1, arrow=arrow(type="closed")) +
    geom_text(data=data.frame(x=408500, y=4015500, label="N"), 
              mapping=aes(x=x, y=y, label=label)) +
    annotation_custom(logo_grob, xmin=422000, xmax=427000, ymin=4012500, 
                      ymax=4015500) +
    theme(panel.background=element_blank(), 
          axis.ticks=element_blank(), 
          axis.text=element_blank(), 
          axis.title=element_blank())
    p1
}


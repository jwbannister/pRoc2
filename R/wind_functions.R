
met_data_query <- function(date_begin, date_end){
    paste0("SELECT i.deployment, m.datetime, m.ws_10m, m.ws_10m_max, ",
           "m.wd_10m, m.wd_sd_10m ", 
           "FROM met.met_1hour m ", 
           "JOIN instruments.deployments i ",
           "ON m.deployment_id=i.deployment_id ",
           "WHERE (datetime - '1 second'::interval)::date BETWEEN '", 
           date_begin, "' AND '", date_end, "';") 
}
met_hour_query <- function(sites, hr){
    paste0("SELECT i.deployment, m.datetime, ",
           "m.ws_10m*2.24 AS ws_mph, max.ws_mph_max ", 
           "FROM met.met_1hour m ", 
           "JOIN instruments.deployments i ",
           "ON m.deployment_id=i.deployment_id ",
           "JOIN (SELECT deployment_id, MAX(ws_10m)*2.24 AS ws_mph_max ",
           "  FROM met.met_5min m1 ",
           "  WHERE (datetime - '1 second'::interval) ", 
           "  BETWEEN ('", hr, "'::timestamp - '1 hour'::interval) ",
           "  AND '", hr, "'::timestamp ",
           "  GROUP BY m1.deployment_id) max ", 
           "ON m.deployment_id=max.deployment_id  ", 
           "WHERE m.datetime='", hr, "'::timestamp ",
           "AND i.deployment IN ('", paste(sites, collapse="', '"), "');")
}
met_location_query <- function(sites){
    paste0("SELECT deployment, ",
           "ST_X(ST_TRANSFORM(geom, 26911)) AS x, ",
           "ST_Y(ST_TRANSFORM(geom, 26911)) AS y, ",
           "ST_X(ST_TRANSFORM(geom, 4324)) AS long, ",
           "ST_Y(ST_TRANSFORM(geom, 4324)) AS lat ",
           "FROM instruments.deployments ",
           "WHERE deployment IN ('", paste(sites, collapse="', '"), "');")
}

format_NWS_datetime <- function(x){
    a <- gsub("T", " ", substr(x, 1, 19))
    b <- as.POSIXct(a, tz='UTC')
    attr(b, "tzone") <-"America/Los_Angeles"
    b
}


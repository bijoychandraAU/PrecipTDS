#' @title Temporal downscaling of precipitation.
#'
#' @description Preci_diasg () generates a higher resolution temporal scale (15-min) from 1-h
#'  precipitation using a modified stochastic disaggregation method.
#' @param obs A \code{dataframe/tibble} of dimension 2 used to denote the observed precipitation
#' that recorded in 15-min.
#' This can be either continuous or discontinuous time series data frame.
#' @param mod A \code{dataframe/tibble} of dimension 2 used to denote the observed precipitation
#' that recorded in 1-h.
#' This can be either continuous or discontinuous time series data frame.
#'
#'
#' @return Preci_diasg returns a continuous time series of precipitation.A \code{tibble} containing the following attributes:
#' \describe{
#'      \item{Datetime}{The generated 15-min continous date and time.}
#'      \item{Preci}{Temporally downscaled precipitation  at 15-min interval.}
#' }
#' @author Bijoychandra Takhellambam, Puneet Srivastava,
#' Jasmeet Lamba, Ryan P. McGehee, Hemendra Kumar, & Di Tian.
#' @importFrom magrittr  %>%
#' @importFrom dplyr mutate group_by n ungroup row_number arrange filter summarise_all bind_rows slice left_join
#' @importFrom Hmisc Lag
#' @importFrom lubridate month as_datetime minutes
#' @importFrom tibble add_row as_tibble
#' @importFrom tidyr replace_na
#' @export
#' @examples
#' Preci_diasg(obs=observed,mod=model)


Preci_diasg <- function(obs,mod){
  df=obs
  colnames(df)=c("datetime","Precip_mm")
  is.na(df) <- sapply(df, is.infinite)
  df =  na.omit(df)
  df=df %>%
    mutate(time2=Lag(datetime, shift = 1))
  df=df %>%
    mutate(dur=as.numeric(difftime(datetime, time2, units = "hours"))) %>%
    mutate(event=ifelse(dur>=1,1,0)) %>%
    mutate(event1=event+c((event[-1] == 1) * 1, 0)) %>%
    mutate(event=event1-event) %>%
    subset(select = -event1) %>%
    na.omit()
  df=df %>%
    group_by(Storm = as.integer(factor(lag(cumsum(event), default = 0)))) %>%
    mutate(strmlen = n())
  MAX_GROUP = 4
  df=df %>%
    group_by(Storm) %>%
    mutate(event_no =
             paste(Storm, floor((row_number()-1)/ (MAX_GROUP)), sep = "-")) %>%
    ungroup()%>%
    group_by(event_no) %>%
    mutate(Depth = sum(Precip_mm)) %>%
    subset(select=-c(event,Storm,strmlen))%>%
    mutate(month = month(datetime))
  df=df %>%
    arrange(Depth)

  df_hour1=mod
  colnames(df_hour1)=c("datetime","Rain_hour_mm")

  is.na(df_hour1) <- sapply(df_hour1, is.infinite)
  df_hour1 =  na.omit(df_hour1)

  df_hour=df_hour1 %>%
    mutate(month = month(datetime),
           Rain_hour_mm=round(Rain_hour_mm,2)) %>%
    filter(Rain_hour_mm != 0)

  rain15 <- function(z){
    x=as.numeric(z[,"Rain_hour_mm"])
    mon=as.numeric(z[,"month"])
    y=z$datetime
    df_mont=df %>%
      filter(month==mon)
    cdf1=df_mont[,c("Depth")]
    cdf2 <- ecdf(cdf1$Depth)
    epsilon=min(cdf1$Depth)
    Ddt=x
    if (Ddt>epsilon) {
      mat=list()
      y=y
      Dt=Ddt
      i<-0
      while (TRUE){
        i<-i+1
        a=cdf2(Dt)
        u1 <- runif(1, 0, a)
        d1=quantile(cdf2, u1)
        event_rain=df_mont %>%
          subset(Depth == max(Depth[Depth<= d1]))
        event_rand=event_rain[sample(nrow(event_rain), 1),]
        D1=event_rain%>%
          filter(event_no==event_rand$event_no) %>%
          subset(select=c("datetime","Precip_mm"))
        y=as.data.frame(y[1])
        colnames(y)=c("datetime")
        df_15=y%>%
          add_row(datetime = as_datetime(y$datetime)+minutes(15)) %>%
          add_row(datetime = as_datetime(y$datetime)+minutes(30)) %>%
          add_row(datetime = as_datetime(y$datetime)+minutes(45))%>%
          mutate(Precip_mm=0)
        random_row1 <- sample.int(n=nrow(df_15),
                                  size=nrow(D1),
                                  replace = FALSE)
        df_15$Precip_mm[random_row1]<-df_15$Precip_mm[random_row1] + D1$Precip_mm
        mat[[i]]<-df_15
        Dt<-Dt-sum(df_15[,"Precip_mm"])
        if (Dt <= epsilon){break}
      }
      rain_15f= mat %>%
        bind_rows() %>%
        group_by(datetime ) %>%
        summarise_all(sum)
      d_remain=Ddt-sum(rain_15f[,2])
      rand_row <- sample.int(n=nrow(rain_15f),
                             size=length(d_remain),
                             replace = FALSE)
      rain_15f$Precip_mm[rand_row]<-rain_15f$Precip_mm[rand_row] + d_remain

    } else {
      #random insertion of remaining rain < minimum rain
      y=as.data.frame(y[1])
      colnames(y)=c("datetime")
      rain_15f=y%>%
        add_row(datetime = as_datetime(y$datetime)+minutes(15)) %>%
        add_row(datetime = as_datetime(y$datetime)+minutes(30)) %>%
        add_row(datetime = as_datetime(y$datetime)+minutes(45))%>%
        mutate(Precip_mm=0)

      rand_row <- sample.int(n=nrow(rain_15f),
                             size=1,
                             replace = FALSE)
      rain_15f$Precip_mm[rand_row]<-rain_15f$Precip_mm[rand_row] + Ddt
    }
    return(rain_15f)
  }
  batchSets <- split(df_hour, seq(nrow(df_hour)))
  rain_15times=lapply(batchSets, rain15)
  rain_times=do.call("rbind", rain_15times)
  rain_times=rain_times %>%
    arrange(datetime)
  start=df_hour1$datetime[1]
  end=df_hour1$datetime[nrow(df_hour1)]
  simu_15min=as.data.frame(seq(start,end+60*60*1, by="15 min"))
  colnames(simu_15min)="datetime"
  simu_15min=simu_15min %>% slice(-n())

  simu_15min=left_join(simu_15min, rain_times, by = 'datetime') %>%
    mutate(Precip_mm = replace_na(Precip_mm, 0)) %>%
    mutate(Precip_mm = round(Precip_mm, 2))
  colnames(simu_15min)=c("Datetime","Preci")
  simu_15min=as_tibble(simu_15min)
  return(simu_15min)
}




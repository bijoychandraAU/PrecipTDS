#' @title Temporal downscaling of precipitation from 1-hour to 30-min.
#'
#' @description PrecipTDS30 () generates a higher resolution temporal scale (30-min) from 1-h
#'  precipitation using a modified stochastic disaggregation method.
#' @param obs A \code{dataframe/tibble} of dimension 2 used to denote the observed precipitation
#' that recorded in 30-min.
#' This can be either continuous or discontinuous time series data frame.
#' @param mod A \code{dataframe/tibble} of dimension 2 used to denote the observed precipitation
#' that recorded in 1-h.
#' This can be either continuous or discontinuous time series data frame.
#'
#'
#' @return PrecipTDS30 returns a continuous time series of precipitation.A \code{tibble} containing the following attributes:
#' \describe{
#'      \item{Datetime}{The generated 30-min continous date and time.}
#'      \item{Preci}{Temporally downscaled precipitation  at 30-min interval.}
#' }
#' @author Bijoychandra Takhellambam, Puneet Srivastava,
#' Jasmeet Lamba, Hemendra Kumar, & Roberto Molinari.
#' @importFrom magrittr  %>%
#' @importFrom dplyr mutate group_by n ungroup row_number arrange filter summarise_all bind_rows slice left_join
#' @importFrom Hmisc Lag
#' @importFrom lubridate month as_datetime minutes
#' @importFrom tibble add_row as_tibble
#' @importFrom tidyr replace_na
#' @importFrom stats ecdf lag na.omit quantile runif
#' @export
#' @examples
#' PrecipTDS30(obs=obs30,mod=model)
PrecipTDS30 <- function(obs,mod){
  dat=obs
  colnames(dat)=c("datetime","Precip_mm")
  is.na(dat) = sapply(dat, is.infinite)
  dat =  na.omit(dat)
  dat=dat %>%
    mutate(time2=Lag(datetime, shift = 1))

  dat=dat %>%
    mutate(dur=as.numeric(difftime(datetime, time2, units = "hours")),
           event=ifelse(dur>=1,1,0)) %>%
    mutate(event1=event+c((event[-1] == 1) * 1, 0),
           event=event1-event) %>%
    subset(select = -event1) %>%
    na.omit()

  dat=dat %>%
    group_by(Storm = as.integer(factor(lag(cumsum(event), default = 0)))) %>%
    mutate(strmlen = n())

  MAX_GROUP = 2

  dat=dat %>%
    group_by(Storm) %>%
    mutate(event_no =
             paste(Storm, floor((row_number()-1)/ (MAX_GROUP)), sep = "-")) %>%
    ungroup()%>%
    group_by(event_no) %>%
    mutate(Depth = sum(Precip_mm)) %>%
    subset(select=-c(event,Storm,strmlen))%>%
    mutate(month = month(datetime))

  dat=dat %>%
    arrange(Depth)

  dat_hour1=mod
  colnames(dat_hour1)=c("datetime","Rain_hour_mm")

  is.na(dat_hour1) <- sapply(dat_hour1, is.infinite)
  dat_hour1 =  na.omit(dat_hour1)

  dat_hour=dat_hour1 %>%
    mutate(month = month(datetime),
           Rain_hour_mm=round(Rain_hour_mm,2)) %>%
    filter(Rain_hour_mm != 0)

  rain15 <- function(z){
    x=as.numeric(z[,"Rain_hour_mm"])
    mon=as.numeric(z[,"month"])
    y=z$datetime
    dat_mont=dat %>%
      filter(month==mon)
    cdat1=dat_mont[,c("Depth")]
    cdat2 <- ecdf(cdat1$Depth)
    epsilon=min(cdat1$Depth)
    Ddt=x
    if (Ddt>epsilon) {
      mat=list()
      y=y
      Dt=Ddt
      i<-0
      while (TRUE){
        i<-i+1
        a=cdat2(Dt)
        u1 <- runif(1, 0, a)
        d1=quantile(cdat2, u1)
        event_rain=dat_mont %>%
          subset(Depth == max(Depth[Depth<= d1]))
        event_rand=event_rain[sample(nrow(event_rain), 1),]
        D1=event_rain%>%
          filter(event_no==event_rand$event_no) %>%
          subset(select=c("datetime","Precip_mm"))
        y=as.data.frame(y[1])
        colnames(y)=c("datetime")

        dat_30=y%>%
          add_row(datetime = as_datetime(y$datetime)+minutes(30)) %>%
          mutate(Precip_mm=0)
        random_row1 <- sample.int(n=nrow(dat_30),
                                  size=nrow(D1),
                                  replace = FALSE)
        dat_30$Precip_mm[random_row1]<-dat_30$Precip_mm[random_row1] + D1$Precip_mm
        mat[[i]]<-dat_30
        Dt<-Dt-sum(dat_30[,"Precip_mm"])
        if (Dt <= epsilon){break}
      }
      rain_30f= mat %>%
        bind_rows() %>%
        group_by(datetime ) %>%
        summarise_all(sum)
      d_remain=Ddt-sum(rain_30f[,2])
      rand_row <- sample.int(n=nrow(rain_30f),
                             size=length(d_remain),
                             replace = FALSE)
      rain_30f$Precip_mm[rand_row]<-rain_30f$Precip_mm[rand_row] + d_remain

    } else {
      #random insertion of remaining rain < minimum rain
      y=as.data.frame(y[1])
      colnames(y)=c("datetime")
      rain_30f=y%>%
        add_row(datetime = as_datetime(y$datetime)+minutes(30)) %>%
        mutate(Precip_mm=0)

      rand_row <- sample.int(n=nrow(rain_30f),
                             size=1,
                             replace = FALSE)
      rain_30f$Precip_mm[rand_row]<-rain_30f$Precip_mm[rand_row] + Ddt
    }
    return(rain_30f)
  }

  #dat_hour=dat_hour[1:10,]
  batchSets <- split(dat_hour, seq(nrow(dat_hour)))
  #z=batchSets[[2]]
  rain_15times=lapply(batchSets, rain15)

  rain_times=do.call("rbind", rain_15times)
  rain_times=rain_times %>%
    arrange(datetime)
  start=dat_hour1$datetime[1]
  end=dat_hour1$datetime[nrow(dat_hour1)]

  simu_30min=as.data.frame(seq(start,end+60*60*1, by="30 min"))
  colnames(simu_30min)="datetime"
  simu_30min=simu_30min %>% slice(-n())

  simu_30min=left_join(simu_30min, rain_times, by = 'datetime') %>%
    mutate(Precip_mm = replace_na(Precip_mm, 0)) %>%
    mutate(Precip_mm = round(Precip_mm, 2))
  colnames(simu_30min)=c("Datetime","Preci")
  simu_30min=as_tibble(simu_30min)
  return(simu_30min)
}


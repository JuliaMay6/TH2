## Подготовка

    library(dplyr)

    ## 
    ## Присоединяю пакет: 'dplyr'

    ## Следующие объекты скрыты от 'package:stats':
    ## 
    ##     filter, lag

    ## Следующие объекты скрыты от 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    library(arrow)

    ## 
    ## Присоединяю пакет: 'arrow'

    ## Следующий объект скрыт от 'package:utils':
    ## 
    ##     timestamp

    library(stringr)
    library(lubridate)

    ## 
    ## Присоединяю пакет: 'lubridate'

    ## Следующий объект скрыт от 'package:arrow':
    ## 
    ##     duration

    ## Следующие объекты скрыты от 'package:base':
    ## 
    ##     date, intersect, setdiff, union

    library(ggplot2)

    dataset <- arrow::read_csv_arrow("traffic_security.csv",schema = schema(timestamp=int64(),src=utf8(),dst=utf8(),port=uint32(),bytes=uint32()))

    dataset1 <- arrow::read_csv_arrow("traffic_security.csv",schema = schema(timestamp=int64(),src=utf8(),dst=utf8(),port=uint32(),bytes=uint32()))

## Задание 2: Надите утечку данных 2

### Другой атакующий установил автоматическую задачу в системном планировщике cron для экспорта содержимого внутренней wiki системы. Эта система генерирует большое количество траффика в нерабочие часы, больше чем остальные хосты. Определите IP этой системы. Известно, что ее IP адрес отличается от нарушителя из предыдущей задачи.

### Нерабочим временем будем считать интервал с 0:00 по 15:00, так как там наименьшая активность по сравнению с интервалом с 16:00 по 24:00, что видно ниже:

    dataset$timestamp_seconds <- dataset$timestamp / 1000
    dataset$timestamp <- as.POSIXct(dataset$timestamp_seconds, origin = "1970-01-01", tz = "Europe/Moscow")
    dataset$hour <-  format(dataset$timestamp, format = "%H")
    dataset$minutes <- format(dataset$timestamp, format = "%M")

    ahours <- dataset %>% group_by(hour) %>% summarise(N = n())
    select(arrange(ahours,desc(N)),N,hour)

    ## # A tibble: 24 × 2
    ##           N hour 
    ##       <int> <chr>
    ##  1 12237573 01   
    ##  2 12226575 02   
    ##  3 12226457 21   
    ##  4 12224721 00   
    ##  5 12220671 23   
    ##  6 12219218 22   
    ##  7 12217746 19   
    ##  8 12213523 20   
    ##  9   537639 11   
    ## 10   495908 12   
    ## # ℹ 14 more rows

### Определим нужный нам IP-адрес (ответ: 12.55.77.96):

    dataset1 %>%
      select(timestamp, src, dst, bytes) %>%
       filter(src != "13.37.84.125") %>%
      mutate(outside_traffic = (str_detect(src,"^((12|13|14)\\.)") & !str_detect(dst,"^((12|13|14)\\.)")), hour = hour(as_datetime(timestamp/1000))) %>%
      filter(outside_traffic == TRUE, hour >= 0 & hour <= 15) %>%
      group_by(src) %>%
      summarise(total_bytes = sum(bytes),) %>%
      arrange(desc(total_bytes)) %>%
      head(1) %>%
      collect()

    ## # A tibble: 1 × 2
    ##   src         total_bytes
    ##   <chr>             <int>
    ## 1 12.55.77.96   289566918

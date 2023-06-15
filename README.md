    library(arrow)

    ## 
    ## Присоединяю пакет: 'arrow'

    ## Следующий объект скрыт от 'package:utils':
    ## 
    ##     timestamp

    library(dplyr)

    ## 
    ## Присоединяю пакет: 'dplyr'

    ## Следующие объекты скрыты от 'package:stats':
    ## 
    ##     filter, lag

    ## Следующие объекты скрыты от 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    library(duckdb)

    ## Загрузка требуемого пакета: DBI

    library(stringr)
    ds <- 
      arrow::read_csv_arrow("traffic_security.csv",schema = schema(timestamp=int64(),src=utf8(),dst=utf8(),port=uint32(),bytes=uint32()))

# Задание 3: Надите утечку данных 3 Еще один нарушитель собирает содержимое электронной почты и отправляет в Интернет используя порт, который обычно используется для другого типа трафика. Атакующий пересылает большое количество информации используя этот порт, которое нехарактерно для других хостов, использующих этот номер порта. Определите IP этой системы. Известно, что ее IP адрес отличается от нарушителей из предыдущих задач.

    ds %>%
      select(src, dst, bytes,port) %>%
      mutate(outside_traffic = (str_detect(src,"^((12|13|14)\\.)") & !str_detect(dst,"^((12|13|14)\\.)"))) %>%
      filter(outside_traffic == TRUE) %>%
      group_by(port) %>%
      summarise(total_data=sum(bytes)) %>%
      filter(total_data < 5*10^9) %>%
      select(port) %>%
      collect() -> ports

    ports <- unlist(ports)
    ports <- as.vector(ports,'numeric')

### Выбираем данные с нужными номерами портов

    ds %>%
      select(src, dst, bytes,port) %>%
      mutate(outside_traffic = (str_detect(src,"^((12|13|14)\\.)") & !str_detect(dst,"^((12|13|14)\\.)"))) %>%
      filter(outside_traffic == TRUE) %>%
      filter(port %in% ports) %>%
      group_by(src,port) %>%
      summarise(total_bytes=sum(bytes)) %>%
      arrange(desc(port)) %>%
      collect() -> df

### Порты с маскимальным кол-вом данных

    df %>%
      group_by(src, port) %>%
      summarise(total_data=sum(total_bytes)) %>%
      arrange(desc(total_data)) %>%
      head(10) %>%
      collect()

    ## # A tibble: 10 × 3
    ## # Groups:   src [3]
    ##    src           port total_data
    ##    <chr>        <int>      <int>
    ##  1 13.37.84.125    36 2070876332
    ##  2 13.37.84.125    95 2031985904
    ##  3 13.37.84.125    21 2027501066
    ##  4 13.37.84.125    78 2018366254
    ##  5 13.37.84.125    32 1989408807
    ##  6 12.55.77.96     31  233345180
    ##  7 13.48.72.30     26    2468348
    ##  8 13.48.72.30     61    2465805
    ##  9 13.48.72.30     77    2453566
    ## 10 13.48.72.30     79    2421971

### Количество хостов к портам

    df %>%
      group_by(port) %>%
      summarise(hosts=n()) %>%
      arrange(hosts) %>%
      head(10) %>%
      collect()

    ## # A tibble: 10 × 2
    ##     port hosts
    ##    <int> <int>
    ##  1    21     1
    ##  2    31     1
    ##  3    32     1
    ##  4    36     1
    ##  5    78     1
    ##  6    95     1
    ##  7    51    24
    ##  8    22  1000
    ##  9    23  1000
    ## 10    25  1000

### Из предыдущих шагов следует вывод, что ip-адрес злоумышленника 12.55.77.96, а порт 31, т.к. из таблицы в 5 пункте видно, что 31 порт использовал только 1 хост и в тоже время из таблицы в 4 пункте видно, что больше всего данных было передано именно по этому порту

    df %>%
      filter(port == 31) %>%
      group_by(src) %>%
      summarise(total_data=sum(total_bytes)) %>%
      collect()

    ## # A tibble: 1 × 2
    ##   src         total_data
    ##   <chr>            <int>
    ## 1 12.55.77.96  233345180

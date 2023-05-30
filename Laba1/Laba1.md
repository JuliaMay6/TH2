## Подготовка

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

    library(stringr)

    dataset <- arrow::read_csv_arrow("traffic_security.csv",schema = schema(timestamp=int64(),src=utf8(),dst=utf8(),port=uint32(),bytes=uint32()))

## Задание 1: Найдите утечку данных из Вашей сети

### Важнейшие документы с результатами нашей исследовательской деятельности в области создания вакцин скачиваются в виде больших заархивированных дампов. Один из хостов в нашей сети используется для пересылки этой информации – он пересылает гораздо больше информации на внешние ресурсы в Интернете, чем остальные компьютеры нашей сети. Определите его IP-адрес.

### Определение IP-адреса, который пересылает больше информации на внешние ресурсы:

    filter(dataset,str_detect(src,"^((12|13|14)\\.)"),
             str_detect(dst,"^((12|13|14)\\.)",negate=TRUE)) %>% 
      select(src,bytes) %>%
      group_by(src)%>% 
      summarise(bytes=sum(bytes))%>%
      slice_max(bytes)%>%
      select(src)

    ## # A tibble: 1 × 1
    ##   src         
    ##   <chr>       
    ## 1 13.37.84.125

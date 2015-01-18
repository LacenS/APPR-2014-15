# Analiza podatkov s programom R, 2014/15

Repozitorij z gradivi pri predmetu APPR v študijskem letu 2014/15.

## Tematika

Analiza deleža uporabnikov interneta po svetu.

Primerjala bom spreminjanje deleža uporabnikov interneta po posameznih državah skozi leta, te podatke bom prikazala na zemljevidu sveta. Poskušala bom tudi najti povezavo med višjim deležem uporabnikov interneta, višjo pričakovano življenjsko dobo ter višjim GDP pc v posamezni državi.

Podatke sem pridobila iz naslednjih spletnih strani:
/- http://data.worldbank.org/indicator/IT.NET.USER.P2
/- http://www.internetlivestats.com/internet-users-by-country/
/- http://www.internetlivestats.com/internet-users/#trend
/- http://www.internetworldstats.com/stats.htm
/- http://www.nationmaster.com/country-info/stats/Health/Life-expectancy-at-birth%2C-total/Years#map
/- http://databank.worldbank.org/data/views/reports/tableview.aspx?isshared=true
/- http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries.zip



## Program

Glavni program se nahaja v datoteki `projekt.r`. Ko ga poženemo, se izvedejo
programi, ki ustrezajo drugi, tretji in četrti fazi projekta:

* obdelava, uvoz in čiščenje podatkov: `uvoz/uvoz.r`
* analiza in vizualizacija podatkov: `vizualizacija/vizualizacija.r`
* napredna analiza podatkov: `analiza/analiza.r`

Vnaprej pripravljene funkcije se nahajajo v datotekah v mapi `lib/`. Podatkovni
viri so v mapi `podatki/`. Slike, ki jih program naredi, se shranijo v mapo
`slike/`. Zemljevidi v obliki SHP, ki jih program pobere, se shranijo v mapo
`zemljevid/`.

## Poročilo

Poročilo se nahaja v mapi `porocilo/`. Za izdelavo poročila v obliki PDF je
potrebno datoteko `porocilo/porocilo.tex` prevesti z LaTeXom. Pred tem je
potrebno pognati program, saj so v poročilu vključene slike iz mape `slike/`.

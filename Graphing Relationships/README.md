Assignment \#5 - Graphing Realtionships
================
Matteo Larrode

<script src="README_files/libs/kePrint-0.0.1/kePrint.js"></script>
<link href="README_files/libs/lightable-0.0.1/lightable.css" rel="stylesheet" />


## Question 1: 2016 and 2020 Democratic vote shares

Let’s first load the packages and data we will need for this question

<details>
<summary>Code</summary>

``` r
library(tidyverse)
library(haven)
library(kableExtra)

vote_2020 <- read_csv("data/us_vote_2020.csv")
dem_share_80_16 <- read_dta("data/leipvote1980_2016wide.dta")
```

</details>

Now I join the 2020 election data to democratic votes shares of the two
party vote, from 1980 to 2016 to the 1980-2016.

<details>
<summary>Code</summary>

``` r
vote_80_16 <- left_join(dem_share_80_16, vote_2020, by = "state") %>%
  mutate(pctdem2020 = dem_percent)
```

</details>

Here is a table of summary statistics for the 2016 and 2020 Democratic
vote shares.

<details>
<summary>Code</summary>

``` r
table1 <- kbl(vote_80_16, format = "html")%>%
  kable_styling()

table1
```

</details>
<table class="table" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> state </th>
   <th style="text-align:right;"> pctdem1980 </th>
   <th style="text-align:right;"> pctdem1984 </th>
   <th style="text-align:right;"> pctdem1988 </th>
   <th style="text-align:right;"> pctdem1992 </th>
   <th style="text-align:right;"> pctdem1996 </th>
   <th style="text-align:right;"> pctdem2000 </th>
   <th style="text-align:right;"> pctdem2004 </th>
   <th style="text-align:right;"> pctdem2008 </th>
   <th style="text-align:right;"> pctdem2012 </th>
   <th style="text-align:right;"> pctdem2016 </th>
   <th style="text-align:left;"> called </th>
   <th style="text-align:left;"> final </th>
   <th style="text-align:right;"> dem_votes </th>
   <th style="text-align:right;"> rep_votes </th>
   <th style="text-align:right;"> other_votes </th>
   <th style="text-align:right;"> dem_percent </th>
   <th style="text-align:right;"> rep_percent </th>
   <th style="text-align:right;"> other_percent </th>
   <th style="text-align:right;"> dem_this_margin </th>
   <th style="text-align:right;"> margin_shift </th>
   <th style="text-align:right;"> vote_change </th>
   <th style="text-align:left;"> stateid </th>
   <th style="text-align:right;"> pctdem2020 </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Alabama </td>
   <td style="text-align:right;"> 0.4932366 </td>
   <td style="text-align:right;"> 0.3873661 </td>
   <td style="text-align:right;"> 0.4025443 </td>
   <td style="text-align:right;"> 0.4617887 </td>
   <td style="text-align:right;"> 0.4626613 </td>
   <td style="text-align:right;"> 0.4241447 </td>
   <td style="text-align:right;"> 0.3710223 </td>
   <td style="text-align:right;"> 0.3910910 </td>
   <td style="text-align:right;"> 0.387838 </td>
   <td style="text-align:right;"> 0.3562837 </td>
   <td style="text-align:left;"> R </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:right;"> 849624 </td>
   <td style="text-align:right;"> 1441170 </td>
   <td style="text-align:right;"> 32488 </td>
   <td style="text-align:right;"> 36.6 </td>
   <td style="text-align:right;"> 62.0 </td>
   <td style="text-align:right;"> 1.4 </td>
   <td style="text-align:right;"> -25.5 </td>
   <td style="text-align:right;"> 2.3 </td>
   <td style="text-align:right;"> 9.4 </td>
   <td style="text-align:left;"> AL </td>
   <td style="text-align:right;"> 36.6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Alaska </td>
   <td style="text-align:right;"> 0.3270082 </td>
   <td style="text-align:right;"> 0.3094409 </td>
   <td style="text-align:right;"> 0.3783668 </td>
   <td style="text-align:right;"> 0.4342574 </td>
   <td style="text-align:right;"> 0.3957150 </td>
   <td style="text-align:right;"> 0.3206305 </td>
   <td style="text-align:right;"> 0.3677372 </td>
   <td style="text-align:right;"> 0.3893521 </td>
   <td style="text-align:right;"> 0.426847 </td>
   <td style="text-align:right;"> 0.4161448 </td>
   <td style="text-align:left;"> R </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:right;"> 153778 </td>
   <td style="text-align:right;"> 189951 </td>
   <td style="text-align:right;"> 15801 </td>
   <td style="text-align:right;"> 42.8 </td>
   <td style="text-align:right;"> 52.8 </td>
   <td style="text-align:right;"> 4.4 </td>
   <td style="text-align:right;"> -10.1 </td>
   <td style="text-align:right;"> 4.7 </td>
   <td style="text-align:right;"> 12.8 </td>
   <td style="text-align:left;"> AK </td>
   <td style="text-align:right;"> 42.8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Arizona </td>
   <td style="text-align:right;"> 0.3178791 </td>
   <td style="text-align:right;"> 0.3288327 </td>
   <td style="text-align:right;"> 0.3925651 </td>
   <td style="text-align:right;"> 0.4869810 </td>
   <td style="text-align:right;"> 0.5122377 </td>
   <td style="text-align:right;"> 0.4671740 </td>
   <td style="text-align:right;"> 0.4472499 </td>
   <td style="text-align:right;"> 0.4568610 </td>
   <td style="text-align:right;"> 0.453866 </td>
   <td style="text-align:right;"> 0.4811137 </td>
   <td style="text-align:left;"> D </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:right;"> 1672143 </td>
   <td style="text-align:right;"> 1661686 </td>
   <td style="text-align:right;"> 53497 </td>
   <td style="text-align:right;"> 49.4 </td>
   <td style="text-align:right;"> 49.1 </td>
   <td style="text-align:right;"> 1.6 </td>
   <td style="text-align:right;"> 0.3 </td>
   <td style="text-align:right;"> 3.9 </td>
   <td style="text-align:right;"> 31.6 </td>
   <td style="text-align:left;"> AZ </td>
   <td style="text-align:right;"> 49.4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Arkansas </td>
   <td style="text-align:right;"> 0.4968029 </td>
   <td style="text-align:right;"> 0.3877241 </td>
   <td style="text-align:right;"> 0.4280836 </td>
   <td style="text-align:right;"> 0.5999227 </td>
   <td style="text-align:right;"> 0.5935283 </td>
   <td style="text-align:right;"> 0.4719931 </td>
   <td style="text-align:right;"> 0.4506425 </td>
   <td style="text-align:right;"> 0.3982828 </td>
   <td style="text-align:right;"> 0.378456 </td>
   <td style="text-align:right;"> 0.3571429 </td>
   <td style="text-align:left;"> R </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:right;"> 423932 </td>
   <td style="text-align:right;"> 760647 </td>
   <td style="text-align:right;"> 34490 </td>
   <td style="text-align:right;"> 34.8 </td>
   <td style="text-align:right;"> 62.4 </td>
   <td style="text-align:right;"> 2.8 </td>
   <td style="text-align:right;"> -27.6 </td>
   <td style="text-align:right;"> -0.7 </td>
   <td style="text-align:right;"> 7.8 </td>
   <td style="text-align:left;"> AR </td>
   <td style="text-align:right;"> 34.8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> California </td>
   <td style="text-align:right;"> 0.4052906 </td>
   <td style="text-align:right;"> 0.4177547 </td>
   <td style="text-align:right;"> 0.4819269 </td>
   <td style="text-align:right;"> 0.5851673 </td>
   <td style="text-align:right;"> 0.5721627 </td>
   <td style="text-align:right;"> 0.5620299 </td>
   <td style="text-align:right;"> 0.5504132 </td>
   <td style="text-align:right;"> 0.6227845 </td>
   <td style="text-align:right;"> 0.618728 </td>
   <td style="text-align:right;"> 0.6612886 </td>
   <td style="text-align:left;"> D </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:right;"> 11110250 </td>
   <td style="text-align:right;"> 6006429 </td>
   <td style="text-align:right;"> 384192 </td>
   <td style="text-align:right;"> 63.5 </td>
   <td style="text-align:right;"> 34.3 </td>
   <td style="text-align:right;"> 2.2 </td>
   <td style="text-align:right;"> 29.2 </td>
   <td style="text-align:right;"> -0.9 </td>
   <td style="text-align:right;"> 23.4 </td>
   <td style="text-align:left;"> CA </td>
   <td style="text-align:right;"> 63.5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Colorado </td>
   <td style="text-align:right;"> 0.3606741 </td>
   <td style="text-align:right;"> 0.3563415 </td>
   <td style="text-align:right;"> 0.4604618 </td>
   <td style="text-align:right;"> 0.5280207 </td>
   <td style="text-align:right;"> 0.4924079 </td>
   <td style="text-align:right;"> 0.4551416 </td>
   <td style="text-align:right;"> 0.4763357 </td>
   <td style="text-align:right;"> 0.5455081 </td>
   <td style="text-align:right;"> 0.527501 </td>
   <td style="text-align:right;"> 0.5268570 </td>
   <td style="text-align:left;"> D </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:right;"> 1804352 </td>
   <td style="text-align:right;"> 1364607 </td>
   <td style="text-align:right;"> 87993 </td>
   <td style="text-align:right;"> 55.4 </td>
   <td style="text-align:right;"> 41.9 </td>
   <td style="text-align:right;"> 2.7 </td>
   <td style="text-align:right;"> 13.5 </td>
   <td style="text-align:right;"> 8.6 </td>
   <td style="text-align:right;"> 17.1 </td>
   <td style="text-align:left;"> CO </td>
   <td style="text-align:right;"> 55.4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Connecticut </td>
   <td style="text-align:right;"> 0.4444280 </td>
   <td style="text-align:right;"> 0.3900083 </td>
   <td style="text-align:right;"> 0.4741885 </td>
   <td style="text-align:right;"> 0.5412512 </td>
   <td style="text-align:right;"> 0.6036351 </td>
   <td style="text-align:right;"> 0.5925566 </td>
   <td style="text-align:right;"> 0.5527495 </td>
   <td style="text-align:right;"> 0.6131836 </td>
   <td style="text-align:right;"> 0.587726 </td>
   <td style="text-align:right;"> 0.5714136 </td>
   <td style="text-align:left;"> D </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:right;"> 1080831 </td>
   <td style="text-align:right;"> 714717 </td>
   <td style="text-align:right;"> 28309 </td>
   <td style="text-align:right;"> 59.3 </td>
   <td style="text-align:right;"> 39.2 </td>
   <td style="text-align:right;"> 1.6 </td>
   <td style="text-align:right;"> 20.1 </td>
   <td style="text-align:right;"> 6.4 </td>
   <td style="text-align:right;"> 10.9 </td>
   <td style="text-align:left;"> CT </td>
   <td style="text-align:right;"> 59.3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Delaware </td>
   <td style="text-align:right;"> 0.4873321 </td>
   <td style="text-align:right;"> 0.4004633 </td>
   <td style="text-align:right;"> 0.4375881 </td>
   <td style="text-align:right;"> 0.5519799 </td>
   <td style="text-align:right;"> 0.5862366 </td>
   <td style="text-align:right;"> 0.5674006 </td>
   <td style="text-align:right;"> 0.5383151 </td>
   <td style="text-align:right;"> 0.6263814 </td>
   <td style="text-align:right;"> 0.594470 </td>
   <td style="text-align:right;"> 0.5600211 </td>
   <td style="text-align:left;"> D </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:right;"> 296268 </td>
   <td style="text-align:right;"> 200603 </td>
   <td style="text-align:right;"> 7475 </td>
   <td style="text-align:right;"> 58.7 </td>
   <td style="text-align:right;"> 39.8 </td>
   <td style="text-align:right;"> 1.5 </td>
   <td style="text-align:right;"> 19.0 </td>
   <td style="text-align:right;"> 7.6 </td>
   <td style="text-align:right;"> 14.2 </td>
   <td style="text-align:left;"> DE </td>
   <td style="text-align:right;"> 58.7 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> District of Columbia </td>
   <td style="text-align:right;"> 0.8481673 </td>
   <td style="text-align:right;"> 0.8614773 </td>
   <td style="text-align:right;"> 0.8524575 </td>
   <td style="text-align:right;"> 0.9029707 </td>
   <td style="text-align:right;"> 0.9012355 </td>
   <td style="text-align:right;"> 0.9048769 </td>
   <td style="text-align:right;"> 0.9052028 </td>
   <td style="text-align:right;"> 0.9340077 </td>
   <td style="text-align:right;"> 0.925876 </td>
   <td style="text-align:right;"> 0.9569247 </td>
   <td style="text-align:left;"> D </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:right;"> 317323 </td>
   <td style="text-align:right;"> 18586 </td>
   <td style="text-align:right;"> 8447 </td>
   <td style="text-align:right;"> 92.1 </td>
   <td style="text-align:right;"> 5.4 </td>
   <td style="text-align:right;"> 2.5 </td>
   <td style="text-align:right;"> 86.8 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 10.6 </td>
   <td style="text-align:left;"> DC </td>
   <td style="text-align:right;"> 92.1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Florida </td>
   <td style="text-align:right;"> 0.4094924 </td>
   <td style="text-align:right;"> 0.3466759 </td>
   <td style="text-align:right;"> 0.3874793 </td>
   <td style="text-align:right;"> 0.4881522 </td>
   <td style="text-align:right;"> 0.5315496 </td>
   <td style="text-align:right;"> 0.4999539 </td>
   <td style="text-align:right;"> 0.4747632 </td>
   <td style="text-align:right;"> 0.5141770 </td>
   <td style="text-align:right;"> 0.504423 </td>
   <td style="text-align:right;"> 0.4938542 </td>
   <td style="text-align:left;"> R </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:right;"> 5297045 </td>
   <td style="text-align:right;"> 5668731 </td>
   <td style="text-align:right;"> 101680 </td>
   <td style="text-align:right;"> 47.9 </td>
   <td style="text-align:right;"> 51.2 </td>
   <td style="text-align:right;"> 0.9 </td>
   <td style="text-align:right;"> -3.4 </td>
   <td style="text-align:right;"> -2.2 </td>
   <td style="text-align:right;"> 17.5 </td>
   <td style="text-align:left;"> FL </td>
   <td style="text-align:right;"> 47.9 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Georgia </td>
   <td style="text-align:right;"> 0.5765632 </td>
   <td style="text-align:right;"> 0.3980218 </td>
   <td style="text-align:right;"> 0.3979638 </td>
   <td style="text-align:right;"> 0.5034213 </td>
   <td style="text-align:right;"> 0.4936773 </td>
   <td style="text-align:right;"> 0.4401625 </td>
   <td style="text-align:right;"> 0.4164577 </td>
   <td style="text-align:right;"> 0.4737166 </td>
   <td style="text-align:right;"> 0.460434 </td>
   <td style="text-align:right;"> 0.4734315 </td>
   <td style="text-align:left;"> D </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:right;"> 2473633 </td>
   <td style="text-align:right;"> 2461854 </td>
   <td style="text-align:right;"> 62229 </td>
   <td style="text-align:right;"> 49.5 </td>
   <td style="text-align:right;"> 49.3 </td>
   <td style="text-align:right;"> 1.2 </td>
   <td style="text-align:right;"> 0.2 </td>
   <td style="text-align:right;"> 5.4 </td>
   <td style="text-align:right;"> 22.1 </td>
   <td style="text-align:left;"> GA </td>
   <td style="text-align:right;"> 49.5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Hawaii </td>
   <td style="text-align:right;"> 0.5108406 </td>
   <td style="text-align:right;"> 0.4429628 </td>
   <td style="text-align:right;"> 0.5480627 </td>
   <td style="text-align:right;"> 0.5671998 </td>
   <td style="text-align:right;"> 0.6427615 </td>
   <td style="text-align:right;"> 0.5982730 </td>
   <td style="text-align:right;"> 0.5440445 </td>
   <td style="text-align:right;"> 0.7299373 </td>
   <td style="text-align:right;"> 0.717038 </td>
   <td style="text-align:right;"> 0.6743984 </td>
   <td style="text-align:left;"> D </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:right;"> 366130 </td>
   <td style="text-align:right;"> 196864 </td>
   <td style="text-align:right;"> 11475 </td>
   <td style="text-align:right;"> 63.7 </td>
   <td style="text-align:right;"> 34.3 </td>
   <td style="text-align:right;"> 2.0 </td>
   <td style="text-align:right;"> 29.5 </td>
   <td style="text-align:right;"> -2.7 </td>
   <td style="text-align:right;"> 33.9 </td>
   <td style="text-align:left;"> HI </td>
   <td style="text-align:right;"> 63.7 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Idaho </td>
   <td style="text-align:right;"> 0.2748677 </td>
   <td style="text-align:right;"> 0.2672443 </td>
   <td style="text-align:right;"> 0.3671218 </td>
   <td style="text-align:right;"> 0.4033852 </td>
   <td style="text-align:right;"> 0.3920097 </td>
   <td style="text-align:right;"> 0.2915151 </td>
   <td style="text-align:right;"> 0.3067726 </td>
   <td style="text-align:right;"> 0.3697541 </td>
   <td style="text-align:right;"> 0.335786 </td>
   <td style="text-align:right;"> 0.3168454 </td>
   <td style="text-align:left;"> R </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:right;"> 287021 </td>
   <td style="text-align:right;"> 554119 </td>
   <td style="text-align:right;"> 26091 </td>
   <td style="text-align:right;"> 33.1 </td>
   <td style="text-align:right;"> 63.9 </td>
   <td style="text-align:right;"> 3.0 </td>
   <td style="text-align:right;"> -30.8 </td>
   <td style="text-align:right;"> 1.0 </td>
   <td style="text-align:right;"> 25.6 </td>
   <td style="text-align:left;"> ID </td>
   <td style="text-align:right;"> 33.1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Illinois </td>
   <td style="text-align:right;"> 0.4566034 </td>
   <td style="text-align:right;"> 0.4352675 </td>
   <td style="text-align:right;"> 0.4895072 </td>
   <td style="text-align:right;"> 0.5858822 </td>
   <td style="text-align:right;"> 0.5960509 </td>
   <td style="text-align:right;"> 0.5618001 </td>
   <td style="text-align:right;"> 0.5520864 </td>
   <td style="text-align:right;"> 0.6273426 </td>
   <td style="text-align:right;"> 0.585775 </td>
   <td style="text-align:right;"> 0.5902147 </td>
   <td style="text-align:left;"> D </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:right;"> 3471915 </td>
   <td style="text-align:right;"> 2446891 </td>
   <td style="text-align:right;"> 114938 </td>
   <td style="text-align:right;"> 57.5 </td>
   <td style="text-align:right;"> 40.6 </td>
   <td style="text-align:right;"> 1.9 </td>
   <td style="text-align:right;"> 17.0 </td>
   <td style="text-align:right;"> -0.1 </td>
   <td style="text-align:right;"> 9.0 </td>
   <td style="text-align:left;"> IL </td>
   <td style="text-align:right;"> 57.5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Indiana </td>
   <td style="text-align:right;"> 0.4020267 </td>
   <td style="text-align:right;"> 0.3792657 </td>
   <td style="text-align:right;"> 0.3987401 </td>
   <td style="text-align:right;"> 0.4616511 </td>
   <td style="text-align:right;"> 0.4685159 </td>
   <td style="text-align:right;"> 0.4199522 </td>
   <td style="text-align:right;"> 0.3957652 </td>
   <td style="text-align:right;"> 0.5052195 </td>
   <td style="text-align:right;"> 0.447996 </td>
   <td style="text-align:right;"> 0.3988076 </td>
   <td style="text-align:left;"> R </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:right;"> 1242413 </td>
   <td style="text-align:right;"> 1729516 </td>
   <td style="text-align:right;"> 61183 </td>
   <td style="text-align:right;"> 41.0 </td>
   <td style="text-align:right;"> 57.0 </td>
   <td style="text-align:right;"> 2.0 </td>
   <td style="text-align:right;"> -16.1 </td>
   <td style="text-align:right;"> 3.1 </td>
   <td style="text-align:right;"> 10.9 </td>
   <td style="text-align:left;"> IN </td>
   <td style="text-align:right;"> 41.0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Iowa </td>
   <td style="text-align:right;"> 0.4293685 </td>
   <td style="text-align:right;"> 0.4627618 </td>
   <td style="text-align:right;"> 0.5514848 </td>
   <td style="text-align:right;"> 0.5373253 </td>
   <td style="text-align:right;"> 0.5573339 </td>
   <td style="text-align:right;"> 0.5016278 </td>
   <td style="text-align:right;"> 0.4966332 </td>
   <td style="text-align:right;"> 0.5484878 </td>
   <td style="text-align:right;"> 0.529594 </td>
   <td style="text-align:right;"> 0.4493487 </td>
   <td style="text-align:left;"> R </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:right;"> 759061 </td>
   <td style="text-align:right;"> 897672 </td>
   <td style="text-align:right;"> 34138 </td>
   <td style="text-align:right;"> 44.9 </td>
   <td style="text-align:right;"> 53.1 </td>
   <td style="text-align:right;"> 2.0 </td>
   <td style="text-align:right;"> -8.2 </td>
   <td style="text-align:right;"> 1.2 </td>
   <td style="text-align:right;"> 8.0 </td>
   <td style="text-align:left;"> IA </td>
   <td style="text-align:right;"> 44.9 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Kansas </td>
   <td style="text-align:right;"> 0.3652451 </td>
   <td style="text-align:right;"> 0.3297052 </td>
   <td style="text-align:right;"> 0.4327250 </td>
   <td style="text-align:right;"> 0.4645894 </td>
   <td style="text-align:right;"> 0.3992763 </td>
   <td style="text-align:right;"> 0.3908309 </td>
   <td style="text-align:right;"> 0.3713290 </td>
   <td style="text-align:right;"> 0.4238772 </td>
   <td style="text-align:right;"> 0.388867 </td>
   <td style="text-align:right;"> 0.3889010 </td>
   <td style="text-align:left;"> R </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:right;"> 570323 </td>
   <td style="text-align:right;"> 771406 </td>
   <td style="text-align:right;"> 30574 </td>
   <td style="text-align:right;"> 41.6 </td>
   <td style="text-align:right;"> 56.2 </td>
   <td style="text-align:right;"> 2.2 </td>
   <td style="text-align:right;"> -14.7 </td>
   <td style="text-align:right;"> 5.9 </td>
   <td style="text-align:right;"> 15.9 </td>
   <td style="text-align:left;"> KS </td>
   <td style="text-align:right;"> 41.6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Kentucky </td>
   <td style="text-align:right;"> 0.4924674 </td>
   <td style="text-align:right;"> 0.3960661 </td>
   <td style="text-align:right;"> 0.4414623 </td>
   <td style="text-align:right;"> 0.5186878 </td>
   <td style="text-align:right;"> 0.5052905 </td>
   <td style="text-align:right;"> 0.4227221 </td>
   <td style="text-align:right;"> 0.3999238 </td>
   <td style="text-align:right;"> 0.4176657 </td>
   <td style="text-align:right;"> 0.384572 </td>
   <td style="text-align:right;"> 0.3432773 </td>
   <td style="text-align:left;"> R </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:right;"> 772474 </td>
   <td style="text-align:right;"> 1326646 </td>
   <td style="text-align:right;"> 37648 </td>
   <td style="text-align:right;"> 36.2 </td>
   <td style="text-align:right;"> 62.1 </td>
   <td style="text-align:right;"> 1.8 </td>
   <td style="text-align:right;"> -25.9 </td>
   <td style="text-align:right;"> 3.9 </td>
   <td style="text-align:right;"> 11.1 </td>
   <td style="text-align:left;"> KY </td>
   <td style="text-align:right;"> 36.2 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Louisiana </td>
   <td style="text-align:right;"> 0.4718911 </td>
   <td style="text-align:right;"> 0.3858084 </td>
   <td style="text-align:right;"> 0.4480871 </td>
   <td style="text-align:right;"> 0.5266514 </td>
   <td style="text-align:right;"> 0.5656084 </td>
   <td style="text-align:right;"> 0.4606075 </td>
   <td style="text-align:right;"> 0.4266906 </td>
   <td style="text-align:right;"> 0.4054283 </td>
   <td style="text-align:right;"> 0.412532 </td>
   <td style="text-align:right;"> 0.3982805 </td>
   <td style="text-align:left;"> R </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:right;"> 856034 </td>
   <td style="text-align:right;"> 1255776 </td>
   <td style="text-align:right;"> 36252 </td>
   <td style="text-align:right;"> 39.9 </td>
   <td style="text-align:right;"> 58.5 </td>
   <td style="text-align:right;"> 1.7 </td>
   <td style="text-align:right;"> -18.6 </td>
   <td style="text-align:right;"> 1.0 </td>
   <td style="text-align:right;"> 5.9 </td>
   <td style="text-align:left;"> LA </td>
   <td style="text-align:right;"> 39.9 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Maine </td>
   <td style="text-align:right;"> 0.4809052 </td>
   <td style="text-align:right;"> 0.3893088 </td>
   <td style="text-align:right;"> 0.4422898 </td>
   <td style="text-align:right;"> 0.5605587 </td>
   <td style="text-align:right;"> 0.6266212 </td>
   <td style="text-align:right;"> 0.5274784 </td>
   <td style="text-align:right;"> 0.5458302 </td>
   <td style="text-align:right;"> 0.5882952 </td>
   <td style="text-align:right;"> 0.578599 </td>
   <td style="text-align:right;"> 0.5159655 </td>
   <td style="text-align:left;"> D </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:right;"> 435072 </td>
   <td style="text-align:right;"> 360737 </td>
   <td style="text-align:right;"> 23652 </td>
   <td style="text-align:right;"> 53.1 </td>
   <td style="text-align:right;"> 44.0 </td>
   <td style="text-align:right;"> 2.9 </td>
   <td style="text-align:right;"> 9.1 </td>
   <td style="text-align:right;"> 6.1 </td>
   <td style="text-align:right;"> 9.6 </td>
   <td style="text-align:left;"> ME </td>
   <td style="text-align:right;"> 53.1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Maryland </td>
   <td style="text-align:right;"> 0.5161914 </td>
   <td style="text-align:right;"> 0.4724247 </td>
   <td style="text-align:right;"> 0.4853557 </td>
   <td style="text-align:right;"> 0.5829990 </td>
   <td style="text-align:right;"> 0.5863842 </td>
   <td style="text-align:right;"> 0.5847083 </td>
   <td style="text-align:right;"> 0.5656558 </td>
   <td style="text-align:right;"> 0.6293009 </td>
   <td style="text-align:right;"> 0.633217 </td>
   <td style="text-align:right;"> 0.6401740 </td>
   <td style="text-align:left;"> D </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:right;"> 1985023 </td>
   <td style="text-align:right;"> 976414 </td>
   <td style="text-align:right;"> 75593 </td>
   <td style="text-align:right;"> 65.4 </td>
   <td style="text-align:right;"> 32.2 </td>
   <td style="text-align:right;"> 2.5 </td>
   <td style="text-align:right;"> 33.2 </td>
   <td style="text-align:right;"> 6.8 </td>
   <td style="text-align:right;"> 9.2 </td>
   <td style="text-align:left;"> MD </td>
   <td style="text-align:right;"> 65.4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Massachusetts </td>
   <td style="text-align:right;"> 0.4990933 </td>
   <td style="text-align:right;"> 0.4860167 </td>
   <td style="text-align:right;"> 0.5398224 </td>
   <td style="text-align:right;"> 0.6209235 </td>
   <td style="text-align:right;"> 0.6863983 </td>
   <td style="text-align:right;"> 0.6478934 </td>
   <td style="text-align:right;"> 0.6274285 </td>
   <td style="text-align:right;"> 0.6319709 </td>
   <td style="text-align:right;"> 0.617857 </td>
   <td style="text-align:right;"> 0.6465201 </td>
   <td style="text-align:left;"> D </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:right;"> 2382202 </td>
   <td style="text-align:right;"> 1167202 </td>
   <td style="text-align:right;"> 81998 </td>
   <td style="text-align:right;"> 65.6 </td>
   <td style="text-align:right;"> 32.1 </td>
   <td style="text-align:right;"> 2.3 </td>
   <td style="text-align:right;"> 33.5 </td>
   <td style="text-align:right;"> 6.3 </td>
   <td style="text-align:right;"> 9.2 </td>
   <td style="text-align:left;"> MA </td>
   <td style="text-align:right;"> 65.6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Michigan </td>
   <td style="text-align:right;"> 0.4645359 </td>
   <td style="text-align:right;"> 0.4045368 </td>
   <td style="text-align:right;"> 0.4602195 </td>
   <td style="text-align:right;"> 0.5461516 </td>
   <td style="text-align:right;"> 0.5732442 </td>
   <td style="text-align:right;"> 0.5263461 </td>
   <td style="text-align:right;"> 0.5172585 </td>
   <td style="text-align:right;"> 0.5837130 </td>
   <td style="text-align:right;"> 0.548005 </td>
   <td style="text-align:right;"> 0.4988333 </td>
   <td style="text-align:left;"> D </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:right;"> 2804040 </td>
   <td style="text-align:right;"> 2649852 </td>
   <td style="text-align:right;"> 85410 </td>
   <td style="text-align:right;"> 50.6 </td>
   <td style="text-align:right;"> 47.8 </td>
   <td style="text-align:right;"> 1.5 </td>
   <td style="text-align:right;"> 2.8 </td>
   <td style="text-align:right;"> 3.0 </td>
   <td style="text-align:right;"> 15.4 </td>
   <td style="text-align:left;"> MI </td>
   <td style="text-align:right;"> 50.6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Minnesota </td>
   <td style="text-align:right;"> 0.5221441 </td>
   <td style="text-align:right;"> 0.5009089 </td>
   <td style="text-align:right;"> 0.5355086 </td>
   <td style="text-align:right;"> 0.5772134 </td>
   <td style="text-align:right;"> 0.5937939 </td>
   <td style="text-align:right;"> 0.5128641 </td>
   <td style="text-align:right;"> 0.5176091 </td>
   <td style="text-align:right;"> 0.5522938 </td>
   <td style="text-align:right;"> 0.539412 </td>
   <td style="text-align:right;"> 0.5082631 </td>
   <td style="text-align:left;"> D </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:right;"> 1717077 </td>
   <td style="text-align:right;"> 1484065 </td>
   <td style="text-align:right;"> 76029 </td>
   <td style="text-align:right;"> 52.4 </td>
   <td style="text-align:right;"> 45.3 </td>
   <td style="text-align:right;"> 2.3 </td>
   <td style="text-align:right;"> 7.1 </td>
   <td style="text-align:right;"> 5.6 </td>
   <td style="text-align:right;"> 11.3 </td>
   <td style="text-align:left;"> MN </td>
   <td style="text-align:right;"> 52.4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Mississippi </td>
   <td style="text-align:right;"> 0.4932167 </td>
   <td style="text-align:right;"> 0.3772129 </td>
   <td style="text-align:right;"> 0.3947892 </td>
   <td style="text-align:right;"> 0.4507151 </td>
   <td style="text-align:right;"> 0.4725278 </td>
   <td style="text-align:right;"> 0.4139915 </td>
   <td style="text-align:right;"> 0.4007559 </td>
   <td style="text-align:right;"> 0.4335807 </td>
   <td style="text-align:right;"> 0.441981 </td>
   <td style="text-align:right;"> 0.4091095 </td>
   <td style="text-align:left;"> R </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:right;"> 539508 </td>
   <td style="text-align:right;"> 756789 </td>
   <td style="text-align:right;"> 17597 </td>
   <td style="text-align:right;"> 41.1 </td>
   <td style="text-align:right;"> 57.6 </td>
   <td style="text-align:right;"> 1.3 </td>
   <td style="text-align:right;"> -16.5 </td>
   <td style="text-align:right;"> 1.3 </td>
   <td style="text-align:right;"> 8.6 </td>
   <td style="text-align:left;"> MS </td>
   <td style="text-align:right;"> 41.1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Missouri </td>
   <td style="text-align:right;"> 0.4643458 </td>
   <td style="text-align:right;"> 0.3997525 </td>
   <td style="text-align:right;"> 0.4800309 </td>
   <td style="text-align:right;"> 0.5650697 </td>
   <td style="text-align:right;"> 0.5354704 </td>
   <td style="text-align:right;"> 0.4828805 </td>
   <td style="text-align:right;"> 0.4638029 </td>
   <td style="text-align:right;"> 0.4993242 </td>
   <td style="text-align:right;"> 0.452213 </td>
   <td style="text-align:right;"> 0.4018245 </td>
   <td style="text-align:left;"> R </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:right;"> 1253014 </td>
   <td style="text-align:right;"> 1718736 </td>
   <td style="text-align:right;"> 54212 </td>
   <td style="text-align:right;"> 41.4 </td>
   <td style="text-align:right;"> 56.8 </td>
   <td style="text-align:right;"> 1.8 </td>
   <td style="text-align:right;"> -15.4 </td>
   <td style="text-align:right;"> 3.2 </td>
   <td style="text-align:right;"> 7.7 </td>
   <td style="text-align:left;"> MO </td>
   <td style="text-align:right;"> 41.4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Montana </td>
   <td style="text-align:right;"> 0.3633476 </td>
   <td style="text-align:right;"> 0.3869860 </td>
   <td style="text-align:right;"> 0.4701181 </td>
   <td style="text-align:right;"> 0.5172406 </td>
   <td style="text-align:right;"> 0.4831259 </td>
   <td style="text-align:right;"> 0.3634364 </td>
   <td style="text-align:right;"> 0.3949992 </td>
   <td style="text-align:right;"> 0.4876870 </td>
   <td style="text-align:right;"> 0.429658 </td>
   <td style="text-align:right;"> 0.3888645 </td>
   <td style="text-align:left;"> R </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:right;"> 244786 </td>
   <td style="text-align:right;"> 343602 </td>
   <td style="text-align:right;"> 15286 </td>
   <td style="text-align:right;"> 40.5 </td>
   <td style="text-align:right;"> 56.9 </td>
   <td style="text-align:right;"> 2.5 </td>
   <td style="text-align:right;"> -16.4 </td>
   <td style="text-align:right;"> 4.1 </td>
   <td style="text-align:right;"> 21.4 </td>
   <td style="text-align:left;"> MT </td>
   <td style="text-align:right;"> 40.5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Nebraska </td>
   <td style="text-align:right;"> 0.2843463 </td>
   <td style="text-align:right;"> 0.2899525 </td>
   <td style="text-align:right;"> 0.3945430 </td>
   <td style="text-align:right;"> 0.3869465 </td>
   <td style="text-align:right;"> 0.3944518 </td>
   <td style="text-align:right;"> 0.3482052 </td>
   <td style="text-align:right;"> 0.3315266 </td>
   <td style="text-align:right;"> 0.4239092 </td>
   <td style="text-align:right;"> 0.388706 </td>
   <td style="text-align:right;"> 0.3645214 </td>
   <td style="text-align:left;"> R </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:right;"> 374583 </td>
   <td style="text-align:right;"> 556846 </td>
   <td style="text-align:right;"> 20283 </td>
   <td style="text-align:right;"> 39.4 </td>
   <td style="text-align:right;"> 58.5 </td>
   <td style="text-align:right;"> 2.1 </td>
   <td style="text-align:right;"> -19.2 </td>
   <td style="text-align:right;"> 5.9 </td>
   <td style="text-align:right;"> 12.7 </td>
   <td style="text-align:left;"> NE </td>
   <td style="text-align:right;"> 39.4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Nevada </td>
   <td style="text-align:right;"> 0.3007267 </td>
   <td style="text-align:right;"> 0.3268432 </td>
   <td style="text-align:right;"> 0.3918141 </td>
   <td style="text-align:right;"> 0.5182478 </td>
   <td style="text-align:right;"> 0.5058653 </td>
   <td style="text-align:right;"> 0.4814316 </td>
   <td style="text-align:right;"> 0.4868240 </td>
   <td style="text-align:right;"> 0.5638674 </td>
   <td style="text-align:right;"> 0.534075 </td>
   <td style="text-align:right;"> 0.5129523 </td>
   <td style="text-align:left;"> D </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:right;"> 703486 </td>
   <td style="text-align:right;"> 669890 </td>
   <td style="text-align:right;"> 32000 </td>
   <td style="text-align:right;"> 50.1 </td>
   <td style="text-align:right;"> 47.7 </td>
   <td style="text-align:right;"> 2.3 </td>
   <td style="text-align:right;"> 2.4 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 24.9 </td>
   <td style="text-align:left;"> NV </td>
   <td style="text-align:right;"> 50.1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> New Hampshire </td>
   <td style="text-align:right;"> 0.3293231 </td>
   <td style="text-align:right;"> 0.3107401 </td>
   <td style="text-align:right;"> 0.3676637 </td>
   <td style="text-align:right;"> 0.5079655 </td>
   <td style="text-align:right;"> 0.5561067 </td>
   <td style="text-align:right;"> 0.4933220 </td>
   <td style="text-align:right;"> 0.5069029 </td>
   <td style="text-align:right;"> 0.5486854 </td>
   <td style="text-align:right;"> 0.528338 </td>
   <td style="text-align:right;"> 0.5019831 </td>
   <td style="text-align:left;"> D </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:right;"> 424921 </td>
   <td style="text-align:right;"> 365654 </td>
   <td style="text-align:right;"> 15607 </td>
   <td style="text-align:right;"> 52.7 </td>
   <td style="text-align:right;"> 45.4 </td>
   <td style="text-align:right;"> 1.9 </td>
   <td style="text-align:right;"> 7.4 </td>
   <td style="text-align:right;"> 7.0 </td>
   <td style="text-align:right;"> 8.3 </td>
   <td style="text-align:left;"> NH </td>
   <td style="text-align:right;"> 52.7 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> New Jersey </td>
   <td style="text-align:right;"> 0.4259086 </td>
   <td style="text-align:right;"> 0.3947861 </td>
   <td style="text-align:right;"> 0.4309884 </td>
   <td style="text-align:right;"> 0.5142032 </td>
   <td style="text-align:right;"> 0.5996678 </td>
   <td style="text-align:right;"> 0.5821141 </td>
   <td style="text-align:right;"> 0.5337054 </td>
   <td style="text-align:right;"> 0.5786463 </td>
   <td style="text-align:right;"> 0.589520 </td>
   <td style="text-align:right;"> 0.5728722 </td>
   <td style="text-align:left;"> D </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:right;"> 2608335 </td>
   <td style="text-align:right;"> 1883274 </td>
   <td style="text-align:right;"> 57744 </td>
   <td style="text-align:right;"> 57.3 </td>
   <td style="text-align:right;"> 41.4 </td>
   <td style="text-align:right;"> 1.3 </td>
   <td style="text-align:right;"> 15.9 </td>
   <td style="text-align:right;"> 1.8 </td>
   <td style="text-align:right;"> 17.4 </td>
   <td style="text-align:left;"> NJ </td>
   <td style="text-align:right;"> 57.3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> New Mexico </td>
   <td style="text-align:right;"> 0.4009173 </td>
   <td style="text-align:right;"> 0.3965040 </td>
   <td style="text-align:right;"> 0.4749008 </td>
   <td style="text-align:right;"> 0.5514216 </td>
   <td style="text-align:right;"> 0.5402413 </td>
   <td style="text-align:right;"> 0.5003192 </td>
   <td style="text-align:right;"> 0.4959967 </td>
   <td style="text-align:right;"> 0.5766490 </td>
   <td style="text-align:right;"> 0.552952 </td>
   <td style="text-align:right;"> 0.5465459 </td>
   <td style="text-align:left;"> D </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:right;"> 501614 </td>
   <td style="text-align:right;"> 401894 </td>
   <td style="text-align:right;"> 20457 </td>
   <td style="text-align:right;"> 54.3 </td>
   <td style="text-align:right;"> 43.5 </td>
   <td style="text-align:right;"> 2.2 </td>
   <td style="text-align:right;"> 10.8 </td>
   <td style="text-align:right;"> 2.6 </td>
   <td style="text-align:right;"> 15.7 </td>
   <td style="text-align:left;"> NM </td>
   <td style="text-align:right;"> 54.3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> New York </td>
   <td style="text-align:right;"> 0.4852852 </td>
   <td style="text-align:right;"> 0.4598228 </td>
   <td style="text-align:right;"> 0.5206860 </td>
   <td style="text-align:right;"> 0.5947835 </td>
   <td style="text-align:right;"> 0.6601750 </td>
   <td style="text-align:right;"> 0.6308908 </td>
   <td style="text-align:right;"> 0.5928777 </td>
   <td style="text-align:right;"> 0.6357668 </td>
   <td style="text-align:right;"> 0.642759 </td>
   <td style="text-align:right;"> 0.6177117 </td>
   <td style="text-align:left;"> D </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:right;"> 5244886 </td>
   <td style="text-align:right;"> 3251997 </td>
   <td style="text-align:right;"> 119978 </td>
   <td style="text-align:right;"> 60.9 </td>
   <td style="text-align:right;"> 37.7 </td>
   <td style="text-align:right;"> 1.4 </td>
   <td style="text-align:right;"> 23.1 </td>
   <td style="text-align:right;"> 0.6 </td>
   <td style="text-align:right;"> 11.6 </td>
   <td style="text-align:left;"> NY </td>
   <td style="text-align:right;"> 60.9 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> North Carolina </td>
   <td style="text-align:right;"> 0.4890032 </td>
   <td style="text-align:right;"> 0.3797214 </td>
   <td style="text-align:right;"> 0.4184246 </td>
   <td style="text-align:right;"> 0.4954154 </td>
   <td style="text-align:right;"> 0.4747001 </td>
   <td style="text-align:right;"> 0.4353600 </td>
   <td style="text-align:right;"> 0.4375803 </td>
   <td style="text-align:right;"> 0.5016596 </td>
   <td style="text-align:right;"> 0.489660 </td>
   <td style="text-align:right;"> 0.4809375 </td>
   <td style="text-align:left;"> R </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:right;"> 2684292 </td>
   <td style="text-align:right;"> 2758775 </td>
   <td style="text-align:right;"> 81737 </td>
   <td style="text-align:right;"> 48.6 </td>
   <td style="text-align:right;"> 49.9 </td>
   <td style="text-align:right;"> 1.5 </td>
   <td style="text-align:right;"> -1.3 </td>
   <td style="text-align:right;"> 2.3 </td>
   <td style="text-align:right;"> 16.5 </td>
   <td style="text-align:left;"> NC </td>
   <td style="text-align:right;"> 48.6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> North Dakota </td>
   <td style="text-align:right;"> 0.2901929 </td>
   <td style="text-align:right;"> 0.3426542 </td>
   <td style="text-align:right;"> 0.4340464 </td>
   <td style="text-align:right;"> 0.4212530 </td>
   <td style="text-align:right;"> 0.4608868 </td>
   <td style="text-align:right;"> 0.3527260 </td>
   <td style="text-align:right;"> 0.3609065 </td>
   <td style="text-align:right;"> 0.4557124 </td>
   <td style="text-align:right;"> 0.398888 </td>
   <td style="text-align:right;"> 0.3019182 </td>
   <td style="text-align:left;"> R </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:right;"> 114902 </td>
   <td style="text-align:right;"> 235595 </td>
   <td style="text-align:right;"> 11322 </td>
   <td style="text-align:right;"> 31.8 </td>
   <td style="text-align:right;"> 65.1 </td>
   <td style="text-align:right;"> 3.1 </td>
   <td style="text-align:right;"> -33.4 </td>
   <td style="text-align:right;"> 2.4 </td>
   <td style="text-align:right;"> 5.1 </td>
   <td style="text-align:left;"> ND </td>
   <td style="text-align:right;"> 31.8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Ohio </td>
   <td style="text-align:right;"> 0.4426452 </td>
   <td style="text-align:right;"> 0.4052931 </td>
   <td style="text-align:right;"> 0.4452594 </td>
   <td style="text-align:right;"> 0.5116816 </td>
   <td style="text-align:right;"> 0.5359695 </td>
   <td style="text-align:right;"> 0.4818157 </td>
   <td style="text-align:right;"> 0.4894124 </td>
   <td style="text-align:right;"> 0.5233384 </td>
   <td style="text-align:right;"> 0.515141 </td>
   <td style="text-align:right;"> 0.4573242 </td>
   <td style="text-align:left;"> R </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:right;"> 2679165 </td>
   <td style="text-align:right;"> 3154834 </td>
   <td style="text-align:right;"> 88203 </td>
   <td style="text-align:right;"> 45.2 </td>
   <td style="text-align:right;"> 53.3 </td>
   <td style="text-align:right;"> 1.5 </td>
   <td style="text-align:right;"> -8.0 </td>
   <td style="text-align:right;"> 0.1 </td>
   <td style="text-align:right;"> 7.7 </td>
   <td style="text-align:left;"> OH </td>
   <td style="text-align:right;"> 45.2 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Oklahoma </td>
   <td style="text-align:right;"> 0.3662787 </td>
   <td style="text-align:right;"> 0.3089017 </td>
   <td style="text-align:right;"> 0.4161019 </td>
   <td style="text-align:right;"> 0.4437788 </td>
   <td style="text-align:right;"> 0.4559939 </td>
   <td style="text-align:right;"> 0.3891933 </td>
   <td style="text-align:right;"> 0.3442960 </td>
   <td style="text-align:right;"> 0.3435492 </td>
   <td style="text-align:right;"> 0.332277 </td>
   <td style="text-align:right;"> 0.3069496 </td>
   <td style="text-align:left;"> R </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:right;"> 503890 </td>
   <td style="text-align:right;"> 1020280 </td>
   <td style="text-align:right;"> 36529 </td>
   <td style="text-align:right;"> 32.3 </td>
   <td style="text-align:right;"> 65.4 </td>
   <td style="text-align:right;"> 2.3 </td>
   <td style="text-align:right;"> -33.1 </td>
   <td style="text-align:right;"> 3.3 </td>
   <td style="text-align:right;"> 7.4 </td>
   <td style="text-align:left;"> OK </td>
   <td style="text-align:right;"> 32.3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Oregon </td>
   <td style="text-align:right;"> 0.4444741 </td>
   <td style="text-align:right;"> 0.4389529 </td>
   <td style="text-align:right;"> 0.5238368 </td>
   <td style="text-align:right;"> 0.5663389 </td>
   <td style="text-align:right;"> 0.5469311 </td>
   <td style="text-align:right;"> 0.5023589 </td>
   <td style="text-align:right;"> 0.5210863 </td>
   <td style="text-align:right;"> 0.5841372 </td>
   <td style="text-align:right;"> 0.562712 </td>
   <td style="text-align:right;"> 0.5615747 </td>
   <td style="text-align:left;"> D </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:right;"> 1340383 </td>
   <td style="text-align:right;"> 958448 </td>
   <td style="text-align:right;"> 75490 </td>
   <td style="text-align:right;"> 56.5 </td>
   <td style="text-align:right;"> 40.4 </td>
   <td style="text-align:right;"> 3.2 </td>
   <td style="text-align:right;"> 16.1 </td>
   <td style="text-align:right;"> 5.1 </td>
   <td style="text-align:right;"> 18.6 </td>
   <td style="text-align:left;"> OR </td>
   <td style="text-align:right;"> 56.5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Pennsylvania </td>
   <td style="text-align:right;"> 0.4613836 </td>
   <td style="text-align:right;"> 0.4629927 </td>
   <td style="text-align:right;"> 0.4883045 </td>
   <td style="text-align:right;"> 0.5554853 </td>
   <td style="text-align:right;"> 0.5516121 </td>
   <td style="text-align:right;"> 0.5214848 </td>
   <td style="text-align:right;"> 0.5125828 </td>
   <td style="text-align:right;"> 0.5522971 </td>
   <td style="text-align:right;"> 0.527319 </td>
   <td style="text-align:right;"> 0.4962878 </td>
   <td style="text-align:left;"> D </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:right;"> 3458229 </td>
   <td style="text-align:right;"> 3377674 </td>
   <td style="text-align:right;"> 79380 </td>
   <td style="text-align:right;"> 50.0 </td>
   <td style="text-align:right;"> 48.8 </td>
   <td style="text-align:right;"> 1.1 </td>
   <td style="text-align:right;"> 1.2 </td>
   <td style="text-align:right;"> 1.9 </td>
   <td style="text-align:right;"> 13.1 </td>
   <td style="text-align:left;"> PA </td>
   <td style="text-align:right;"> 50.0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Rhode Island </td>
   <td style="text-align:right;"> 0.5616605 </td>
   <td style="text-align:right;"> 0.4817027 </td>
   <td style="text-align:right;"> 0.5587787 </td>
   <td style="text-align:right;"> 0.6184372 </td>
   <td style="text-align:right;"> 0.6900421 </td>
   <td style="text-align:right;"> 0.6564912 </td>
   <td style="text-align:right;"> 0.6057751 </td>
   <td style="text-align:right;"> 0.6419814 </td>
   <td style="text-align:right;"> 0.640167 </td>
   <td style="text-align:right;"> 0.5831101 </td>
   <td style="text-align:left;"> D </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:right;"> 307486 </td>
   <td style="text-align:right;"> 199922 </td>
   <td style="text-align:right;"> 10349 </td>
   <td style="text-align:right;"> 59.4 </td>
   <td style="text-align:right;"> 38.6 </td>
   <td style="text-align:right;"> 2.0 </td>
   <td style="text-align:right;"> 20.8 </td>
   <td style="text-align:right;"> 5.3 </td>
   <td style="text-align:right;"> 11.6 </td>
   <td style="text-align:left;"> RI </td>
   <td style="text-align:right;"> 59.4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> South Carolina </td>
   <td style="text-align:right;"> 0.4921458 </td>
   <td style="text-align:right;"> 0.3588195 </td>
   <td style="text-align:right;"> 0.3792785 </td>
   <td style="text-align:right;"> 0.4536466 </td>
   <td style="text-align:right;"> 0.4677928 </td>
   <td style="text-align:right;"> 0.4185240 </td>
   <td style="text-align:right;"> 0.4136464 </td>
   <td style="text-align:right;"> 0.4545557 </td>
   <td style="text-align:right;"> 0.446917 </td>
   <td style="text-align:right;"> 0.4253739 </td>
   <td style="text-align:left;"> R </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:right;"> 1091541 </td>
   <td style="text-align:right;"> 1385103 </td>
   <td style="text-align:right;"> 36685 </td>
   <td style="text-align:right;"> 43.4 </td>
   <td style="text-align:right;"> 55.1 </td>
   <td style="text-align:right;"> 1.5 </td>
   <td style="text-align:right;"> -11.7 </td>
   <td style="text-align:right;"> 2.6 </td>
   <td style="text-align:right;"> 19.5 </td>
   <td style="text-align:left;"> SC </td>
   <td style="text-align:right;"> 43.4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> South Dakota </td>
   <td style="text-align:right;"> 0.3436654 </td>
   <td style="text-align:right;"> 0.3670049 </td>
   <td style="text-align:right;"> 0.4680762 </td>
   <td style="text-align:right;"> 0.4773897 </td>
   <td style="text-align:right;"> 0.4806641 </td>
   <td style="text-align:right;"> 0.3838529 </td>
   <td style="text-align:right;"> 0.3908671 </td>
   <td style="text-align:right;"> 0.4570429 </td>
   <td style="text-align:right;"> 0.407815 </td>
   <td style="text-align:right;"> 0.3403023 </td>
   <td style="text-align:left;"> R </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:right;"> 150471 </td>
   <td style="text-align:right;"> 261043 </td>
   <td style="text-align:right;"> 11095 </td>
   <td style="text-align:right;"> 35.6 </td>
   <td style="text-align:right;"> 61.8 </td>
   <td style="text-align:right;"> 2.6 </td>
   <td style="text-align:right;"> -26.2 </td>
   <td style="text-align:right;"> 3.6 </td>
   <td style="text-align:right;"> 14.2 </td>
   <td style="text-align:left;"> SD </td>
   <td style="text-align:right;"> 35.6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Tennessee </td>
   <td style="text-align:right;"> 0.4985008 </td>
   <td style="text-align:right;"> 0.4181815 </td>
   <td style="text-align:right;"> 0.4178136 </td>
   <td style="text-align:right;"> 0.5259804 </td>
   <td style="text-align:right;"> 0.5128664 </td>
   <td style="text-align:right;"> 0.4803713 </td>
   <td style="text-align:right;"> 0.4281456 </td>
   <td style="text-align:right;"> 0.4236853 </td>
   <td style="text-align:right;"> 0.396489 </td>
   <td style="text-align:right;"> 0.3637888 </td>
   <td style="text-align:left;"> R </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:right;"> 1143711 </td>
   <td style="text-align:right;"> 1852475 </td>
   <td style="text-align:right;"> 57665 </td>
   <td style="text-align:right;"> 37.5 </td>
   <td style="text-align:right;"> 60.7 </td>
   <td style="text-align:right;"> 1.9 </td>
   <td style="text-align:right;"> -23.2 </td>
   <td style="text-align:right;"> 2.8 </td>
   <td style="text-align:right;"> 21.8 </td>
   <td style="text-align:left;"> TN </td>
   <td style="text-align:right;"> 37.5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Texas </td>
   <td style="text-align:right;"> 0.4283266 </td>
   <td style="text-align:right;"> 0.3621370 </td>
   <td style="text-align:right;"> 0.4365367 </td>
   <td style="text-align:right;"> 0.4775784 </td>
   <td style="text-align:right;"> 0.4733938 </td>
   <td style="text-align:right;"> 0.3904373 </td>
   <td style="text-align:right;"> 0.3848981 </td>
   <td style="text-align:right;"> 0.4406406 </td>
   <td style="text-align:right;"> 0.419921 </td>
   <td style="text-align:right;"> 0.4529171 </td>
   <td style="text-align:left;"> R </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:right;"> 5259126 </td>
   <td style="text-align:right;"> 5890347 </td>
   <td style="text-align:right;"> 165583 </td>
   <td style="text-align:right;"> 46.5 </td>
   <td style="text-align:right;"> 52.1 </td>
   <td style="text-align:right;"> 1.5 </td>
   <td style="text-align:right;"> -5.6 </td>
   <td style="text-align:right;"> 3.4 </td>
   <td style="text-align:right;"> 26.2 </td>
   <td style="text-align:left;"> TX </td>
   <td style="text-align:right;"> 46.5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Utah </td>
   <td style="text-align:right;"> 0.2203481 </td>
   <td style="text-align:right;"> 0.2487998 </td>
   <td style="text-align:right;"> 0.3261212 </td>
   <td style="text-align:right;"> 0.3624642 </td>
   <td style="text-align:right;"> 0.3798051 </td>
   <td style="text-align:right;"> 0.2827449 </td>
   <td style="text-align:right;"> 0.2665356 </td>
   <td style="text-align:right;"> 0.3547364 </td>
   <td style="text-align:right;"> 0.253738 </td>
   <td style="text-align:right;"> 0.3762116 </td>
   <td style="text-align:left;"> R </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:right;"> 560282 </td>
   <td style="text-align:right;"> 865140 </td>
   <td style="text-align:right;"> 62867 </td>
   <td style="text-align:right;"> 37.6 </td>
   <td style="text-align:right;"> 58.1 </td>
   <td style="text-align:right;"> 4.2 </td>
   <td style="text-align:right;"> -20.5 </td>
   <td style="text-align:right;"> -2.4 </td>
   <td style="text-align:right;"> 31.5 </td>
   <td style="text-align:left;"> UT </td>
   <td style="text-align:right;"> 37.6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Vermont </td>
   <td style="text-align:right;"> 0.4640006 </td>
   <td style="text-align:right;"> 0.4133509 </td>
   <td style="text-align:right;"> 0.4821829 </td>
   <td style="text-align:right;"> 0.6025420 </td>
   <td style="text-align:right;"> 0.6318283 </td>
   <td style="text-align:right;"> 0.5544035 </td>
   <td style="text-align:right;"> 0.6030100 </td>
   <td style="text-align:right;"> 0.6889918 </td>
   <td style="text-align:right;"> 0.682473 </td>
   <td style="text-align:right;"> 0.6518689 </td>
   <td style="text-align:left;"> D </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:right;"> 242820 </td>
   <td style="text-align:right;"> 112704 </td>
   <td style="text-align:right;"> 11904 </td>
   <td style="text-align:right;"> 66.1 </td>
   <td style="text-align:right;"> 30.7 </td>
   <td style="text-align:right;"> 3.2 </td>
   <td style="text-align:right;"> 35.4 </td>
   <td style="text-align:right;"> 9.0 </td>
   <td style="text-align:right;"> 16.6 </td>
   <td style="text-align:left;"> VT </td>
   <td style="text-align:right;"> 66.1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Virginia </td>
   <td style="text-align:right;"> 0.4318414 </td>
   <td style="text-align:right;"> 0.3732431 </td>
   <td style="text-align:right;"> 0.3964105 </td>
   <td style="text-align:right;"> 0.4744499 </td>
   <td style="text-align:right;"> 0.4893941 </td>
   <td style="text-align:right;"> 0.4585276 </td>
   <td style="text-align:right;"> 0.4586630 </td>
   <td style="text-align:right;"> 0.5318258 </td>
   <td style="text-align:right;"> 0.519674 </td>
   <td style="text-align:right;"> 0.5282438 </td>
   <td style="text-align:left;"> D </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:right;"> 2413568 </td>
   <td style="text-align:right;"> 1962430 </td>
   <td style="text-align:right;"> 84526 </td>
   <td style="text-align:right;"> 54.1 </td>
   <td style="text-align:right;"> 44.0 </td>
   <td style="text-align:right;"> 1.9 </td>
   <td style="text-align:right;"> 10.1 </td>
   <td style="text-align:right;"> 4.8 </td>
   <td style="text-align:right;"> 12.0 </td>
   <td style="text-align:left;"> VA </td>
   <td style="text-align:right;"> 54.1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Washington </td>
   <td style="text-align:right;"> 0.4290465 </td>
   <td style="text-align:right;"> 0.4342886 </td>
   <td style="text-align:right;"> 0.5080771 </td>
   <td style="text-align:right;"> 0.5759170 </td>
   <td style="text-align:right;"> 0.5719465 </td>
   <td style="text-align:right;"> 0.5294477 </td>
   <td style="text-align:right;"> 0.5364654 </td>
   <td style="text-align:right;"> 0.5875202 </td>
   <td style="text-align:right;"> 0.576283 </td>
   <td style="text-align:right;"> 0.5878930 </td>
   <td style="text-align:left;"> D </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:right;"> 2369612 </td>
   <td style="text-align:right;"> 1584651 </td>
   <td style="text-align:right;"> 133368 </td>
   <td style="text-align:right;"> 58.0 </td>
   <td style="text-align:right;"> 38.8 </td>
   <td style="text-align:right;"> 3.3 </td>
   <td style="text-align:right;"> 19.2 </td>
   <td style="text-align:right;"> 3.5 </td>
   <td style="text-align:right;"> 21.5 </td>
   <td style="text-align:left;"> WA </td>
   <td style="text-align:right;"> 58.0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> West Virginia </td>
   <td style="text-align:right;"> 0.5236978 </td>
   <td style="text-align:right;"> 0.4472757 </td>
   <td style="text-align:right;"> 0.5237690 </td>
   <td style="text-align:right;"> 0.5776884 </td>
   <td style="text-align:right;"> 0.5835466 </td>
   <td style="text-align:right;"> 0.4675792 </td>
   <td style="text-align:right;"> 0.4352029 </td>
   <td style="text-align:right;"> 0.4332626 </td>
   <td style="text-align:right;"> 0.363257 </td>
   <td style="text-align:right;"> 0.2784218 </td>
   <td style="text-align:left;"> R </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:right;"> 235984 </td>
   <td style="text-align:right;"> 545382 </td>
   <td style="text-align:right;"> 13286 </td>
   <td style="text-align:right;"> 29.7 </td>
   <td style="text-align:right;"> 68.6 </td>
   <td style="text-align:right;"> 1.7 </td>
   <td style="text-align:right;"> -38.9 </td>
   <td style="text-align:right;"> 3.1 </td>
   <td style="text-align:right;"> 11.4 </td>
   <td style="text-align:left;"> WV </td>
   <td style="text-align:right;"> 29.7 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Wisconsin </td>
   <td style="text-align:right;"> 0.4740969 </td>
   <td style="text-align:right;"> 0.4537618 </td>
   <td style="text-align:right;"> 0.5182347 </td>
   <td style="text-align:right;"> 0.5279451 </td>
   <td style="text-align:right;"> 0.5591919 </td>
   <td style="text-align:right;"> 0.5011507 </td>
   <td style="text-align:right;"> 0.5019180 </td>
   <td style="text-align:right;"> 0.5705568 </td>
   <td style="text-align:right;"> 0.534634 </td>
   <td style="text-align:right;"> 0.4958898 </td>
   <td style="text-align:left;"> D </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:right;"> 1630866 </td>
   <td style="text-align:right;"> 1610184 </td>
   <td style="text-align:right;"> 56991 </td>
   <td style="text-align:right;"> 49.4 </td>
   <td style="text-align:right;"> 48.8 </td>
   <td style="text-align:right;"> 1.7 </td>
   <td style="text-align:right;"> 0.6 </td>
   <td style="text-align:right;"> 1.4 </td>
   <td style="text-align:right;"> 10.8 </td>
   <td style="text-align:left;"> WI </td>
   <td style="text-align:right;"> 49.4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Wyoming </td>
   <td style="text-align:right;"> 0.3086737 </td>
   <td style="text-align:right;"> 0.2859960 </td>
   <td style="text-align:right;"> 0.3857512 </td>
   <td style="text-align:right;"> 0.4620798 </td>
   <td style="text-align:right;"> 0.4251208 </td>
   <td style="text-align:right;"> 0.2901770 </td>
   <td style="text-align:right;"> 0.2968730 </td>
   <td style="text-align:right;"> 0.3343798 </td>
   <td style="text-align:right;"> 0.288394 </td>
   <td style="text-align:right;"> 0.2429761 </td>
   <td style="text-align:left;"> R </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:right;"> 73491 </td>
   <td style="text-align:right;"> 193559 </td>
   <td style="text-align:right;"> 9715 </td>
   <td style="text-align:right;"> 26.6 </td>
   <td style="text-align:right;"> 69.9 </td>
   <td style="text-align:right;"> 3.5 </td>
   <td style="text-align:right;"> -43.4 </td>
   <td style="text-align:right;"> 2.9 </td>
   <td style="text-align:right;"> 8.2 </td>
   <td style="text-align:left;"> WY </td>
   <td style="text-align:right;"> 26.6 </td>
  </tr>
</tbody>
</table>

Describe the overall pattern, in one sentence.

## Question 2

Using the data from 1, generate a vote swing scatter plot with the 2020
Democratic percentage of the vote on the Y axis and the 2016 Democratic
percentage of the vote on the X axis, labeling the points with the state
names, and adding a 45 degree line to the plot using geom_abline().
Describe your graph. What is is overall pattern in the data? What
challenges are created by labeling the states? Try labeling only some of
the states, justifying your choice of states. Are there unusual states
in the plot? If so, which ones? Make a graph that omits Washington D.C.
from the data. What effect does that have on the structure of the graph?

## Question 3

Again using the data from 1, filter out Washington D.C., reshape the
data from wide form to long form using pivot_longer() and generate a
line plot for the 50 states from 1980 to 2020, graphing percent
Democratic on the y-axis and year on the x-axis. Choose a set of states
that you find interesting, and emphasize those states using colors.
Describe the overall patterns in the data over time. What central
features are particularly noteworthy, and why? What story are you
telling with your use of color to highlight a set of states? Remember
that in section 7.1, Healy faceted his election graph by the four census
regions. Generate a second graph structured as before, this time
faceting by census region.

## Question 4: Final Project Data

Now using some data that you would like to use in your final project,
answer the following: Describe in detail the source of your data. What
question do you want to ask? That is, what is your research question,
or, put differently, what question will your graph answer? Create a
scatterplot (or alternative graph, as appropriate) that illustrates the
relationship between your outcome of interest and your main independent
variable of interest. Describe the graph in a paragraph. What story does
the graph convey?

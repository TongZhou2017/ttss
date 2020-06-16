# ttss
Tong's tools - second structure summary


version: 0.0.0.9000

Author: Tong Zhou (tongzhou2017@gmail.com)

---

## install

```
library(devtools)

install_github()
```


## setup input string

```
str = "{{{-{{{{{{{,{{{{{{{{{-----{{{-{{{,,<<---<<<----<<___>>--->>>>>,,,,,,[[[,,,,,,((((--((--((----((((,,,,,,,,,,,,<<<<----<<<<<<<_____>>>>>>>----->>>>,,,,,,<<>>,,,,<<<<<<-------<<<<________>>>>------>>>>>>,,,,,,,,,<<<<-<<<>>>>>>>,,,))))---))))--))))<<<--<-<<<----<<<<<<<<_______>>>>>>>>>>>----->>>>,,,<<<<<<<<____>>>>--->>>>]]],<<<<<<__________>>>>>>,<<<<____>>>>,,,}}}}}}-},,,,,<-<<<---<<<<<___>>>>>->>>>,}}-}}}}}},,,,<<<<<<<<<<<<<____>>>>>>>>>>->>>,,,,,,<<<-<<<_______>>>>->>,,,,,,,,,<<<<<<-------<<<<-----<<____>>------->>>>>>>>>>,}}}}}}}}}}::::::::::[[[-----[,((((---(((-------------------------------------------------------------------------------------------------------------------------------------------------------------------(((((((((--(((((((<<--<<<<<<<<---<<<______>>>------>>>>>>>>-->>,,,,<--<<____>>>)))))))-)))))-))))---)))---)))),,,,<<<<<<---<<---<<<<_________>>>>--->>>>>>>>,,,,,,,,,,((((((,<<<--<<<<<<<__>>>>>>>-->>>,,,<<____>>,,,)))----)))]]]]:<<<------<<<<____>>>>---->>>::::::::{{{{{-{{{{{{{--{{,,{{{{{{{{{{{{{{{{{,,,,<<<<________>>>>,,,,,,,,{{{{{{{,,,,<<<<<--------<<<<<<________>>>>>>------>>>>>,,,[[-[[[[--[[[[[[[[[,,,((((((<<<____>>>,,<<<<______>>>>,,)))))),,,,,((((,<<<-<<<<--<<<<____________>>>>-->>>>>>>,,<<<<<<-<<_____>>>>>>>>,,,,,)))),,,,]]]]-]]]---]-]]]]]]],,,,,}}}}}}},,,}}-}}}}}}}}}},,,<<<<<<<-----<<<---<<--<<<<____>>>>-->>---->>>------>>>>>>>,,,,,,<---<<<<<<<<________>>>>>>>>--->,,,,,}}}}},,,,,<<<<<<<<_______>>>>>>>>,,,,,,}}---}}}}}}}}}}-}}::<-<<-<<-<-<<<<<<<<--<-<<<<<<<<<<<<--<<<<<<<<<<<<<<____>>>>>>>>>>>>>>-->>>>>>>>>>>>>-->>>>>>>>-->->>-->>--"
```

## summary and output

```
\# load package

library(ttss)

\# smmary str, print paied information into p.txt, print unpaied information into u.txt

ttss_summary(str,file_paired = "p.txt", file_unpaired = "u.txt")
```


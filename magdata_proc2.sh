#!/bin/bash

declare -a st=("BMT" "BOU" "BRD" "BSL" "CKI" "CYG" "GUI" "HBK" "IPM" "KAK" "KDU" "KMH" "PIL" "SJG" "TAM" "TDC" "TUC" "HON" "KNY" "JAI" "LZH" "ABG" "MLT" "QSB" "IZN")

echo "enter year [yyyy]"

read year

search_dir="/home/isaac/datos/intermagnet/$year"

declare -a st_name=("bmt" "bou" "brd" "bsl" "cki" "cyg" "gui" "hbk" "ipm" "kak" "kdu" "kmh" "pil" "sjg" "tam" "tdc" "tuc" "hon" "kny" "jai" "lzh" "abg" "mlt" "qsb" "izn")

for i in ${!st[@]};do
        for j in "$search_dir/${st[$i]}/"*.min ; #cat $j >> ${search_dir}/${st[$i]}/*.min.out; done 
        do
                echo $j
                awk '{if ($1 ~ /^[2]...-..-../){gsub(/[T]/, " "); gsub(/[Z]/, ""); print} \
                else if ($1 ~ /^"2...-..-../){gsub(/['\"']/, ""); print}}' $j >> $j".out"
        done
done

#for i in '*.min' ;do cat $i >> *.min.out ;done

#awk '{if ($1 ~ /^[2]...-..-../){gsub(/[T]/, " "); gsub(/[Z]/, ""); print} \
#else if ($1 ~ /^"2...-..-../){gsub(/['\"']/, ""); print}}' *min.out >> *min.dat 

#rm *min.out
#echo "done" 

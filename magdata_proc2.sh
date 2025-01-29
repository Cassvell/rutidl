#!/bin/bash

declare -a st=("BMT" "BOU" "BRD" "BSL" "CKI" "CYG" "GUI" "HBK" "IPM" "KAK" "KDU" "KMH" "PIL" "SJG" "TAM" "TDC" "TUC" "HON" "KNY" "JAI" "LZH" "ABG" "MLT" "QSB" "IZN")

echo "enter year [yyyy]"

read year

search_dir="/home/isaac/datos/intermagnet/$year"

declare -a st_name=("bmt" "bou" "brd" "bsl" "cki" "cyg" "gui" "hbk" "ipm" "kak" "kdu" "kmh" "pil" "sjg" "tam" "tdc" "tuc" "hon" "kny" "jai" "lzh" "abg" "mlt" "qsb" "izn")

# Process each station directory
for i in "${!st_name[@]}"; do
    station_dir="$search_dir/${st_name[$i]}"
    
    # Check if directory exists
    if [[ ! -d "$station_dir" ]]; then
        echo "Skipping ${st_name[$i]}: Directory not found"
        continue
    fi

    # Process each .min file
    for j in $station_dir/*.min; do
        [[ -f "$j" ]] || continue  # Skip if no files found

        output_file="${j}.out"

        # Overwrite output file at the start
        awk '{
            if ($1 ~ /^[2]...-..-../) {
                gsub(/[T]/, " "); gsub(/[Z]/, ""); print
            } else if ($1 ~ /^"2...-..-../) {
                gsub(/"/, ""); print
            }
        }' "$j" > "$output_file"

        echo "Processed: $j -> $output_file"
    done
done

echo "Processing completed!"



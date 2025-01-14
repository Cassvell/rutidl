#!/bin/bash

if [[ -z "$1" ]]; then
  echo "Usage: $0 <obs>"
  exit 1
fi

obs="$1"
# Directory containing .eps files (update with your actual directory)
input_dir="/home/isaac/geomstorm/rutidl/output/wavelet/${obs}/evento13"
output_dir="/home/isaac/geomstorm/rutidl/output/wavelet/${obs}/evento13/png"

# Ensure the output directory exists
mkdir -p "$output_dir"

# Loop through all .eps files in the input directory
for eps_file in "$input_dir"/*.eps; do
  # Extract the file name without the extension
  base_name=$(basename "$eps_file" .eps)
  
  # Convert to .png format
  convert "$eps_file" "$output_dir/$base_name.png"
  
  echo "Converted $eps_file to $output_dir/$base_name.png"
done

echo "Conversion completed!"


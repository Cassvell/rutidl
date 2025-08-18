#!/bin/bash

# Check if ghostscript (gs) is installed
if ! command -v gs &> /dev/null; then
    echo "Error: ghostscript (gs) is not installed. Install it with:"
    echo "sudo apt install ghostscript"
    exit 1
fi

# Loop through all .eps files in the current directory
for eps_file in *.eps; do
    if [[ -f "$eps_file" ]]; then
        pdf_file="${eps_file%.eps}.pdf"
        
        echo "Converting $eps_file to $pdf_file..."
        
        # Convert EPS to PDF using ghostscript while preserving dimensions and resolution
        gs -q -dNOPAUSE -dBATCH -dEPSCrop -sDEVICE=pdfwrite -dPDFSETTINGS=/prepress -sOutputFile="$pdf_file" "$eps_file"
        
        if [[ $? -eq 0 ]]; then
            echo "✅ Success: $pdf_file created."
        else
            echo "❌ Error: Failed to convert $eps_file."
        fi
    fi
done

echo "Conversion complete."

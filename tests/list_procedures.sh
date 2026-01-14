#!/bin/bash
#
# Script to extract procedure names from w3emc source files
# and generate a list for testing module interface completeness
#
# Usage: list_procedures.sh <src_dir> <output_file>
#

set -e

if [ $# -ne 2 ]; then
    echo "Usage: $0 <src_dir> <output_file>"
    exit 1
fi

SRC_DIR="$1"
OUTPUT_FILE="$2"

# Check if source directory exists
if [ ! -d "$SRC_DIR" ]; then
    echo "Error: Source directory '$SRC_DIR' does not exist"
    exit 1
fi

# Create output directory if it doesn't exist
OUTPUT_DIR=$(dirname "$OUTPUT_FILE")
mkdir -p "$OUTPUT_DIR"

# Extract procedure names from Fortran source files
# - Look for subroutine and function declarations
# - Convert to lowercase for consistency
# - Sort and remove duplicates
grep -roPih '^\s*(subroutine|function)\s+\K\w+' "$SRC_DIR" --exclude "mersenne_twister.f" | tr '[A-Z]' '[a-z]' | sort | uniq > "$OUTPUT_FILE"

echo "Generated procedure list with $(wc -l < "$OUTPUT_FILE") procedures in: $OUTPUT_FILE"

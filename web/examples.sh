#!/bin/bash

files=(src/examples/*.ml src/evaluation/*.ml)

for file in "${files[@]}"; do
  contents="$(sed 's/"/\&quot;/g' < "$file")"
  # name=$(echo "$file" | sed 's@src/@@g')
  name="$file"
  transformed_contents="<option value=\"$contents\">$name</option>"
  output+="$transformed_contents"
done

MORE="$output" TZ=UTC-8 INITIAL_OUTPUT="Built $(date)" envsubst < "$1" > index1.html
cp index1.html "$2"


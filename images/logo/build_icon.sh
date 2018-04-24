#!/bin/bash

set -e

. icon_sizes.sh

ICON_FILES=()

for SIZE in "${SIZES[@]}"; do
	CUR_ICON="logo${SIZE}.png"
	ICON_FILES+=("${CUR_ICON}")
	inkscape logo.svg -e "${CUR_ICON}" -w "${SIZE}" -h "${SIZE}"
done

convert "${ICON_FILES[@]}" logo.ico

#!/bin/bash

# Usage: ./install_icon.sh <dest dir>
#
# To install the icons into the system, <dest dir> must be like this:
# /usr/share

set -e

. icon_sizes.sh

DEST_DIR="$1/icons/hicolor"

for SIZE in "${SIZES[@]}"; do
	CUR_ICON="logo${SIZE}.png"
	CUR_DIR="${DEST_DIR}/${SIZE}x${SIZE}/apps"
	mkdir -p "${CUR_DIR}"
	install -T "${CUR_ICON}" "${CUR_DIR}/tester.png"
done

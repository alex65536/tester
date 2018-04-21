#!/bin/sh

# Updates version of Tester packages
# Usage ./updVersion.sh VERSION
# VERSION must be major.minor.release

set -e

VERSION="$1"

if ! echo "${VERSION}" | tr '\n' ' ' | grep -E '^[0-9]+\.[0-9]+\.[0-9]+\ $' >/dev/null; then
	echo "Invalid version format: ${VERSION}"
	echo "Must be: major.minor.release"
	exit 1
fi

echo "${VERSION}" | sed -E -e 's/([0-9]+)\.([0-9]+)\.([0-9]+)/\1\n\2\n\3/' | {
	read -r MAJOR
	read -r MINOR
	read -r RELEASE
	echo "Setting version to ${MAJOR}.${MINOR}.${RELEASE}"
	
	# shellcheck disable=SC2016
	COMMAND='
		echo '\''Processing "{}"'\''
		REGEX='\''(<Version\sMajor=\")[0-9]+(\"\sMinor=\")[0-9+](\"\sRelease=\")[0-9]+(\".*/>)'\''
		REPL="'"\\\\1${MAJOR}\\\\2${MINOR}\\\\3${RELEASE}\\\\4"'"
		if ! grep -E "${REGEX}" '\''{}'\'' >/dev/null; then
			echo '\''Unable to process "{}"'\''
		else
			sed -i -E -e '\''s#'\''"${REGEX}"'\''#'\''"${REPL}"'\''#'\'' '\''{}'\''
		fi
	'
	find ../src -type f -name "*.lpk" -exec sh -c "${COMMAND}" \;
	
	for FILE in ../src/*.lpi; do
		echo "Processing \"${FILE}\""
		sed -i -E -e '
			s#(<MajorVersionNr Value=\")[0-9]+(\"/>)#\1'"${MAJOR}"'\2#
			s#(<MinorVersionNr Value=\")[0-9]+(\"/>)#\1'"${MINOR}"'\2#
			s#(<RevisionNr Value=\")[0-9]+(\"/>)#\1'"${RELEASE}"'\2#
			s#(ProductVersion=\")[0-9\.]+(\")#\1'"${MAJOR}.${MINOR}.${RELEASE}"'\2#
		' "${FILE}"
	done
}

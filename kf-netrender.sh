#!/bin/sh
# find directory of this script
ME="$(dirname "$(readlink -e "${0}")")"
# find kf next to this script
KF="${ME}/kf.exe"
# unset wine if using Windows
WINE="wine"
# current directory is network share
TODO="$(pwd)/todo"
DONE="$(pwd)/done"
CLIENT="$1"
if [ "x${CLIENT}" = x ]
then
	CLIENT="$(hostname)-$$"
fi
CLIENT="$(pwd)/client/${CLIENT}"
mkdir -p "${TODO}"
mkdir -p "${DONE}"
mkdir -p "${CLIENT}"
while true
do
	KFR="$(ls --sort=version "${TODO}/"*.kfr | head -n 1)"
	if [ "x${KFR}" != "x" ]
	then
		mv -v "${KFR}" "${CLIENT}/" &&
		( mv -v "${KFR%.kfr}."* "${CLIENT}/" || echo "(no extras, ignored)" ) &&
		LOCATION="${CLIENT}/$(basename "${KFR}")"
		if [ ! -f "${LOCATION}" ]
		then
			echo "location '${LOCATION}' does not exist, aborting"
			exit 1
		fi
		SETTINGS="${LOCATION%.kfr}.kfs"
		ret=1
		if [ -f "${SETTINGS}" ]
		then
			${WINE} "${KF}" \
				--load-settings "${SETTINGS}" \
				--load-location "${LOCATION}" \
				--save-png "${LOCATION%.kfr}.png" \
				--save-exr "${LOCATION%.kfr}.exr" && ret=0
		else
			${WINE} "${KF}" \
				--load-location "${LOCATION}" \
				--save-png "${LOCATION%.kfr}.png" \
				--save-exr "${LOCATION%.kfr}.exr" && ret=0
		fi
		if [ "x${ret}" != x0 ]
		then
			echo "job failed, aborting"
			exit 1
		fi
		mv -v "${CLIENT}/"* "${DONE}/" || exit 1
	else
		echo "(no work to do, sleeping...)\r"
		sleep 1 || exit 1
	fi
done

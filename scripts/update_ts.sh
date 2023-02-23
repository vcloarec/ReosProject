#!/usr/bin/env bash
###########################################################################
#    update_ts.sh
#    ---------------------
#    Date                 : November 2014
#    Copyright            : (C) 2014 by Juergen E. Fischer
#    Email                : jef at norbit dot de
###########################################################################
#                                                                         #
#   This program is free software; you can redistribute it and/or modify  #
#   it under the terms of the GNU General Public License as published by  #
#   the Free Software Foundation; either version 2 of the License, or     #
#   (at your option) any later version.                                   #
#                                                                         #
###########################################################################

# Modified by Vincent Cloarec (vcloarec at gmail dot com) for Reos Project

#set here the Qt directory
QTDIR=/opt/Qt/5.15.2/gcc_64/bin/

set -e

export SRCDIR=$PWD

retries=20
action=$1

case "$action" in
pull|push|update)
	;;

*)
	echo "usage: $(basename $0) {pull|{push|update} builddirectory [lang...]}"
	exit 1
esac

cleanup() {
	cd $SRCDIR

	if [ -f i18n/backup.tar ]; then
		echo Restoring files...
		tar -xf i18n/backup.tar
		rm i18n/backup.tar
	fi

	echo Removing temporary files
	#find src python \( -name "*-i18n.ui" -o -name "*-i18n.cpp" -o -name "*-i18n.ts" \) -delete

	trap "" EXIT
}

export QT_SELECT=5
PATH=$QTDIR/bin:$PATH
if type cygpath >/dev/null 2>&1; then
	SRCDIR=$(cygpath -am $SRCDIR)
fi

if type qmake-qt5 >/dev/null 2>&1; then
	QMAKE=qmake-qt5
else
	QMAKE=qmake
fi

if ! type pylupdate5 >/dev/null 2>&1; then
      echo "pylupdate5 not found"
      exit 1
fi

if type lupdate-qt5 >/dev/null 2>&1; then
	LUPDATE=lupdate-qt5
else
	LUPDATE=lupdate
fi

if ! type tx >/dev/null 2>&1; then
	echo "tx not found"
	exit 1
fi


files=
if [ -d "$2" ]; then
	builddir=$(realpath $2)
	textcpp=
	shift
	shift
	if [[ $# -gt 0 ]]; then
		for t in i18n/qgis_*.ts; do
			for l in "$@"; do
				if [ "i18n/qgis_$l.ts" = "$t" ]; then
					continue 2
				fi
			done
			files="$files $t"
		done
	fi

elif [ "$action" != "pull" ]; then
	echo Build directory not found
	exit 1
else
	shift
fi

trap cleanup EXIT

if [[ "$(git name-rev --name-only HEAD)" =~ ^release-[0-9]+_[0-9]+$ ]]; then
	TX_FLAGS=-b
fi

echo Saving translations
[ $action = push ] && files="$files i18n/reos_*.ts"
[ -n "${files## }" ] && tar --remove-files -cf i18n/backup.tar $files

if [ $action = push ]; then
	echo Pulling source from transifex...
	fail=1
	for i in $(seq $retries); do
		tx pull -s -l none $TX_FLAGS && fail=0 && break
		echo Retry $i/$retries...
		sleep 10
	done
	if (( fail )) || ! [ -f "i18n/reos_en.ts" ]; then
		echo Download of source translation failed
		exit 1
	fi
	cp i18n/reos_en.ts /tmp/reos_en.ts-downloaded
	perl scripts/ts_clear.pl  # reset English translations
elif [ $action = pull ]; then
	rm -f i18n/reos_*.ts

	echo Pulling new translations...
	if [ "$#" -gt 0 ]; then
		o=$*
		o="-l ${o// /,}"
	else
		o="-a"
	fi

	fail=1
	echo "tx option "$o
	echo "current directory "$PWD
	for i in $(seq $retries); do
		tx pull $o -s $TX_FLAGS && fail=0 && break
		echo Retry $i/$retries...
		sleep 10
	done

	if (( fail )); then
		echo "Could not pull translations"
		exit 1
	fi
fi


echo Creating qmake project file
	$QMAKE -project -o reos_ts.pro -nopwd $SRCDIR/src $SRCDIR/i18n

	QT_INSTALL_HEADERS=$(qmake -query QT_INSTALL_HEADERS)
	
	echo "TR_EXCLUDE = ${QT_INSTALL_HEADERS%
}/*" >>reos_ts.pro

echo Updating translations
	$LUPDATE -no-ui-lines -no-obsolete -locations absolute -verbose reos_ts.pro
	
	

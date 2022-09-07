#!/bin/bash
# elpa/elpy-20220627.1416/

#If /set_snippets.sh -c is called nothing happens, just checking


CHECKONLY=1

if [ "$#" -eq  "0" ] ;  then
#    echo "No arguments supplied"
    CHECKONLY=1
else
    if [ ! -z "$1" ] ; then
	if [ "$1" == "-c" ] ; then
#	    echo "P1 is -c"
	    CHECKONLY=0
	    echo "ReadOnly Mode"
	else	    
#	    echo "P1 is not -c"
#	    echo "P1 is {$1}"
	    CHECKONLY=1
	fi
    else
#	echo "P1 is not set"
	CHECKONLY=1
    fi    
fi

#echo ${CHECKONLY}
#echo "$1"

if [ "$1" = "-l" ] ; then
    echo "Delete Mode"
fi

FILPA=$(pwd)
cd elpa
ELPY=$(ls -d elpy*)
LONGPA=${ELPY}/snippets/python-mode/
cd ..

for file in snippets/*; do
	ORIGFILE=$(pwd)/${file}
       	FULLFILE=${FILPA}/elpa/${LONGPA}$(basename ${file})
	OLDFILE=${FULLFILE}_old
	if [ -L ${FULLFILE} ] ; then
		if [ -e ${FULLFILE} ] ; then
      		    echo "Good link: ${FULLFILE} "
		    #Check option -c
		    if [ "$CHECKONLY" = 1 ] ; then
			if [ -f ${OLDFILE} ]; then
				echo ${OLDFILE}
				rm ${OLDFILE}
			fi
			if [ "$1" = "-l" ] ; then
			    echo "Remove set removing: ${FULLFILE} "
			    rm ${FULLFILE}
			else
			    echo "Moving to old_file instead of deleting"
			    mv ${FULLFILE} ${OLDFILE}			
			fi
		    fi
		    
		else
      		    echo "Broken link: ${FULLFILE} "
		    if [ "$CHECKONLY" = 1 ] ; then
			echo "rm ${FULLFILE}"
			rm ${FULLFILE}
		    fi		    
   		fi
	elif [ -e ${FULLFILE} ] ; then
	    echo "Not a link: ${FULLFILE}"
	    if [ "$CHECKONLY" = 1 ] ; then
		if [ -f ${OLDFILE} ]; then
			echo ${OLDFILE}
			rm ${OLDFILE}
		fi
		echo "Moving as it's not a link"
                mv ${FULLFILE} ${OLDFILE}
	    fi	    
	else
   		echo "Missing: ${FULLFILE}"
	fi
	
	if [ "$CHECKONLY" = 1 ] ; then
	    echo "Setting Link"
	    ln -s ${ORIGFILE} ${FULLFILE}
	fi	
done




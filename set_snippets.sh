#!/bin/bash
# elpa/elpy-20220627.1416/
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
			if [ -f ${OLDFILE} ]; then
				echo ${OLDFILE}
				rm ${OLDFILE}
			fi
			mv ${FULLFILE} ${OLDFILE}

		else
      			echo "Broken link: ${FULLFILE} "
			rm ${FULLFILE}
   		fi
	elif [ -e ${FULLFILE} ] ; then
		echo "Not a link: ${FULLFILE}"
		if [ -f ${OLDFILE} ]; then
			echo ${OLDFILE}
			rm ${OLDFILE}
		fi
                mv ${FULLFILE} ${OLDFILE}
	else
   		echo "Missing: ${FULLFILE}"
	fi

	ln -s ${ORIGFILE} ${FULLFILE}
done

#	if [ -L ${FULLFILE} ] && [ -e ${FULLFILE} ]; then
#		OLDFILE=${FULLFILE}_old		
#		if [ -L ${OLDFILE} ] && [ -e ${OLDFILE} ]; then
#			echo ${OLDFILE}
#			rm ${OLDFILE}
#		fi
#		echo ${FULLFILE}
#		mv ${FULLFILE} ${OLDFILE}
#	else
#	       	ln -s ${ORIGFILE} ${FULLFILE}; 
#	fi
#done

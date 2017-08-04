#  BSD 3-Clause License
#  
#  Copyright (c) 2017, Ali ElShakankiry
#  All rights reserved.
#  
#  Redistribution and use in source and binary forms, with or without
#  modification, are permitted provided that the following conditions are met:
#  
#  * Redistributions of source code must retain the above copyright notice, this
#    list of conditions and the following disclaimer.
#  
#  * Redistributions in binary form must reproduce the above copyright notice,
#    this list of conditions and the following disclaimer in the documentation
#    and/or other materials provided with the distribution.
#  
#  * Neither the name of the copyright holder nor the names of its
#    contributors may be used to endorse or promote products derived from
#    this software without specific prior written permission.
#  
#  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
#  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
#  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
#  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
#  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
#  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
#  SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
#  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
#  OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
#  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

#!/bin/bash

# echo "Transforming $1". . .
DIR=""
CALLBACK=""

if [[ "$1" == "" || "$1" == "-h" ]]; then
	printf "Usage:\t./UID.sh [FILENAME]\n"
	printf "\t./UID.sh [FILENAME] [DIRECTORY]\n"
	echo "DIRECTORY is the directory to move the output files to"
	exit 1
fi

filename=$(echo $1 | cut -d '.' -f 1)
extension=$(echo $1 | cut -d '.' -f 2)

mkdir -p SCL5
mkdir -p SOURCE

for i in $*
do
	if [ "$i" == "-nocallback" ]
	then
		CALLBACK="-nocallback"
		continue
   	fi
	if [[ "$DIR" == ""  && "$i" != "-nocallback"  && "$i" != *".scl5" ]];
    then
	    DIR=$i
	fi
done

# Unique naming for all declarations
#echo "Naming declarations -> "$filename"_declarations.scl5"
txl SCL5/$1 TXL/UID_declarations.txl > SCL5/"$filename"_declarations.scl5

# Unique naming for all References
#echo "Naming references -> "$filename"_UID.scl5"
txl SCL5/"$filename"_declarations.scl5 TXL/UID_references.txl -idchars '$' > SCL5/"$filename"_UID.scl5 # -idchars '$'

# Checking imports of file
#txl "$filename"_UID.scl5 TXL/checkImports.txl > /dev/null

# Generate C header file for scl5 file
#echo "Creating header file -> "$filename"_Generated.h.unsorted"
txl SCL5/"$filename"_UID.scl5 TXL/generateHeader.txl -idchars '$' > SCL5/"$filename"_Generated.h.unsorted

# Generate C header sorted file 
#echo "Creating header file -> "$filename"_Generated.h"
txl SCL5/"$filename"_Generated.h.unsorted TXL/sortHeader.Txl -idchars '$' > SOURCE/"$filename"_Generated.h

# Generate annotated scl5 file for source optimizations
#echo "Creating annotated scl5 -> "$filename"_UID_annotated.scl5"
txl SCL5/"$filename"_UID.scl5 TXL/generateOptimizations.txl -idchars '$' > SCL5/"$filename"_UID_annotated.scl5

#echo "Creating source file -> "$filename"_Generated.c"
txl -s 120 SCL5/"$filename"_UID_annotated.scl5 TXL/generateSource.txl -idchars '$' - $CALLBACK > SOURCE/"$filename"_Generated.c

#sleep 1
#mv "$filename"_Generated.c testingGenerator/

if [[ "$DIR" != "" ]]; then
	cp SOURCE/"$filename"_Generated.h SOURCE/"$filename"_Generated.c $DIR
fi

# Check imports in a module against table of exports from all module
#txl renamedFULL.scl5 checkImports.txl -idchars '$'

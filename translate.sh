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

DIR=""
CALLBACK=""

if [[ "$1" == "-h" || $# > 2 ]]; then
	printf "Usage:\t./translate.sh [DIRECTORY] [-nocallback]\n"
	printf "\targuments are optional"
	echo "DIRECTORY is an additional directory to move the output files to"
	echo "This script will always copy generated files into ../testingGenerator"
	echo "-nocallback tells the parser generator whether to include callback generation"
	exit 1
fi

for i in $*
do
	if [ "$i" == "-nocallback" ]
	then
		CALLBACK=$i
		continue
   	fi
	if [[ "$DIR" == ""  && "$i" != "-nocallback"  && "$i" != "*.scl5" ]];
    then
	    DIR=$i
	fi
done

./UID.sh RTPS.scl5 $DIR $CALLBACK
./UID.sh IGMP.scl5 $DIR $CALLBACK
./UID.sh NTP.scl5 $DIR $CALLBACK
./UID.sh ARP.scl5 $DIR $CALLBACK

# UDP
./UID.sh UDP.scl5 $DIR $CALLBACK
./IMPORTS.sh UDP.scl5

# echo $CALLBACK
# echo $DIR
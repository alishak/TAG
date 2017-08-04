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

# Todo Take file inputs as arguments
echo "Transforming $1". . .

if [[ "$1" == "" || "$1" == "-h" ]]; then
	echo "Usage: IMPORTS.sh FILENAME"
	exit 1
fi

filename=$(echo $1 | cut -d '.' -f 1)
extension=$(echo $1 | cut -d '.' -f 2)

# Unique naming for all declarations
#echo "Naming declarations -> "$filename"_declarations.scl5"
#txl $1 UID/UID_declarations.txl > "$filename"_declarations.scl5

# Unique naming for all References
#echo "Naming references -> "$filename"_UID.scl5"
#txl "$filename"_declarations.scl5 UID/UID_references.txl -idchars '$' > "$filename"_UID.scl5 # -idchars '$'

# Checking imports of file
txl SCL5/"$filename"_UID_annotated.scl5 TXL/checkImports.txl > /dev/null

# Older parts of script
#txl FULL.scl5 UID_references.txl -idchars '$' > renamedFULL.scl5
#txl MAIN_FULL.scl5 UID_references.txl -idchars '$' > MAIN_renamedFULL.scl5

# Check imports in a module against table of exports from all module
#txl renamedFULL.scl5 checkImports.txl -idchars '$'
#txl RTPS.scl5 UID_declarations.txl > FULL.scl5
#txl MAIN.scl5 UID_declarations.txl > MAIN_FULL.scl5
#cat FULL.scl5

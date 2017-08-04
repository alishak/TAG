%  BSD 3-Clause License
%  
%  Copyright (c) 2017, Ali ElShakankiry
%  All rights reserved.
%  
%  Redistribution and use in source and binary forms, with or without
%  modification, are permitted provided that the following conditions are met:
%  
%  * Redistributions of source code must retain the above copyright notice, this
%    list of conditions and the following disclaimer.
%  
%  * Redistributions in binary form must reproduce the above copyright notice,
%    this list of conditions and the following disclaimer in the documentation
%    and/or other materials provided with the distribution.
%  
%  * Neither the name of the copyright holder nor the names of its
%    contributors may be used to endorse or promote products derived from
%    this software without specific prior written permission.
%  
%  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
%  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
%  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
%  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
%  SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
%  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
%  OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
%  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

include "../GRM/c.grm"

define program
	[cprogram]
end define

rule main
	replace [program]
		P[program]
	construct newP [program]
		P [sortStructs]
	deconstruct not P
		newP
	by
		newP
end rule

rule sortStructs
	skipping [declaration]
	%skipping [repeat function_definition_or_declaration]
	replace $ [repeat function_definition_or_declaration]
		'typedef aStruct [struct_or_union_specifier] ';
		Rest [repeat function_definition_or_declaration]
	deconstruct * [struct_or_union_body] aStruct
		Body [struct_or_union_body]
	construct NestedNames [repeat upperlowerid]
		_ [^ Body]
	where 
		Rest [containsStructDef each NestedNames]
	by
		Rest [insertStruct1 NestedNames aStruct]
				[insertStruct2 NestedNames aStruct]
end rule

function containsStructDef Name [upperlowerid]
	match * [decl_qualifier_or_type_specifier]
		struct Body [struct_or_union_body] Name
end function

function insertStruct1 NestedNames [repeat upperlowerid] aStruct [struct_or_union_specifier]
	replace [repeat function_definition_or_declaration]
		A [function_definition_or_declaration]
		Rest [repeat function_definition_or_declaration]
	where not 
		Rest [containsStructDef each NestedNames]
	by
		A
		typedef aStruct ';
		Rest
end function

function insertStruct2 NestedNames[repeat upperlowerid] aStruct [struct_or_union_specifier]
	replace [repeat function_definition_or_declaration]
		A [function_definition_or_declaration]
		Rest [repeat function_definition_or_declaration]
	where  
		Rest [containsStructDef each NestedNames]
	by
		A
		Rest [insertStruct1 NestedNames aStruct]
				[insertStruct2 NestedNames aStruct]
end function

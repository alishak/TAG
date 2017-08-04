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

include "../GRM/ASNOne.Grm"

%redefine decl
%	[id] [opt hashID]
%end redefine

%define hashID
%   '# [id]
%end define
redefine construction_assignment_statement
	[decl] '::= [type_decision] [opt scl_additions]				[NL]
end redefine

function main
	replace [program]
		P [program]
	by
		P [findImports]
end function

%renRefFieldTypes
%renRefConstraints

rule findImports
	skipping [module_definition]
	replace $ [module_definition]
		ID [id] 'DEFINITIONS OP [opt tag_default] '::= 'BEGIN
			Exports [opt export_block]
			Imports [opt import_block]
			Body [repeat rule_definition]
		'END
	by
		ID 'DEFINITIONS OP '::= 'BEGIN 
		Exports
		Imports [checkBlock ID]
		Body
		'END
end rule

rule checkBlock ModuleName [id]
	skipping [import_block]
	replace $ [import_block]
		'IMPORTS LIST [list import_list+] ';
	%construct ExportTable [repeat import_list]
	%	_ [read "Exports.txt"]
	by
		'IMPORTS LIST [checkImportLists ModuleName] ';
end rule

function checkImportLists ModuleName [id]
	replace [list import_list+]
		LIST [list decl] 'FROM MOD [id] ', REST [list import_list+]
	by
		LIST  'FROM MOD [checkList ModuleName LIST] ', REST [checkImportLists ModuleName]
end function

function checkList Modulename [id] LIST [list decl]
	replace [id]
		ImportModuleName [id]
	construct File [stringlit]
		_ [+ ImportModuleName] [+ ".exports"]
	construct ExportTable [opt import_list]
		_ [read File]
	deconstruct ExportTable
		EXLIST [list decl] 'FROM MOD [id]
	by
		ImportModuleName [checkEachImport EXLIST Modulename each LIST]

end function

function checkEachImport EXLIST [list decl] ModuleName [id] ImportItem [decl]
	match [id]
		ImportModuleName [id]
	deconstruct not * [list decl] EXLIST
		ImportItem
	deconstruct ImportItem
	    _ [id] '^ ImportItemName [id]

	construct QUIT [stringlit]
		_ [+ "Undefined import  \""]
		[+ ImportItemName]
		[+ "\" from module \""]
		[+ ImportModuleName]
		[+ "\" in module \""]
		[+ ModuleName]
		[+ "\""]
		[print]
		[quit 99]
end function
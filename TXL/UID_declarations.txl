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
%   [id] [opt hashID]
%end redefine

%define hashID
%   '^ [id]
%end define
redefine construction_assignment_statement
	[decl] '::= [type_decision] [opt scl_additions]				[NL]
end redefine

function main
   replace [program]
	  P [program]
   by
	P [noUnderscores]
		[doDecls]
end function

rule noUnderscores
	replace $ [id]
		ID [id]
	construct US [number]
		_ [index ID "_"]
	deconstruct not US
		0
	construct Err [id]
	_ [+ "Warning, \""] [+ US] [+ "\" declaration \""] [+ ID] [+ "\" contains underscore '_' "]
	construct Message [id]
		_	[message Err]
			%[quit 99]
	construct LenS [number]
		_ [# ID]
	construct sub1 [number]
		US[- 1]
	construct sub2 [number]
		US[+ 1]
	construct ID2 [id]
		ID[:sub2 LenS]
	construct NEWID [id]
		ID[:0 sub1] [+ "$"] [+ ID2]
	by
		NEWID
		%ID
end rule

rule doDecls
	replace $ [module_definition]
		ID [id] 'DEFINITIONS TAG [opt tag_default] ::= 'BEGIN
		EX [opt export_block]
		IM [opt import_block]
		RERULE [repeat rule_definition]
		'END
	by
		ID 'DEFINITIONS TAG ::= 'BEGIN
		EX
		IM
		RERULE [renameTypeRule ID]
				[renameValueRule ID]
				[renameConstAssignment ID]	
		'END		
end rule

rule renameValueRule ModName [id]
	skipping [value_rule_definition]
	replace $ [value_rule_definition]
		ID [id] TYPE [type] '::= V [value]
	construct UniqueRuleName [id]
		ID [+ '_ ] [+ ModName]
	by
		UniqueRuleName '^ ID TYPE '::= V	
end rule

rule renameConstAssignment ModName [id]
	skipping [construction_assignment_statement]
	replace $ [construction_assignment_statement]
		ID [id] '::= TYPE [type_decision] OPT [opt scl_additions]
	construct UniqueRuleName [id]
		ID [+ '_ ] [+ ModName]
	by
		UniqueRuleName '^ ID  '::= TYPE OPT
end rule

rule renameTypeRule ModName [id]
	skipping [type_rule_definition]
	replace $ [type_rule_definition]
		ID [id] '::= TYPE [type] ADD [opt scl_additions]
	construct UniqueRuleName [id]
		ID [+ '_ ] [+ ModName]
	by
		UniqueRuleName '^ ID '::= TYPE [renameType UniqueRuleName] ADD 	
end rule

function renameType RuleName [id]
	replace [type]
		T [type]
	by
		T [renameSequenceType RuleName]
			[renameSetType RuleName]
			%
end function

rule renameSequenceType RuleName [id]
	skipping [sequence_type]
	replace $ [sequence_type]
		'SEQUENCE OS [opt size_constraint] '{
			LE [list element_type] OC [opt ',]
		'}
	by
		'SEQUENCE OS '{
			LE [renameElementType RuleName] OC
		'}
end rule

rule renameSetType RuleName [id]
	skipping [set_type]
	replace $ [set_type]
		'SET OS [opt size_constraint] '{
			LE [list element_type]
		'}
	by
		'SET OS '{
			LE [renameElementType RuleName]
		'}
end rule
 
rule renameElementType RuleName [id]
	skipping [element_type]
	replace $ [element_type]
		E [element_type]
	by
		E [renameNamedType RuleName]
end rule

rule renameNamedType RuleName [id]
	skipping [named_type]
	replace $ [named_type]
		ID [id] TYPE [type]
	construct UniqueElementName [id]
		ID [+ '_ ] [+ RuleName] [+ " ^ "] [+ ID]
	by
		UniqueElementName TYPE
end rule

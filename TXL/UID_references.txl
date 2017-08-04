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
%   '^ [id]
%end define

define pair
 [id] [id]
end define

define dot_rp
	'. [referenced_element]
end define

redefine referenced_element
	[referencable_primaries] [repeat dot_rp]
end redefine

redefine construction_assignment_statement
		[decl] '::= [type_decision] [opt scl_additions]				[NL]
end redefine

function main
	replace [program]
		P [program]
	by
		P [renRefUserFieldTypes]
		[createExportTable]
end function

%renRefFieldTypes
%renRefConstraints

rule renRefUserFieldTypes
	skipping [module_definition]
	replace $ [module_definition]
		ID [id] 'DEFINITIONS OP [opt tag_default] '::= 'BEGIN
			Exports [opt export_block]
			Imports [opt import_block]
			Body [repeat rule_definition]
		'END
	construct P [pair]
		E E
	export NameList [repeat pair]
		P
	by
		ID 'DEFINITIONS OP '::= 'BEGIN 
		Exports [renameExports ID]
		Imports [renameImports]
		Body [addDecls]
				[renameUserFieldTypes]
				[renameRefContraint Body]
				[renameConstructionAssignments ID]
		'END
end rule

rule renameExports Id [id]
	skipping [export_block]
	replace $ [export_block]
		'EXPORTS LIST [list decl] ';
	by
		'EXPORTS LIST [renameExportRefs Id] ';
end rule

function renameExportRefs ModuleName [id]
	replace [list decl]
		REF [id] ', REST [list decl]
	by
		REF [+ "_"] [+ ModuleName] '^ REF ', REST [renameExportRefs ModuleName]
end function

rule renameImports
	skipping [import_block]
	replace $ [import_block]
		'IMPORTS DECLS [list decl] 'FROM MODULE [id] ', REST [list import_list] ';
	by
		'IMPORTS DECLS [renameImportRefs MODULE] 'FROM MODULE ', REST [renameImportsList] ';
end rule

rule renameImportsList
	replace $ [list import_list]
		DECLS [list decl] 'FROM MODULE [id] ', REST [list import_list]
	by
		DECLS [renameImportRefs MODULE] 'FROM MODULE ', REST
end rule

function renameImportRefs ModuleName [id]
	replace [list decl]
		REF [id] ', REST [list decl]
	by
		REF [+"_"] [+ ModuleName] '^ REF ', REST [renameImportRefs ModuleName]
end function

rule addDecls
	skipping [rule_definition]
	replace $ [rule_definition]
		RD [rule_definition]
	by
		RD [addTypeRuleDef]
			[addConstructionAssignment]
end rule

function  addTypeRuleDef
	replace [rule_definition]
		First [id] '^ Short [id] '::= TP [type] OP [opt scl_additions]
	import NameList [repeat pair]
	construct check [repeat pair]
		NameList [checkIfPresent Short]
	construct newpair [pair]
		First Short
	export NameList
		NameList [. newpair]
	by
		First '^ Short '::= TP OP
end function

function  addConstructionAssignment
	replace [rule_definition]
		First [id] '^ Short [id] '::= TP [type_decision]
	import NameList [repeat pair]
	construct check [repeat pair]
		NameList [checkIfPresent Short]
	construct newpair [pair]
		First Short
	export NameList
		NameList [. newpair]
	by
		First '^ Short '::= TP
end function

function checkIfPresent ID [id]
	match [repeat pair]
		RE [repeat pair]
	deconstruct not * [pair] RE
		First [id] ID
end function

rule renameUserFieldTypes
	import NameList [repeat pair]
	skipping [named_type]
	replace $ [named_type]
		DECL [decl] T [type]
	by
		DECL T [replaceSizeType NameList]
			[replaceSetOfType NameList]
			% [replaceSequenseof Namelist]
end rule

function replaceSizeType NL [repeat pair]
	replace [type]
		ID [id] SC [size_constraint] OP [opt endian] OPSL [opt slack]
	deconstruct * [pair] NL
		First [id] ID
	by
		First SC OP OPSL
end function

function replaceSetOfType NL [repeat pair]
	replace [type]
		'SET 'OF ID [id] SC [size_constraint]
	deconstruct * [pair] NL
		First [id] ID
	by
		'SET 'OF First SC
end function

%%%%%%%%%%%%%%%%%%%%%%% NOTE: Still need to do and test SET type for doSET

%%% Back  & Front Constraint renaming 
rule renameRefContraint Rules [repeat rule_definition]
	skipping [type_rule_definition]
	replace $ [type_rule_definition]
		R [type_rule_definition]
	by 
		R [doSeq Rules]
			%[doSet Rules]
end rule

rule doSeq Rules [repeat rule_definition]
	skipping [type_rule_definition]
	replace $ [type_rule_definition]
		D [decl] '::= 'SEQUENCE OS [opt size_constraint] '{
			EL [list element_type] OP [opt ',]
		'} ENC [opt encoding_grammar_indicator] SZ [opt size_markers_block]
		'<transfer>
		RETR [repeat transfer_statement]
		'</transfer>
		CST [opt constraints_block]
	by
		D '::= 'SEQUENCE OS '{
			EL OP
		'} ENC SZ 
		'<transfer>
		RETR [checkBackConstraint EL Rules D] [checkForwardConstraint EL Rules D]
		'</transfer>
		CST
end rule

rule checkBackConstraint Elist [list element_type] Rules [repeat rule_definition] RefName [decl]
	%skipping [back_block]
	replace $ [back_block]
		%'Back '{ OR [referenced_element] OP [opt equality_op_relational_expression] '}
		'Back '{ OR [or_expression] '}
	deconstruct not OR
		'GLOBAL '( RE [referenced_element] ') '== REST [relational_expression] 
	by
		%'Back '{ RF [renameTransferStatement Elist Rules] [renameTransferFromRuleDef Elist Rules] [checkForUndefinedStats RefName] OP '}
		'Back '{ OR [renameTransferStatement Elist Rules] [renameTransferFromRuleDef Elist Rules][checkForUndefinedStats RefName] '}
end rule

rule checkForwardConstraint Elist [list element_type] Rules [repeat rule_definition] RefName [decl]
	%skipping [forward_block]
	replace $ [forward_block]
		'Forward '{ OR [or_expression] '}
	by
		'Forward '{ OR [renameTransferStatement Elist Rules] [renameTransferFromRuleDef Elist Rules] '}
end rule

rule renameTransferStatement Elist [list element_type] Rules [repeat rule_definition]
	skipping [referenced_element]
	replace $ [referenced_element]
		SHORT [id] REST [repeat dot_rp]
	deconstruct * [named_type]  Elist
		FULL [id] '^ SHORT TYPE [type]
	by
		FULL REST [recurseRename TYPE Rules]
end rule

function recurseRename Type [type] Rules [repeat rule_definition]
	replace [repeat dot_rp]
		'. ID [id] REST [repeat dot_rp]
	deconstruct Type
		RTYPE [id] _ [size_constraint]
	deconstruct * [rule_definition] Rules
		RTYPE '^ Short [id] '::= 'SEQUENCE _ [opt size_constraint] '{
			EL [list element_type] _ [opt ',]
		'} _ [opt scl_additions]
	deconstruct * [named_type] EL
		FULL [id] '^ ID TYPE2 [type]
	by
		'. FULL REST [recurseRename TYPE2 Rules]
end function

rule renameTransferFromRuleDef Elist [list element_type] Rules [repeat rule_definition]
	skipping [referenced_element]
	replace $ [referenced_element]
		SHORT [id] REST [repeat dot_rp]
	deconstruct * [rule_definition] Rules
		FULL [id] '^ SHORT '::= 'SEQUENCE _ [opt size_constraint] '{
			EL [list element_type] _ [opt ',]
		'} _ [opt scl_additions]
	by
		FULL REST
end rule

% Possible issue - what if the first id has been renamed ( with underscores )
% and additional ids have not been renamed? Need to check for that
rule checkForUndefinedStats RefName [decl]
	skipping [referenced_element]
	replace $ [referenced_element]
		ID [id] REST [repeat dot_rp]
	construct US [number]
		_ [index ID "_"]
	deconstruct US
		0
	deconstruct RefName
		LONG [id] '^ SHORT [id]
 	construct err [id]
		_ [+ "Undefined statement reference \""]
		[+ ID]
		[+ "\" in type rule \""]
		[+ LONG]
		[+ "\""]
		[print]
		%[quit 99]
	by
		ID REST
end rule

rule renameConstructionAssignments ModName [id]
	skipping [construction_assignment_statement]
	replace $ [construction_assignment_statement]
		CA [construction_assignment_statement]
	by
		CA [addConstDef ModName]
			%[addConstructionAssignment]
end rule

function  addConstDef ModName [id]
	replace [construction_assignment_statement]
		First [id] '^ Short [id] '::= TD [type_decision] OPT [opt scl_additions]
	by
		First '^ Short '::= TD [renConst ModName] OPT
end function

%% NOTE: type decisions that have a '.' are considered part of an import's module, and so are renamed
%% accordingly in renTRImports

rule renConst ModName [id]
	skipping [type_decision]
	replace $ [type_decision]
		'( TR [type_reference] ALT [repeat alternative_decision]')
	by
		'( TR [renTR ModName] [renTRImports] ALT [recurseRenConst ModName] [recurseRenConstImports] ')
end rule

function renTR ModName [id]
	replace [type_reference]
		ID [id]
	construct NEW [id]
		ID [+ "_"] [+ ModName]
	by
		NEW
end function

function recurseRenConst ModName [id]
	replace [repeat alternative_decision]
		'| ID [id] REST [repeat alternative_decision]
	construct NEW [id]
		ID [+ "_"] [+ ModName]
	by
		'| NEW REST [recurseRenConst ModName]
end function

function renTRImports
	replace [type_reference]
		ID [id] OP [dotID]
	deconstruct OP
		'. TR [id]
	construct IMPORTID [id]
		TR [+ "_"] [+ ID]
	by
		ID '. IMPORTID
end function

function recurseRenConstImports
	replace [repeat alternative_decision]
		'| ID [id] OP [dotID] REST [repeat alternative_decision]
	deconstruct OP
		'. TR [id]
	construct IMPORTID [id]
		TR [+ "_"] [+ ID]
	by
		'| ID '. IMPORTID REST [recurseRenConstImports]
end function


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Creating table of exports to check against imports in other modules.
rule createExportTable
	skipping [module_definition]
	replace $ [module_definition]
		ID [id] 'DEFINITIONS OP [opt tag_default] '::= 'BEGIN
			Exports [opt export_block]
			Imports [opt import_block]
			Body [repeat rule_definition]
		'END
	%construct Table [program]
	%	P1 [checkExports ID]
	by
		ID 'DEFINITIONS OP '::= 'BEGIN
			Exports [checkExports ID]
			Imports 
			Body
		'END
end rule

rule checkExports ModuleName [id]
	skipping [export_block]
	replace $ [export_block]
		'EXPORTS DECL [list decl] '; 
	by
		'EXPORTS DECL [checkExport ModuleName] ';
end rule

function checkExport ModuleName [id]
	replace [list decl]
		LIST [list decl]
	construct ExportTable [import_list]
		LIST 'FROM ModuleName 
	construct outputFile [stringlit]
		_[+ ModuleName] [+ ".exports"]
	construct output [import_list]
		ExportTable [write outputFile]
	by
		LIST
end function

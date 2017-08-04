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

define optimizable
	'@ 'optimizable
end define

define global
	'@ 'GLOBAL
end define

define at
	'@
end define

define annotation
	%size    %value
	[opt at] [opt number] [opt number] [opt global] [opt optimizable] 
end define

redefine type_rule_definition
	[decl] [opt annotation] '::= [type] [opt scl_additions]
end redefine

redefine type_reference
	[id] [opt dotID] [opt annotation]
end redefine

define dot_rp
	'. [referenced_element]
end define

redefine referenced_element
	[referencable_primaries] [repeat dot_rp]
end redefine

redefine decl
    [id] [opt hatID] [opt global] [opt optimizable] %[opt annotation]
end redefine

redefine element_type
   [named_type] [opt position_value]
   | [named_type] 'OPTIONAL [opt position_value]
   |	 [named_type] 'DEFAULT [value] 
   | 	 [id]'COMPONENTS 'OF [id] 
end redefine

define position_value
  'POS
end define

redefine construction_assignment_statement
	[decl] '::= [type_decision] [opt scl_additions]				[NL]
end redefine

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Main program

function main
	replace [program]
		P [program]
	%construct AuxStructs [repeat rule_definition]
	%export AuxStructs 
	construct TPRULES [repeat type_rule_definition]
		_ [^ P]
	by
		P [annotateTypeRules TPRULES] 
			[checkforOptimization]
end function

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Annotate Type Rules and Type Decisions
rule annotateTypeRules typeRules [repeat type_rule_definition]
	replace $ [type_rule_definition]
		TRD [type_rule_definition]
	by
		TRD [annotatePOS]
			[getSizeAndValue typeRules]
			
end rule

% IMPORTANT: Need to extract type_rule_definitions again, because they are now annotated, cannot pass 
% the originally extracted type_rule_definitions from main function.
function checkforOptimization
	replace [program]
		P [program]
	construct TPRULES [repeat type_rule_definition]
		_ [^ P]
	by
		P [annotateTypeDecisionsHelper TPRULES]
			[checkOptimizable TPRULES]
			[fillTypeRefs]
			[checkGlobalOptimization]
			[ensureGlobalSizeValues TPRULES]

end function

rule annotateTypeDecisionsHelper typeRules [repeat type_rule_definition]
	replace $ [construction_assignment_statement]
		LONG [id] '^ SHORT [id] '::= '( TR [type_reference] RTR [repeat alternative_decision] ') OP [opt scl_additions]
	by
		LONG '^ SHORT '::= '( TR [copyAnnotation typeRules] RTR [copyAnnotationAltDecision typeRules] ') OP
end rule


% Ensure there is a global constraint to generate optimized global type decisions
rule checkGlobalOptimization
	replace $ [construction_assignment_statement]
		LONG [id] '^ SHORT [id] '@ 'GLOBAL '@ 'optimizable '::= '( TR [type_reference] RTR [repeat alternative_decision] ') OP [opt scl_additions]
	deconstruct not * [transfer_statement] OP
		'Back '{ 'GLOBAL '( RE [referenced_element] ') '}
	construct errmsg [stringlit]
		_ [+ "WARNING: Type decision is globally optimizable without reference to global variable constraint in transfer block! "]
		[+ "Check "] [+ LONG] [+ " type decision and add GLOBAL constraint."]
		[print]
	by
		LONG '^ SHORT '@ 'optimizable '::= '( TR RTR ') OP
end rule

rule ensureGlobalSizeValues TPRULES [repeat type_rule_definition]
	replace $ [construction_assignment_statement]
		LONG [id] '^ SHORT [id] '@ 'GLOBAL '@ 'optimizable '::= '( TR [type_reference] RTR [repeat alternative_decision] ') OP [opt scl_additions]
	construct newTR [type_reference]
		TR [checkTR TPRULES]
	construct newRTR [repeat alternative_decision]
		RTR [checkRTR TPRULES]
	%construct fixTypeRules
	%	[revertOptimizableTypeRules]
	by
		LONG '^ SHORT '@ 'GLOBAL '@ 'optimizable '::= '( newTR newRTR ') OP  %'@ 'optimizable
end rule

function checkTR TPRULES [repeat type_rule_definition]
	replace [type_reference]
		ID [id] OPID [opt dotID] OPANN [opt annotation]
	deconstruct * [type_rule_definition] TPRULES
		ID '^ SHORT [id] ANN [opt annotation] '::= 'SEQUENCE OS [opt size_constraint] '{
			LE [list element_type] OC [opt ',]
		'} ENC [opt encoding_grammar_indicator] SZ [opt size_markers_block]
		'<transfer>
		RETR [repeat transfer_statement]
		'</transfer>
		CST [opt constraints_block]
	construct newANN [opt annotation]
		OPANN [getGlobalConstraints LE RETR]  [addGlobalAnnotation RETR]
	by
		ID OPID newANN
end function

rule checkRTR TPRULES [repeat type_rule_definition]
	replace $ [alternative_decision]
		'| ID [id] OPID [opt dotID] OPANN [opt annotation]
	deconstruct * [type_rule_definition] TPRULES
		ID '^ SHORT [id] ANN [opt annotation] '::= 'SEQUENCE OS [opt size_constraint] '{
			LE [list element_type] OC [opt ',]
		'} ENC [opt encoding_grammar_indicator] SZ [opt size_markers_block]
		'<transfer>
		RETR [repeat transfer_statement]
		'</transfer>
		CST [opt constraints_block]
	construct newANN [opt annotation]
		OPANN [getGlobalConstraints LE RETR]  [addGlobalAnnotation RETR]
	by
		'| ID OPID newANN
end rule

% NOTE: not dealing with opt dotID type_reference (imported type references)
function copyAnnotation typeRules [repeat type_rule_definition]
	replace [type_reference]
		TR [type_reference]
	deconstruct TR
		ID [id] OP [opt dotID]
	deconstruct * [type_rule_definition] typeRules
		ID '^ SHORT [id] ANN [opt annotation] '::= 'SEQUENCE OS [opt size_constraint] '{
			LE [list element_type] OC [opt ',]
		'} ENC [opt encoding_grammar_indicator] SZ [opt size_markers_block]
		'<transfer>
		RETR [repeat transfer_statement]
		'</transfer>
		CST [opt constraints_block]
	by
		ID OP ANN
end function

rule copyAnnotationAltDecision typeRules [repeat type_rule_definition]
	replace $ [alternative_decision]
		'| TR [type_reference]
	by
		'| TR [copyAnnotation typeRules]
end rule

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function annotatePOS
	replace [type_rule_definition]
		ID [id] '^ SHORT [id] '::= 'SEQUENCE OS [opt size_constraint] '{
			LE [list element_type] OC [opt ',]
		'} ENC [opt encoding_grammar_indicator] SZ [opt size_markers_block]
		'<transfer>
		RETR [repeat transfer_statement]
		'</transfer>
		CST [opt constraints_block]
	construct POSConstraints [repeat pos_expression]
		_ [^ RETR]
	construct NEWLE [list element_type]
		LE [checkMatchingPOSConstraint POSConstraints] [checkMatchingPOSConstraintOptional POSConstraints]
	by
		ID '^ SHORT '::= 'SEQUENCE OS '{
			NEWLE OC
		'} ENC SZ
		'<transfer>
		RETR
		'</transfer>
		CST
end function

rule checkMatchingPOSConstraint POSConstraints [repeat pos_expression]
	replace $ [element_type]
		LONG [id] '^ SHORT [id] TP [type]
	deconstruct * [pos_expression] POSConstraints
		'POS '( LONG ')
	by
		LONG '^ SHORT TP 'POS
end rule

rule checkMatchingPOSConstraintOptional POSConstraints [repeat pos_expression]
	replace $ [element_type]
		LONG [id] '^ SHORT [id] TP [type] 'OPTIONAL
	deconstruct * [pos_expression] POSConstraints
		'POS '( LONG ')
	by
		LONG '^ SHORT TP 'OPTIONAL 'POS
end rule

function getSizeAndValue typeRules [repeat type_rule_definition]
	replace [type_rule_definition]
		ID [id] '^ SHORT [id] '::= 'SEQUENCE OS [opt size_constraint] '{
			LE [list element_type] OC [opt ',]
		'} ENC [opt encoding_grammar_indicator] SZ [opt size_markers_block]
		'<transfer>
		RETR [repeat transfer_statement]
		'</transfer>
		CST [opt constraints_block]
	% NOTE: global constraints will be overwritten by any other optimizable element_types
	construct sizeAndValue [opt annotation]
		_ [getBuiltinType LE RETR] [getSizeBasedType typeRules LE RETR] [getGlobalConstraints LE RETR]  [addGlobalAnnotation RETR]
	by
		ID '^ SHORT sizeAndValue '::= 'SEQUENCE OS '{
			LE OC
		'} ENC SZ
		'<transfer>
		RETR
		'</transfer>
		CST
end function 

function getGlobalConstraints LE [list element_type] RETR [repeat transfer_statement]
	deconstruct * [transfer_statement] RETR
		'Back '{ 'GLOBAL '( RE [referenced_element] ') '== NUM [number] '}
	replace [opt annotation]
		A [opt annotation]
	deconstruct not A
		'@ SZ [opt number] VAL [opt number] OPOP [opt optimizable]
	by
		'@ '0 NUM
end function

function addGlobalAnnotation RETR [repeat transfer_statement]
	replace [opt annotation]
		A [opt annotation]
	deconstruct * [transfer_statement] RETR
		'Back '{ 'GLOBAL '( RE [referenced_element] ') '== NUM [number] '}
	construct checkAnn [opt annotation]
		A [addToOptimizableAnn] [addToBlankAnn]
	by
		checkAnn
end function

function addToOptimizableAnn
	replace [opt annotation]
		ANN [opt annotation]
	deconstruct ANN
		'@ size [opt number] value [opt number] OPOP [opt optimizable]
	by
		'@ size value '@ 'GLOBAL OPOP
end function

function addToBlankAnn
	replace [opt annotation]
		ANN [opt annotation]
	deconstruct not ANN
		'@ size [opt number] value [opt number]
	deconstruct not ANN 
		'@ size [opt number] value [opt number] '@ 'GLOBAL
	by
		'@ 'GLOBAL
end function

function getBuiltinType LE [list element_type] RETR [repeat transfer_statement]
	replace [opt annotation]
		A [opt annotation]
	by 
		A [getIntegerType LE RETR] [getStringType LE RETR]
end function

function getIntegerType ListElements [list element_type] Transfers [repeat transfer_statement]
	replace [opt annotation]
		A [opt annotation]
	% Looking for the first field in type_rule_definition
	deconstruct * [element_type] ListElements
		REF [decl] 'INTEGER '( 'SIZE NUM [number] 'BYTES ') OP [opt endian] %', REST [list element_type]
	deconstruct REF
		LONG [id] '^ SHORT [id]
	deconstruct * [transfer_statement] Transfers
		'Back '{ LONG '== VALUE [number] '}
	by
		'@ NUM VALUE
end function

function getStringType ListElements [list element_type] Transfers [repeat transfer_statement]
	replace [opt annotation]
		A [opt annotation]
	% Looking for the first field in type_rule_definition
	deconstruct * [element_type] ListElements
		REF [decl] 'OCTET 'STRING '( 'SIZE NUM [number] 'BYTES ') OPEN [opt endian] OPSL [opt slack] %', REST [list element_type]
	deconstruct REF
		LONG [id] '^ SHORT [id]
	deconstruct * [transfer_statement] Transfers
		'Back '{ LONG '== VALUE [number] '}
	by
		'@ NUM VALUE
end function

function getSizeBasedType typeRules [repeat type_rule_definition] LE [list element_type] RETR [repeat transfer_statement]
	replace [opt annotation]
		A [opt annotation]
	deconstruct * [element_type] LE
		REF [decl] TYPE [id] '( 'SIZE 'DEFINED') OPEN [opt endian] OPSL [opt slack] %', REST [list element_type]
	deconstruct REF
		LONG [id] '^ SHORTID [id]
	deconstruct * [transfer_statement] RETR
		'Back '{ LONG REP [repeat dot_rp] '== VALUE [number] '}
	construct finalSize [number]
		_ [findRecursiveSize typeRules TYPE REP]
	by 
		'@ finalSize VALUE
end function

% This will recursively look for the last field that is in the constraint and find its size
% It will also ensure that it is looking for the corrent field inside of a type_rule_definition
% and not just the first one
function findRecursiveSize typeRules [repeat type_rule_definition] TYPE [id] REP [repeat dot_rp]
	replace [number] 
		N [number]
	deconstruct * [type_rule_definition] typeRules
		TYPE '^ SHORT [id] ANN [opt annotation] '::= 'SEQUENCE OS [opt size_constraint] '{
			LE [list element_type] OC [opt ',]
		'} ADD [opt scl_additions]
	deconstruct REP
		'. RE [id] REST [repeat dot_rp]
	deconstruct * [element_type] LE
		RE '^ SHORTID2 [id] NEWTYPE [type]
	construct currentElement [element_type]
		RE '^ SHORTID2 NEWTYPE
	construct finalElementType [element_type]
		currentElement [findRecursiveSizeHelper typeRules LE REP]
	by
		N [getIntegerSize finalElementType] [getStringSize finalElementType]
end function

function findRecursiveSizeHelper typeRules [repeat type_rule_definition] LE [list element_type] REP [repeat dot_rp] 
	replace [element_type]
		ET [element_type]
	deconstruct REP
		'. RE [id] REST [repeat dot_rp]
	deconstruct * [element_type] LE
		RE '^ SHORT [id] TYPE [id] '( 'SIZE 'DEFINED') OPEN [opt endian] OPSL [opt slack]
	deconstruct * [type_rule_definition] typeRules
		TYPE '^ SHORTID [id] ANN [opt annotation] '::= 'SEQUENCE OS [opt size_constraint] '{
			NEWLE [list element_type] OC [opt ',]
		'} ADD [opt scl_additions]
	construct element [element_type]
		ET [findRecursiveSizeHelper typeRules NEWLE REST]
	deconstruct REST
		'. finalRE [id]
	deconstruct * [element_type] NEWLE
		finalRE '^ SHORTID2 [id] NEWTYPE [builtin_type]
	by
		finalRE '^ SHORTID2 NEWTYPE
end function

function getIntegerSize ListElement [element_type]
	replace [number]
		N [number]
	% Looking for the specified field in type_rule_definition
	deconstruct ListElement
		LONG [id] '^ SHORT [id] 'INTEGER '( 'SIZE NUM [number] 'BYTES ') OP [opt endian] %', REST [list element_type]
	by
		NUM
end function

function getStringSize ListElement [element_type]
	replace [number]
		N [number]
	% Looking for the specified field in type_rule_definition
	deconstruct ListElement
		LONG [id] '^ SHORT [id] OCTET 'STRING '( 'SIZE NUM [number] 'BYTES ') OPEN [opt endian] OPSL [opt slack] %', REST [list element_type]
	by
		NUM
end function

%function changeOriginalNum NUM [number]
%	replace [number]
%		N [number]
%	by
%		NUM
%end function

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Checks if Type decisions are optimizable
% Need to check is all of the sizes of the fields are the SAME and all values are mutually exclusive!
% If these conditions are not satisfied, remove the type_reference that breaks the conditions.
% The original construction_assignment_statement is matched against the new one. If they are equal and 
% nothing was removed, then the construction_assignment_statement is optimizable.

rule checkOptimizable typeRules [repeat type_rule_definition]
	replace $ [construction_assignment_statement]
		LONG [id] '^ SHORT [id] '::= '( TR [type_reference] RTR [repeat alternative_decision]') OPT [opt scl_additions]	
	deconstruct TR
		ID [id] OPTID [opt dotID] '@ SZ [opt number] VL [opt number] OPGLOB [opt global]
	where
		RTR [allHaveAnnotations]
	construct checkedAssignment [construction_assignment_statement]
		LONG '^ SHORT '::= '( TR RTR [removeDuplicates TR] ')
	where
		checkedAssignment [isIdenticalToOriginal LONG SHORT TR RTR]
	by
		LONG '^ SHORT OPGLOB '@ 'optimizable '::= '( TR [annotateOptimizableRef] RTR [annotateRepeatOptimizableRef] ') OPT
end rule

rule allHaveAnnotations
	match $ [alternative_decision]
		'| ID [id] OPTID [opt dotID] '@ SZ [opt number] VL [opt number] OPGLOB [opt global]
end rule

rule removeDuplicates TypeReference [type_reference]
	replace [repeat alternative_decision]
		'| TR [type_reference] REST [repeat alternative_decision]
	deconstruct TR
		ID [id] DTID [opt dotID] '@ SZ [opt number] VL [opt number] OPGLOB [opt global]
	deconstruct TypeReference
		TRID [id] TRDTID [opt dotID] ANN [opt annotation]
	where
		ANN [differentSize SZ] [equalValue VL]
	by
		REST [removeDuplicates TR]
end rule

function differentSize Size [opt number]
	match [opt annotation]
		'@ SZ [opt number] VL [opt number]
	where not
		SZ [= Size]
end function

function equalValue Value [opt number]
	match [opt annotation]
		'@ SZ [opt number] VL [opt number]
	where
		VL [= Value]
end function

function isIdenticalToOriginal Long [id] Short [id] TR [type_reference] RTR [repeat alternative_decision]
	match [construction_assignment_statement]
		CAS [construction_assignment_statement]
	deconstruct CAS
		Long '^ Short '::= '( TR RTR ') OP [opt scl_additions]
end function

function annotateOptimizableRef
	replace [type_reference]
		ID [id] DTID [opt dotID] '@ SZ [opt number] VL [opt number] OPGLOB [opt global] OPT [opt optimizable]
	by
		ID DTID '@ SZ VL OPGLOB '@ 'optimizable
end function

rule annotateRepeatOptimizableRef
	replace $ [repeat alternative_decision]
		'| TR [type_reference] REST [repeat alternative_decision]
	by
		'| TR [annotateOptimizableRef] REST
end rule

function fillTypeRefs
	replace [program]
		P [program]
	construct TPDECS [repeat construction_assignment_statement]
		_ [^ P]
	construct fillRefs [repeat type_reference]
	export fillRefs
	construct checkForRefs [repeat construction_assignment_statement]
		TPDECS [checkForMatch]
	by
		P [annotateTypeRulesOpt]
end function

rule checkForMatch %OPGLOB [opt global]
	replace $ [construction_assignment_statement]
		LONG [id] '^ SHORT [id] OPGLOB [opt global] '@ 'optimizable '::= '( TR [type_reference] RTR [repeat alternative_decision] ') OPT [opt scl_additions]
	import fillRefs [repeat type_reference]
	construct typeRefs [repeat type_reference]
		_ [^ RTR]
	export fillRefs
		fillRefs [. TR] [. typeRefs]
	by
		LONG '^ SHORT OPGLOB '@ 'optimizable '::= '( TR RTR ') OPT
end rule

rule annotateTypeRulesOpt
	replace $ [type_rule_definition]
		LONG [id] '^ SHORT [id] ANN [opt annotation] '::= 'SEQUENCE OS [opt size_constraint] '{
			LE [list element_type] OC [opt ',]
		'} ADD [opt scl_additions]
	%deconstruct * [construction_assignment_statement] TypeDecs
	%	LONG [id] '^ SHORT [id] OPT [opt annotation] '::= '( TR [type_reference] RTR [repeat alternative_decision] ')
	import fillRefs [repeat type_reference]
	deconstruct * [type_reference] fillRefs
		LONG dotID [opt dotID] OPTANN [opt annotation]
	deconstruct ANN
		'@ size [opt number] value [opt number] OPGLOB [opt global]
	construct finalANN [opt annotation]
		'@ size value OPGLOB '@ 'optimizable
	by 
		LONG '^ SHORT finalANN'::= 'SEQUENCE OS '{
			LE OC
		'} ADD	
end rule

%function addOptimizable typeRules [repeat type_rule_definition]
%	match [type_reference]
%		ID [id] dotID [opt dotID] ANN [opt annotation]
%	deconstruct * [type_rule_definition] typeRules
%		ID '^ SHORT [id] ANNOT [opt annotation] '::= 'SEQUENCE OS [opt size_constraint] '{
%			LE [list element_type] OC [opt ',]
%		'} ADD [opt scl_additions]
%	construct optimizableANN [opt annotation]
%		ANNOT [addOPT]
%end function
%
%rule addOptimizableHelper typeRules [repeat type_rule_definition]
%	replace $ [alternative_decision]
%		'| TR [type_reference]
%	by
%		'| TR [addOptimizableHelper typeRules]
%end rule
%
%function addOPT
%	replace [opt annotation]
%		'@ size [number] value [number]
%	by
%		'@ size value '@ 'optimizable
%end function

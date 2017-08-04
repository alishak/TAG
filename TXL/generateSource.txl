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
include "../GRM/c.grm"

redefine type_rule_definition
	% scl5
	[decl] [opt annotation] '::= [type] [opt scl_additions]
	% C
	| [repeat function_definition_or_declaration]
end redefine

redefine construction_assignment_statement
	% scl5
	[decl] [opt optimizable] '::= [type_decision] [opt scl_additions]				[NL]
	% C
	| [repeat function_definition_or_declaration]
end redefine

redefine program
	...
	| [repeat preprocessor]
		[repeat rule_definition]
		[opt preprocessor]
end redefine

define charlit_or_id
    [charlit] | [id]
end define

define optimizable
	'@'optimizable
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

redefine constant
	...
	| 'NULL
end redefine

%redefine cexpression
%	...
%	| [relational_expression]
%end redefine

define derefID
	[SPOFF]'->[id][SPON]
	| [SPOFF]'.[id][SPON]
	%'->[id]
	%| '.[id]
end define

redefine constant
	...
	| [constant] [repeat derefID]'[[id]']
	%| [id] [repeat derefID] '[i']
	| [constant] [repeat derefID]
	%| [id] [repeat derefID]
	| [relational_expression]
end redefine

redefine size_of_expn
	...
	| 'sizeof '( [referenced_element] ')
end redefine

redefine referencable_primaries
	...
	%| [SPOFF][id]'->[id][SPON]
	| [SPOFF][id][repeat derefID][SPON]
end redefine

redefine alternative_decision
	...
	| [repeat alternative_decision]
end redefine

redefine label
	...
	| [EX][SP][SP] 'case [constant_cexpression] ': [IN] [NL] [unlabeled_statement]
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

redefine pos_expression
	...
	| [referenced_element]
end redefine

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Main program

function main
	replace [program]
		P [program]
	deconstruct P
		ID [id] 'DEFINITIONS TAG [opt tag_default] ::= 'BEGIN
		OPT [opt module_body]
		'END
	construct num [number]
		'0
	construct noCallbackArg [number]
		num [checkTXLargs]
	export noCallbackArg
	construct FunctionDeclarations [repeat rule_definition]
	export FunctionDeclarations
	construct AuxFunctions [repeat rule_definition]
	export AuxFunctions
	construct lengthToCheck [number]
		'0
	export lengthToCheck
	%construct freeName  [id]
	construct freeName [id]
		_ [+ "free"] [+ "PDU_"] [+ ID]
	export freeName
	construct TPRULES [repeat type_rule_definition]
		_ [^ P]
	% build list of type decisions as well
	construct TPDECS [repeat construction_assignment_statement]
		_ [^ P]
	construct ALLRULES [repeat rule_definition]
		_ [^ P]
	export ALLRULES
	% Note: The first PDU statement to parse may also be a construction_assignment_statement
	% i.e. igmpv2.scl5
	by
		P [checkGlobalConstraints]
			[createFreeFunctionTypeRule ALLRULES]
			[createFreeFunctionTypeDecision ALLRULES]
			[createParseFunctions ID TPRULES TPDECS] % add type decisions list here 
			[addAuxiliaryData]
end function

function checkTXLargs 
	import TXLargs [repeat stringlit]
	deconstruct * [stringlit] TXLargs
		"-nocallback"
	replace [number]
		num [number]
	by
		'1
end function

rule checkGlobalConstraints
	match [program]
		P [program]
	construct globalConstraints [repeat rule_definition]
	export globalConstraints
	construct checkAllGlobals [program]
		P [checkTypeRules] [checkConstructionAssignments]
end rule

rule checkTypeRules
	match $ [type_rule_definition]
		DECL [decl] OPANN [opt annotation] '::= TP [type] SCLADD [opt scl_additions]
	construct checkAdditions [opt scl_additions]
		SCLADD [checkGlobalAdditions]
end rule

rule checkConstructionAssignments
	match $ [construction_assignment_statement]
		DECL [decl] OPOP [opt optimizable] '::= TD [type_decision] SCLADD [opt scl_additions]
	construct checkAdditions [opt scl_additions]
		SCLADD [checkGlobalAdditions]
end rule

% TODO: Only checking for equality and left hand side global + assuming NUM is a number
% because we check for a value in generateOptimizations. Globals will override any optimizable 
% back constraints.
rule checkGlobalAdditions
	match $ [transfer_statement]
		'Back '{ 'GLOBAL '( RE [referenced_element] ') '== NUM [number] '} %REST [relational_expression] '}
	deconstruct RE
		ID [id]
	construct globalExpression [repeat rule_definition]
		'extern 'int ID ';
	import globalConstraints [repeat rule_definition]
	export globalConstraints
		globalConstraints [. globalExpression]
end rule

function addAuxiliaryData
	replace [program]
		ID [id] 'DEFINITIONS TAG [opt tag_default] ::= 'BEGIN
		EX [opt export_block]
		IM [opt import_block]
		BODY [repeat rule_definition]
		'END
	import FunctionDeclarations [repeat rule_definition]
	construct ModuleHeader [stringlit]
		_ [+ "#include \""] [+ ID] [+ "_Generated"] [+ ".h\""]
	construct ModuleHeaderPRE [opt preprocessor]
		_ [parse ModuleHeader]
	deconstruct ModuleHeaderPRE
		FinalHeaderInclude [preprocessor]
	import AuxFunctions [repeat rule_definition]
	construct Includes [repeat preprocessor]
		'#define BIGENDIAN (0x0)
		'#define LITTLEENDIAN (0x1)
		FinalHeaderInclude
		%'#include "globals.h"
		'#include "pglobals.h"
		'#include "putilities.h"
		%'#include "callback.h"
	construct FinalAuxFunctions [repeat rule_definition]
		AuxFunctions [removeDuplicates AuxFunctions] [checkRest AuxFunctions]
	import globalConstraints [repeat rule_definition]
	by
		Includes
		FunctionDeclarations [removeDuplicates AuxFunctions] [checkRest AuxFunctions] [. globalConstraints] [. BODY] [. FinalAuxFunctions]
end function

function removeDuplicates AuxFunctions [repeat rule_definition]
	replace [repeat rule_definition]
		Rule [rule_definition] REST [repeat rule_definition]
	deconstruct * [rule_definition] REST
		Rule
	by
		REST [removeDuplicates REST] [checkRest REST]
end function

function checkRest AuxFunctions [repeat rule_definition]
	replace [repeat rule_definition]
		Rule [rule_definition] REST [repeat rule_definition]
	deconstruct not * [rule_definition] REST
		Rule
	by
		Rule REST [removeDuplicates REST] [checkRest REST]
end function

% Always begin parsing from PDU
rule createParseFunctions ModName [id] TPRULES [repeat type_rule_definition] TPDECS [repeat construction_assignment_statement] % add list of typ decisions	
	replace $ [rule_definition]
		A [rule_definition]
	by
		A %[doPDUTypeRule ModName TPRULES TPDECS]
			%[doPDUypeDecision ....]
			[doSimpleTypeRule ModName TPRULES TPDECS]
			[doChoiceOptimizedTypeRule ModName TPRULES TPDECS]
			%[doOptimizedPDUTypeDecision ModName TPRULES TPDECS]
			%[doSimplePDUTypeDecision ModName TPRULES TPDECS]
			[doSimpleTypeDecision ModName TPRULES TPDECS]
			[doOptimizedTypeDecision ModName TPRULES TPDECS]
			[doGloballyOptimizedTypeDecision ModName TPRULES TPDECS]
			%[doInlinedTypeRUle...]
			%[doInlinedTypeDecision ....\]
end rule

%rule doPDUTypeRule ModName [id] TPRULES [repeat type_rule_definition] TPDECS [repeat construction_assignment_statement]
%	replace $ [type_rule_definition]
%		LONG [id] '^ PDU ANN [opt annotation]'::= 'SEQUENCE '{
%			LE [list element_type] OC [opt ',]
%		'} OP [opt scl_additions]
%	import FunctionDeclarations [repeat rule_definition]
%	construct pName [id]
%		_ [+ "parse"] [+ ModName] [+ "Packet"]
%	construct pModDecl [rule_definition] %[function_definition_or_declaration]
%		bool pName (PDU * thePDU, struct HeaderInfo * headerinfo, char * 'progname);
%	construct traceDecl [rule_definition]
%		extern 'FILE * 'traceFile;
%	export FunctionDeclarations
%		FunctionDeclarations [. pModDecl] [. traceDecl]
%	construct ALLOCNAME [id]
%		mainpdu
%	construct mainBodySetup [repeat declaration_or_statement] 
%		%//setup position fields in PDU
%		thePDU ->curPos = 0;
%		thePDU ->curBitPos = 0;
%		thePDU ->remaining = thePDU ->len;
%		uint8_t endianness = 'BIGENDIAN;
%	construct mainFunctionBody [repeat declaration_or_statement]
%		_ [functionBody mainBodySetup]
%	construct mainMalloc [repeat declaration_or_statement]
%		_ [malloc LONG ALLOCNAME]
%	%import lengthToCheck [number]
%	%export lengthToCheck
%	%	lengthToCheck [zeroLength]
%	construct initializePointers [repeat declaration_or_statement]
%		_ [initializePointer ALLOCNAME LONG LE each LE]
%	construct parseBySizes [repeat declaration_or_statement]
%		_ [parseBySize ALLOCNAME LONG OP LE each LE ]
%	%NOTE: Need to get actual size from annotated scl5 file
%	%import lengthToCheck [number]
%	%construct lengthCheck [repeat declaration_or_statement]
%	%	_ [lengthCheck ALLOCNAME lengthToCheck]
%	% need to determine constraint callback
%	import freeName [id]
%	construct MOD [id]
%		_ [+ "PDU_"] [+ ModName]
%	construct return [repeat declaration_or_statement]
%		%if(headerinfo != NULL)
%			%constraintcallback. MOD (ALLOCNAME, headerinfo);
%	    %if('traceFile) {
%	    %    for(int i = 0; i < 'mainpdu ->submsgcount; ++i) {
%	    %        fprintf('traceFile, "   %d, ", 'mainpdu ->submsg'['i'].'type);
%	    %    }
%	    %}
%		freeName( ALLOCNAME );
%		return true;
%	by
%		bool pName (PDU * thePDU, struct HeaderInfo * headerinfo, char * 'progname) {
%			mainFunctionBody [. mainMalloc] [. initializePointers] [. parseBySizes] [. return] %[. lengthCheck]
%		}
%end rule
function checkPDUFName SHORT [id] LONG [id]
	replace [id]
		fName [id]
	deconstruct SHORT
		'PDU
	construct index [number]
		_ [index LONG "_"]
	construct startIndex [number]
		index [+ 1]
	construct length [number]
		_ [# LONG]
	construct finalID [id]
		LONG [: startIndex length]	
	construct finalName [id]
		_ [+ "parse"] [+ finalID]
	by
		finalName
end function

rule doSimpleTypeRule ModName [id] TPRULES [repeat type_rule_definition] TPDECS [repeat construction_assignment_statement]
	replace $ [type_rule_definition]
		TypeRule [type_rule_definition]
	deconstruct TypeRule
		LONG [id] '^ SHORT [id] ANN [opt annotation]'::= 'SEQUENCE '{
			LE [list element_type] OC [opt ',]
		'} OP [opt scl_additions]
	import FunctionDeclarations [repeat rule_definition]
	construct funcName [id]
		_ [+ "parse"] [+ SHORT]
	construct pName [id]
		funcName [checkPDUFName SHORT LONG]
	construct MNPDU [id]
		_ [+ "PDU_"] [+ ModName]
	construct pModDecl [rule_definition] %[function_definition_or_declaration]
		bool pName (LONG * LONG[tolower], PDU * thePDU, char * 'progname, uint8_t endianness);
		%bool pName (LONG LONG[tolower], PDU * thePDU, char * 'progname);
	export FunctionDeclarations
		FunctionDeclarations [. pModDecl]
	construct ALLOCNAME [id]
		mainpdu
	construct lower [id]
		LONG[tolower]
	construct mainFunctionBody [repeat declaration_or_statement]
	import lengthToCheck [number]
	export lengthToCheck
		lengthToCheck [zeroLength]
	construct initializePointers [repeat declaration_or_statement]
		_ [initializePointer lower LONG LE each LE]
	construct parseBySizes [repeat declaration_or_statement]
		_ [parseBySize lower LONG OP LE each LE ]
	%NOTE: Need to get actual size from annotated scl5 file
	construct globalCheck [repeat declaration_or_statement]
		_ [addGlobalCheck OP]
	import lengthToCheck 
	construct lengthCheck [repeat declaration_or_statement]
		_ [lengthCheck ALLOCNAME lengthToCheck]
	construct toPass [id]
		_ [+ "&"] [+ LONG]
	construct callback [repeat declaration_or_statement]
		_ [genCallback OP LONG LONG]	
	construct return [repeat declaration_or_statement]
		return true;
	construct optimizedTypeRule [type_rule_definition]
		TypeRule [doChoiceOptimizedTypeRule ModName TPRULES TPDECS]
	construct optimizedTypeRuleDef [repeat function_definition_or_declaration]
		_ [^ optimizedTypeRule]
	by
		%bool pName (MNPDU * mainpdu, PDU * thePDU, char * 'progname) {
		%bool pName (LONG LONG[tolower] , PDU * thePDU, char * 'progname) {
		bool pName (LONG * LONG[tolower] , PDU * thePDU, char * 'progname, uint8_t endianness) {
			mainFunctionBody [. globalCheck] [. lengthCheck] [. initializePointers] [. parseBySizes] [. callback] [. return]
		}
		optimizedTypeRuleDef
end rule
function addGlobalCheck OPSCL [opt scl_additions]
	deconstruct * [transfer_statement] OPSCL
		'Back '{ 'GLOBAL '( ID [id] ') '== REST [relational_expression] '}
	replace [repeat declaration_or_statement]
		Stmts [repeat declaration_or_statement]
	construct globalCheck [repeat declaration_or_statement]
		if(ID != REST) {
			return false;
		}
	by
		Stmts [. globalCheck]
end function

function doChoiceOptimizedTypeRule ModName [id] TPRULES [repeat type_rule_definition] TPDECS [repeat construction_assignment_statement]
	replace [type_rule_definition]
		LONG [id] '^ SHORT [id] '@ size [number] value [number] OPGLOB [opt global] '@ 'optimizable '::= 'SEQUENCE '{
			LE [list element_type] OC [opt ',]
		'} OP [opt scl_additions]
	%deconstruct not OPGLOB
	%	'@ 'GLOBAL
	deconstruct not size
		'0
	import FunctionDeclarations [repeat rule_definition]
	construct funcName [id]
		_ [+ "parse"] [+ SHORT]
	construct pName [id]
		funcName [checkPDUFName SHORT LONG]
	construct MNPDU [id]
		_ [+ "PDU_"] [+ ModName]
	construct getSize [number]
		size [getBits]
	construct typeSize [id]
		_ [+ "uint"] [+ getSize] [+ "_t"]
	construct pModDecl [rule_definition] %[function_definition_or_declaration]
		bool pName [+ "_O"] (LONG * LONG[tolower], PDU * thePDU, char * 'progname, typeSize * 'type, uint8_t endianness);
		%bool pName (LONG LONG[tolower], PDU * thePDU, char * 'progname, typeSize * 'type);
	export FunctionDeclarations
		FunctionDeclarations [. pModDecl]
	construct ALLOCNAME [id]
		mainpdu
	construct lower [id]
		LONG[tolower]
	construct mainFunctionBody [repeat declaration_or_statement]
	import lengthToCheck [number]
	export lengthToCheck
		lengthToCheck [zeroLength]
	construct initializePointers [repeat declaration_or_statement]
		_ [initializePointer lower LONG LE each LE]
	deconstruct LE
		ET [element_type] ', REST [list element_type]
	construct globalCheck [repeat declaration_or_statement]
		_ [addGlobalCheck OP]
	construct addParsedType [repeat declaration_or_statement]
		_ [addType lower LONG ET]
	construct parseBySizes [repeat declaration_or_statement]
		_ [parseBySize lower LONG OP REST each REST ]
	%NOTE: Need to get actual size from annotated scl5 file
	import lengthToCheck 
	construct lengthCheck [repeat declaration_or_statement]
		_ [lengthCheck ALLOCNAME lengthToCheck]
	construct toPass [id]
		_ [+ "&"] [+ LONG]	
	construct callback [repeat declaration_or_statement]
		_ [genCallback OP LONG LONG]
	construct return [repeat declaration_or_statement]
		%if(headerinfo != NULL)
			%constraintcallback.ModName (ALLOCNAME, headerinfo);
		return true;
	by
		%bool pName (MNPDU * mainpdu, PDU * thePDU, char * 'progname) {
		%bool pName (LONG LONG[tolower] , PDU * thePDU, char * 'progname, typeSize * 'type) {
		bool pName [+"_O"] (LONG * LONG[tolower] , PDU * thePDU, char * 'progname, typeSize * 'type, uint8_t endianness) {
			mainFunctionBody [. globalCheck] [. addParsedType] [. lengthCheck] [. initializePointers] [. parseBySizes] [. callback] [. return]
		}
end function

function genCallback OP [opt scl_additions] LONG [id] toPass [id]
	import noCallbackArg [number]
	deconstruct not noCallbackArg
		'1
	replace [repeat declaration_or_statement]
		Stmts [repeat declaration_or_statement]
	deconstruct OP
		GR [opt encoding_grammar_indicator] SZ [opt size_markers_block] 
		TR [opt transfer_rules_block]
		CB [opt constraints_block] 
	deconstruct * [transfer_statement] TR
		'Callback
	construct callName [id]
		_ [+ LONG] [+ "_callback"]
	construct callback [declaration_or_statement]
		callName(toPass[tolower], thePDU);
	by
		Stmts [. callback]
end function

%rule doOptimizedPDUTypeDecision ModName [id] TPRULES [repeat type_rule_definition] TPDECS [repeat construction_assignment_statement]
%	replace $ [construction_assignment_statement]
%		LONG [id] '^ PDU '@ 'optimizable '::= TD [type_decision]
%	construct pName [id]
%		_ [+ "parse"] [+ ModName] [+ "Packet"]
%	construct MNPDU [id]
%		_ [+ "PDU_"] [+ ModName]
%	construct ALLOCNAME [id]
%		mainpdu
%	import FunctionDeclarations [repeat rule_definition]
%	construct pModDecl [rule_definition]
%		bool pName (PDU * thePDU, struct HeaderInfo * headerinfo, char * 'progname);
%	export FunctionDeclarations
%		FunctionDeclarations [. pModDecl]
%	construct mainFunctionBody [repeat declaration_or_statement]
%	deconstruct TD 
%		'( TR [type_reference] RTR [repeat alternative_decision] ')
%	deconstruct TR
%		ID [id] DTID [opt dotID] ANN [opt annotation]
%	deconstruct ANN
%		'@ SZ [number] value [number] '@ 'optimizable
%	construct size [number]
%		SZ [getBits]
%	construct lengthCheck [repeat declaration_or_statement]
%		_ [lengthCheck ALLOCNAME SZ]
%	construct uintDecl [id]
%		_ [+ "uint"] [+ size] [+ "_t"]
%	construct finalSize [id]
%		_ [+ "get"] [+ size]
%	construct mainMalloc [repeat declaration_or_statement]
%		_ [malloc LONG ALLOCNAME]
%	construct mainBodySetup [repeat declaration_or_statement] 
%		%//setup position fields in PDU
%		thePDU ->curPos = 0;
%		thePDU ->curBitPos = 0;
%		thePDU ->remaining = thePDU ->len;
%		uint8_t endianness = 'BIGENDIAN;
%	import freeName [id]
%	construct parseByOptimizedDecisions [repeat declaration_or_statement]
%	%construct parseByOptimizedDecisions [compound_statement]
%		_ [parseByOptimizedDecision ALLOCNAME ALLOCNAME TD ALLOCNAME]
%	construct switchBody [repeat declaration_or_statement]
%	%construct switchBody [compound_statement]
%	%construct getEndianness [id]
%	%	_ [getEndianness ET]
%	construct MOD [id]
%		_ [+ "PDU_"] [+ ModName]
%	construct switchStmt [repeat declaration_or_statement]
%		uintDecl 'type = finalSize (thePDU, endianness);
%		%uintDecl 'type = finalSize (thePDU);
%		switch('type) {
%			switchBody [. parseByOptimizedDecisions]
%			
%			%default:
%			%return false;
%		}
%		freeName( ALLOCNAME );
%		return true;	
%	by
%		bool pName (PDU * thePDU, struct HeaderInfo * headerinfo, char * 'progname) {
%			mainFunctionBody [. mainBodySetup] [. mainMalloc] [. lengthCheck] [. switchStmt]
%		}
%end rule
%
%rule doSimplePDUTypeDecision ModName [id] TPRULES [repeat type_rule_definition] TPDECS [repeat construction_assignment_statement] 
%	replace $ [construction_assignment_statement]
%		LONG [id] '^ PDU '::= TD [type_decision]
%	construct pName [id]
%		_ [+ "parse"] [+ ModName] [+ "Packet"]
%	construct MNPDU [id]
%		_ [+ "PDU_"] [+ ModName]
%	construct ALLOCNAME [id]
%		mainpdu
%	import FunctionDeclarations [repeat rule_definition]
%	construct pModDecl [rule_definition] %[function_definition_or_declaration]
%		bool pName (PDU * thePDU, struct HeaderInfo * headerinfo, char * 'progname);
%	export FunctionDeclarations
%		FunctionDeclarations [. pModDecl]
%	construct mainFunctionBody [repeat declaration_or_statement]
%	construct mainMalloc [repeat declaration_or_statement]
%		_ [malloc LONG ALLOCNAME]	
%	construct mainBodySetup [repeat declaration_or_statement] 
%		%//setup position fields in PDU
%		thePDU ->curPos = 0;
%		thePDU ->curBitPos = 0;
%		thePDU ->remaining = thePDU ->len;
%		uint8_t endianness = 'BIGENDIAN;
%	import freeName [id]
%	construct MOD [id]
%		_ [+ "PDU_"][+ ModName]
%	export MOD
%	construct parseByDecisions [repeat declaration_or_statement]
%		_ [parseByDecisionPDU ALLOCNAME ALLOCNAME TD ALLOCNAME freeName]
%	construct return [repeat declaration_or_statement]
%		freeName( ALLOCNAME );
%		return false;
%	by
%		bool pName (PDU * thePDU, struct HeaderInfo * headerinfo, char * 'progname) {
%			mainFunctionBody [. mainBodySetup] [. mainMalloc] [. parseByDecisions] [. return]
%		}
%end rule

rule doGloballyOptimizedTypeDecision ModName [id] TPRULES [repeat type_rule_definition] TPDECS [repeat construction_assignment_statement]
	replace $ [construction_assignment_statement]
		LONG [id] '^ SHORT [id] '@ 'GLOBAL '@ 'optimizable '::= TD [type_decision] OPT [opt scl_additions]
	deconstruct * [transfer_statement] OPT
		'Back '{ 'GLOBAL '( GLOBID [id] ') '}
	construct funcName [id]
		_ [+ "parse"] [+ SHORT]
	construct pName [id]
		funcName [checkPDUFName SHORT LONG]
	construct MNPDU [id]
		_ [+ "PDU_"] [+ ModName]
	construct ALLOCNAME [id]
		mainpdu
	import FunctionDeclarations [repeat rule_definition]
	construct pModDecl [rule_definition]
		bool pName (LONG * LONG[tolower], PDU * thePDU, char * 'progname, uint8_t endianness);
	export FunctionDeclarations
		FunctionDeclarations [. pModDecl]
	construct mainFunctionBody [repeat declaration_or_statement]
	deconstruct TD 
		'( TR [type_reference] RTR [repeat alternative_decision] ')
	deconstruct TR
		ID [id] DTID [opt dotID] ANN [opt annotation]
	deconstruct ANN
		'@ SZ [number] value [number] '@ GLOBAL
	construct size [number]
		SZ [getBits]
	construct lengthCheck [repeat declaration_or_statement]
		_ [lengthCheck ALLOCNAME SZ]
	construct GLOB [id]
		'GLOBAL
	construct parseByOptimizedDecisions [repeat declaration_or_statement]
	%construct parseByOptimizedDecisions [compound_statement]
		_ [parseByOptimizedDecision ALLOCNAME ModName TD LONG OPT GLOB]
	construct switchBody [repeat declaration_or_statement]
	%construct switchBody [compound_statement]
	%construct getEndianness [id]
	%	_ [getEndianness ET]
	construct switchStmt [repeat declaration_or_statement]
		
		switch(GLOBID) {
			switchBody [. parseByOptimizedDecisions]
			
			%default:
			%return false;
		}
		return true;	
	by
		bool pName (LONG *  LONG[tolower], PDU * thePDU, char * 'progname, uint8_t endianness) {
			mainFunctionBody [. lengthCheck] [. switchStmt]
		}
end rule

rule doOptimizedTypeDecision ModName [id] TPRULES [repeat type_rule_definition] TPDECS [repeat construction_assignment_statement]
	replace $ [construction_assignment_statement]
		LONG [id] '^ SHORT [id] '@ 'optimizable '::= TD [type_decision] OPT [opt scl_additions]
	construct funcName [id]
		_ [+ "parse"] [+ SHORT]
	construct pName [id]
		funcName [checkPDUFName SHORT LONG]
	construct MNPDU [id]
		_ [+ "PDU_"] [+ ModName]
	construct ALLOCNAME [id]
		mainpdu
	import FunctionDeclarations [repeat rule_definition]
	construct pModDecl [rule_definition]
		bool pName (LONG * LONG[tolower], PDU * thePDU, char * 'progname, uint8_t endianness);
	export FunctionDeclarations
		FunctionDeclarations [. pModDecl]
	construct mainFunctionBody [repeat declaration_or_statement]
	deconstruct TD 
		'( TR [type_reference] RTR [repeat alternative_decision] ')
	deconstruct TR
		ID [id] DTID [opt dotID] ANN [opt annotation]
	deconstruct ANN
		'@ SZ [number] value [number] OPGLOB [opt global] '@ 'optimizable
	construct size [number]
		SZ [getBits]
	construct lengthCheck [repeat declaration_or_statement]
		_ [lengthCheck ALLOCNAME SZ]
	construct uintDecl [id]
		_ [+ "uint"] [+ size] [+ "_t"]
	construct finalSize [id]
		_ [+ "get"] [+ size] [+ "_e"]
	construct GLOB [id]
		'NOTGLOBAL
	construct parseByOptimizedDecisions [repeat declaration_or_statement]
	%construct parseByOptimizedDecisions [compound_statement]
		_ [parseByOptimizedDecision ALLOCNAME ModName TD LONG OPT GLOB]
	construct switchBody [repeat declaration_or_statement]
	%construct switchBody [compound_statement]
	%construct getEndianness [id]
	%	_ [getEndianness ET]
	construct switchStmt [repeat declaration_or_statement]
		uintDecl 'type = finalSize (thePDU, endianness);
		%uintDecl 'type = finalSize (thePDU);
		switch('type) {
			switchBody [. parseByOptimizedDecisions]
			
			%default:
			%return false;
		}
		return true;	
	by
		bool pName (LONG *  LONG[tolower], PDU * thePDU, char * 'progname, uint8_t endianness) {
			mainFunctionBody [. lengthCheck] [. switchStmt]
		}
end rule

rule doSimpleTypeDecision ModName [id] TPRULES [repeat type_rule_definition] TPDECS [repeat construction_assignment_statement] 
	replace $ [construction_assignment_statement]
		LONG [id] '^ SHORT [id] '::= TD [type_decision] OPT [opt scl_additions]
	construct funcName [id]
		_ [+ "parse"] [+ SHORT]
	construct pName [id]
		funcName [checkPDUFName SHORT LONG]
	construct MNPDU [id]
		_ [+ "PDU_"] [+ ModName]
	construct ALLOCNAME [id]
		mainpdu
	import FunctionDeclarations [repeat rule_definition]
	construct pModDecl [rule_definition] %[function_definition_or_declaration]
		bool pName (LONG * LONG[tolower], PDU * thePDU, char * 'progname, uint8_t endianness);
	export FunctionDeclarations
		FunctionDeclarations [. pModDecl]
	construct mainFunctionBody [repeat declaration_or_statement]
	construct parseByDecisions [repeat declaration_or_statement]
		_ [parseByDecision ALLOCNAME ModName TD LONG OPT]
	construct return [declaration_or_statement]
		return false;
	by
		bool pName (LONG *  LONG[tolower], PDU * thePDU, char * 'progname, uint8_t endianness) {
			mainFunctionBody [. parseByDecisions] [. return]
		}
end rule

function functionBody body [repeat declaration_or_statement]
	replace [repeat declaration_or_statement]
		I [repeat declaration_or_statement]
	by
		body
end function

function parseByDecision mallocName [id] RuleName [id] TD [type_decision] LONG [id] OP [opt scl_additions]
	replace [repeat declaration_or_statement]
		Stmts [repeat declaration_or_statement]
	deconstruct TD 
		'( TR [type_reference] RTR [repeat alternative_decision] ')
	deconstruct TR
		ID [id] DTID [opt dotID] ANN [opt annotation]
	construct typeToPass [id]
		ID [checkDTID DTID]
	construct index [number]
		_ [index ID "_"]
	construct finalIndex [number]
		index [- 1]
	construct typeToParse [id]
		ID [: 1 finalIndex]
	construct pName [id]
		_ [+ "parse"] [+ typeToParse] %[+ DTID]
	construct IDVAL [id]
		typeToPass [+ "_VAL"]
	construct body [repeat declaration_or_statement]
		LONG[tolower]'->'type = IDVAL;
	construct lowerLong [id]
		LONG[tolower]
	construct lowerTypeToPass [id]
		typeToPass[tolower]
	construct IDTOPASS [id]
		_ [+ "&"] [+ lowerLong] [+ "->ptr."] [+ lowerTypeToPass]		
	construct callback [repeat declaration_or_statement]
		_ [genCallback OP ID IDTOPASS]	
	construct return [declaration_or_statement]
		return true;
	construct firstDecision [repeat declaration_or_statement]
		unsigned 'long pos = thePDU ->curPos;
		unsigned 'long remaining = thePDU ->remaining;
		if( pName ('& LONG[tolower]->ptr'. typeToPass[tolower], thePDU, 'progname, endianness)) {
			body [. callback] [. return]
		} 
	construct typeRefs [repeat type_reference]
		_ [^ RTR]
	by
		Stmts [. firstDecision]
			 [parseSimpleTypeDecision mallocName LONG OP each typeRefs]
			 %[pasrseInlineTypeDeicsionType ET P]
end function

function checkDTID DTID [opt dotID]
	replace [id]
		ID [id]
	deconstruct DTID
		'. ID2 [id]
	by
		ID2
end function

%function parseByDecisionPDU mallocName [id] RuleName [id] TD [type_decision] LONG [id] freeName [id]
%	replace [repeat declaration_or_statement]
%		Stmts [repeat declaration_or_statement]
%	deconstruct TD 
%		'( TR [type_reference] RTR [repeat alternative_decision] ')
%	deconstruct TR
%		ID [id] DTID [opt dotID] ANN [opt annotation]
%	construct index [number]
%		_ [index ID "_"]
%	construct finalIndex [number]
%		index [- 1]
%	construct typeToParse [id]
%		ID [: 1 finalIndex]
%	construct pName [id]
%		_ [+ "parse"] [+ typeToParse] %[+ DTID]
%	construct IDVAL [id]
%		ID [+ "_VAL"]
%	import MOD [id]
%	construct firstDecision [repeat declaration_or_statement]
%		unsigned 'long pos = thePDU ->curPos;
%		unsigned 'long remaining = thePDU ->remaining;
%		if( pName ('& LONG[tolower]->ptr'. ID[tolower], thePDU, 'progname, endianness)) {
%			LONG[tolower]'->'type = IDVAL;
%			%if(headerinfo != NULL)
%			%	constraintcallback.MOD (mallocName, headerinfo);
%			freeName ( mallocName );
%			return true;
%		} 
%	construct typeRefs [repeat type_reference]
%		_ [^ RTR]
%	by
%		Stmts [. firstDecision]
%			 [parseSimpleTypeDecisionPDU mallocName LONG freeName each typeRefs]
%			 %[pasrseInlineTypeDeicsionType ET P]
%end function
%
%function parseSimpleTypeDecisionPDU mallocName [id]  LONG [id] freeName [id] TR [type_reference]
%	replace [repeat declaration_or_statement]
%		Stmts [repeat declaration_or_statement]
%	deconstruct TR
%		ID [id] DTID [opt dotID] ANN [opt annotation]
%	construct index [number]
%		_ [index ID "_"]
%	construct finalIndex [number]
%		index [- 1]
%	construct typeToParse [id]
%		ID [: 1 finalIndex]
%	construct pName [id]
%		_ [+ "parse"] [+ typeToParse] %[+ DTID]
%	construct IDVAL [id]
%		ID [+ "_VAL"]
%	import MOD [id]
%	construct Stmt [repeat declaration_or_statement]
%		thePDU ->curPos = pos;
%		thePDU ->remaining = remaining;
%		if ( pName ('& LONG[tolower]->ptr'. ID[tolower], thePDU, 'progname, endianness)) {
%			LONG[tolower]'->'type = IDVAL;
%			%if(headerinfo != NULL)
%			%	constraintcallback.MOD (mallocName, headerinfo);
%			freeName( mallocName );
%			return true;
%		}
%	by
%		Stmts [. Stmt]
%end function

function parseSimpleTypeDecision mallocName [id]  LONG [id] OP [opt scl_additions] TR [type_reference] 
	replace [repeat declaration_or_statement]
		Stmts [repeat declaration_or_statement]
	deconstruct TR
		ID [id] DTID [opt dotID] ANN [opt annotation]
	construct typeToPass [id]
		ID [checkDTID DTID]
	construct index [number]
		_ [index ID "_"]
	construct finalIndex [number]
		index [- 1]
	construct typeToParse [id]
		ID [: 1 finalIndex]
	construct pName [id]
		_ [+ "parse"] [+ typeToParse] %[+ DTID]
	construct IDVAL [id]
		typeToPass [+ "_VAL"]
	construct body [repeat declaration_or_statement]
		LONG[tolower]'->'type = IDVAL;
	construct lowerLong [id]
		LONG[tolower]
	construct lowerTypeToPass [id]
		typeToPass[tolower]
	construct IDTOPASS [id]
		_ [+ "&"] [+ lowerLong] [+ "->ptr."] [+ lowerTypeToPass]
	construct callback [repeat declaration_or_statement]
		_ [genCallback OP ID IDTOPASS]	
	construct return [declaration_or_statement]
		return true;
	construct Stmt [repeat declaration_or_statement]
		thePDU ->curPos = pos;
		thePDU ->remaining = remaining;
		if ( pName ('& LONG[tolower]->ptr'. typeToPass[tolower], thePDU, 'progname, endianness)) {
			body [. callback] [. return]
		}
	by
		Stmts [. Stmt]
end function

function parseByOptimizedDecision mallocName [id] RuleName [id] TD [type_decision] LONG [id] OP [opt scl_additions] GLOB [id]
	replace [repeat declaration_or_statement]
	%replace [compound_statement]
		%Stmts [compound_statement]
		Stmts [repeat declaration_or_statement]
	deconstruct TD 
		'( TR [type_reference] RTR [repeat alternative_decision] ')
	deconstruct TR
		ID [id] DTID [opt dotID] ANN [opt annotation]
	deconstruct ANN
		'@ SZ [number] value [number] OPGLOB [opt global] OPOP [opt optimizable]
	where
		ANN [checkGLOBAL] [checkOPTIMIZABLE]
	construct size [number]
		SZ [getBits]
	construct index [number]
		_ [index ID "_"]
	construct finalIndex [number]
		index [- 1]
	construct typeToParse [id]
		ID [: 1 finalIndex]
	construct pName [id]
		_ [+ "parse"] [+ typeToParse] [+ "_O"] %[+ DTID]
	construct uintDecl [id]
		_ [+ "uint"] [+ size] [+ "_t"]
	construct finalSize [id]
		_ [+ "get"] [+ size]
	construct IDVAL [id]
		ID [+ "_VAL"]
	construct body [repeat declaration_or_statement]
		LONG[tolower]'->'type = IDVAL;
	construct lowerLong [id]
		LONG[tolower]
	construct lowerTypeToPass [id]
		ID[tolower]
	construct IDTOPASS [id]
		_ [+ "&"] [+ lowerLong] [+ "->ptr."] [+ lowerTypeToPass]		
	construct callback [repeat declaration_or_statement]
		_ [genCallback OP ID IDTOPASS]	
	construct firstDecision [repeat declaration_or_statement]
		_ [checkDecision value body callback OPGLOB typeToParse LONG ID GLOB]
	construct typeRefs [repeat type_reference]
		_ [^ RTR]
	by
		Stmts [. firstDecision]
			[parseOptimizedTypeDecision mallocName LONG OP GLOB each typeRefs]
			 [defaultStatement]
			 %[pasrseInlineTypeDeicsionType ET P]
end function

function checkGLOBAL
	match [opt annotation]
		'@ SZ [number] value [number] '@ 'GLOBAL OPOP [opt optimizable]
end function

function checkOPTIMIZABLE
	match [opt annotation]
		'@ SZ [number] value [number] OPGLOB [opt global] '@ 'optimizable
end function

function checkDecision VAL [number] body [repeat declaration_or_statement] callback [repeat declaration_or_statement] decType [opt global] typeToParse [id] LONG [id] ID [id] GLOB [id]
	replace [repeat declaration_or_statement]
		Stmts [repeat declaration_or_statement]
	construct decision [repeat declaration_or_statement]
		_ [genGlobalDecision VAL body callback decType typeToParse LONG ID GLOB] [genOptimizedDecision VAL body callback decType typeToParse LONG ID GLOB] 
	by
		Stmts [. decision]
end function

function genGlobalDecision VAL [number] body [repeat declaration_or_statement] callback [repeat declaration_or_statement] decType [opt global] typeToParse [id] LONG [id] ID [id] GLOB [id]
	replace [repeat declaration_or_statement]
		Stmts [repeat declaration_or_statement]
	deconstruct GLOB
		'GLOBAL
	construct pName [id]
		_ [+ "parse"] [+ typeToParse] %[+ DTID]	
	construct decision [repeat declaration_or_statement]
			case VAL :
			%if(! pName ('& LONG[tolower]->ptr'. ID[tolower], thePDU, 'progname, &'type, endianness)) {
			%	LONG[tolower]'->'type = IDVAL;
			%	return false;
			%}
			if(pName ('& LONG[tolower]->ptr'. ID[tolower], thePDU, 'progname, endianness)){
				body [. callback]
			} else { 
				return false; 
			}
			break;	
	by
		Stmts [. decision]
end function

function genOptimizedDecision VAL [number] body [repeat declaration_or_statement] callback [repeat declaration_or_statement] decType [opt global] typeToParse [id] LONG [id] ID [id] GLOB [id]
	replace [repeat declaration_or_statement]
		Stmts [repeat declaration_or_statement]
	deconstruct not GLOB
		'GLOBAL	
	construct pName [id]
		_ [+ "parse"] [+ typeToParse] [+ "_O"] %[+ DTID]
	construct decision [repeat declaration_or_statement]
			case VAL :
			%if(! pName ('& LONG[tolower]->ptr'. ID[tolower], thePDU, 'progname, &'type, endianness)) {
			%	LONG[tolower]'->'type = IDVAL;
			%	return false;
			%}
			if(pName ('& LONG[tolower]->ptr'. ID[tolower], thePDU, 'progname, &'type, endianness)){
				body [. callback]
			} else { 
				return false; 
			}
			break;	
	by
		Stmts [. decision]
end function

function parseOptimizedTypeDecision mallocName [id]  LONG [id] OP [opt scl_additions] GLOB [id] TR [type_reference]
	replace [repeat declaration_or_statement]
	%replace [compound_statement]
		%Stmts [compound_statement]
		Stmts [repeat declaration_or_statement]
	deconstruct TR
		ID [id] DTID [opt dotID] ANN [opt annotation]
	%deconstruct ANN
	%	'@ SZ [number] value [number] OPGLOB [opt global] '@ 'optimizable
	deconstruct ANN
		'@ SZ [number] value [number] OPGLOB [opt global] OPOP [opt optimizable]
	where
		ANN [checkGLOBAL] [checkOPTIMIZABLE]
	construct size [number]
		SZ [getBits]
	construct index [number]
		_ [index ID "_"]
	construct finalIndex [number]
		index [- 1]
	construct typeToParse [id]
		ID [: 1 finalIndex]
	construct pName [id]
		_ [+ "parse"] [+ typeToParse] [+ "_O"]%[+ DTID]
	construct uintDecl [id]
		_ [+ "uint"] [+ size] [+ "_t"]
	construct finalSize [id]
		_ [+ "get"] [+ size]
	construct IDVAL [id]
		ID [+ "_VAL"]
	construct body [repeat declaration_or_statement]
		LONG[tolower]'->'type = IDVAL;
	construct lowerLong [id]
		LONG[tolower]
	construct lowerTypeToPass [id]
		ID[tolower]
	construct IDTOPASS [id]
		_ [+ "&"] [+ lowerLong] [+ "->ptr."] [+ lowerTypeToPass]		
	construct callback [repeat declaration_or_statement]
		_ [genCallback OP ID IDTOPASS]	
	%construct Stmt [repeat declaration_or_statement]
	construct decision [repeat declaration_or_statement]
			_ [checkDecision value body callback OPGLOB typeToParse LONG ID GLOB]
	%construct Stmt [compound_statement]
%			case value : 
			%if(! pName ('& LONG[tolower]->ptr'. ID[tolower], thePDU, 'progname, &'type, endianness)) {
			%	LONG[tolower]'->'type = IDVAL;
			%	return false;
			%}
%			if(pName ('& LONG[tolower]->ptr'. ID[tolower], thePDU, 'progname, &'type, endianness)) {
%				body [. callback]
%			} else {	
%				return false;
%			}
%			break;
	by
		Stmts [. decision]
end function

function defaultStatement
	replace [repeat declaration_or_statement]
		Stmts [repeat declaration_or_statement]
	construct default [repeat declaration_or_statement]
		default :
			return false;
	by
		Stmts [. default]
end function

function malloc TYPE [id] NAME [id]
	replace [repeat declaration_or_statement]
		I [repeat declaration_or_statement]
	construct err [stringlit]
		_ [+ "%s: internal malloc error file: %s line: %d \n"]
	by
		TYPE '* NAME = ( TYPE *)malloc(sizeof( TYPE ));
		if( NAME == 'NULL) {
			fprintf(stderr, err ,progname,__FILE__,__LINE__);
			%exit(1);
			return false;
		}
end function

function lengthCheck NAME [id] size [number]
	replace [repeat declaration_or_statement]
		I [repeat declaration_or_statement]
	deconstruct size
		finalSize [number]
	construct err [stringlit]
		_ [+ "%s: incorrect PDU length or type: %s line: %d\n"]
	by
		if(!lengthRemaining(thePDU, finalSize, 'progname)) {
			%fprintf(stderr,"%s: incorrect PDU length or type: %s line: %d\n",progname, __FILE__ , __LINE__);
			%free(NAME);
			%NAME = 'NULL;
			return false;
		}
end function

function initializePointer mallocName [id] RuleName [id] LET [list element_type] ET [element_type]
	replace [repeat declaration_or_statement]
		Stmts [repeat declaration_or_statement]
	by
	   Stmts [initializeSetOfPointer mallocName RuleName LET ET]
				[initializeOptionalPointer mallocName RuleName LET ET]
				[initializeOctetStringSizeConstrainedPointer mallocName RuleName LET ET]
end function

function initializeSetOfPointer mallocName [id] RuleName [id] LET [list element_type] ET [element_type] 
	deconstruct ET 
		LONG [id] '^ SHORT [id] 'SET 'OF TYPE [id] SZ [size_constraint]
	replace [repeat declaration_or_statement]
		Stmts [repeat declaration_or_statement]
	construct initialize [repeat declaration_or_statement]
		mallocName ->SHORT[tolower] = NULL;
	by
	   Stmts [. initialize]
end function

function initializeOptionalPointer mallocName [id] RuleName [id] LET [list element_type] ET [element_type] 
	deconstruct ET 
		LONG [id] '^ SHORT [id] TYPE [type] 'OPTIONAL
	replace [repeat declaration_or_statement]
		Stmts [repeat declaration_or_statement]
	construct initialize [repeat declaration_or_statement]
		mallocName ->SHORT[tolower] = NULL;
	by
	   Stmts [. initialize]
end function

function initializeOctetStringSizeConstrainedPointer mallocName [id] RuleName [id] LET [list element_type] ET [element_type] 
	deconstruct ET 
		LONG [id] '^ SHORT [id] 'OCTET 'STRING '(SIZE 'CONSTRAINED) OPT [opt endian] OPSL [opt slack]
	replace [repeat declaration_or_statement]
		Stmts [repeat declaration_or_statement]
	construct initialize [repeat declaration_or_statement]
		mallocName ->SHORT[tolower] = NULL;
	by
	   Stmts [. initialize]
end function

function parseBySize mallocName [id] RuleName [id] OP [opt scl_additions] LET [list element_type] ET [element_type] 
	replace [repeat declaration_or_statement]
		Stmts [repeat declaration_or_statement]
	by
		Stmts [checkPOSElements mallocName ET]
			 [checkPOSElementsOptional mallocName ET]
			 
			 % Parse after saving required positions
			 %[parseIntegerTypebySizePDU mallocName RuleName ET]
			 [parseIntegerTypebySize mallocName RuleName ET]
			 [parseReal4Type mallocName RuleName ET]
			 [parseReal8Type mallocName RuleName ET]
			 [parseOctetStringTypebySize mallocName RuleName ET]
			 [parseOctetStringTypebyLargeSize mallocName RuleName ET]
			 [parseOctetStringTypeConstrained mallocName RuleName OP LET ET]
			 %[parseUserDefinedTypeMain mallocName ET]
			 %[parseUserDefinedTypeOptionalMain mallocName OP LET ET]
			 %[parseUserDefinedTypeOptionalMainWithNum mallocName OP LET ET]
			 
			 %[checkForwardLengthUserDefined mallocName RuleName OP LET ET] % This is to create a constrainedPDU to pass in
			 [parseUserDefineTypes mallocName RuleName OP LET ET]
			 %[parseUserDefinedType mallocName ET]
			 %[parseUserDefinedTypeOptional mallocName OP LET ET]
			 %[parseUserDefinedTypeOptionalWithNum mallocName OP LET ET]
			 
			 %[parseUserDefinedInlineType ET P]
			 %[parseSimpleTypeDecisionType ET P]
			 %[pasrseInlineTypeDeicsionType ET P]
			 [parseSetOfTypeDecisionTerminateConstrained mallocName RuleName OP ET]
			 [parseSetOfTypeDecisionCardinalityConstrained mallocName RuleName OP LET ET]
			 %[parseSetOfTypeDecisionEndConstrained mallocName RuleName OP ET]
			 [parseSetOfTypeDecision mallocName RuleName OP ET]

			 % IMPORTANT: Have to be the last calls to follow back constraints properly
			 [checkEndiannessConstraints RuleName ET OP]
			 [checkBackConstraints mallocName RuleName ET OP]
end function

function checkPOSElements mallocName [id] ET [element_type]
	replace [repeat declaration_or_statement]
		Stmts [repeat declaration_or_statement]
	deconstruct ET
		LONG [id] '^ SHORT [id] TP [type] 'POS
	construct savePos [declaration_or_statement]
		'unsigned 'long SHORT[tolower][+ "POS"] '= 'thePDU -> 'curPos;
	by
		Stmts [. savePos]
end function

function checkPOSElementsOptional mallocName [id] ET [element_type]
	replace [repeat declaration_or_statement]
		Stmts [repeat declaration_or_statement]
	deconstruct ET
		LONG [id] '^ SHORT [id] TP [type] 'OPTIONAL 'POS
	construct savePos [declaration_or_statement]
		'unsigned 'long SHORT[tolower][+ "POS"] '= 'thePDU -> 'curPos;
	by
		Stmts [. savePos]
end function

function addType mallocName [id] RuleName [id] ET [element_type] 
	replace [repeat declaration_or_statement]
		Stmts [repeat declaration_or_statement]
	by
	   Stmts [parseIntegerTypebySizeKnown mallocName RuleName ET]
			 [parseOctetStringTypebySizeKnown mallocName RuleName ET]
			 %[parseUserDefinedTypeMain mallocName ET]
			 %[parseUserDefinedTypeKnown mallocName ET]
			 %[parseUserDefinedInlineType ET P]
			 %[parseSimpleTypeDecisionType ET P]
			 %[pasrseInlineTypeDeicsionType ET P]
			 %[parseSetOfTypeDecision mallocName RuleName ET]
end function

function zeroLength
	replace [number]
		length [number]
	by
		'0
end function

function addToLength size [number]
	replace [number]
		length [number]
	by
		size [+ length]
end function 

function getEndianness ET [element_type]
	replace [id]
		ID [id]
	construct endian [id]
		'endianness
	by
		endian [checkSizeBasedTypeBIG ET]
					[checkSizeBasedTypeLITTLE ET]
					[checkIntegerTypeBIG ET]
					[checkIntegerTypeLITTLE ET]
					[checkRealTypeBIG ET]
					[checkRealTypeLITTLE ET]
					[checkOctetStringBIG ET]
					[checkOctetStringLITTLE ET]
end function

function checkSizeBasedTypeBIG ET [element_type]
	deconstruct ET
		LONG [id] '^ SHORT [id] TYPE [id] SZ [size_constraint] OP [opt endian] POS [opt position_value]
	deconstruct OP
		'BIGENDIAN
	replace [id]
		ID [id]
	by
		'BIGENDIAN
end function

function checkSizeBasedTypeLITTLE ET [element_type]
	deconstruct ET
		LONG [id] '^ SHORT [id] TYPE [id] SZ [size_constraint] OP [opt endian] POS [opt position_value]
	deconstruct OP
		'LITTLEENDIAN
	replace [id]
		ID [id]
	by
		'LITTLEENDIAN
end function

function checkIntegerTypeBIG ET [element_type]
	deconstruct ET
		LONG [id] '^ SHORT [id] 'INTEGER SZ [size_constraint] OP [opt endian] POS [opt position_value]
	deconstruct OP
		'BIGENDIAN
	replace [id]
		ID [id]
	by
		'BIGENDIAN
end function

function checkIntegerTypeLITTLE ET [element_type]
	deconstruct ET
		LONG [id] '^ SHORT [id] 'INTEGER SZ [size_constraint] OP [opt endian] POS [opt position_value]
	deconstruct OP
		'LITTLEENDIAN
	replace [id]
		ID [id]
	by
		'LITTLEENDIAN
end function

function checkRealTypeBIG ET [element_type]
	deconstruct ET
		LONG [id] '^ SHORT [id] 'REAL SZ [size_constraint] OP [opt endian] POS [opt position_value]
	deconstruct OP
		'BIGENDIAN
	replace [id]
		ID [id]
	by
		'BIGENDIAN
end function

function checkRealTypeLITTLE ET [element_type]
	deconstruct ET
		LONG [id] '^ SHORT [id] 'REAL SZ [size_constraint] OP [opt endian] POS [opt position_value]
	deconstruct OP
		'LITTLEENDIAN
	replace [id]
		ID [id]
	by
		'LITTLEENDIAN
end function

function checkOctetStringBIG ET [element_type]
	deconstruct ET
		LONG [id] '^ SHORT [id] 'OCTET 'STRING SZ [opt size_constraint] OP [opt endian] OPSL [opt slack] POS [opt position_value]
	deconstruct OP
		'BIGENDIAN
	replace [id]
		ID [id]
	by
		'BIGENDIAN
end function

function checkOctetStringLITTLE ET [element_type]
	deconstruct ET
		LONG [id] '^ SHORT [id] 'OCTET 'STRING SZ [opt size_constraint] OP [opt endian] OPSL [opt slack] POS [opt position_value]
	deconstruct OP
		'LITTLEENDIAN
	replace [id]
		ID [id]
	by
		'LITTLEENDIAN
end function

function checkEndiannessConstraints RuleName [id] ET [element_type] OP [opt scl_additions] 
	replace [repeat declaration_or_statement]
		Stmts [repeat declaration_or_statement]
	%construct RTR [repeat transfer_statement]
	%	_ [^ OP]
	by
		Stmts [changeEndianness RuleName ET OP]
				%[checkForwardExists RuleName ET OP]
				%[checkNormalBackConstraints RuleName ET each RTR]
end function

 
function changeEndianness RuleName [id] ET [element_type] OP [opt scl_additions] 
	deconstruct * [transfer_statement] OP
		'Forward '{ 'ENDIANNESS '== ID [id] '& NUM [number] '}
	deconstruct ET
		ID '^ SHORT [id] TYPE [type]
	construct Stmt [declaration_or_statement]
		'endianness = RuleName[tolower] '->SHORT[tolower] '& NUM ;
	replace [repeat declaration_or_statement]
		Stmts [repeat declaration_or_statement]
	by
		Stmts [. Stmt]
end function

function checkBackConstraints mallocName [id] RuleName [id] ET [element_type] OP [opt scl_additions] 
	replace [repeat declaration_or_statement]
		Stmts [repeat declaration_or_statement]
	construct RTR [repeat transfer_statement]
		_ [^ OP]
	by
		Stmts %[checkPDUBackConstraints mallocName RuleName ET each RTR]
				[checkNormalBackConstraints mallocName RuleName ET each RTR]
end function

function checkPDUBackConstraints mallocName [id] RuleName [id] ET [element_type] TR [transfer_statement] 

	deconstruct mallocName
		mainpdu
	deconstruct TR
		'Back '{ OR [or_expression] '} %REF [id] REP [repeat dot_rp] REPOR [repeat or_and_expression] '}
	construct IDS [repeat id]
		_ [^ OR]
	deconstruct IDS
		ID [id] REST [repeat id]
	construct ShortEXP [or_expression]
		OR [createShortExpression]
	deconstruct ET
		ID '^ SHORT [id] TYPE [type]
	construct ShortIDS [repeat id]
		_ [^ ShortEXP]
	deconstruct ShortIDS
		ShortID [id] ShortREST [repeat id]	
	construct finalEXP [or_expression]
		ShortEXP [modifyShortExpression ShortID mallocName]	
	construct err [stringlit]
		_ [+ "%s: back constraint not met! file: %s line: %d \n"]
	construct freeName [id]
		_ [+ "free"] [+ RuleName]
	construct Stmt [repeat declaration_or_statement]
		if(!(finalEXP)) {
			%fprintf(stderr, err, 'progname, __FILE__,__LINE__);
			freeName('mainpdu);
			'mainpdu = NULL;
			return false;
		}
	replace [repeat declaration_or_statement]
		Stmts [repeat declaration_or_statement]
	by
		Stmts [. Stmt]
end function

function checkNormalBackConstraints mallocName [id] RuleName [id] ET [element_type] TR [transfer_statement] 
	deconstruct not mallocName
		mainpdu
	deconstruct TR
		'Back '{ OR [or_expression] '} %REF [id] REP [repeat dot_rp] REPOR [repeat or_and_expression] '}
	construct IDS [repeat id]
		_ [^ OR]
	deconstruct IDS
		ID [id] REST [repeat id]
	deconstruct ET
		ID '^ SHORT [id] TYPE [type]
	construct ShortEXP [or_expression]
		OR [convertStringlitToHex] [createShortExpression]
	construct ShortIDS [repeat id]
		_ [^ ShortEXP]
	deconstruct ShortIDS
		ShortID [id] ShortREST [repeat id]
	construct finalEXP [or_expression]
		ShortEXP [modifyShortExpression ShortID RuleName] %[addDotIDS ShortID]
	construct err [stringlit]
		_ [+ "%s: back constraint not met! file: %s line: %d \n"]
	construct Stmt [repeat declaration_or_statement]
		if(!(finalEXP)) {
			%fprintf(stderr, err, 'progname, __FILE__,__LINE__);
			return false;
		}
	replace [repeat declaration_or_statement]
		Stmts [repeat declaration_or_statement]
	by
		Stmts [. Stmt]
end function

rule createShortExpression
	replace $ [id]
		ID [id]
	construct index [number]
		_ [index ID "_"]
	construct finalIndex [number]
		index [- 1]
	construct finalID [id]
		ID [: 1 finalIndex]
	by 
		finalID[tolower]
end rule

rule modifyShortExpression ShortID [id] RuleName [id]
	replace $ [referencable_primaries]
		ID [referencable_primaries]
	deconstruct ID
		ShortID
	by
		RuleName[tolower]'->ShortID
end rule

function parseIntegerTypebySizePDU mallocName [id] RuleName [id] ET [element_type]
	deconstruct mallocName
		mainpdu
	deconstruct ET
		LONG [id] '^ SHORT [id] 'INTEGER '(SIZE SZ [number] 'BYTES) OP [opt endian] POS [opt position_value]
	construct getSize [number]
		SZ [getBits]
	import lengthToCheck [number]
	export lengthToCheck
		lengthToCheck [addToLength SZ]
	construct size [id]
		_ [+ "get"] [+ getSize] [+ "_e"]
	construct getEndianness [id]
		_ [getEndianness ET]
	construct Stmt [declaration_or_statement]
		%mallocName -> RuleName '. SHORT = size(thePDU);
		%RuleName[tolower] '.SHORT[tolower] = size(thePDU);
		'mainpdu '->SHORT[tolower] = size(thePDU, getEndianness);
	replace [repeat declaration_or_statement]
		Stmts [repeat declaration_or_statement]
	by
		Stmts [. Stmt]
end function

function parseIntegerTypebySize mallocName [id] RuleName [id] ET [element_type]
	deconstruct not mallocName
		mainpdu
	deconstruct ET
		LONG [id] '^ SHORT [id] 'INTEGER '(SIZE SZ [number] 'BYTES) OP [opt endian] POS [opt position_value]
	construct getSize [number]
		SZ [getBits]
	import lengthToCheck [number]
	export lengthToCheck
		lengthToCheck [addToLength SZ]
	construct size [id]
		_ [+ "get"] [+ getSize] [+ "_e"]
	construct getEndianness [id]
		_ [getEndianness ET]
	construct Stmt [declaration_or_statement]
		%mallocName -> RuleName '. SHORT = size(thePDU);
		%RuleName[tolower] '.SHORT[tolower] = size(thePDU);
		RuleName[tolower] '->SHORT[tolower] = size(thePDU, getEndianness);
	replace [repeat declaration_or_statement]
		Stmts [repeat declaration_or_statement]
	by
		Stmts [. Stmt]
end function

function parseIntegerTypebySizeKnown mallocName [id] RuleName [id] ET [element_type]
	deconstruct ET
		LONG [id] '^ SHORT [id] 'INTEGER '(SIZE SZ [number] 'BYTES) OP [opt endian] POS [opt position_value]
	construct Stmt [declaration_or_statement]
		%RuleName[tolower] '.SHORT[tolower] = *'type;
		RuleName[tolower] '->SHORT[tolower] = *'type;
	replace [repeat declaration_or_statement]
		Stmts [repeat declaration_or_statement]
	by
		Stmts [. Stmt]
end function

function parseReal4Type mallocName [id] RuleName [id] ET [element_type]
	deconstruct not mallocName
		mainpdu
	deconstruct ET
		LONG [id] '^ SHORT [id] 'REAL '(SIZE SZ [number] 'BYTES) OP [opt endian] POS [opt position_value]
	construct getSize [number]
		SZ
	where
		SZ [= 4]
	import lengthToCheck [number]
	export lengthToCheck
		lengthToCheck [addToLength SZ]
	construct size [id]
		_ [+ "getReal4"] [+ "_e"]
	construct getEndianness [id]
		_ [getEndianness ET]
	construct Stmt [declaration_or_statement]
		%mallocName -> RuleName '. SHORT = size(thePDU);
		%RuleName[tolower] '.SHORT[tolower] = size(thePDU);
		RuleName[tolower] '->SHORT[tolower] = size(thePDU, getEndianness);
	replace [repeat declaration_or_statement]
		Stmts [repeat declaration_or_statement]
	by
		Stmts [. Stmt]
end function

function parseReal8Type mallocName [id] RuleName [id] ET [element_type]
	deconstruct not mallocName
		mainpdu
	deconstruct ET
		LONG [id] '^ SHORT [id] 'REAL '(SIZE SZ [number] 'BYTES) OP [opt endian] POS [opt position_value]
	construct getSize [number]
		SZ
	where
		SZ [= 8]
	import lengthToCheck [number]
	export lengthToCheck
		lengthToCheck [addToLength SZ]
	construct size [id]
		_ [+ "getReal8"] [+ "_e"]
	construct getEndianness [id]
		_ [getEndianness ET]
	construct Stmt [declaration_or_statement]
		%mallocName -> RuleName '. SHORT = size(thePDU);
		%RuleName[tolower] '.SHORT[tolower] = size(thePDU);
		RuleName[tolower] '->SHORT[tolower] = size(thePDU, getEndianness);
	replace [repeat declaration_or_statement]
		Stmts [repeat declaration_or_statement]
	by
		Stmts [. Stmt]
end function

function parseOctetStringTypebySize mallocName [id] RuleName [id] ET [element_type]
	deconstruct ET
		LONG [id] '^ SHORT [id] 'OCTET 'STRING '(SIZE SZ [number] 'BYTES) OP [opt endian] OPSL [opt slack] POS [opt position_value]
	where
		SZ [<= 8]
	construct getSize [number]
		SZ [getBits]
	import lengthToCheck [number]
	export lengthToCheck
		lengthToCheck [addToLength SZ]
	construct size [id]
		_ [+ "get"] [+ getSize] [+ "_e"]
	construct getEndianness [id]
		_ [getEndianness ET]
	construct Stmt [declaration_or_statement]
		%RuleName[tolower] '.SHORT[tolower] = size(thePDU);
		RuleName[tolower] '->SHORT[tolower] = size(thePDU, getEndianness);
	replace [repeat declaration_or_statement]
		Stmts [repeat declaration_or_statement]
	by
		Stmts [. Stmt]
end function

function parseOctetStringTypebyLargeSize mallocName [id] RuleName [id] ET [element_type]
	deconstruct ET
		LONG [id] '^ SHORT [id] 'OCTET 'STRING '(SIZE SZ [number] 'BYTES) OP [opt endian] OPSL [opt slack] POS [opt position_value]
	where
		SZ [> 8]
	construct getSize [number]
		SZ [getBits]
	import lengthToCheck [number]
	export lengthToCheck
		lengthToCheck [addToLength SZ]
	construct size [id]
		_ [+ "get"] [+ getSize] [+ "_e"]
	construct thePDU [id]
		'thePDU
	construct data [referencable_primaries]
		thePDU [+ "->data"]
	construct curPos [referencable_primaries]
		thePDU [+ "->curPos"]
	construct Stmt [repeat declaration_or_statement]
		%RuleName[tolower] '.SHORT[tolower] = size(thePDU);
		%RuleName[tolower] '->SHORT[tolower] = size(thePDU);
		%memcpy(RuleName[tolower] '->SHORT[tolower], data'[curPos'], getSize);
		memcpy(RuleName[tolower] '->SHORT[tolower], '& data'[curPos'], SZ);
		curPos += SZ;
		%curPos += getSize;

		if( RuleName[tolower] '->SHORT[tolower] '[ SZ '- '1 '] != ''\0'' ) {
			%fprintf(stderr, nullerr,progname, __FILE__ , __LINE__);
			%exit(1);
		}
	replace [repeat declaration_or_statement]
		Stmts [repeat declaration_or_statement]
	by
		Stmts [. Stmt]
end function

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% NOTE:
% This type of Octet string needs to have a Forward constraint on its size
% If it doesn't, we are assuming that the rest of the PDU is part of this
% Octet string and saving the remaining length in a dataLength variable inside
% struct this string belongs to!
% This is currently the case for DATASUBs with serializedDATA in RTPS
function parseOctetStringTypeConstrained mallocName [id] RuleName [id] OP [opt scl_additions] LET [list element_type] ET [element_type]
	deconstruct ET
		LONG [id] '^ SHORT [id] 'OCTET 'STRING '(SIZE 'CONSTRAINED) OPT [opt endian] OPSL [opt slack] POS [opt position_value]
	deconstruct OP
		ENC [opt encoding_grammar_indicator] SZ [opt size_markers_block]
		'<transfer>
		RETR [repeat transfer_statement]
		'</transfer>
		CST [opt constraints_block]
	construct lengthCheck [repeat declaration_or_statement]
	construct convertedExpression [repeat declaration_or_statement]
		lengthCheck [checkLengthToParse LONG RETR RuleName LET]
	replace [repeat declaration_or_statement]
		Stmts [repeat declaration_or_statement]
	by
		Stmts [. convertedExpression]
end function

function checkLengthToParse LONG [id] RETR [repeat transfer_statement] RuleName [id] LET [list element_type]
	replace [repeat declaration_or_statement]
		Stmts [repeat declaration_or_statement]
	deconstruct * [transfer_statement] RETR
		'Forward '{ 'LENGTH ( LONG ) '== REST [relational_expression] '}
	construct RESTConverted [relational_expression]
		REST [convertIDToShort] [checkOptionalLengths LET RuleName] [convertSizeOf] [convertPOS] [convertPDUREMAINING] 
	%construct expression [cexpression]
	construct expression [constant]
		RESTConverted [convertToFullID RuleName]
	construct lengtherr [stringlit]
		_ [+ "%s: incorrect PDU length or type: %s line: %d\n"]
	construct err [stringlit]
		_ [+ "%s: internal malloc error file: %s line: %d \n"]
	construct nullerr [stringlit]
		_ [+ "%s: WARNING, slack is NOT null terminated. file: %s line: %d\n"]
	construct thePDU [id]
		'thePDU
	construct data [referencable_primaries]
		thePDU [+ "->data"]
	construct curPos [referencable_primaries]
		thePDU [+ "->curPos"]
	construct saveLength [repeat declaration_or_statement]
		unsigned 'long remaining = thePDU ->remaining;
	construct lengthCheck [repeat declaration_or_statement]
		if(!lengthRemaining(thePDU, expression , progname)) {
			%fprintf(stderr, lengtherr ,progname,__FILE__,__LINE__);
			return false;
		}
	construct convertExpression [repeat declaration_or_statement]
		RuleName[tolower]->LONG[convertIDToShort] '= (unsigned char *)malloc(sizeof(unsigned char) * ( expression ));
		if( RuleName[tolower]->LONG[convertIDToShort] == 'NULL) {
			fprintf(stderr, err ,progname,__FILE__,__LINE__);
			%exit(1);
			return false;
		}
		RuleName[tolower]->LONG[convertIDToShort][+ "_length"] = expression;
		memcpy( RuleName[tolower]->LONG[convertIDToShort], '& data'[curPos'], expression );
		curPos += expression ;

		if( RuleName[tolower]->LONG[convertIDToShort] '[ expression '- '1 '] != ''\0'' ) {
			%fprintf(stderr, nullerr,progname, __FILE__ , __LINE__);
			%exit(1);
		}
	by
		Stmts [. saveLength][. lengthCheck] [. convertExpression]
end function

rule checkOptionalLengths LET [list element_type] RuleName [id]
	replace $ [unary_expression]
		SZ [unary_expression]
	deconstruct SZ
		'SIZEOF '( ID [id] ')
	deconstruct * [element_type] LET
		LONG [id] '^ ID TYPE [id] '(SIZE 'DEFINED) OPEND [opt endian] OPSL [opt slack] 'OPTIONAL POS [opt position_value]
	import ALLRULES [repeat rule_definition]
	deconstruct * [type_rule_definition] ALLRULES 
		TYPE '^ SHORTID [id] '::= 'SEQUENCE '{
			LE [list element_type] OC [opt ',]
		'} OP [opt scl_additions]
	deconstruct * [element_type] LE
		LONGLE [id] '^ SHORTLE [id] 'SET 'OF TPLE [id] '(SIZE 'CONSTRAINED)
	by
		RuleName[tolower] -> ID -> SHORTLE[+ "length"]
end rule

rule convertIDToShort 
	replace $ [id]
		ID [id]
	construct index [number]
		_ [index ID "_"]
	construct finalIndex [number]
		index [- 1]
	construct typeToParse [id]
		ID [: 1 finalIndex]
	by 
		typeToParse [tolower]
end rule

rule convertSizeOf
	replace $ [size_of_expn]
		'SIZEOF '( RE [referenced_element] ')
	by
		'sizeof '( RE ')
end rule

rule convertPDUREMAINING
	replace $ [primary]
		'PDUREMAINING
	by
		'remaining
end rule

rule convertPOS
	replace $ [pos_expression]
		'POS '( RE [referenced_element] ')
	deconstruct RE
		ID [id]
	by
		ID[+ "POS"]
end rule

rule convertToFullID RuleName [id]
	replace $ [referencable_primaries]
		RP [referencable_primaries]
	deconstruct RP
		ID [id]
	deconstruct not ID
		'remaining
	construct LenID [number]
		_ [# ID]
	construct minusTwo [number]
		LenID [- 2]
	construct LastThree [id]
		ID [: minusTwo LenID]
	deconstruct not LastThree
		'POS
	by
		RuleName[tolower][+ "->"] [+ ID]
end rule

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function parseOctetStringTypebySizeKnown mallocName [id] RuleName [id] ET [element_type]
	deconstruct ET
		LONG [id] '^ SHORT [id] 'OCTET 'STRING '(SIZE SZ [number] 'BYTES)  OP [opt endian] OPSL [opt slack] POS [opt position_value]
	construct Stmt [declaration_or_statement]
		RuleName[tolower] '->SHORT[tolower] = *'type;
	replace [repeat declaration_or_statement]
		Stmts [repeat declaration_or_statement]
	by
		Stmts [. Stmt]
end function

function getBits
	replace [number]
		Num [number]
	construct Eight [number]
		'8
	construct numBits [number]
		Eight [* Num]
	by 
		numBits
end function

function parseUserDefineTypes mallocName [id] RuleName [id] OP [opt scl_additions] LET [list element_type] ET [element_type]
	replace [repeat declaration_or_statement]
		Stmts [repeat declaration_or_statement]
	construct slackModSavePos [repeat declaration_or_statement]		
		_ [checkSlackModSavePos mallocName RuleName ET]
	construct forwardConstraint [repeat declaration_or_statement]
		_ [checkForwardLengthUserDefined mallocName RuleName OP LET ET]
			%[checkForwardLengthUserDefinedOptional mallocName RuleName OP LET ET]
	construct PDUID [id]
		'thePDU
	construct PDU [id]
		PDUID [checkPDU forwardConstraint]
	construct parseDefinedType [repeat declaration_or_statement]
		_ [parseUserDefinedType mallocName ET PDU]
			[parseUserDefinedTypeOptional mallocName OP LET ET PDU]
			[parseUserDefinedTypeOptionalWithNum mallocName OP LET ET PDU]
	construct slacks [repeat declaration_or_statement]
		_ [doSlacks forwardConstraint ET] [doSlackMods ET]
	by
		Stmts [. slackModSavePos] [. forwardConstraint] [. parseDefinedType] [. slacks]
end function

function checkPDU constraint [repeat declaration_or_statement]
	replace [id]
		'thePDU
	deconstruct not constraint
		_ [empty]
	
	construct cPDU [id]
		_ [+ "&constrainedPDU"]
	by
		cPDU
end function

function checkSlackModSavePos mallocName [id] RuleName [id] ET [element_type]
	replace [repeat declaration_or_statement]
		Stmts [repeat declaration_or_statement]
	construct slackMod [repeat declaration_or_statement]
		_ [slackMod mallocName RuleName ET] [slackModOptional mallocName RuleName ET]
	by
		Stmts [. slackMod]
end function

function slackMod mallocName [id] RuleName [id] ET [element_type]
	replace [repeat declaration_or_statement]
		Stmts [repeat declaration_or_statement]
	deconstruct ET
		LONG [id] '^ SHORT [id] TYPE [id] '(SIZE 'DEFINED)  OPEN [opt endian] 'SLACKMOD4 POS [opt position_value]	
	construct pos [id]
		_ [+ "pos"] [+ SHORT]	
	construct savePos [repeat declaration_or_statement]
		'unsigned 'long pos = thePDU ->curPos;
	by
		Stmts [. savePos]
end function

function slackModOptional mallocName [id] RuleName [id] ET [element_type]
	replace [repeat declaration_or_statement]
		Stmts [repeat declaration_or_statement]
	deconstruct ET
		LONG [id] '^ SHORT [id] TYPE [id] '(SIZE 'DEFINED)  OPEN [opt endian] 'SLACKMOD4 'OPTIONAL POS [opt position_value]	
	construct pos [id]
		_ [+ "pos"] [+ SHORT]	
	construct savePos [repeat declaration_or_statement]
		'unsigned 'long pos = thePDU ->curPos;
	by
		Stmts [. savePos]
end function

function generateEndingForwardConstraint constraint [repeat declaration_or_statement] ET [element_type]
	replace [repeat declaration_or_statement]
		Stmts [repeat declaration_or_statement]
	deconstruct not constraint
		_ [empty]
	construct Stmt [repeat declaration_or_statement]
		thePDU ->curPos = constrainedPDU.curPos;
		thePDU ->remaining -= (thePDU ->'curPos - 'pos);
	by
		Stmts [. Stmt]
end function

function doSlacks constraint [repeat declaration_or_statement] ET [element_type]
	replace [repeat declaration_or_statement]
		Stmts [repeat declaration_or_statement]
	deconstruct not constraint
		_ [empty]
	construct slackCheck [repeat declaration_or_statement]
		_ [checkSlack ET] [noSlack ET] [checkSlackOptional ET] [noSlackOptional ET] 
	by
		Stmts [. slackCheck]
end function

function doSlackMods ET [element_type]
	replace [repeat declaration_or_statement]
		Stmts [repeat declaration_or_statement]
	construct slackCheck [repeat declaration_or_statement]
		_ [checkSlackMod ET] [checkSlackModOptional ET] 
	by
		Stmts [. slackCheck]
end function

function checkSlack ET [element_type]
	replace [repeat declaration_or_statement]
		Stmts [repeat declaration_or_statement]
	deconstruct ET
		LONG [id] '^ SHORT [id] TYPE [id] '(SIZE 'DEFINED)  OPEN [opt endian] OPSL [opt slack] POS [opt position_value]	
	deconstruct OPSL
		'SLACK
	construct lengthCheck [repeat declaration_or_statement]
		if(!lengthRemaining(thePDU, (constrainedPDU.len - (constrainedPDU.curPos - 'pos) ) , 'progname)) {
			%fprintf(stderr,"%s: incorrect PDU length or type: %s line: %d\n",progname, __FILE__ , __LINE__);
			return false;
		}	
	construct slack [repeat declaration_or_statement]
		% Skipping slack bytes . . .
		thePDU ->curPos = constrainedPDU.curPos;
		if(!checkSlack(thePDU, constrainedPDU.remaining)) {
			return false;
		}
		%thePDU ->remaining -= (constrainedPDU.len - (thePDU ->'curPos - 'pos) );
	by
		Stmts  [. slack] %[. lengthCheck]
end function

function checkSlackOptional ET [element_type]
	replace [repeat declaration_or_statement]
		Stmts [repeat declaration_or_statement]
	deconstruct ET
		LONG [id] '^ SHORT [id] TYPE [id] '(SIZE 'DEFINED)  OPEN [opt endian] OPSL [opt slack] 'OPTIONAL POS [opt position_value]	
	deconstruct OPSL
		'SLACK
	construct lengthCheck [repeat declaration_or_statement]
		if(!lengthRemaining(thePDU, (constrainedPDU.len - (constrainedPDU.curPos - 'pos) ) , 'progname)) {
			%fprintf(stderr,"%s: incorrect PDU length or type: %s line: %d\n",progname, __FILE__ , __LINE__);
			return false;
		}
	construct slack [repeat declaration_or_statement]
		% Skipping slack bytes . . .
		thePDU ->curPos = constrainedPDU.curPos;
		if(!checkSlack(thePDU, constrainedPDU.remaining)) {
			return false;
		}
		%thePDU ->remaining -= (constrainedPDU.len - (thePDU ->'curPos - 'pos) );
	by
		Stmts [. slack] %[. lengthCheck] 
end function

function noSlack ET [element_type]
	replace [repeat declaration_or_statement]
		Stmts [repeat declaration_or_statement]
	deconstruct ET
		LONG [id] '^ SHORT [id] TYPE [id] '(SIZE 'DEFINED)  OPEN [opt endian] OPSL [opt slack] POS [opt position_value]	
	deconstruct not OPSL
		'SLACK
	deconstruct not OPSL
		'SLACKMOD4
	construct slack [repeat declaration_or_statement]
		% Ensure slack bytes 
		if(constrainedPDU.remaining != 0) {
			return false;
		}
		thePDU ->curPos += constrainedPDU.len;
	construct lengthCheck [repeat declaration_or_statement]
		if(!lengthRemaining(thePDU, constrainedPDU.len , 'progname)) {
			%fprintf(stderr,"%s: incorrect PDU length or type: %s line: %d\n",progname, __FILE__ , __LINE__);
			return false;
		}
		
	by
		Stmts [. slack] %[. lengthCheck]
end function

function noSlackOptional ET [element_type]
	replace [repeat declaration_or_statement]
		Stmts [repeat declaration_or_statement]
	deconstruct ET
		LONG [id] '^ SHORT [id] TYPE [id] '(SIZE 'DEFINED)  OPEN [opt endian] OPSL [opt slack] 'OPTIONAL POS [opt position_value]	
	deconstruct not OPSL
		'SLACK
	deconstruct not OPSL
		'SLACKMOD4
	construct slack [repeat declaration_or_statement]
		% Ensure slack bytes 
		if(constrainedPDU.remaining != 0) {
			return false;
		}
		thePDU ->curPos += constrainedPDU.len;
	construct lengthCheck [repeat declaration_or_statement]
		if(!lengthRemaining(thePDU, constrainedPDU.len , 'progname)) {
			%fprintf(stderr,"%s: incorrect PDU length or type: %s line: %d\n",progname, __FILE__ , __LINE__);
			return false;
		}
	by
		Stmts [. slack] %[. lengthCheck]
end function

function checkSlackMod ET [element_type]
	replace [repeat declaration_or_statement]
		Stmts [repeat declaration_or_statement]
	deconstruct ET
		LONG [id] '^ SHORT [id] TYPE [id] '(SIZE 'DEFINED)  OPEN [opt endian] 'SLACKMOD4 POS [opt position_value]	
	construct pos [id]
		_ [+ "pos"] [+ SHORT]	
	construct lengthCheck [repeat declaration_or_statement]
		if(!lengthRemaining(thePDU, ( ((thePDU ->curPos - pos) '% 4 ? 4 - ((thePDU ->curPos - pos) '% 4) : 0) ) , 'progname)) {
			%fprintf(stderr,"%s: incorrect PDU length or type: %s line: %d\n",progname, __FILE__ , __LINE__);
			return false;
		}
	construct slack [repeat declaration_or_statement]
		% Skipping slack bytes . . .
		%thePDU ->curPos = constrainedPDU.curPos;
		if(!checkSlack(thePDU, ( ((thePDU ->curPos - pos) '% 4 ? 4 - ((thePDU ->curPos - pos) '% 4) : 0) ))) {
			return false;
		}
		%thePDU ->remaining -= ( ((thePDU ->curPos - pos) '% 4 ? 4 - ((thePDU ->curPos - pos) '% 4) : 0) );
	by
		Stmts [. lengthCheck] [. slack] 
end function

function checkSlackModOptional ET [element_type]
	replace [repeat declaration_or_statement]
		Stmts [repeat declaration_or_statement]
	deconstruct ET
		LONG [id] '^ SHORT [id] TYPE [id] '(SIZE 'DEFINED)  OPEN [opt endian] 'SLACKMOD4 'OPTIONAL POS [opt position_value]	
	construct pos [id]
		_ [+ "pos"] [+ SHORT]		
	construct lengthCheck [repeat declaration_or_statement]
		if(!lengthRemaining(thePDU, ( ((thePDU ->curPos - pos) '% 4 ? 4 - ((thePDU ->curPos - pos) '% 4) : 0) ) , 'progname)) {
			%fprintf(stderr,"%s: incorrect PDU length or type: %s line: %d\n",progname, __FILE__ , __LINE__);
			return false;
		}	
	construct slack [repeat declaration_or_statement]
		% Skipping slack bytes . . .
		%thePDU ->curPos = constrainedPDU.curPos;
		if(!checkSlack(thePDU, ( ((thePDU ->curPos - pos) '% 4 ? 4 - ((thePDU ->curPos - pos) '% 4) : 0) ))) {
			return false;
		}		
		%thePDU ->curPos += ( ((thePDU ->curPos - pos) '% 4 ? 4 - ((thePDU ->curPos - pos) '% 4) : 0) );
		%thePDU ->remaining -= ( ((thePDU ->curPos - pos) '% 4 ? 4 - ((thePDU ->curPos - pos) '% 4) : 0) );
	by
		Stmts [. lengthCheck] [. slack] 
end function

function checkForwardLengthUserDefined mallocName [id] RuleName [id] OP [opt scl_additions] LET [list element_type] ET [element_type]
	deconstruct ET
		LONG [id] '^ SHORT [id] TYPE [id] '(SIZE 'DEFINED)  OPEN [opt endian] OPSL [opt slack] POS [opt position_value]	
	deconstruct OP
		ENC [opt encoding_grammar_indicator] SZ [opt size_markers_block]
		'<transfer>
		RETR [repeat transfer_statement]
		'</transfer>
		CST [opt constraints_block]
	deconstruct * [transfer_statement] RETR
		'Forward '{ 'LENGTH ( LONG ) '== REST [relational_expression] '}
	replace [repeat declaration_or_statement]
		Stmts [repeat declaration_or_statement]
	construct RESTConverted [relational_expression]
		REST [convertIDToShort] [checkOptionalLengths LET RuleName] [convertSizeOf] [convertPOS] [convertPDUREMAINING] 
	%construct expression [cexpression]
	construct expression [constant]
		RESTConverted [convertToFullID RuleName]
	construct lengtherr [stringlit]
		_ [+ "%s: incorrect PDU length or type: %s line: %d\n"]
	construct nullerr [stringlit]
		_ [+ "%s: WARNING, slack is NOT null terminated. file: %s line: %d\n"]
	construct savePos [repeat declaration_or_statement]
		unsigned 'long pos = thePDU ->'curPos;
	construct lengthCheck [repeat declaration_or_statement]
		if(!lengthRemaining(thePDU, expression , progname)) {
			%fprintf(stderr, lengtherr ,progname,__FILE__,__LINE__);
			return false;
		}
	%construct constrainedPDU [id]
	%	'constrainedPDU
	construct convertExpression [repeat declaration_or_statement]
		'PDU 'constrainedPDU;
		constrainedPDU.'data = 'thePDU ->'data;
		constrainedPDU.len = expression;
		constrainedPDU.watermark = 'thePDU ->watermark;
		constrainedPDU.curPos = 'pos;
		constrainedPDU.curBitPos = '0;
		constrainedPDU.remaining = expression;
	by
		Stmts [. savePos] [. lengthCheck] [. convertExpression]
end function

function parseUserDefinedType mallocName [id] ET [element_type] PDU [id]
	%deconstruct not mallocName
	%	mainpdu
	deconstruct ET
		LONG [id] '^ SHORT [id] TYPE [id] '(SIZE 'DEFINED) OP [opt endian] OPSL [opt slack] POS [opt position_value]
	replace [repeat declaration_or_statement]
		Stmts [repeat declaration_or_statement]
	construct index [number]
		_ [index TYPE "_"]
	construct finalIndex [number]
		index [- 1]
	construct typeToParse [id]
		TYPE [: 1 finalIndex]
	construct pName [id]
		_ [+ "parse"] [+ typeToParse]
	construct getEndianness [id]
		_ [getEndianness ET]
	construct Stmt [repeat declaration_or_statement]
		TYPE SHORT[tolower] ';
		if(!pName(& SHORT[tolower], PDU, 'progname, getEndianness)) {
			%free (mallocName);
			%mallocName = NULL;
			return false;
		}
		mallocName ->SHORT[tolower] = SHORT[tolower]';
	by
		Stmts [. Stmt]
end function

function parseUserDefinedTypeOptional mallocName [id] OP [opt scl_additions] LET [list element_type] ET [element_type] PDU [id]
	%deconstruct not mallocName
	%	mainpdu	
	deconstruct ET
		LONG [id] '^ SHORT [id] TYPE [id] '(SIZE 'DEFINED) OPEND [opt endian] OPSL [opt slack] 'OPTIONAL POS [opt position_value]
	deconstruct * [transfer_statement] OP
		'Forward '{ EXISTS( LONG ) '== REST [relational_expression] '} %ID [id] '& NUM [number] '}
	deconstruct not * [transfer_statement] OP
		'Forward '{ EXISTS( LONG ) '== ID [id] '& NUM [number] '}
	%deconstruct *[element_type] LET
	%	ID '^ SHORTMASK [id] TYPEMASK [type]
	replace [repeat declaration_or_statement]
		Stmts [repeat declaration_or_statement]
	construct index [number]
		_ [index TYPE "_"]
	construct finalIndex [number]
		index [- 1]
	construct typeToParse [id]
		TYPE [: 1 finalIndex]
	construct pName [id]
		_ [+ "parse"] [+ typeToParse]
	construct getEndianness [id]
		_ [getEndianness ET]
	construct Stmt [repeat declaration_or_statement]
		if(REST[convertIDToShort] [convertPDUREMAINING] [convertToFullID mallocName]) { %mallocName ->SHORTMASK[tolower] & NUM) {
			mallocName ->SHORT[tolower] = (TYPE *) malloc(sizeof(TYPE));
			if(mallocName ->SHORT[tolower] == NULL) {
				%free (mallocName ->SHORT[tolower]);
				%mallocName ->SHORT[tolower] = NULL;
				return false;
			}
			if(!pName(mallocName ->SHORT[tolower], PDU, 'progname, getEndianness)) {
				free (mallocName ->SHORT[tolower]);
				mallocName ->SHORT[tolower] = NULL;
				return false;
			}
		} else {
			mallocName ->SHORT[tolower] = NULL;
		}
	construct finalStmt [repeat declaration_or_statement]
		Stmt [convertRemaining]
	by
		Stmts [. finalStmt]
end function

rule convertRemaining
	replace $ [constant]
		'remaining
	by
		thePDU ->'remaining
end rule

function parseUserDefinedTypeOptionalWithNum mallocName [id] OP [opt scl_additions] LET [list element_type] ET [element_type] PDU [id]
	%deconstruct not mallocName
	%	mainpdu	
	deconstruct ET
		LONG [id] '^ SHORT [id] TYPE [id] '(SIZE 'DEFINED) OPEND [opt endian] OPSL [opt slack] 'OPTIONAL POS [opt position_value]
	deconstruct * [transfer_statement] OP
		'Forward '{ EXISTS( LONG ) '== ID [id] '& NUM [number] '}
	deconstruct *[element_type] LET
		ID '^ SHORTMASK [id] TYPEMASK [type]
	replace [repeat declaration_or_statement]
		Stmts [repeat declaration_or_statement]
	construct index [number]
		_ [index TYPE "_"]
	construct finalIndex [number]
		index [- 1]
	construct typeToParse [id]
		TYPE [: 1 finalIndex]
	construct pName [id]
		_ [+ "parse"] [+ typeToParse]
	construct getEndianness [id]
		_ [getEndianness ET]
	construct Stmt [repeat declaration_or_statement]
		if(mallocName ->SHORTMASK[tolower] & NUM) {
			mallocName ->SHORT[tolower] = (TYPE *) malloc(sizeof(TYPE));
			if(mallocName ->SHORT[tolower] == NULL) {
				%free (mallocName ->SHORT[tolower]);
				%mallocName ->SHORT[tolower] = NULL;
				return false;
			}
			if(!pName(mallocName ->SHORT[tolower], PDU, 'progname, getEndianness)) {
				free (mallocName ->SHORT[tolower]);
				mallocName ->SHORT[tolower] = NULL;
				return false;
			}
		} else {
			mallocName ->SHORT[tolower] = NULL;
		}
	by
		Stmts [. Stmt]
end function

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SET OF ELEMENTS

function parseSetOfTypeDecisionTerminateConstrained mallocName [id] RuleName [id]  OP [opt scl_additions] ET [element_type]
	deconstruct ET
		LONG [id] '^ SHORT [id] 'SET 'OF TYPE [id] '(SIZE 'CONSTRAINED)
	deconstruct OP
		EN [opt encoding_grammar_indicator] SZ [opt size_markers_block] TR [opt transfer_rules_block] CON [opt constraints_block] 
	deconstruct * [transfer_statement] TR
		'Forward '{ 'TERMINATE ( LONG ) '== RULE [id] '}
	import ALLRULES [repeat rule_definition]
	deconstruct * [rule_definition] ALLRULES
		RULE '^ SHORTID [id] ANN [opt annotation] '::= TP [type] OPTSCL [opt scl_additions]		
	replace [repeat declaration_or_statement]
		Stmts [repeat declaration_or_statement]
	construct index [number]
		_ [index TYPE "_"]
	construct finalIndex [number]
		index [- 1]
	construct typeToParse [id]
		TYPE [: 1 finalIndex]
	construct parse [id]
		_ [+ "parse"] [+ typeToParse]
	construct msg [stringlit]
		_ [+ SHORT] [+ "Count : %d\n"]
	construct functionName [id]
		_ [+ "parseSetOf"] [+ typeToParse] [+ "_O"]
	construct parseFunctionName [id]
		_ [+ "parse"] [+ typeToParse]
	import FunctionDeclarations [repeat rule_definition]
	construct  setDeclaration [rule_definition]
		TYPE * functionName (PDU * thePDU, int n, int *size, char *progname, uint8_t endianness);
	export FunctionDeclarations
		FunctionDeclarations [. setDeclaration]
	import AuxFunctions [repeat rule_definition]
	construct err [stringlit]
		_ [+ "%s: internal malloc error file: %s line: %d \n"]
	construct lowerType [id]
		TYPE[tolower]
	construct terminator [id]
		RULE [+ "_VAL"]
	construct recursiveParse [rule_definition]
		TYPE * functionName (PDU *thePDU, int n, int *size, char *progname, uint8_t endianness) {
			TYPE SHORT[tolower];
			if( parseFunctionName ('& SHORT[tolower], thePDU, 'progname, endianness)) {
				if(SHORT[tolower] .'type == terminator) {
					
					TYPE *result = (TYPE *) malloc((n+1) * sizeof(TYPE));
					if (result == NULL) {
						%fprintf (stderr, "%s: internal malloc error file: %s line: %d \n", 'progname, __FILE__, __LINE__);
						return NULL;
					}
					*size = n+1;
					result '[n'] = SHORT[tolower];
					return result;
				} else {
					TYPE *result = functionName (thePDU, n+1, size, 'progname, endianness);
					if(result != NULL) {
						result '[n'] = SHORT[tolower];
					}
						
					return result;
				}
			} else {
				if(n == 0) {
					*size = 0;
					return NULL;
				} else {
					return NULL;
				}
			}
		}
%	deconstruct not * [rule_definition] AuxFunctions
%		recursiveParse	
	export AuxFunctions
		AuxFunctions [. recursiveParse]
	construct addedStmts [repeat declaration_or_statement]
		_ [checkForwardLengthConstraint mallocName functionName LONG SHORT OP]
	by
		Stmts [. addedStmts]
end function

function parseSetOfTypeDecisionCardinalityConstrained mallocName [id] RuleName [id]  OP [opt scl_additions] LET [list element_type] ET [element_type]
	deconstruct ET
		LONG [id] '^ SHORT [id] 'SET 'OF TYPE [id] '(SIZE 'CONSTRAINED)
	deconstruct OP
		EN [opt encoding_grammar_indicator] SZ [opt size_markers_block] TR [opt transfer_rules_block] CON [opt constraints_block] 
	%deconstruct not * [transfer_statement] TR
	%	'Forward '{ 'TERMINATE ( LONG ) '== RULE [id] '}
	%deconstruct not * [transfer_statement] TR
	%	'Forward '{ 'END ( LONG ) '}	
	deconstruct * [transfer_statement] TR
		'Forward '{ 'CARDINALITY ( LONG ) '== RULE [id] '}
	import ALLRULES [repeat rule_definition]
	deconstruct * [element_type] LET
		RULE '^ SHORTCARD [id] TPCARD [type]
	replace [repeat declaration_or_statement]
		Stmts [repeat declaration_or_statement]
	construct index [number]
		_ [index TYPE "_"]
	construct finalIndex [number]
		index [- 1]
	construct typeToParse [id]
		TYPE [: 1 finalIndex]
	construct parse [id]
		_ [+ "parse"] [+ typeToParse]
	construct msg [stringlit]
		_ [+ SHORT] [+ "Count : %d\n"]
	construct functionName [id]
		_ [+ "parseSetOf"] [+ typeToParse]
	construct parseFunctionName [id]
		_ [+ "parse"] [+ typeToParse] %[+ "_O"]
	import FunctionDeclarations [repeat rule_definition]
	construct  setDeclaration [rule_definition]
		TYPE * functionName (PDU * thePDU, int size, char *progname, uint8_t endianness);
	export FunctionDeclarations
		FunctionDeclarations [. setDeclaration]
	import AuxFunctions [repeat rule_definition]
	construct err [stringlit]
		_ [+ "%s: internal malloc error file: %s line: %d \n"]
	construct lowerType [id]
		TYPE[tolower]
	construct cardinality [constant]
		mallocName ->SHORTCARD[tolower]
	construct recursiveParse [rule_definition]
		TYPE * functionName (PDU *thePDU, int size, char *progname, uint8_t endianness) {
			TYPE *result = (TYPE *) malloc( size * sizeof(TYPE));
			if (result == NULL) {
				%fprintf(stderr, err, 'progname, __FILE__,__LINE__);
				return NULL;
			}

			for (int i = 0; i < size; ++i) {
				TYPE SHORT[tolower];
				if( parseFunctionName ('& SHORT[tolower], thePDU, 'progname, endianness) )
					result'[i'] = SHORT[tolower];
				else
					return NULL;
			}
			return result;
		}
%	deconstruct not * [rule_definition] AuxFunctions
%		recursiveParse	
	export AuxFunctions
		AuxFunctions [. recursiveParse]
	construct freeMain [repeat declaration_or_statement]
		_ [checkMain mallocName SHORT]	
	construct newStmts [repeat declaration_or_statement]
		mallocName -> SHORT[tolower][+ "count"] = cardinality;
		mallocName -> SHORT[tolower][+ "length"] = thePDU ->curPos; 
		mallocName -> SHORT[tolower] = functionName(thePDU, cardinality, 'progname, endianness);
	construct endStmts [repeat declaration_or_statement]	
		mallocName -> SHORT[tolower][+ "length"] = thePDU ->curPos - mallocName -> SHORT[tolower][+ "length"]; 
	by
		Stmts [. newStmts] [. freeMain] [. endStmts]
end function

function parseSetOfTypeDecisionEndConstrained mallocName [id] RuleName [id]  OP [opt scl_additions] ET [element_type]
	deconstruct ET
		LONG [id] '^ SHORT [id] 'SET 'OF TYPE [id] '(SIZE 'CONSTRAINED)
	deconstruct OP
		EN [opt encoding_grammar_indicator] SZ [opt size_markers_block] TR [opt transfer_rules_block] CON [opt constraints_block] 
	deconstruct not * [transfer_statement] TR
		'Forward '{ 'TERMINATE ( LONG ) '== RULE [id] '}
	deconstruct not * [transfer_statement] TR
		'Forward '{ 'CARDINALITY ( LONG ) '== RULE [id] '}
	deconstruct * [transfer_statement] TR
		'Forward '{ 'END ( LONG ) '}
	replace [repeat declaration_or_statement]
		Stmts [repeat declaration_or_statement]
	construct index [number]
		_ [index TYPE "_"]
	construct finalIndex [number]
		index [- 1]
	construct typeToParse [id]
		TYPE [: 1 finalIndex]
	construct parse [id]
		_ [+ "parse"] [+ typeToParse]
	construct msg [stringlit]
		_ [+ SHORT] [+ "Count : %d\n"]
	construct functionName [id]
		_ [+ "parseSetOf"] [+ typeToParse]
	construct parseFunctionName [id]
		_ [+ "parse"] [+ typeToParse] %[+ "_O"]
	import FunctionDeclarations [repeat rule_definition]
	construct  setDeclaration [rule_definition]
		TYPE * functionName (PDU * thePDU, int n, int *size, char *progname, uint8_t endianness);
	export FunctionDeclarations
		FunctionDeclarations [. setDeclaration]
	import AuxFunctions [repeat rule_definition]
	construct err [stringlit]
		_ [+ "%s: internal malloc error file: %s line: %d \n"]
	construct lowerType [id]
		TYPE[tolower]
	construct recursiveParse [rule_definition]
		TYPE * functionName (PDU * thePDU, int n, int *size, char *progname, uint8_t endianness) {
			%TYPE * SHORT[tolower] = (TYPE * )malloc(sizeof(TYPE));
			TYPE SHORT[tolower];
			%if( SHORT[tolower] == NULL) {
			%	fprintf(stderr, err, 'progname, __FILE__, __LINE__);
			%	exit(1);
			%}

			if (! parseFunctionName('& SHORT[tolower], thePDU, 'progname, endianness)){
				if(n == 0) { 
					*'size = 0; 
					return NULL;
				}
				TYPE * result = (TYPE *)malloc('(n) * sizeof(TYPE));
				if( result == NULL) {
					fprintf(stderr, err, 'progname, __FILE__, __LINE__);
					%exit(1);
					return NULL;
				}

				*'size = n;
				return result;

			} else {
				TYPE * result = functionName(thePDU,n+1,size, 'progname, endianness);
				result '[n'] = SHORT[tolower];
				return result;
			}
		}
%	deconstruct not * [rule_definition] AuxFunctions
%		recursiveParse	
	export AuxFunctions
		AuxFunctions [. recursiveParse]
	construct addedStmts [repeat declaration_or_statement]
		_ [checkForwardLengthConstraint mallocName functionName LONG SHORT OP]
	by
		Stmts [. addedStmts]
end function

function parseSetOfTypeDecision mallocName [id] RuleName [id]  OP [opt scl_additions] ET [element_type]
	deconstruct ET
		LONG [id] '^ SHORT [id] 'SET 'OF TYPE [id] '(SIZE 'CONSTRAINED)
	deconstruct OP
		EN [opt encoding_grammar_indicator] SZ [opt size_markers_block] TR [opt transfer_rules_block] CON [opt constraints_block] 
	deconstruct not * [transfer_statement] TR
		'Forward '{ 'TERMINATE ( LONG ) '== RULE [id] '}
	deconstruct not * [transfer_statement] TR
		'Forward '{ 'CARDINALITY ( LONG ) '== RULE [id] '}
	replace [repeat declaration_or_statement]
		Stmts [repeat declaration_or_statement]
	construct index [number]
		_ [index TYPE "_"]
	construct finalIndex [number]
		index [- 1]
	construct typeToParse [id]
		TYPE [: 1 finalIndex]
	construct parse [id]
		_ [+ "parse"] [+ typeToParse]
	construct msg [stringlit]
		_ [+ SHORT] [+ "Count : %d\n"]
	construct functionName [id]
		_ [+ "parseSetOf"] [+ typeToParse]
	construct parseFunctionName [id]
		_ [+ "parse"] [+ typeToParse] %[+ "_O"]
	import FunctionDeclarations [repeat rule_definition]
	construct  setDeclaration [rule_definition]
		TYPE * functionName (PDU * thePDU, int n, int *size, char *progname, uint8_t endianness);
	export FunctionDeclarations
		FunctionDeclarations [. setDeclaration]
	import AuxFunctions [repeat rule_definition]
	construct err [stringlit]
		_ [+ "%s: internal malloc error file: %s line: %d \n"]
	construct lowerType [id]
		TYPE[tolower]
	construct recursiveParse [rule_definition]
		TYPE * functionName (PDU * thePDU, int n, int *size, char *progname, uint8_t endianness) {
			%TYPE * SHORT[tolower] = (TYPE * )malloc(sizeof(TYPE));
			TYPE SHORT[tolower];
			%if( SHORT[tolower] == NULL) {
			%	fprintf(stderr, err, 'progname, __FILE__, __LINE__);
			%	exit(1);
			%}

			if (! parseFunctionName('& SHORT[tolower], thePDU, 'progname, endianness)){
				if(n == 0) { 
					*'size = 0; 
					return NULL;
				}
				TYPE * result = (TYPE *)malloc('(n) * sizeof(TYPE));
				if( result == NULL) {
					fprintf(stderr, err, 'progname, __FILE__, __LINE__);
					%exit(1);
					return NULL;
				}

				*'size = n;
				return result;

			} else {
				TYPE * result = functionName(thePDU,n+1,size, 'progname, endianness);
				result '[n'] = SHORT[tolower];
				return result;
			}
		}
%	deconstruct not * [rule_definition] AuxFunctions
%		recursiveParse	
	export AuxFunctions
		AuxFunctions [. recursiveParse]
	construct addedStmts [repeat declaration_or_statement]
		_ [checkForwardLengthConstraint mallocName functionName LONG SHORT OP]
			[checkEndConstraint TR LONG]
	by
		Stmts [. addedStmts]
end function

function checkEndConstraint TR [opt transfer_rules_block] LONG [id]
	deconstruct * [transfer_statement] TR
		'Forward '{ 'END ( LONG ) '}
	replace [repeat declaration_or_statement]
		Stmts [repeat declaration_or_statement]
	construct checkEnd [repeat declaration_or_statement]
		if(thePDU ->'remaining != 0) { 
			return false;
		}
	by
		Stmts [. checkEnd]
end function

function checkForwardLengthConstraint mallocName [id] functionName [id] LONG [id] SHORT [id] OP [opt scl_additions]
	replace [repeat declaration_or_statement]
		Stmts [repeat declaration_or_statement]
	by
		Stmts [withLengthConstraint mallocName functionName LONG SHORT OP]
				[withoutLengthConstraint mallocName functionName LONG SHORT OP]
end function

function withLengthConstraint mallocName [id] functionName [id] LONG [id] SHORT [id] OP [opt scl_additions]
	deconstruct OP
		EN [opt encoding_grammar_indicator] SZ [opt size_markers_block] TR [opt transfer_rules_block] CON [opt constraints_block] 
	deconstruct * [transfer_statement] TR
		'Forward '{ 'LENGTH ( LONG ) '== REST [relational_expression] '}
	construct convertedREST [relational_expression]
		REST [replacePDULENGTH]
	replace [repeat declaration_or_statement]
		Stmts [repeat declaration_or_statement]
	construct freeMain [repeat declaration_or_statement]
		_ [checkMain mallocName SHORT]
	construct newStmts [repeat declaration_or_statement]
		int size = '0;
		PDU *constrainedPDU;
		constrainedPDU = (PDU*)malloc(sizeof(PDU));
		if (constrainedPDU == NULL){
			%fprintf(stderr,"%s: internal malloc error file: %s line: %d\n",progname, __FILE__ , __LINE__);
			return false;
		}
		constrainedPDU ->len = thePDU ->len;
		constrainedPDU ->watermark= thePDU ->watermark;
		constrainedPDU ->curPos = thePDU ->curPos;
		constrainedPDU ->data = thePDU ->data;
		constrainedPDU ->remaining = convertedREST;
		mallocName -> SHORT[tolower][+ "length"] = thePDU ->curPos; 
		mallocName -> SHORT[tolower] = functionName(constrainedPDU, '0 , &size, 'progname, endianness);

	construct endStmts [repeat declaration_or_statement]
		mallocName -> SHORT[tolower][+ "count"] = size;
		thePDU ->curPos = constrainedPDU ->curPos;
		mallocName -> SHORT[tolower][+ "length"] = thePDU ->curPos - mallocName -> SHORT[tolower][+ "length"]; 
		free(constrainedPDU);
		constrainedPDU = NULL;
	by 
		Stmts [. newStmts] [. freeMain] [. endStmts]
end function

function checkMain mallocName [id] SHORT [id]
	replace [repeat declaration_or_statement]
		decl [repeat declaration_or_statement]
	import freeName [id]
	construct freeStmt [declaration_or_statement]
		freeName('mainpdu);
	by
		decl [mainFree mallocName SHORT] [notMainFree mallocName SHORT]
end function

function mainFree mallocName [id] SHORT [id]
	replace [repeat declaration_or_statement]
		decl [repeat declaration_or_statement]
	deconstruct mallocName
		'mainpdu
	import freeName [id]
	construct NullCheck [repeat declaration_or_statement]
		if(mallocName -> SHORT[tolower] == NULL) {
			freeName(mallocName);
			return false;		
		}
	by
		decl [. NullCheck]
end function

function notMainFree mallocName [id] SHORT [id]
	replace [repeat declaration_or_statement]
		decl [repeat declaration_or_statement]
	deconstruct not mallocName
		'mainpdu
	import freeName [id]
	construct NullCheck [declaration_or_statement]
		if(mallocName -> SHORT[tolower] == NULL) {
			return false;		
		}
	by
		decl [. NullCheck]
end function

rule replacePDULENGTH
	replace $ [primary]
		'PDULENGTH
	construct RP [referencable_primaries]
		'thePDU '-> 'len
	by
		RP
end rule

function withoutLengthConstraint mallocName [id] functionName [id] LONG [id] SHORT [id] OP [opt scl_additions]
	deconstruct OP
		EN [opt encoding_grammar_indicator] SZ [opt size_markers_block] TR [opt transfer_rules_block] CON [opt constraints_block] 
	deconstruct not * [transfer_statement] TR
		'Forward '{ 'LENGTH ( LONG ) '== REST [relational_expression] '}
	replace [repeat declaration_or_statement]
		Stmts [repeat declaration_or_statement]
	construct freeMain [repeat declaration_or_statement]
		_ [checkMain mallocName SHORT]	
	construct newStmts [repeat declaration_or_statement]
		int size = '0;
		mallocName -> SHORT[tolower][+ "length"] = thePDU ->curPos; 
		mallocName -> SHORT[tolower] = functionName(thePDU, '0 , &size, 'progname, endianness);
	construct endStmts [repeat declaration_or_statement]
		mallocName -> SHORT[tolower][+ "count"] = size;		
		mallocName -> SHORT[tolower][+ "length"] = thePDU ->curPos - mallocName -> SHORT[tolower][+ "length"]; 
	by
		Stmts [. newStmts] [. freeMain] [. endStmts]
end function

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Free Functions to avoid any memory leaks after any parse

function createFreeFunctionTypeRule RULES [repeat rule_definition]
	replace [program]
		P [program]
	deconstruct * [rule_definition] RULES
		LONG [id] '^ PDU ANN [opt annotation]'::= 'SEQUENCE '{
			LE [list element_type] OC [opt ',]
		'} OP [opt scl_additions]
	construct freeName [id]
		_ [+ "free"] [+ LONG]
	construct ALLOCNAME [constant]
		'mainpdu
	construct freeBody [repeat declaration_or_statement]
	construct lexicalValue [number]
		1
	deconstruct LE
		ET [element_type] ', REST [list element_type]
	construct mainFree [repeat declaration_or_statement]
		_ [freeMainPDU RULES ALLOCNAME lexicalValue ET each LE]
	construct finalFree [repeat declaration_or_statement]
		free(ALLOCNAME);
		ALLOCNAME = NULL;
	import FunctionDeclarations [repeat rule_definition]
	construct freeDecl [rule_definition]
		void freeName(LONG * 'mainpdu);
	export FunctionDeclarations
		FunctionDeclarations [. freeDecl]
	construct freeFunction [rule_definition]
		void freeName(LONG * 'mainpdu) {
			freeBody [. mainFree] %[. finalFree]
		}
	import AuxFunctions [repeat rule_definition]
	export AuxFunctions
		AuxFunctions [. freeFunction]
	by
		P
end function
	
function freeMainPDU RULES [repeat rule_definition] ALLOCNAME [constant] LVAL [number] previousET [element_type] ET [element_type]
	replace [repeat declaration_or_statement]
		Stmts [repeat declaration_or_statement]
	construct addedStmts [repeat declaration_or_statement]
		_ %[freeSetOfMain RULES ALLOCNAME LVAL previousET ET]
			[freeSetOf RULES ALLOCNAME LVAL previousET ET]
			[freeUserDefinedType RULES ALLOCNAME LVAL previousET ET]
			[freeUserDefinedTypeOptional RULES ALLOCNAME LVAL previousET ET]
			[freeOctetStringTypeConstrained RULES ALLOCNAME ET]
	by
		Stmts [. addedStmts]
end function

function freeSetOfMain RULES [repeat rule_definition] ALLOCNAME [constant] LVAL [number] previousET [element_type] ET [element_type]
	deconstruct ALLOCNAME
		ALLOCID [id]
	construct ALLOCLOWER [id]
	%	ALLOCNAME[tolower]
		ALLOCID[tolower]
	deconstruct ALLOCLOWER
		'mainpdu
	deconstruct ET
		LONG [id] '^ SHORT [id] 'SET 'OF TYPE [id] '(SIZE 'CONSTRAINED) POS [opt position_value]
	replace [repeat declaration_or_statement]
		Stmts [repeat declaration_or_statement]
	construct shortLower [id]
		SHORT[tolower]
	construct longLower [id]
		LONG[tolower]
	construct setOf [id]
		%_ [+ longLower] [+ "count"]
		_ [+ shortLower] [+ "count"]
	construct index [id]
		_ [getLexicalIndex LVAL]
	%construct Alloc [referencable_primaries]
	construct currentAlloc [constant]
		%ALLOCNAME ->longLower '[index'] %_ [+ ALLOCNAME] [+ "->"] [+ shortLower]
		%ALLOCNAME ->shortLower '[index'] %_ [+ ALLOCNAME] [+ "->"] [+ shortLower]
		ALLOCNAME ->shortLower'[ index '] %_ [+ ALLOCNAME] [+ "->"] [+ shortLower]
		%ALLOCNAME ->shortLower '[index'] %_ [+ ALLOCNAME] [+ "->"] [+ shortLower]
	%	_ [^ Alloc]
	construct setOfBody [repeat declaration_or_statement]
		_ [freeTypeDecision RULES currentAlloc TYPE LVAL ET ET]
	construct Stmt [repeat declaration_or_statement]
		if(ALLOCNAME ->shortLower != NULL) {
			for(int index = 0; index < ALLOCNAME ->setOf; ++index) {
				if(& currentAlloc != NULL) {
					setOfBody 
				}
			}
			if(ALLOCNAME ->SHORT[tolower] != NULL) {
				free(ALLOCNAME ->SHORT[tolower]);
				ALLOCNAME ->SHORT[tolower] = NULL;
			}
		}
	by
		Stmts [. Stmt]
end function

function freeSetOf RULES [repeat rule_definition] ALLOCNAME [constant] LVAL [number] previousET [element_type] ET [element_type]
	%deconstruct not ALLOCNAME
	%	'mainpdu
	deconstruct ET
		LONG [id] '^ SHORT [id] 'SET 'OF TYPE [id] '(SIZE 'CONSTRAINED) POS [opt position_value]
	replace [repeat declaration_or_statement]
		Stmts [repeat declaration_or_statement]
	construct shortLower [id]
		SHORT[tolower]
	construct longLower [id]
		LONG[tolower]
	construct setOf [id]
		%_ [+ longLower] [+ "count"]
		_ [+ shortLower] [+ "count"]
	construct shortLowerConst [constant]
		shortLower
	construct setOfConst [constant]
		setOf
	construct index [id]
		_ [getLexicalIndex LVAL]
	construct ALLOCNAMESETOF [constant]
		ALLOCNAME [optionalMember previousET ET setOf]
			[normalMember previousET ET setOf] 
	construct ALLOCNAMESHORT [constant]
		ALLOCNAME [optionalMember previousET ET shortLower]
			[normalMember previousET ET shortLower] 
	construct shortIndex [constant]
		shortLower'[ index ']
	construct currentAlloc [constant]
		%ALLOCNAME ->longLower '[index'] %_ [+ ALLOCNAME] [+ "->"] [+ shortLower]
		%ALLOCNAME ->shortLower '[index'] %_ [+ ALLOCNAME] [+ "->"] [+ shortLower]
		ALLOCNAME [optionalMember previousET ET shortLower]
			[normalMember previousET ET shortLower] '[ index ']  %_ [+ ALLOCNAME] [+ "->"] [+ shortLower]
	construct setOfBody [repeat declaration_or_statement]
		_ [freeTypeDecision RULES currentAlloc TYPE LVAL ET ET] [freeSetOfSequence RULES currentAlloc TYPE LVAL ET ET]
	construct Stmt [repeat declaration_or_statement]
		for(int index = 0; index < ALLOCNAMESETOF ; ++index) {
			if(& currentAlloc != NULL) {
				setOfBody 
			}
		}
		if(ALLOCNAMESHORT != NULL) {
			free(ALLOCNAMESHORT);
			ALLOCNAMESHORT = NULL;
		}
	construct optionalCheck [repeat declaration_or_statement]
		_ [checkForOptionalPreviousET previousET ALLOCNAME shortLower Stmt] [checkForNotOptionalPreviousET previousET ALLOCNAME shortLower Stmt]
	by
		Stmts [. optionalCheck]
end function

function checkForOptionalPreviousET ET [element_type] ALLOCNAME [constant] shortLower [id] Stmt [repeat declaration_or_statement]
	replace [repeat declaration_or_statement]
		Stmts [repeat declaration_or_statement]
	deconstruct ET 
		LONG [id] '^ SHORT [id] TYPE [id] '(SIZE 'DEFINED) OPT [opt endian] OPSL [opt slack] 'OPTIONAL POS [opt position_value]
	construct additionalFree [repeat declaration_or_statement]
		free(ALLOCNAME);
		ALLOCNAME = NULL;
	construct optionalCheck [repeat declaration_or_statement]
		if(ALLOCNAME != NULL) {
			if(ALLOCNAME ->shortLower != NULL) {
				Stmt %[. additionalFree]
			}
		}
	by
		Stmts [. optionalCheck]
end function

function checkForNotOptionalPreviousET ET [element_type] ALLOCNAME [constant] shortLower [id] Stmt [repeat declaration_or_statement]
	replace [repeat declaration_or_statement]
		Stmts [repeat declaration_or_statement]
	deconstruct not ET 
		LONG [id] '^ SHORT [id] TYPE [id] '(SIZE 'DEFINED) OPT [opt endian] OPSL [opt slack] 'OPTIONAL POS [opt position_value]
	construct optionalCheck [repeat declaration_or_statement]
		if(ALLOCNAME '.shortLower != NULL) {
			Stmt
		}
	by
		Stmts [. optionalCheck]
end function

function optionalMember previousET [element_type] currentET [element_type] shortLower [id]
	deconstruct currentET
		LONG [id] '^ SHORT [id] 'SET 'OF TYPE [id] '(SIZE 'CONSTRAINED) POS [opt position_value]
	construct index [number]
		_ [index LONG "_"]
	construct startingIndex [number]
		index [+ 1]
	construct lengthIndex [number]
		_ [# LONG]
	construct typeToCheck [id]
		LONG [: startingIndex lengthIndex]
	deconstruct previousET
		LONG2 [id] '^ SHORT2 [id] typeToCheck '(SIZE DEFINED) OPT [opt endian] OPSL [opt slack] 'OPTIONAL
	replace [constant]
		ALLOCNAME [constant]
	by
		ALLOCNAME -> shortLower
end function

function normalMember previousET [element_type] currentET [element_type] shortLower [id]
	deconstruct currentET
		LONG [id] '^ SHORT [id] 'SET 'OF TYPE [id] '(SIZE 'CONSTRAINED) POS [opt position_value]
	construct index [number]
		_ [index LONG "_"]
	construct startingIndex [number]
		index [+ 1]
	construct lengthIndex [number]
		_ [# LONG]
	construct typeToCheck [id]
		LONG [: startingIndex lengthIndex]
	deconstruct not previousET
		LONG2 [id] '^ SHORT2 [id] typeToCheck '(SIZE DEFINED) OPT [opt endian] OPSL [opt slack] 'OPTIONAL
	replace [constant]
		ALLOCNAME [constant]
	by
		ALLOCNAME .shortLower
end function

function getLexicalIndex VAL [number]
	replace [id]
		index [id]
	by
		index [first VAL] [second VAL] [third VAL]
end function

function first VAL [number]
	deconstruct VAL
		1
	replace [id]
		index [id]
	by 
		'i
end function

function second VAL [number]
	deconstruct VAL
		2
	replace [id]
		index [id]
	by 
		'j
end function

function third VAL [number]
	deconstruct VAL
		3
	replace [id]
		index [id]
	by 
		'k
end function

function freeSetOfSequence RULES [repeat rule_definition] ALLOCNAME [constant] TYPE [id] LVAL [number] ET [element_type] previousET [element_type]
	deconstruct * [rule_definition] RULES
		TYPE '^ SHORTID [id] OPTANN [opt annotation]'::= 'SEQUENCE '{
			LE [list element_type] OC [opt ',]
		'} OP [opt scl_additions]
	construct newLexicalIndex [number]
		LVAL [+ 1]
	replace [repeat declaration_or_statement]
		Stmts [repeat declaration_or_statement]
	construct NewAllocName [constant]
		 ALLOCNAME%.SHORTID[tolower]
		 %ALLOCNAME.SHORTID[tolower]
	construct TRFreeBody [repeat declaration_or_statement]
		 _ [freeMainPDU RULES NewAllocName newLexicalIndex previousET each LE]
	construct freeSHORTID [repeat declaration_or_statement]
		if(ALLOCNAME.SHORTID[tolower] != NULL) {
			free(ALLOCNAME.SHORTID[tolower]);
			ALLOCNAME.SHORTID[tolower] = NULL;
		}
		%if(ALLOCNAME.SHORTID[tolower] != NULL) {
		%	free(ALLOCNAME.SHORTID[tolower]);
		%	ALLOCNAME.SHORTID[tolower] = NULL;
		%}		
	construct Stmt [repeat declaration_or_statement]
		TRFreeBody %[. freeSHORTID]
	by
		Stmts [. Stmt]
end function

function freeTypeDecision RULES [repeat rule_definition] ALLOCNAME [constant] TYPE [id] LVAL [number] ET [element_type] previousET [element_type]
	deconstruct * [rule_definition] RULES
		TYPE '^ SHORT [id] OPGLOB [opt global] OPOPT [opt optimizable] '::= '( TR [type_reference] RTR [repeat alternative_decision]')
	deconstruct TR
		LONGID [id] OPT [opt dotID] ANN [opt annotation]
	construct SHORTVAL [id]
		_ [+ LONGID] [+ "_VAL"]
	construct newLexicalIndex [number]
		LVAL [+ 1]
	construct otherCases [repeat declaration_or_statement]
	construct repeatCases [repeat declaration_or_statement]
		otherCases [freeAltDecisions RULES ALLOCNAME newLexicalIndex RTR previousET]
	deconstruct * [rule_definition] RULES
		LONGID '^ SHORTID [id] OPTANN [opt annotation]'::= 'SEQUENCE '{
			LE [list element_type] OC [opt ',]
		'} OP [opt scl_additions]
	replace [repeat declaration_or_statement]
		Stmts [repeat declaration_or_statement]
	construct err [stringlit]
		_ [+ "Error, unknown kind %s:%d\n"]
	construct NewAllocName [constant]
		 ALLOCNAME.'ptr.LONGID[tolower]
		 %ALLOCNAME.SHORTID[tolower]
	construct TRFreeBody [repeat declaration_or_statement]
		 _ [freeMainPDU RULES NewAllocName newLexicalIndex previousET each LE]
	construct freeSHORTID [repeat declaration_or_statement]
		if(ALLOCNAME.'ptr.LONGID[tolower] != NULL) {
			free(ALLOCNAME.'ptr.LONGID[tolower]);
			ALLOCNAME.'ptr.LONGID[tolower] = NULL;
		}
		%if(ALLOCNAME.SHORTID[tolower] != NULL) {
		%	free(ALLOCNAME.SHORTID[tolower]);
		%	ALLOCNAME.SHORTID[tolower] = NULL;
		%}		
	construct Stmt [repeat declaration_or_statement]
		if(ALLOCNAME.'type > 0) {
			switch(ALLOCNAME.'type) {
				case SHORTVAL:
				{
					TRFreeBody %[. freeSHORTID]
				}
				break;
				repeatCases
				%default:
				%fprintf(stderr, err,__FILE__,__LINE__);
			}
		}
	by
		Stmts [. Stmt]
end function

function freeAltDecisions RULES [repeat rule_definition] ALLOCNAME [constant] LVAL [number] RTR [repeat alternative_decision] previousET [element_type]
	deconstruct RTR 
		'| LONGID [id] OPT [opt dotID] ANN [opt annotation] REST [repeat alternative_decision]
	replace [repeat declaration_or_statement]
		Stmts [repeat declaration_or_statement]
	construct SHORTVAL [id]
		_ [+ LONGID] [+ "_VAL"]
	deconstruct * [rule_definition] RULES
		LONGID '^ SHORTID [id] OPTANN [opt annotation]'::= 'SEQUENCE '{
			LE [list element_type] OC [opt ',]
		'} OP [opt scl_additions]
	construct NewAllocName [constant]
		 ALLOCNAME.'ptr.LONGID[tolower]
		 %ALLOCNAME.SHORTID[tolower]
	construct TRFreeBody [repeat declaration_or_statement]
		_ [freeMainPDU RULES NewAllocName LVAL previousET each LE]
	construct freeSHORTID [repeat declaration_or_statement]
		if(ALLOCNAME.'ptr.LONGID[tolower] != NULL) {
			free(ALLOCNAME.'ptr.LONGID[tolower]);
			ALLOCNAME.'ptr.LONGID[tolower] = NULL;
		}
		%if(ALLOCNAME.SHORTID[tolower] != NULL) {
		%	free(ALLOCNAME.SHORTID[tolower]);
		%	ALLOCNAME.SHORTID[tolower] = NULL;
		%}		
	construct Stmt [repeat declaration_or_statement]
		case SHORTVAL':
		{
			TRFreeBody %[. freeSHORTID]
		}
		break;
	by
		Stmts [. Stmt] [freeAltDecisions RULES ALLOCNAME LVAL REST previousET] %[. Stmt] [freeAltDecisions RULES ALLOCNAME REST]
end function

function freeUserDefinedType RULES [repeat rule_definition] ALLOCNAME [constant] LVAL [number] previousET [element_type] ET [element_type]
	deconstruct ET
		LONG [id] '^ SHORT [id] TYPE [id] '(SIZE 'DEFINED)  OPT [opt endian] OPSL [opt slack] POS [opt position_value]
	deconstruct * [rule_definition] RULES
		TYPE '^ SHORTID [id] ANN [opt annotation]'::= 'SEQUENCE '{
			LE [list element_type] OC [opt ',]
		'} OP [opt scl_additions]
	construct Stmt [repeat declaration_or_statement]
		if(ALLOCNAME .SHORT[tolower] != NULL) {
			free(ALLOCNAME .SHORT[tolower]);
			ALLOCNAME .SHORT[tolower] = NULL;
		}	
	construct NewAllocName [constant]
		ALLOCNAME.SHORT[tolower]
		%ALLOCNAME.'ptr.LONG[tolower]
	construct freeBody [repeat declaration_or_statement]
		_ [freeMainPDU RULES NewAllocName LVAL ET each LE]
	replace [repeat declaration_or_statement]
		Stmts [repeat declaration_or_statement]
	by
		Stmts [. freeBody] %[. Stmt]
end function

function freeUserDefinedTypeOptional RULES [repeat rule_definition] ALLOCNAME [constant] LVAL [number] previousET [element_type] ET [element_type]
	deconstruct ET
		LONG [id] '^ SHORT [id] TYPE [id] '(SIZE 'DEFINED) OPT [opt endian] OPSL [opt slack] 'OPTIONAL POS [opt position_value]
	deconstruct * [rule_definition] RULES
		TYPE '^ SHORTID [id] ANN [opt annotation]'::= 'SEQUENCE '{
			LE [list element_type] OC [opt ',]
		'} OP [opt scl_additions]
	construct Stmt [repeat declaration_or_statement]
		if(ALLOCNAME .SHORT[tolower] != NULL) {
			free(ALLOCNAME .SHORT[tolower]);
			ALLOCNAME .SHORT[tolower] = NULL;
		}
	construct NewAllocName [constant]
		%ALLOCNAME.SHORT[tolower]
		ALLOCNAME .SHORT[tolower]
	construct freeBody [repeat declaration_or_statement]
		_ [freeMainPDU RULES NewAllocName LVAL ET each LE]
	replace [repeat declaration_or_statement]
		Stmts [repeat declaration_or_statement]
	by
		Stmts [. freeBody] [. Stmt]
end function

function freeOctetStringTypeConstrained RULES [repeat rule_definition] ALLOCNAME [constant] ET [element_type]
	deconstruct ET
		LONG [id] '^ SHORT [id] 'OCTET 'STRING '(SIZE 'CONSTRAINED) OPT [opt endian] OPSL [opt slack] POS [opt position_value]
	replace [repeat declaration_or_statement]
		Stmts [repeat declaration_or_statement]
	construct Stmt [repeat declaration_or_statement]
		if(ALLOCNAME .SHORT[tolower] != NULL) {
			free(ALLOCNAME .SHORT[tolower]);
			ALLOCNAME .SHORT[tolower] = NULL;
		}	
	by
		Stmts [. Stmt]
end function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function createFreeFunctionTypeDecision RULES [repeat rule_definition]
	replace [program]
		P [program]
	by
		P [createFreeInterfaceTypeDecision RULES] [createFreeNormalTypeDecision RULES]
end function

function createFreeInterfaceTypeDecision RULES [repeat rule_definition]
	replace [program]
		P [program]
	deconstruct * [rule_definition] RULES
		LONG [id] '^ PDU OPGLOB [opt global] OPT [opt optimizable]'::= TD [type_decision] OPTSCL [opt scl_additions]
	deconstruct TD
		'( TR [type_reference] RTR [repeat alternative_decision] ')
	deconstruct TR
		LONG2 [id] '. ID2 [id] OPANN [opt annotation]
	construct ALLOCNAME [constant]
		'mainpdu
	construct LVAL [number]
		1
	construct freeDTID [id]
		_ [+ "free"] [+ ID2]
	construct freeName [id]
		_ [+ "free"] [+ LONG]	
	construct externalFree [rule_definition]
		extern void freeDTID(ID2 *ALLOCNAME);
	construct freeBody [repeat declaration_or_statement]
	import FunctionDeclarations [repeat rule_definition]
	construct freeDecl [rule_definition]
		void freeName(LONG * 'mainpdu);
	export FunctionDeclarations
		FunctionDeclarations [. freeDecl] %[. externalFree]
	construct mainFree [repeat declaration_or_statement]
		if(mainpdu ->'type == ID2[+ "_VAL"])
			freeDTID(&mainpdu ->'ptr.ID2[tolower]);
	construct otherDecisions [repeat declaration_or_statement]
		_ [freeOtherInterfaceDecisions LONG RULES ALLOCNAME LVAL RTR]
	construct freeFunction [rule_definition]
		void freeName( LONG * 'mainpdu ) {
			freeBody [. mainFree] [. otherDecisions] %[. finalFree]
		}
	import AuxFunctions [repeat rule_definition]
	export AuxFunctions
		AuxFunctions [. freeFunction]		
	by
		P
end function 

function freeOtherInterfaceDecisions RuleName [id] RULES [repeat rule_definition] ALLOCNAME [constant] LVAL [number] RTR [repeat alternative_decision]
	replace [repeat declaration_or_statement]
		Stmts [repeat declaration_or_statement]
	deconstruct RTR
		'| 	LONG [id] '. ID2 [id] OPANN [opt annotation] RESTRTR [repeat alternative_decision]
	construct freeDTID [id]
		_ [+ "free"] [+ ID2]
	construct externalFree [rule_definition]
		extern void freeDTID(ID2 *ALLOCNAME);
	import FunctionDeclarations [repeat rule_definition]
	export FunctionDeclarations
		FunctionDeclarations %[. externalFree]	
	construct otherDecisions [repeat declaration_or_statement]
		_ [freeOtherInterfaceDecisions RuleName RULES ALLOCNAME LVAL RESTRTR]
	construct freeOtherInterface [repeat declaration_or_statement]
		 else if (mainpdu ->'type == ID2[+ "_VAL"])
			freeDTID(&mainpdu ->'ptr.ID2[tolower]);
	by
		Stmts [. freeOtherInterface]	
end function

function createFreeNormalTypeDecision RULES [repeat rule_definition]
	replace [program]
		P [program]
	deconstruct * [rule_definition] RULES
		LONG [id] '^ PDU OPGLOB [opt global] OPT [opt optimizable]'::= TD [type_decision] OPTSCL [opt scl_additions]
	deconstruct TD
		'( TR [type_reference] RTR [repeat alternative_decision] ')
	deconstruct TR
		ID [id] OPANN [opt annotation]
	construct freeName [id]
		_ [+ "free"] [+ LONG]
	construct ALLOCNAME [constant]
		'mainpdu
	construct freeBody [repeat declaration_or_statement]
	construct lexicalValue [number]
		1
	construct TRList [repeat type_reference]
		_ [^ RTR] [. TR]
	construct mainFree [repeat declaration_or_statement]
		_ [freeMainPDUTypeDecision RULES ALLOCNAME lexicalValue each TRList]
	construct finalFree [repeat declaration_or_statement]
		free(ALLOCNAME);
		ALLOCNAME = NULL;
	import FunctionDeclarations [repeat rule_definition]
	construct freeDecl [rule_definition]
		void freeName(LONG * 'mainpdu);
	export FunctionDeclarations
		FunctionDeclarations [. freeDecl]
	construct freeFunction [rule_definition]
		void freeName( LONG * 'mainpdu ) {
			freeBody [. mainFree] %[. finalFree]
		}
	import AuxFunctions [repeat rule_definition]
	export AuxFunctions
		AuxFunctions [. freeFunction]
	by
		P
end function

function freeMainPDUTypeDecision RULES [repeat rule_definition] ALLOCNAME [constant] LVAL [number] TR [type_reference]
	deconstruct TR
		LONG [id] OPID [opt dotID] OPANN [opt annotation]
	deconstruct * [rule_definition] RULES
		LONG '^ SHORT [id] ANN [opt annotation]'::= 'SEQUENCE '{
			LE [list element_type] OC [opt ',]
		'} OP [opt scl_additions]
	deconstruct LE
		ET [element_type] ', REST [list element_type]
	replace [repeat declaration_or_statement]
		Stmts [repeat declaration_or_statement]
	construct NewAllocName [constant]
		ALLOCNAME -> 'ptr '.LONG[tolower]
	construct addedStmts [repeat declaration_or_statement]
		_ [freeMainPDU RULES NewAllocName LVAL ET each LE]
	construct checkCase [repeat declaration_or_statement]
		if (mainpdu ->'type == LONG[+ "_VAL"]) {
			addedStmts
		}
	by
		Stmts [. checkCase]
end function

function freeOtherDecisions RULES [repeat rule_definition] ALLOCNAME [constant] LVAL [number] RTR [repeat alternative_decision]
	replace [repeat declaration_or_statement]
		Stmts [repeat declaration_or_statement]
	deconstruct RTR
		'| 	LONG [id] OPID [opt dotID] OPANN [opt annotation] RESTRTR [repeat alternative_decision]
	deconstruct * [rule_definition] RULES
		LONG '^ SHORT [id] ANN [opt annotation]'::= 'SEQUENCE '{
			LE [list element_type] OC [opt ',]
		'} OP [opt scl_additions]
	deconstruct LE
		ET [element_type] ', REST [list element_type]
	construct NewAllocName [constant]
		ALLOCNAME -> 'ptr '.LONG[tolower]
	construct otherDecisions [repeat declaration_or_statement]
		_ [freeOtherDecisions RULES ALLOCNAME LVAL RESTRTR]
	construct addedStmts [repeat declaration_or_statement]
		_ [freeMainPDU RULES NewAllocName LVAL ET each LE]
	construct checkCase [repeat declaration_or_statement]
	if (mainpdu ->'type == LONG[+ "_VAL"]) {
		addedStmts
	}
	by
		Stmts [. checkCase]	
end function

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%rule convertStringlitToHex
%	replace $ [charlit]
%		S [charlit]
%	deconstruct S
%		C [charlit_or_id]
%	by
%		C [stringConversion]
%end rule

rule convertStringlitToHex
    replace $ [primary]
        S [charlit]
    construct SLen [number]
        _ [# S][+ 1]
    where
    	SLen [> 2]
    construct HexNum [id]
       _ [unquote ''0x']
         [addEachChar S SLen '1]
   by
      HexNum
end rule

function addEachChar S [charlit] Slen [number] i [number]
    where
       i [< Slen]
    construct E [number]
       i [+ 1]
    construct C [charlit]
    	S [: i i]
    	  [doLookup]
    replace [id]
        N [id]
    by
        N [+ C] [addEachChar S Slen E]
end function

define HexPair
   [charlit] [charlit]
end define

function doLookup
	construct Tbl [repeat HexPair]
		''R' ''52' ''T' ''54' ''P' ''50' ''S' ''53' ''X' ''58'
    replace [charlit]
        C [charlit]
    deconstruct * [HexPair] Tbl
        C N [charlit]
    by
      N
end function 

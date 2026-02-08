grammar Delphi;

// ===================================================================
// DELPHI GRAMMAR for Version 12.3+ (2025)
//
// Complete ANTLR4 grammar for modern Delphi language
// Supports: Inline Variables, Generics, Attributes, Helper Types,
// Anonymous Methods, Operator Overloading and current features
// ===================================================================

// -------------------------------------------------------------------
// TOP-LEVEL FILE STRUCTURE
// -------------------------------------------------------------------

// Main file structure - programs, units, libraries or packages
file : (program_ | unit_ | library_ | package_) EOF ;

// Program structure: program Name; uses...; declarations; begin..end.
program_ : 'program' qualifiedIdent ('(' identList ')')? ';'
           (usesClause)?
           (declarationSection)*
           compoundStatement '.' ;

// Unit structure: unit Name; interface..implementation..end.
unit_ : 'unit' qualifiedIdent portabilityDirective* ';'
        interfaceSection
        implementationSection
        (initializationSection)?
        (finalizationSection)?
        'end' '.' ;

// Library structure: library Name; declarations; exports; end.
library_ : 'library' IDENT ';'
           (usesClause)?
           (declarationSection)*
           (exportsClause)*
           (initializationSection)?
           (finalizationSection)?
           'end' '.' ;

// Package structure: package Name; requires...; contains...; end.
package_ : 'package' IDENT ';'
           (requiresClause)?
           (containsClause)?
           'end' '.' ;

// -------------------------------------------------------------------
// UNIT SECTIONS
// -------------------------------------------------------------------

// Interface section with public declarations
interfaceSection : 'interface' (usesClause)? (interfaceDecl)* ;

// Implementation section with private code
implementationSection : 'implementation' (usesClause)? (declarationSection)* ;

// Optional initialization section
initializationSection : 'initialization' statementList ;

// Optional finalization section
finalizationSection : 'finalization' statementList ;

// -------------------------------------------------------------------
// USES, REQUIRES, CONTAINS, EXPORTS
// -------------------------------------------------------------------

// Uses clause: uses Unit1, Unit2 in 'file.pas';
usesClause : 'uses' usedUnit (',' usedUnit)* ';' ;

usedUnit : qualifiedIdent ('in' STRING)? ;

// Package requires clause
requiresClause : 'requires' qualifiedIdent (',' qualifiedIdent)* ';' ;

// Package contains clause
containsClause : 'contains' qualifiedIdent ('in' STRING)?
                 (',' qualifiedIdent ('in' STRING)?)* ';' ;

// Exports clause for DLLs/libraries
exportsClause : 'exports' exportItem (',' exportItem)* ';' ;

exportItem : IDENT ('(' formalParameterList ')')?
             ('name' constExpression)?
             ('index' constExpression)?
             ('resident')? ;

// -------------------------------------------------------------------
// INTERFACE DECLARATIONS (public)
// -------------------------------------------------------------------

interfaceDecl : constSection
              | resourcestringSection
              | typeSection
              | varSection
              | threadvarSection
              | procedureInterfaceDecl
              | functionInterfaceDecl ;

// Forward declarations in interface: procedure/function heading + directives
procedureInterfaceDecl : attributeSection* ('class')? procedureHeading ';' directiveList ;
functionInterfaceDecl  : attributeSection* ('class')? functionHeading ';' directiveList ;

// Zero or more directives separated by semicolons
directiveList : (directive ';')* ;

// -------------------------------------------------------------------
// IMPLEMENTATION DECLARATIONS (private)
// -------------------------------------------------------------------

declarationSection : labelDeclSection
                   | constSection
                   | resourcestringSection
                   | typeSection
                   | varSection
                   | threadvarSection
                   | exportsClause
                   | procedureDeclaration
                   | functionDeclaration
                   | constructorImpl
                   | destructorImpl ;

// -------------------------------------------------------------------
// CUSTOM ATTRIBUTES for RTTI: [AttributeName(params)]
// -------------------------------------------------------------------

attributeSection : '[' attributeList ']' ;

attributeList : attribute (',' attribute)* ;

attribute : qualifiedIdent (attributeArguments)? ;

attributeArguments : '(' (attributeArgumentList)? ')' ;

attributeArgumentList : attributeArgument (',' attributeArgument)* ;

attributeArgument : (IDENT '=')? expression ;

// -------------------------------------------------------------------
// CONSTANT DECLARATIONS
// -------------------------------------------------------------------

constSection : 'const' (constDeclaration)+ ;

constDeclaration : attributeSection* IDENT '=' constExpression portabilityDirective* ';'
                 | attributeSection* IDENT ':' typeDefinition '=' typedConstant portabilityDirective* ';' ;

constExpression : expression ;

// Typed constants with specific syntax
typedConstant : constExpression
              | arrayConstant
              | recordConstant ;

// Array constants: (value1, value2, value3)
arrayConstant : '(' typedConstant (',' typedConstant)* ')' ;

// Record constants: (field1: value1; field2: value2)
recordConstant : '(' recordFieldConstant (';' recordFieldConstant)* ')' ;

recordFieldConstant : IDENT ':' typedConstant ;

// -------------------------------------------------------------------
// RESOURCESTRING DECLARATIONS
// -------------------------------------------------------------------

resourcestringSection : 'resourcestring' (resourcestringDecl)+ ;

resourcestringDecl : IDENT '=' constExpression ';' ;

// -------------------------------------------------------------------
// TYPE DECLARATIONS
// -------------------------------------------------------------------

typeSection : 'type' (typeDeclaration)+ ;

typeDeclaration : attributeSection* IDENT genericDecl? '=' 'type'? typeDefinition portabilityDirective* ';' ;

// All possible type definitions
typeDefinition : typeId
               | enumeratedType
               | subrangeType
               | arrayType
               | recordType
               | setType
               | fileType
               | pointerType
               | proceduralType
               | variantType
               | classType
               | classForwardDecl
               | interfaceType
               | interfaceForwardDecl
               | dispInterfaceType
               | classRefType
               | stringType
               | helperType
               | packedType ;

// Packed modifier for compound types
packedType : 'packed' (arrayType | recordType | setType | fileType | classType) ;

// Forward declarations
classForwardDecl : 'class' ;
interfaceForwardDecl : 'interface' ;

// -------------------------------------------------------------------
// TYPE IDENTIFIERS & GENERICS
// -------------------------------------------------------------------

// Type identifier with optional generic instance: TList<Integer>
typeId : qualifiedIdent (genericInstance)? ;

// Qualified identifier: Unit.Type
qualifiedIdent : IDENT ('.' IDENT)* ;

// Generic type instance: <Type1, Type2>
genericInstance : '<' typeList '>' ;

typeList : typeDefinition (',' typeDefinition)* ;

// Generic parameter declaration: <T: class, constructor>
genericDecl : '<' genericParameter (',' genericParameter)* '>' ;

genericParameter : IDENT (':' constraintList)? ;

constraintList : constraint (',' constraint)* ;

constraint : 'class' | 'constructor' | 'record' | typeId ;

// -------------------------------------------------------------------
// HELPER TYPES (XE3+)
// -------------------------------------------------------------------

helperType : ('record' | 'class') 'helper' ('(' typeId ')')? 'for' typeId
             helperBody 'end' ;

helperBody : (helperMember)* ;

helperMember : attributeSection* fieldDecl ';'
             | attributeSection* methodHeadingInType
             | attributeSection* propertyDeclaration ;

// -------------------------------------------------------------------
// ARRAY TYPES
// -------------------------------------------------------------------

arrayType : 'array' '[' ordinalType (',' ordinalType)* ']' 'of' typeDefinition  // Static
          | 'array' 'of' 'const'                                                 // Open array of const
          | 'array' 'of' typeDefinition ;                                         // Dynamic

// Ordinal types for array indices
ordinalType : subrangeType | enumeratedType | typeId ;

// Subrange: 1..100
subrangeType : constExpression '..' constExpression ;

// Enumeration: (value1 = 0, value2, value3 = 5)
enumeratedType : '(' enumeratedElement (',' enumeratedElement)* ')' ;

enumeratedElement : IDENT ('=' constExpression)? ;

// -------------------------------------------------------------------
// RECORD TYPES
// -------------------------------------------------------------------

recordType : 'record' recordBody 'end' ;

recordBody : (recordMember)* (variantSection)? ;

// Record members: fields, methods, properties, nested declarations, operators
recordMember : attributeSection* fieldDecl ';'
             | attributeSection* methodHeadingInType
             | attributeSection* propertyDeclaration
             | attributeSection* recordConstSection
             | attributeSection* recordTypeSection
             | attributeSection* recordVarSection
             | attributeSection* operatorDeclaration
             | classMemberVisibility ;

// Nested const section in record
recordConstSection : 'const' (constDeclaration)+ ;

// Nested type section in record
recordTypeSection : 'type' (typeDeclaration)+ ;

// Nested var section in record
recordVarSection : 'var' (varDeclarationInRecord)+ ;

varDeclarationInRecord : attributeSection* identList ':' typeDefinition ';' ;

// Record field list
fieldList : fieldDecl (';' fieldDecl)* ;

fieldDecl : identList ':' typeDefinition ;

// Variant records: case selector of options
variantSection : 'case' (IDENT ':')? typeId 'of'
                 variantGroup (';' variantGroup)* ';'? ;

variantGroup : constExpression (',' constExpression)* ':' '(' (fieldList ';'?)? ')' ;

// -------------------------------------------------------------------
// SET, FILE, POINTER TYPES
// -------------------------------------------------------------------

setType : 'set' 'of' ordinalType ;

fileType : 'file' ('of' typeDefinition)? ;

pointerType : '^' typeId ;

// -------------------------------------------------------------------
// PROCEDURAL TYPES & METHOD REFERENCES
// -------------------------------------------------------------------

proceduralType : procedureType | functionType | methodReferenceType ;

// Method reference (anonymous method): reference to procedure/function
methodReferenceType : 'reference' 'to' (procedureType | functionType) ;

procedureType : 'procedure' ('(' formalParameterSection (';' formalParameterSection)* ')')? ('of' 'object')? callingConvention? ;

functionType : 'function' ('(' formalParameterSection (';' formalParameterSection)* ')')? ':' resultType ('of' 'object')? callingConvention? ;

callingConvention : 'cdecl' | 'pascal' | 'register' | 'safecall' | 'stdcall' | 'winapi' ;

// -------------------------------------------------------------------
// CLASS DECLARATIONS
// -------------------------------------------------------------------

classType : 'class' ('sealed' | 'abstract')? (heritage)? classBody 'end' ;

// Class inheritance: (BaseClass, Interface1, Interface2)
heritage : '(' classTypeList ')' ;

classTypeList : typeId (',' typeId)* ;

classBody : (classMemberSection)* ;

// Class sections with visibility modifiers
classMemberSection : (classMemberVisibility)? (classMember)+ ;

classMemberVisibility : 'private'
                      | 'protected'
                      | 'public'
                      | 'published'
                      | 'strict' 'private'
                      | 'strict' 'protected' ;

// Class members: fields, methods, properties, class vars, nested types/consts
classMember : attributeSection* fieldDecl ';'
            | attributeSection* methodHeadingInType
            | attributeSection* propertyDeclaration
            | attributeSection* classVarSection
            | attributeSection* classConstSection
            | attributeSection* classTypeSection ;

// Class variables: class var Field: Type;
classVarSection : 'class' 'var' (varDeclarationInRecord)+ ;

// Nested const in class
classConstSection : 'const' (constDeclaration)+ ;

// Nested type in class
classTypeSection : 'type' (typeDeclaration)+ ;

// -------------------------------------------------------------------
// METHOD HEADINGS (inside type declarations)
// -------------------------------------------------------------------

// Unified method headings inside class/record/interface types
methodHeadingInType : ('class')? 'procedure' IDENT genericDecl? ('(' formalParameterSection (';' formalParameterSection)* ')')? ';' methodDirectiveList
                    | ('class')? 'function' IDENT genericDecl? ('(' formalParameterSection (';' formalParameterSection)* ')')? ':' resultType ';' methodDirectiveList
                    | 'constructor' IDENT ('(' formalParameterSection (';' formalParameterSection)* ')')? ';' methodDirectiveList
                    | 'destructor' IDENT ('(' formalParameterSection (';' formalParameterSection)* ')')? ';' methodDirectiveList ;

// Method directives can appear after the semicolon
methodDirectiveList : (methodDirective ';')* ;

methodDirective : 'virtual'
                | 'dynamic'
                | 'abstract'
                | 'override'
                | 'overload'
                | 'reintroduce'
                | 'static'
                | 'inline'
                | 'final'
                | 'cdecl'
                | 'pascal'
                | 'register'
                | 'safecall'
                | 'stdcall'
                | 'winapi'
                | 'varargs'
                | 'message' constExpression
                | 'deprecated' (STRING)?
                | 'platform'
                | 'experimental'
                | 'library'
                | 'unsafe' ;

// -------------------------------------------------------------------
// OPERATOR OVERLOADING
// -------------------------------------------------------------------

operatorDeclaration : 'class' 'operator' operatorName
                      '(' formalParameterSection (';' formalParameterSection)* ')'
                      ':' resultType ';' (methodDirective ';')* ;

operatorName : 'implicit' | 'explicit' | 'negative' | 'positive' | 'inc' | 'dec'
             | 'logicalnot' | 'trunc' | 'round' | 'in' | 'equal' | 'notequal'
             | 'greaterthan' | 'greaterthanorequal' | 'lessthan' | 'lessthanorequal'
             | 'add' | 'subtract' | 'multiply' | 'divide' | 'intdivide' | 'modulus'
             | 'leftshift' | 'rightshift' | 'logicaland' | 'logicalor' | 'logicalxor'
             | 'bitwiseand' | 'bitwiseor' | 'bitwisexor' ;

// -------------------------------------------------------------------
// PROPERTIES
// -------------------------------------------------------------------

// Properties with read/write/index specifiers and array indexers
propertyDeclaration : ('class')? 'property' IDENT (propertyIndex)? (':' typeDefinition)? (propertySpecifier)* ';' (propertyDefault ';')? ;

// Array property index: property Items[Index: Integer]: T
propertyIndex : '[' propertyParameterList ']' ;

propertyParameterList : propertyParameter (';' propertyParameter)* ;

propertyParameter : ('const' | 'var')? identList ':' typeId ;

propertySpecifier : 'read' qualifiedIdent
                  | 'write' qualifiedIdent
                  | 'stored' (qualifiedIdent | constExpression)
                  | 'index' constExpression
                  | 'implements' typeId (',' typeId)* ;

propertyDefault : 'default' constExpression?
                | 'nodefault' ;

// -------------------------------------------------------------------
// INTERFACE TYPES
// -------------------------------------------------------------------

interfaceType : 'interface' (heritage)? ('[' GUID ']')? (interfaceBody)? 'end' ;

interfaceBody : (interfaceMember)* ;

interfaceMember : attributeSection* 'procedure' IDENT ('(' formalParameterSection (';' formalParameterSection)* ')')? ';' (methodDirective ';')*
                | attributeSection* 'function' IDENT ('(' formalParameterSection (';' formalParameterSection)* ')')? ':' resultType ';' (methodDirective ';')*
                | attributeSection* propertyDeclaration ;

// -------------------------------------------------------------------
// DISPINTERFACE TYPES
// -------------------------------------------------------------------

dispInterfaceType : 'dispinterface' (heritage)? ('[' GUID ']')? (dispInterfaceBody)? 'end' ;

dispInterfaceBody : (dispInterfaceMember)* ;

dispInterfaceMember : 'procedure' IDENT ('(' formalParameterSection (';' formalParameterSection)* ')')? ';' dispDirectiveList
                    | 'function' IDENT ('(' formalParameterSection (';' formalParameterSection)* ')')? ':' resultType ';' dispDirectiveList
                    | propertyDeclaration
                    | 'dispid' constExpression ';' ;

dispDirectiveList : ('dispid' constExpression ';')? ;

// -------------------------------------------------------------------
// CLASS REFERENCE TYPES
// -------------------------------------------------------------------

classRefType : 'class' 'of' typeId ;

// -------------------------------------------------------------------
// STRING TYPES
// -------------------------------------------------------------------

stringType : 'string' ('[' constExpression ']')?
           | 'ansistring' ('(' constExpression ')')?
           | 'widestring'
           | 'unicodestring'
           | 'shortstring'
           | 'rawbytestring'
           | 'utf8string' ;

// Variant types for dynamic typing
variantType : 'variant' | 'olevariant' ;

// -------------------------------------------------------------------
// VARIABLE DECLARATIONS
// -------------------------------------------------------------------

varSection : 'var' (varDeclaration)+ ;

threadvarSection : 'threadvar' (varDeclaration)+ ;

varDeclaration : attributeSection* identList ':' typeDefinition ('=' constExpression)? portabilityDirective* ';'
               | attributeSection* identList ':' typeDefinition 'absolute' (qualifiedIdent | constExpression) ';'
               | attributeSection* '[' 'weak' ']' IDENT ':' typeDefinition ';'
               | attributeSection* '[' 'unsafe' ']' IDENT ':' typeDefinition ';' ;

// Inline variables (Delphi 10.3+): var x := value;
inlineVarDecl : 'var' IDENT (':' typeDefinition)? ':=' expression
              | 'const' IDENT (':' typeDefinition)? ':=' expression ;

// Comma-separated identifier lists
identList : IDENT (',' IDENT)* ;

// -------------------------------------------------------------------
// LABEL DECLARATIONS
// -------------------------------------------------------------------

labelDeclSection : 'label' labelId (',' labelId)* ';' ;

labelId : IDENT | NUMBER ;

// -------------------------------------------------------------------
// PROCEDURE / FUNCTION DECLARATIONS (implementation)
// -------------------------------------------------------------------

// Qualified names for method implementations: TMyClass.MethodName
// Also supports generic types: TMyClass<T>.MethodName
procedureDeclaration : attributeSection* ('class')? procedureHeading ';' directiveList (nestedDeclaration)* block ';'
                     | attributeSection* ('class')? procedureHeading ';' 'external' externalSpecifier ';'
                     | attributeSection* ('class')? procedureHeading ';' 'forward' ';' ;

functionDeclaration : attributeSection* ('class')? functionHeading ';' directiveList (nestedDeclaration)* block ';'
                    | attributeSection* ('class')? functionHeading ';' 'external' externalSpecifier ';'
                    | attributeSection* ('class')? functionHeading ';' 'forward' ';' ;

constructorImpl : attributeSection* 'constructor' qualifiedMethodName
                  ('(' formalParameterSection (';' formalParameterSection)* ')')? ';'
                  directiveList (nestedDeclaration)* block ';' ;

destructorImpl : attributeSection* 'destructor' qualifiedMethodName
                 ('(' formalParameterSection (';' formalParameterSection)* ')')? ';'
                 directiveList (nestedDeclaration)* block ';' ;

// External procedure linkage
externalSpecifier : (STRING | qualifiedIdent)? ('name' constExpression)? ('index' constExpression)? ('delayed')? ;

// Nested procedures/functions
nestedDeclaration : procedureDeclaration | functionDeclaration ;

// Procedure/function headers with qualified names for implementations
procedureHeading : 'procedure' qualifiedMethodName genericDecl?
                   ('(' formalParameterSection (';' formalParameterSection)* ')')? ;

functionHeading : 'function' qualifiedMethodName genericDecl?
                  ('(' formalParameterSection (';' formalParameterSection)* ')')? ':' resultType ;

// Qualified method name: TClass.Method or TClass<T>.Method
qualifiedMethodName : IDENT (genericInstance? '.' IDENT)* ;

// -------------------------------------------------------------------
// PARAMETERS
// -------------------------------------------------------------------

formalParameterSection : parameterModifier? parameterGroup ;

parameterModifier : 'var' | 'const' | 'out' | '[' 'ref' ']' ;

parameterGroup : identList (':' parameterType)? ('=' constExpression)? ;

parameterType : 'array' 'of' 'const'      // Open array of const
              | 'array' 'of' typeId        // Open array
              | typeId ;

resultType : typeDefinition ;

// -------------------------------------------------------------------
// DIRECTIVES
// -------------------------------------------------------------------

directive : 'inline'
          | 'assembler'
          | 'overload'
          | 'reintroduce'
          | 'virtual'
          | 'dynamic'
          | 'abstract'
          | 'override'
          | 'final'
          | 'static'
          | callingConvention
          | 'varargs'
          | 'local'
          | 'export'
          | portabilityDirective
          | 'message' constExpression
          | 'dispid' constExpression ;

portabilityDirective : 'platform'
                     | 'deprecated' (STRING)?
                     | 'library'
                     | 'experimental'
                     | 'unsafe' ;

// -------------------------------------------------------------------
// BLOCKS & STATEMENTS
// -------------------------------------------------------------------

block : (declarationSection)* compoundStatement ;

compoundStatement : 'begin' statementList 'end' ;

statementList : (statement (';' statement)*)? ;

statement : simpleStatement
          | structuredStatement
          | inlineVarDecl
          | labelStatement ;

labelStatement : labelId ':' statement ;

// -------------------------------------------------------------------
// SIMPLE STATEMENTS
// -------------------------------------------------------------------

simpleStatement : designator ':=' expression                          // Assignment
                | designator '+=' expression                          // Addition assignment (non-standard but widely used)
                | designator '-=' expression                          // Subtraction assignment
                | designator                                          // Method call (parameterless or via designator)
                | 'inherited' (IDENT ('(' expressionList ')')?)?      // Inherited call
                | 'goto' labelId                                      // Goto
                | 'raise' (expression ('at' expression)?)?            // Exception raising
                | 'exit' ('(' expression ')')?                        // Exit with optional result
                | /* empty */ ;                                        // Empty statement

// -------------------------------------------------------------------
// STRUCTURED STATEMENTS
// -------------------------------------------------------------------

structuredStatement : compoundStatement
                    | conditionalStatement
                    | loopStatement
                    | withStatement
                    | exceptionStatement
                    | assemblerStatement ;

// Conditional statements
conditionalStatement : ifStatement | caseStatement ;

ifStatement : 'if' expression 'then' statement ('else' statement)? ;

caseStatement : 'case' expression 'of'
                caseSelector (';' caseSelector)*
                (';'? 'else' statementList (';')?)?
                'end' ;

caseSelector : caseLabelList ':' statement ;

caseLabelList : caseLabel (',' caseLabel)* ;

caseLabel : constExpression ('..' constExpression)? ;

// -------------------------------------------------------------------
// LOOP STATEMENTS
// -------------------------------------------------------------------

loopStatement : repeatStatement
              | whileStatement
              | forStatement ;

repeatStatement : 'repeat' statementList 'until' expression ;

whileStatement : 'while' expression 'do' statement ;

forStatement : 'for' forLoopVar ':=' expression ('to' | 'downto') expression 'do' statement
             | 'for' forInVar 'in' expression 'do' statement ;

// For-loop variable: plain IDENT or inline var
forLoopVar : IDENT
           | 'var' IDENT (':' typeDefinition)? ;

forInVar : IDENT
         | 'var' IDENT (':' typeDefinition)? ;

// -------------------------------------------------------------------
// WITH STATEMENT
// -------------------------------------------------------------------

withStatement : 'with' expression (',' expression)* 'do' statement ;

// -------------------------------------------------------------------
// EXCEPTION HANDLING
// -------------------------------------------------------------------

exceptionStatement : tryExceptStatement | tryFinallyStatement ;

tryExceptStatement : 'try' statementList 'except' exceptionBlock 'end' ;

tryFinallyStatement : 'try' statementList 'finally' statementList 'end' ;

exceptionBlock : (exceptionHandler (';')?)+ ('else' statementList)?
               | statementList ;

exceptionHandler : 'on' (IDENT ':')? typeId 'do' statement ;

// -------------------------------------------------------------------
// INLINE ASSEMBLY
// -------------------------------------------------------------------

assemblerStatement : 'asm' asmContent 'end' ;

// Assembly content is treated as opaque text until 'end'
// Full assembly parsing requires a separate mode/grammar
asmContent : (~'end')* ;

// -------------------------------------------------------------------
// EXPRESSIONS
// -------------------------------------------------------------------

expression : simpleExpression (relOp simpleExpression)? ;

simpleExpression : ('+' | '-')? term (addOp term)* ;

term : factor (mulOp factor)* ;

factor : designator ('(' expressionList ')')?        // Variable access or function call
       | '@' designator                              // Address operator
       | '@' '@' designator                          // Double address-of (procedural)
       | NUMBER                                      // Numeric literals
       | STRING                                      // String literals
       | MULTILINESTRING                             // Multiline strings (Delphi 12+)
       | NIL                                         // Nil pointer
       | 'True' | 'False'                            // Boolean literals
       | '(' expression ')'                          // Parenthesized expression
       | 'not' factor                                // Boolean/bitwise negation
       | bracketConstructor                          // Set/array constructors
       | 'inherited' (IDENT ('(' expressionList ')')?)? // Inherited expression
       | anonymousMethod                             // Anonymous method literal
       | 'typeof' '(' typeId ')'                     // Type reference operator
       | 'sizeof' '(' (typeId | designator) ')'      // Size operator
       | 'default' '(' typeId ')'                    // Default value (generics)
       | 'assigned' '(' expression ')'               // Assigned check
       | 'low' '(' (typeId | expression) ')'         // Low bound
       | 'high' '(' (typeId | expression) ')'        // High bound
       | 'length' '(' expression (',' expression)? ')' // Length function
       | 'pred' '(' expression ')'                   // Predecessor
       | 'succ' '(' expression ')'                   // Successor
       | 'ord' '(' expression ')'                    // Ordinal value
       | 'chr' '(' expression ')'                    // Character from ordinal
       | 'abs' '(' expression ')'                    // Absolute value
       | 'include' '(' expression ',' expression ')' // Set include
       | 'exclude' '(' expression ',' expression ')' // Set exclude
       | typeId '(' expression ')' ;                  // Type casting

// Anonymous method literals (Delphi 2009+)
anonymousMethod : 'procedure' ('(' formalParameterSection (';' formalParameterSection)* ')')? block
                | 'function' ('(' formalParameterSection (';' formalParameterSection)* ')')? ':' resultType block ;

// Unified constructor for sets and dynamic arrays: [1, 2, 3]
bracketConstructor : '[' (constructorElementList)? ']' ;

constructorElementList : constructorElement (',' constructorElement)* ;

constructorElement : expression ('..' expression)? ;

// -------------------------------------------------------------------
// DESIGNATORS
// -------------------------------------------------------------------

// Designators: variable.field[index]^<GenericArgs>
designator : qualifiedIdent (designatorPart)* ;

designatorPart : '.' IDENT
               | '[' expressionList ']'
               | '^'
               | '(' expressionList ')'     // Function/method call
               | '(' ')'                    // Parameterless function call with parens
               | genericInstance ;           // Generic instantiation

fieldDesignator : qualifiedIdent ;

// Expression lists for parameters and array indices
expressionList : expression (',' expression)* ;

// -------------------------------------------------------------------
// OPERATORS
// -------------------------------------------------------------------

relOp : '='  | '<>' | '<'  | '<=' | '>'  | '>='
      | 'in' | 'is' | 'as' ;

addOp : '+' | '-' | 'or' | 'xor' ;

mulOp : '*' | '/' | 'div' | 'mod' | 'and' | 'shl' | 'shr' ;

// -------------------------------------------------------------------
// LEXER RULES
// -------------------------------------------------------------------

// Identifiers: names for variables, types, etc.
// Note: Delphi identifiers are case-insensitive; handle in semantic layer
// Identifiers may also use & prefix to escape keywords: &type, &begin
IDENT : '&'? [a-zA-Z_][a-zA-Z0-9_]* ;

// Numeric literals
NUMBER : DIGIT+ ('.' DIGIT+)? (('E' | 'e') ('+' | '-')? DIGIT+)?  // Decimal/float
       | '$' HEX_DIGIT+                                             // Hexadecimal: $FF
       | '&' [0-7]+                                                 // Octal: &777 (rare)
       | '%' [01]+ ;                                                // Binary: %1010

// Character literal sequences: #13#10, #$0D#$0A
CHAR_LITERAL : ('#' (DIGIT+ | '$' HEX_DIGIT+))+ ;

// String literals with embedded character codes
STRING : ( '\'' (~['\r\n] | '\'\'')* '\'' | CHAR_LITERAL )
         ( ( '\'' (~['\r\n] | '\'\'')* '\'' | CHAR_LITERAL ) )* ;

// Multiline string literals (Delphi 12+): '''content'''
MULTILINESTRING : '\'\'\'' .*? '\'\'\'' ;

// GUID literals: ['{12345678-1234-1234-1234-123456789ABC}']
// GUIDs in Delphi are typically strings, but the interface GUID is in brackets
GUID : '\'' '{' HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT
       '-' HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT
       '-' HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT
       '-' HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT
       '-' HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT
       '}' '\'' ;

// Nil keyword
NIL : 'nil' ;

// Lexer fragments
fragment DIGIT : [0-9] ;
fragment HEX_DIGIT : [0-9A-Fa-f] ;

// Whitespace (ignored)
WS : [ \t\r\n]+ -> skip ;

// Comments (ignored)
COMMENT : '{' ~[$] ~[}]* '}' -> skip ;       // Brace comments (not directives)
BLOCK_COMMENT : '(*' .*? '*)' -> skip ;       // Parenthesis comments
LINE_COMMENT : '//' ~[\r\n]* -> skip ;        // Line comments

// Compiler directives (ignored for parsing purposes)
COMPILER_DIRECTIVE : '{$' ~[}]* '}' -> skip ;

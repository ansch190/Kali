grammar Delphi;

// ===================================================================
// DELPHI GRAMMAR for Version 12.3+ (2025)
// 
// Complete ANTLR4 grammar for modern Delphi language
// Supports: Inline Variables, Generics, Attributes, Helper Types,
// Anonymous Methods, Operator Overloading and current features
// ===================================================================

// Main file structure - programs, units, libraries or packages
file : (program_ | unit_ | library_ | package_) EOF ;

// Program structure: program Name; uses...; declarations; begin..end.
program_ : 'program' IDENT ('(' identList ')')? ';'
          (usesClause)?
          (declarationSection)*
          compoundStatement '.' ;

// Unit structure: unit Name; interface..implementation..end.
unit_ : 'unit' IDENT ('.' IDENT)* ';'
       interfaceSection
       implementationSection
       (initializationSection)?
       (finalizationSection)?
       'end' '.' ;

// Library structure: library Name; declarations; end.
library_ : 'library' IDENT ';'
          (usesClause)?
          (declarationSection)*
          (initializationSection)?
          (finalizationSection)?
          'end' '.' ;

// Package structure: package Name; requires...; contains...; end.
package_ : 'package' IDENT ';'
          (requiresClause)?
          (containsClause)?
          'end' '.' ;

// Interface section with public declarations
interfaceSection : 'interface' (usesClause)? (interfaceDecl)* ;

// Implementation section with private code
implementationSection : 'implementation' (usesClause)? (declarationSection)* ;

// Optional initialization section
initializationSection : 'initialization' statementList ;

// Optional finalization section
finalizationSection : 'finalization' statementList ;

// Uses clause: uses Unit1, Unit2 in 'file.pas';
usesClause : 'uses' usedUnit (',' usedUnit)* ';' ;

usedUnit : IDENT ('.' IDENT)* ('in' STRING)? ;

// Package requires clause
requiresClause : 'requires' IDENT (',' IDENT)* ';' ;

// Package contains clause
containsClause : 'contains' IDENT ('in' STRING)? (',' IDENT ('in' STRING)?)* ';' ;

// Interface declarations (public)
interfaceDecl : constSection
              | typeSection
              | varSection
              | procedureHeading ';' (directive)*    // Forward declarations
              | functionHeading ';' (directive)* ;

// Implementation declarations (private)
declarationSection : labelDeclSection
                   | constSection
                   | typeSection
                   | varSection
                   | procedureDeclaration
                   | functionDeclaration ;

// Custom attributes for RTTI: [AttributeName(params)]
attributeSection : '[' attributeList ']' ;

attributeList : attribute (',' attribute)* ;

attribute : IDENT (attributeArguments)? ;

attributeArguments : '(' (attributeArgumentList)? ')' ;

attributeArgumentList : attributeArgument (',' attributeArgument)* ;

attributeArgument : (IDENT '=')? expression ;

// Constant declarations: const NAME = value;
constSection : 'const' (constDeclaration)+ ;

constDeclaration : attributeSection* IDENT '=' constExpression ';'
                 | attributeSection* IDENT ':' typeId '=' typedConstant ';' ;

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

// Type declarations: type NAME = definition;
typeSection : 'type' (typeDeclaration)+ ;

typeDeclaration : attributeSection* IDENT genericDecl? '=' typeDefinition ';'
                | attributeSection* IDENT genericDecl? '=' 'type' typeDefinition ';' ;

// All possible type definitions
typeDefinition : typeId
               | arrayType
               | recordType
               | setType
               | fileType
               | pointerType
               | proceduralType
               | variantType
               | classType
               | interfaceType
               | classRefType
               | stringType
               | helperType ;

// Type identifier with optional generic instance: TList<Integer>
typeId : IDENT ('.' IDENT)* (genericInstance)? ;

// Generic type instance: <Type1, Type2>
genericInstance : '<' typeList '>' ;

typeList : typeDefinition (',' typeDefinition)* ;

// Generic parameter declaration: <T: class, constructor>
genericDecl : '<' genericParameter (',' genericParameter)* '>' ;

genericParameter : IDENT (':' constraintList)? ;

constraintList : constraint (';' constraint)* ;

constraint : 'class' | 'constructor' | 'record' | typeId ;

// Helper types (XE3+): record helper for Integer
helperType : 'record' 'helper' 'for' typeId helperBody 'end'
           | 'class' 'helper' 'for' typeId helperBody 'end' ;

helperBody : (helperMember)* ;

helperMember : attributeSection* fieldDecl ';'
             | attributeSection* methodDeclaration
             | attributeSection* propertyDeclaration ;

// Array types: array[1..10] of Integer; array of String
arrayType : 'array' ('[' ordinalType (',' ordinalType)* ']')? 'of' typeDefinition
          | 'packed' 'array' '[' ordinalType (',' ordinalType)* ']' 'of' typeDefinition
          | 'array' 'of' typeDefinition ;  // Dynamic arrays

// Ordinal types for array indices
ordinalType : subrangeType | enumeratedType | typeId ;

// Subrange: 1..100
subrangeType : constExpression '..' constExpression ;

// Enumeration: (value1, value2, value3)
enumeratedType : '(' identList ')' ;

// Record types with optional variant part
recordType : 'packed'? 'record' recordBody 'end' ;

recordBody : (recordMember)* (variantSection)? ;

// Record members: fields, methods, properties, nested declarations
recordMember : attributeSection* fieldDecl ';'
             | attributeSection* methodDeclaration
             | attributeSection* propertyDeclaration
             | attributeSection* recordConstSection
             | attributeSection* recordTypeSection
             | attributeSection* recordVarSection
             | attributeSection* operatorDeclaration ;

// Nested const section in record
recordConstSection : 'const' (constDeclaration)+ ;

// Nested type section in record
recordTypeSection : 'type' (typeDeclaration)+ ;

// Nested var section in record
recordVarSection : 'var' identList ':' typeDefinition ';' ;

// Record field list
fieldList : fieldDecl (';' fieldDecl)* ;

fieldDecl : identList ':' typeDefinition ;

// Variant records: case selector of options
variantSection : 'case' (IDENT ':')? typeId 'of'
                variantGroup (';' variantGroup)* ;

variantGroup : constExpression (',' constExpression)* ':' '(' (fieldList)? ')' ;

// Set types: set of Byte
setType : 'set' 'of' ordinalType ;

// File types: file of Record; file (untyped)
fileType : 'file' ('of' typeDefinition)? ;

// Pointer types: ^Integer
pointerType : '^' typeId ;

// Procedural types and method references
proceduralType : procedureType | functionType | methodReferenceType ;

// Method reference (anonymous method): reference to procedure
methodReferenceType : 'reference' 'to' (procedureType | functionType) ;

procedureType : 'procedure' (formalParameterList)? ('of' 'object')? ;

functionType : 'function' (formalParameterList)? ':' resultType ('of' 'object')? ;

// Class declarations with inheritance and members
classType : 'class' ('sealed' | 'abstract')? (heritage)? (classBody)? 'end' ;

// Class inheritance: (BaseClass, Interface1, Interface2)
heritage : '(' classTypeList ')' ;

classTypeList : typeId (',' typeId)* ;

classBody : (classMemberSection)* ;

// Class sections with visibility modifiers
classMemberSection : (classVisibility)? (classMember)+ ;

classVisibility : 'private' | 'protected' | 'public' | 'published' 
                | 'strict' ('private' | 'protected') ;

// Class members: fields, methods, properties
classMember : attributeSection* fieldDecl ';'
            | attributeSection* methodDeclaration
            | attributeSection* propertyDeclaration
            | attributeSection* classVarDecl ';' ;

// Class variables: class var Field: Type;
classVarDecl : 'class' 'var' identList ':' typeDefinition ;

// All method types
methodDeclaration : procedureDeclaration
                  | functionDeclaration
                  | constructorDeclaration
                  | destructorDeclaration
                  | operatorDeclaration ;

// Operator overloading: class operator Add(a, b: T): T;
operatorDeclaration : 'class' 'operator' operatorName '(' formalParameterList ')' ':' resultType ';' (directive)* ;

// Supported operator names
operatorName : 'implicit' | 'explicit' | 'negative' | 'positive' | 'inc' | 'dec'
             | 'logicalnot' | 'trunc' | 'round' | 'in' | 'equal' | 'notequal'
             | 'greaterthan' | 'greaterthanorequal' | 'lessthan' | 'lessthanorequal'
             | 'add' | 'subtract' | 'multiply' | 'divide' | 'intdivide' | 'modulus'
             | 'leftshift' | 'rightshift' | 'logicaland' | 'logicalor' | 'logicalxor'
             | 'bitwiseand' | 'bitwiseor' | 'bitwisexor' ;

constructorDeclaration : 'constructor' IDENT (formalParameterList)? ';' (directive)* ;

destructorDeclaration : 'destructor' IDENT (formalParameterList)? ';' (directive)* ;

// Properties with read/write specifiers
propertyDeclaration : 'property' IDENT (':' typeId)? (propertyInterface)* ';' (directive)* ;

propertyInterface : 'read' fieldDesignator
                  | 'write' fieldDesignator
                  | 'stored' (IDENT | constExpression)
                  | 'default' constExpression
                  | 'nodefault'
                  | 'implements' typeId ;

// Interface declarations with optional GUID
interfaceType : 'interface' (heritage)? ('(' GUID ')')? (interfaceBody)? 'end' ;

interfaceBody : (interfaceMember)* ;

interfaceMember : procedureHeading ';'
                | functionHeading ';'
                | propertyDeclaration ;

// Class reference types: class of TObject
classRefType : 'class' 'of' typeId ;

// String types with optional length specifiers
stringType : 'string' ('[' constExpression ']')?
           | 'ansistring' ('(' constExpression ')')?
           | 'widestring'
           | 'unicodestring' ;

// Variant types for dynamic typing
variantType : 'variant' | 'olevariant' ;

// Variable declarations: var Name: Type;
varSection : 'var' (varDeclaration)+ ;

varDeclaration : attributeSection* identList ':' typeDefinition ('=' constExpression)? ';'
               | attributeSection* identList ':' typeDefinition 'absolute' (IDENT | constExpression) ';'
               | attributeSection* '[' 'weak' ']' IDENT ':' typeDefinition ';' ;  // Weak references

// Inline variables (Delphi 10.3+): var x := value;
inlineVarDecl : 'var' IDENT (':' typeDefinition)? ':=' expression
              | 'const' IDENT (':' typeDefinition)? '=' constExpression ;

// Comma-separated identifier lists
identList : IDENT (',' IDENT)* ;

// Label declarations for goto statements
labelDeclSection : 'label' labelId (',' labelId)* ';' ;

labelId : IDENT | NUMBER ;

// Procedure declarations with optional nested functions
procedureDeclaration : procedureHeading ';' (directive)* (nestedDeclaration)* block ';' ;

functionDeclaration : functionHeading ';' (directive)* (nestedDeclaration)* block ';' ;

// Nested procedures/functions (Pascal feature)
nestedDeclaration : procedureDeclaration | functionDeclaration ;

// Procedure/function headers
procedureHeading : 'procedure' IDENT genericDecl? (formalParameterList)? ;

functionHeading : 'function' IDENT genericDecl? (formalParameterList)? ':' resultType ;

// Parameter lists: (param1: Type; var param2: Type)
formalParameterList : '(' formalParameterSection (';' formalParameterSection)* ')' ;

formalParameterSection : parameterGroup
                       | 'var' parameterGroup      // By reference
                       | 'const' parameterGroup    // Read-only reference
                       | 'out' parameterGroup ;    // Output parameter

parameterGroup : identList (':' parameterType)? ('=' constExpression)? ;

// Parameter types with optional array modifier
parameterType : ('array' 'of')? typeId ;

resultType : typeId ;

// Procedure/function directives
directive : 'inline'
          | 'assembler'
          | 'forward'
          | 'external' (STRING)? ('name' STRING)?
          | 'public' ('name' STRING)?
          | 'export'
          | 'far'
          | 'near'
          | 'resident'
          | 'virtual'
          | 'dynamic'
          | 'abstract'
          | 'override'
          | 'overload'
          | 'reintroduce'
          | 'static'
          | 'cdecl'
          | 'pascal'
          | 'register'
          | 'safecall'
          | 'stdcall'
          | 'varargs'
          | 'local'
          | 'platform'
          | 'deprecated' (STRING)?
          | 'library'
          | 'experimental'
          | 'unsafe'
          | 'final' ;

// Code blocks with declarations and statements
block : (declarationSection)* compoundStatement ;

// Compound statements: begin..end
compoundStatement : 'begin' statementList 'end' ;

statementList : statement (';' statement)* ;

// All statement types
statement : simpleStatement
          | structuredStatement
          | inlineVarDecl          // Inline variable declarations
          | labelStatement ;

// Labeled statements for goto
labelStatement : labelId ':' statement ;

// Simple statements: assignments, calls, control flow
simpleStatement : designator ':=' expression          // Assignment
                | designator ('(' expressionList ')')? // Method call
                | 'inherited' (IDENT ('(' expressionList ')')?)? // Inherited call
                | 'goto' labelId                      // Goto statement
                | 'raise' (expression ('at' expression)?)? // Exception raising
                | 'exit' ('(' expression ')')?        // Exit with optional result
                | /* empty */ ;                       // Empty statement

// Structured statements: compounds, conditionals, loops, etc.
structuredStatement : compoundStatement
                    | conditionalStatement
                    | loopStatement
                    | withStatement
                    | exceptionStatement
                    | assemblerStatement ;

// Conditional statements: if and case
conditionalStatement : ifStatement | caseStatement ;

// If-then-else statements
ifStatement : 'if' expression 'then' statement ('else' statement)? ;

// Case statements with optional else clause
caseStatement : 'case' expression 'of'
               caseSelector (';' caseSelector)*
               ('else' statementList)?
               'end' ;

caseSelector : caseLabelList ':' statement ;

caseLabelList : caseLabel (',' caseLabel)* ;

// Case labels with optional ranges: 1, 2..5, 'A'..'Z'
caseLabel : constExpression ('..' constExpression)? ;

// Loop statements: repeat, while, for
loopStatement : repeatStatement
              | whileStatement
              | forStatement ;

// Repeat-until loops
repeatStatement : 'repeat' statementList 'until' expression ;

// While-do loops
whileStatement : 'while' expression 'do' statement ;

// For loops: for-to/downto and for-in with inline variables
forStatement : 'for' (inlineVarDecl | IDENT ':=' expression) ('to' | 'downto') expression 'do' statement
             | 'for' (inlineVarDecl | IDENT) 'in' expression 'do' statement ;

// With statements for record access
withStatement : 'with' designator (',' designator)* 'do' statement ;

// Exception handling: try-except and try-finally
exceptionStatement : tryExceptStatement | tryFinallyStatement ;

tryExceptStatement : 'try' statementList 'except' exceptionBlock 'end' ;

tryFinallyStatement : 'try' statementList 'finally' statementList 'end' ;

exceptionBlock : (exceptionHandler)* ('else' statementList)? ;

// Exception handlers: on Exception do statement
exceptionHandler : 'on' (IDENT ':')? typeId 'do' statement ;

// Inline assembly blocks
assemblerStatement : 'asm' assemblerCode 'end' ;

assemblerCode : ~('end')* ;

// Expression hierarchy: relational -> additive -> multiplicative -> factors
expression : simpleExpression (relOp simpleExpression)? ;

simpleExpression : ('+' | '-')? term (addOp term)* ;

term : factor (mulOp factor)* ;

// Factors: variables, literals, function calls, operators
factor : designator ('(' expressionList ')')?        // Variable access or function call
       | '@' designator                              // Address operator
       | NUMBER                                      // Numeric literals
       | STRING                                      // String literals
       | MULTILINESTRING                             // Multiline strings (Delphi 12+)
       | NIL                                         // Nil pointer
       | 'True' | 'False'                           // Boolean literals
       | '(' expression ')'                         // Parenthesized expression
       | 'not' factor                               // Boolean negation
       | bracketConstructor                         // Set/array constructors
       | 'inherited' IDENT                          // Inherited identifier
       | anonymousMethod                            // Anonymous method literal
       | 'typeof' '(' typeId ')'                    // Type reference operator
       | 'sizeof' '(' (typeId | designator) ')'    // Size operator
       | typeId '(' expression ')' ;                // Type casting

// Anonymous method literals (Delphi 2009+)
anonymousMethod : 'procedure' (formalParameterList)? block
                | 'function' (formalParameterList)? ':' resultType block ;

// Unified constructor for sets and dynamic arrays: [1, 2, 3]
bracketConstructor : '[' (constructorElementList)? ']' ;

constructorElementList : constructorElement (',' constructorElement)* ;

// Constructor elements with optional ranges for sets: 1..10
constructorElement : expression ('..' expression)? ;

// Designators: variable.field[index]^
designator : IDENT ('.' IDENT | '[' expressionList ']' | '^')* ;

fieldDesignator : IDENT ;

// Expression lists for parameters and array indices
expressionList : expression (',' expression)* ;

// Relational operators
relOp : '=' | '<>' | '<' | '<=' | '>' | '>=' | 'in' | 'is' | 'as' ;

// Additive operators
addOp : '+' | '-' | 'or' | 'xor' ;

// Multiplicative operators
mulOp : '*' | '/' | 'div' | 'mod' | 'and' | 'shl' | 'shr' ;

// ===================================================================
// LEXER RULES
// ===================================================================

// Identifiers: names for variables, types, etc.
IDENT : [a-zA-Z_][a-zA-Z0-9_]* ;

// Numeric literals: decimal, hex, octal, binary, character codes
NUMBER : [0-9]+ ('.' [0-9]+)? (('E' | 'e') ('+' | '-')? [0-9]+)?  // Decimal with optional exponent
       | '$' [0-9A-Fa-f]+                                          // Hexadecimal: $FF
       | '&' [0-7]+                                                // Octal: &777
       | '%' [01]+                                                 // Binary: %1010
       | '#' [0-9]+                                               // Character code: #13
       | '#' '$' [0-9A-Fa-f]+ ;                                   // Hex character: #$0D

// String literals with escape sequences
STRING : '\'' (~['\r\n] | '\'\'')* '\''                          // Single-quoted strings with '' escape
       | '"' (~["\r\n] | '""')* '"'                              // Double-quoted strings with "" escape
       | '#' [0-9]+                                              // Character literal
       | '#' '$' [0-9A-Fa-f]+ ;                                  // Hex character literal

// Multiline string literals (Delphi 12+): '''content'''
MULTILINESTRING : '\'\'\'' .*? '\'\'\'' ;

// GUID literals: {12345678-1234-1234-1234-123456789ABC}
GUID : '{' [0-9A-Fa-f]{8} '-' [0-9A-Fa-f]{4} '-' [0-9A-Fa-f]{4} '-' [0-9A-Fa-f]{4} '-' [0-9A-Fa-f]{12} '}' ;

// Nil keyword
NIL : 'nil' ;

// Whitespace: spaces, tabs, newlines (ignored)
WS : [ \t\r\n]+ -> skip ;

// Comments: {}, (*..*), //
COMMENT : '{' (~[}])* '}' -> skip                                // Brace comments
        | '(*' .*? '*)' -> skip                                  // Parenthesis comments
        | '//' ~[\r\n]* -> skip ;                                // Line comments

// Compiler directives: {$IFDEF}, {$INCLUDE}, etc. (ignored)
COMPILER_DIRECTIVE : '{$' (~[}])* '}' -> skip ;

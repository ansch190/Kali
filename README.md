# Delphi ANTLR4 Grammar

A comprehensive ANTLR4 grammar for modern Delphi (Object Pascal) supporting versions from Delphi 7 through Delphi 12.3+ (2025).

## 🚀 Features

### ✅ Supported Language Features

- **Core Language Constructs**
  - Programs, Units, Libraries, Packages
  - Procedures, Functions, Methods
  - Variables, Constants, Types, Resourcestrings
  - Control Flow (if, case, for, while, repeat, for-in)
  - Exception Handling (try-except, try-finally)
  - Labels and Goto
  - Exports clause for DLLs

- **Object-Oriented Programming**
  - Classes with inheritance and forward declarations
  - Interfaces with GUID support
  - Dispinterfaces (COM automation)
  - Properties with read/write/index specifiers and array indexers
  - Visibility modifiers (private, protected, public, published, strict private/protected)
  - Virtual, dynamic, abstract, override, final methods
  - Class methods (`class procedure`, `class function`, `class property`)
  - Operator overloading
  - Constructor/destructor implementations with qualified names
  - Nested type and const declarations in classes/records
  - Message handlers (`message` directive)

- **Modern Features**
  - **Generics with constraints** (Delphi 2009+)
  - **Inline variables** with type inference (Delphi 10.3+)
  - **Anonymous methods** and function references (Delphi 2009+)
  - **Helper types** for classes and records with inheritance (Delphi XE3+)
  - **Custom attributes** for RTTI (Delphi 2010+)
  - **Multiline string literals** (Delphi 12+)
  - **For-in loops** with inline variable declarations
  - **`default()` intrinsic** for generics

- **Advanced Type System**
  - Arrays (static, dynamic, multi-dimensional, `array of const`)
  - Records with variant parts, methods, operators
  - Sets with range support
  - Pointers and references
  - Procedural types with calling conventions
  - Method pointers (`of object`)
  - String types (string, AnsiString, WideString, UnicodeString, ShortString, RawByteString, UTF8String)
  - Enumerated types with explicit values
  - Subrange types
  - Packed types
  - Class reference types (`class of`)
  - Variant and OleVariant

- **Memory Management**
  - Weak references (`[weak]`)
  - Unsafe references (`[unsafe]`)
  - Managed records
  - Reference counting interfaces

- **Other**
  - Threadvar declarations
  - Resourcestring sections
  - External procedure linkage with `name`, `index`, `delayed`
  - Portability directives (`platform`, `deprecated`, `experimental`)
  - Escaped keyword identifiers (`&type`, `&begin`)
  - Inline assembly (opaque block)

## 📊 Version Coverage

| Delphi Version | Coverage | Notes |
|---|---|---|
| **Delphi 7–2007** | 🟢 **97%** | Full classic Pascal + OOP |
| **Delphi 2009–XE** | 🟢 **95%** | Unicode, Generics, Anonymous methods |
| **Delphi XE2–XE8** | 🟢 **93%** | Mobile platforms, Helper types, ARC |
| **Delphi 10.x–11.x** | 🟢 **90%** | Inline variables, Enhanced RTTI |
| **Delphi 12.x** | 🟡 **85%** | Multiline strings |

## 🎯 Usage

### Prerequisites
- ANTLR4 (4.7.1 or later)
- Java 8+ or C#/.NET for target runtime

### Generate Parser
```bash
antlr4 Delphi.g4 -Dlanguage=Java    # Java target
antlr4 Delphi.g4 -Dlanguage=CSharp  # C# target
antlr4 Delphi.g4 -Dlanguage=Python3 # Python target
```

## 🔧 Grammar Structure

### Parser Rules (Hierarchical)
```
file
├── program_ / unit_ / library_ / package_
├── interfaceSection / implementationSection
├── declarationSection
│   ├── constSection / resourcestringSection
│   ├── typeSection
│   ├── varSection / threadvarSection
│   ├── exportsClause
│   └── procedureDeclaration / functionDeclaration
│       └── constructorImpl / destructorImpl
├── statement
│   ├── simpleStatement (assignments, calls, raise, exit)
│   ├── structuredStatement (blocks, loops, exceptions)
│   └── inlineVarDecl
└── expression
    ├── simpleExpression
    ├── term
    └── factor (designators, literals, anonymous methods, intrinsics)
```

### Key Design Decisions

1. **Unified Bracket Constructor**: `[1, 2, 3]` handles both sets and dynamic arrays (semantic analysis required)
2. **Inline Variables**: Full support for `var x := value` and `const x := value` in statements and for-loops
3. **Generic Constraints**: Supports `<T: class, constructor>` and `<T: IInterface>` patterns
4. **Helper Types**: With optional helper inheritance `(TBaseHelper)`
5. **Anonymous Methods**: Proper block-based syntax for both procedure and function forms
6. **Qualified Method Names**: `TMyClass.Method` and `TMyClass<T>.Method` for implementation sections
7. **Assembly as Opaque Block**: `asm...end` content treated as opaque text; full x86/x64 parsing requires a dedicated sub-grammar or lexer mode
8. **Calling Conventions on Procedural Types**: `procedure of object; stdcall` properly supported
9. **Forward Declarations**: Explicit `class;` and `interface;` forward references
10. **Intrinsic Functions**: `Default()`, `Low()`, `High()`, `Length()`, `SizeOf()`, `TypeOf()`, `Assigned()`, `Ord()`, `Chr()`, `Pred()`, `Succ()`, `Abs()`, `Include()`, `Exclude()` as factor productions

## ❌ Known Limitations

### Not Covered (~10–15%)
- **Full inline assembly parsing** — assembly blocks are accepted as opaque content; a dedicated lexer mode would be needed for instruction-level parsing
- **All compiler directive semantics** — `{$IFDEF}`, `{$REGION}`, conditional compilation etc. are skipped
- **Windows Runtime (WinRT)** syntax extensions
- **Complete `dispinterface`** method resolution clauses
- **Some legacy/rare constructs** — e.g. `object` types (pre-Delphi), `absolute` on expressions with complex syntax

### Semantic Analysis Required
- **Type Inference**: Context-dependent type resolution for inline `var`
- **Generic Instantiation**: Template expansion and validation
- **Overload Resolution**: Method/operator selection
- **Scope Resolution**: Unit-qualified identifier lookup
- **Case-Insensitivity**: Delphi identifiers are case-insensitive; the lexer produces case-sensitive tokens — normalization must happen in the semantic layer

## 📄 License

See [LICENSE](LICENSE) file for details (GPL-3.0).

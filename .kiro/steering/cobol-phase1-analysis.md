---
inclusion: manual
---

# Phase 1: COBOL Initial Analysis & Documentation

You are analyzing a COBOL codebase to prepare for cloud migration.

## Objectives

1. Analyze COBOL codebase structure
2. Extract and document business logic
3. Map data structures for migration

## Step 1.1: COBOL Code Analysis

### Tasks

1. **Identify all COBOL programs**
   - Scan for .cbl, .cob, .CBL, .COB files
   - List all programs with file sizes and line counts

2. **Map program structure**
   - Identify main programs vs subprograms
   - Document CALL relationships between programs
   - Map file I/O operations (SELECT, FD, READ, WRITE, REWRITE, DELETE)
   - Identify database operations (EXEC SQL statements)
   - Detect CICS/IMS transaction processing (EXEC CICS, EXEC DLI)

3. **Document data structures**
   - Extract WORKING-STORAGE SECTION variables
   - Extract FILE SECTION definitions
   - Extract LINKAGE SECTION parameters
   - Note all COPY/COPYBOOK references

4. **Identify external dependencies**
   - List all copybooks referenced
   - Identify JCL files (if present)
   - Document database tables accessed
   - Note external file dependencies

5. **Create dependency graph**
   - Show program-to-program relationships
   - Show program-to-file relationships
   - Show program-to-database relationships

### Output

Create `cobol-analysis-report.md` with:
- Executive summary
- Program inventory table
- Dependency graph (Mermaid diagram)
- Complexity assessment
- Migration risk assessment

## Step 1.2: Business Logic Documentation

### Tasks

1. **For each COBOL program, document:**
   - Business function/purpose
   - Input parameters (LINKAGE SECTION)
   - Output/return values
   - Key business rules and calculations
   - Control flow and decision points
   - Complex algorithms or computations
   - Error handling and validation logic
   - Transaction boundaries

2. **Extract business rules:**
   - IF/EVALUATE conditions
   - COMPUTE statements
   - PERFORM loops with business logic
   - Data validation rules
   - Business calculations

3. **Document workflows:**
   - Program execution flow
   - Decision trees
   - Loop structures
   - Error paths

### Output

Create `business-logic-documentation.md` with:
- Program-by-program business logic
- Business rules catalog
- Workflow diagrams (Mermaid)
- Calculation formulas
- Validation rules

## Step 1.3: Data Structure Mapping

### Tasks

1. **Extract all COBOL data structures:**
   - WORKING-STORAGE variables with PIC clauses
   - COMP/COMP-3/BINARY specifications
   - OCCURS clauses (arrays/tables)
   - REDEFINES clauses
   - Level 88 condition names
   - Date fields and formats

2. **Create COBOL-to-Java mapping:**
   - PIC 9(n) → Integer/Long/BigDecimal
   - PIC X(n) → String
   - COMP-3 → BigDecimal
   - COMP → int/long
   - Date formats → java.time.LocalDate/LocalDateTime
   - OCCURS → List<> or arrays
   - REDEFINES → Union types or separate classes

3. **Create COBOL-to-TypeScript mapping:**
   - PIC 9(n) → number/bigint
   - PIC X(n) → string
   - COMP-3 → Decimal (from decimal.js)
   - Date formats → Date
   - OCCURS → Array<>
   - REDEFINES → Union types

4. **Generate class/interface definitions:**
   - Java POJO classes with Lombok annotations
   - TypeScript interfaces and classes
   - Include field validation rules
   - Preserve field lengths and precision

5. **Document file layouts:**
   - FD (File Description) sections
   - Record structures
   - Fixed-length vs variable-length
   - Key fields for indexed files

### Output

Create `data-structure-mapping.md` with:
- Complete COBOL data inventory
- Type mapping tables
- Generated Java class definitions
- Generated TypeScript interface definitions
- File layout specifications
- Conversion notes and caveats

## Validation Checklist

Before completing Phase 1:
- [ ] All COBOL programs identified and cataloged
- [ ] All dependencies mapped
- [ ] Business logic documented for each program
- [ ] All data structures extracted and mapped
- [ ] Migration complexity assessed
- [ ] Risk areas identified
- [ ] All three documentation files created

## Next Phase

After Phase 1 completion, proceed to:
- Phase 2 (Java Migration) if target is Java
- Phase 3 (Node.js Migration) if target is Node.js

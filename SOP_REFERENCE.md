# COBOL Migration SOP Reference

This document provides a complete reference of all Standard Operating Procedures (SOPs) created for the COBOL to Cloud migration.

## SOP Files Location

All SOPs are located in: `.kiro/steering/`

## Master SOP

### cobol-migration-master.md
**Purpose**: Orchestrates the entire migration process across all phases

**Usage in Kiro**: `#cobol-migration-master`

**Parameters Required**:
- `cobol_directory`: Path to COBOL source code
- `target_platform`: "java", "nodejs", or "both"
- `output_directory`: Where to generate migrated code

**Description**: This is the main orchestration SOP that coordinates all 6 phases of the migration. It ensures phases are executed sequentially and maintains migration state.

---

## Phase-Specific SOPs

### 1. cobol-phase1-analysis.md
**Purpose**: Initial analysis and documentation of COBOL codebase

**Usage in Kiro**: `#cobol-phase1-analysis`

**What It Does**:
- Identifies all COBOL programs and dependencies
- Maps program structure and relationships
- Extracts business logic
- Documents data structures
- Creates COBOL-to-Java/TypeScript mappings

**Outputs**:
- `cobol-analysis-report.md`
- `business-logic-documentation.md`
- `data-structure-mapping.md`

**Key Tasks**:
- Step 1.1: COBOL Code Analysis
- Step 1.2: Business Logic Documentation
- Step 1.3: Data Structure Mapping

---

### 2. cobol-phase2-java.md
**Purpose**: Convert COBOL code to Java

**Usage in Kiro**: `#cobol-phase2-java`

**What It Does**:
- Sets up Java project with Maven/Gradle
- Converts COBOL data structures to Java POJOs
- Migrates business logic to Java services
- Converts file I/O operations
- Converts database operations to JPA/JDBC

**Outputs**:
- Complete Java project structure
- Spring Boot application
- JPA entities and repositories
- Service classes
- Utility classes
- Unit and integration tests

**Key Tasks**:
- Step 2.1: Java Project Setup
- Step 2.2: Convert Data Structures to Java
- Step 2.3: Convert Program Logic to Java
- Step 2.4: Convert File I/O to Java
- Step 2.5: Convert Database Operations to Java

**Technologies Used**:
- Java 17+
- Spring Boot 3.x
- JPA/Hibernate
- JUnit 5
- Lombok
- BigDecimal for precision

---

### 3. cobol-phase3-nodejs.md
**Purpose**: Convert COBOL code to Node.js/TypeScript

**Usage in Kiro**: `#cobol-phase3-nodejs`

**What It Does**:
- Sets up Node.js/TypeScript project
- Converts COBOL data structures to TypeScript interfaces
- Migrates business logic to TypeScript services
- Converts file I/O operations
- Converts database operations to ORM

**Outputs**:
- Complete Node.js/TypeScript project
- Express.js application (optional)
- TypeScript interfaces and classes
- Service modules
- Repository modules
- Utility modules
- Jest tests

**Key Tasks**:
- Step 3.1: Node.js Project Setup
- Step 3.2: Convert Data Structures to TypeScript
- Step 3.3: Convert Program Logic to Node.js
- Step 3.4: Convert File I/O to Node.js
- Step 3.5: Convert Database Operations to Node.js

**Technologies Used**:
- TypeScript 5.x
- Node.js 18 LTS
- Express.js
- Decimal.js for precision
- Jest for testing
- TypeORM/Sequelize/Prisma

---

### 4. cobol-phase4-testing.md
**Purpose**: Create comprehensive test suites and validate migration

**Usage in Kiro**: `#cobol-phase4-testing`

**What It Does**:
- Creates unit tests for all components
- Creates integration tests for workflows
- Creates comparison tests (COBOL vs migrated)
- Generates test data and fixtures
- Validates numeric precision and data integrity
- Performs performance testing

**Outputs**:
- Unit test suites (JUnit/Jest)
- Integration test suites
- Comparison test framework
- Test data and fixtures
- Test reports
- Performance benchmarks

**Key Tasks**:
- Step 4.1: Create Java Test Suite
- Step 4.2: Create Node.js Test Suite
- Step 4.3: Create Test Data
- Step 4.4: Validation Testing
- Step 4.5: Test Documentation

**Test Types**:
- Unit tests (80%+ coverage)
- Integration tests
- Comparison tests (COBOL output vs migrated output)
- Performance tests
- Data integrity tests

---

### 5. cobol-phase5-utilities.md
**Purpose**: Create utility libraries and migration tools

**Usage in Kiro**: `#cobol-phase5-utilities`

**What It Does**:
- Creates COBOL compatibility layers
- Builds data conversion utilities
- Creates validation and comparison tools
- Provides reusable migration components

**Outputs**:
- CobolDataTypes utility class
- CobolBehavior utility class
- CobolFileHandler utility class
- Migration validation tool
- Comprehensive documentation

**Key Tasks**:
- Step 5.1: Java Compatibility Layer
- Step 5.2: Node.js Compatibility Layer
- Step 5.3: Migration Validation Tool

**Utilities Created**:

#### Java Utilities
- `CobolDataTypes.java`: COMP-3, EBCDIC, date conversions, padding
- `CobolBehavior.java`: COBOL arithmetic, rounding, truncation
- `CobolFileHandler.java`: Fixed-length records, copybook parsing

#### Node.js Utilities
- `cobolDataTypes.ts`: COMP-3, EBCDIC, date conversions, padding
- `cobolBehavior.ts`: COBOL arithmetic, rounding, truncation
- `cobolFileHandler.ts`: Fixed-length records, copybook parsing

#### Validation Tool
- Command-line tool to compare COBOL and migrated outputs
- Generates detailed diff reports
- Supports batch processing

---

### 6. cobol-phase6-documentation.md
**Purpose**: Create comprehensive documentation and handoff materials

**Usage in Kiro**: `#cobol-phase6-documentation`

**What It Does**:
- Creates migration overview documentation
- Generates technical specifications
- Writes developer guides
- Creates operational runbooks
- Documents deployment procedures

**Outputs**:
- `migration-overview.md`: Executive summary and architecture
- `technical-specification.md`: APIs, database schema, data models
- `developer-guide.md`: Setup, development, troubleshooting
- `operations-runbook.md`: Daily ops, incident response, maintenance

**Key Tasks**:
- Step 6.1: Migration Overview Documentation
- Step 6.2: Technical Specification
- Step 6.3: Developer Guide
- Step 6.4: Operations Runbook

**Documentation Includes**:
- Architecture diagrams
- API documentation
- Database schema
- Deployment procedures
- Monitoring setup
- Troubleshooting guides
- Emergency procedures
- Contact information

---

## SOP Usage Patterns

### Using with Python Agent

```bash
# Full migration (all phases)
python cobol_migration_agent.py --cobol-dir ./cobol-src --target java

# Single phase
python cobol_migration_agent.py --cobol-dir ./cobol-src --target java --phase phase1
```

### Using with Kiro Chat

```
# Master SOP
#cobol-migration-master
Migrate COBOL from ./cobol-src to Java

# Specific phase
#cobol-phase1-analysis
Analyze COBOL codebase in ./cobol-src

#cobol-phase2-java
Convert COBOL programs to Java

#cobol-phase3-nodejs
Convert COBOL programs to Node.js

#cobol-phase4-testing
Create comprehensive test suite

#cobol-phase5-utilities
Build COBOL compatibility utilities

#cobol-phase6-documentation
Generate migration documentation
```

---

## SOP Customization

All SOPs can be customized for your organization:

### 1. Edit SOP Files
Located in `.kiro/steering/`, edit markdown files to:
- Add organization-specific standards
- Modify data type mappings
- Include custom validation rules
- Add company-specific templates

### 2. Add New SOPs
Create new SOP files for:
- Custom migration patterns
- Organization-specific workflows
- Additional validation steps
- Custom reporting

### 3. Front Matter Options
Each SOP has YAML front matter:

```yaml
---
inclusion: manual  # or "always" or "fileMatch"
---
```

- `manual`: Reference with `#sop-name` in chat
- `always`: Automatically included in all conversations
- `fileMatch`: Included when specific files are in context

---

## SOP Dependencies

```
Phase 1 (Analysis)
    ↓
Phase 2 (Java) OR Phase 3 (Node.js) OR Both
    ↓
Phase 4 (Testing)
    ↓
Phase 5 (Utilities)
    ↓
Phase 6 (Documentation)
```

**Note**: Phase 1 must be completed before other phases. Phases 2 and 3 can run independently or together.

---

## Quick Reference Table

| SOP File | Phase | Purpose | Key Outputs |
|----------|-------|---------|-------------|
| `cobol-migration-master.md` | Master | Orchestration | Phase coordination |
| `cobol-phase1-analysis.md` | 1 | Analysis | Analysis reports, mappings |
| `cobol-phase2-java.md` | 2 | Java Migration | Java project, tests |
| `cobol-phase3-nodejs.md` | 3 | Node.js Migration | TypeScript project, tests |
| `cobol-phase4-testing.md` | 4 | Testing | Test suites, validation |
| `cobol-phase5-utilities.md` | 5 | Utilities | Compatibility layers, tools |
| `cobol-phase6-documentation.md` | 6 | Documentation | Docs, runbooks |

---

## Best Practices

1. **Always start with Phase 1**: Understanding your codebase is critical
2. **Review outputs between phases**: Don't blindly proceed through all phases
3. **Customize SOPs**: Adapt to your organization's needs
4. **Test incrementally**: Validate each program before moving to the next
5. **Document assumptions**: Note any differences from COBOL behavior
6. **Version control**: Commit after each successful phase

---

## Support

For questions or issues:
1. Review the specific SOP file in `.kiro/steering/`
2. Check `COBOL_MIGRATION_README.md` for detailed documentation
3. Review phase outputs in `migrated/phase-outputs/`
4. Check generated documentation in `migrated/docs/`

---

**All SOPs are ready to use! Start your migration today.**

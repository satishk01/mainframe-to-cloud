# COBOL to Cloud Migration Agent

A comprehensive Strands agent system that orchestrates the migration of mainframe COBOL code to modern cloud-based Java or Node.js applications using structured SOPs (Standard Operating Procedures).

## Overview

This agent system guides you through a complete 6-phase migration process:

1. **Phase 1: Initial Analysis & Documentation** - Analyze COBOL codebase, extract business logic, map data structures
2. **Phase 2: Java Migration** - Convert COBOL to Java with Spring Boot
3. **Phase 3: Node.js Migration** - Convert COBOL to TypeScript/Node.js
4. **Phase 4: Testing & Validation** - Create comprehensive test suites and validate migration
5. **Phase 5: Migration Utilities** - Build compatibility layers and validation tools
6. **Phase 6: Documentation & Handoff** - Create technical docs and operational runbooks

## Features

- **SOP-Driven Migration**: Each phase follows detailed Standard Operating Procedures
- **Multi-Platform Support**: Migrate to Java, Node.js, or both simultaneously
- **Incremental Execution**: Run all phases or execute individual phases
- **Comprehensive Testing**: Automated comparison testing between COBOL and migrated code
- **COBOL Compatibility**: Built-in utilities to preserve COBOL behavior (COMP-3, precision, etc.)
- **Complete Documentation**: Auto-generates technical specs, developer guides, and runbooks

## Prerequisites

1. **Python 3.8+** installed
2. **AI Provider** - Choose one:
   
   **Option A: Anthropic API (Direct)**
   - Anthropic API Key - Set as environment variable:
     ```cmd
     set ANTHROPIC_API_KEY=your_api_key_here
     ```
   
   **Option B: AWS Bedrock (Recommended for AWS Users)**
   - AWS Account with Bedrock access
   - AWS credentials configured:
     ```cmd
     aws configure
     # OR
     set AWS_ACCESS_KEY_ID=your_access_key
     set AWS_SECRET_ACCESS_KEY=your_secret_key
     set AWS_REGION=us-east-1
     ```
   - Bedrock model access enabled (Claude 3.5 Sonnet recommended)

3. **COBOL Source Code** - Directory containing .cbl or .cob files

## Installation

```bash
pip install -r requirements.txt
```

## Project Structure

```
.
├── .kiro/
│   └── steering/                          # SOP files
│       ├── cobol-migration-master.md      # Master orchestration SOP
│       ├── cobol-phase1-analysis.md       # Phase 1 SOP
│       ├── cobol-phase2-java.md           # Phase 2 SOP
│       ├── cobol-phase3-nodejs.md         # Phase 3 SOP
│       ├── cobol-phase4-testing.md        # Phase 4 SOP
│       ├── cobol-phase5-utilities.md      # Phase 5 SOP
│       └── cobol-phase6-documentation.md  # Phase 6 SOP
├── cobol_migration_agent.py               # Main agent orchestrator
├── requirements.txt                       # Python dependencies
└── COBOL_MIGRATION_README.md             # This file
```

## Usage

### Full Migration (All Phases)

#### Using Anthropic API:

Migrate to Java:
```bash
python cobol_migration_agent.py --cobol-dir ./cobol-src --target java
```

Migrate to Node.js:
```bash
python cobol_migration_agent.py --cobol-dir ./cobol-src --target nodejs
```

Migrate to both platforms:
```bash
python cobol_migration_agent.py --cobol-dir ./cobol-src --target both
```

#### Using AWS Bedrock:

Migrate to Java (default Claude 3.5 Sonnet):
```bash
python cobol_migration_bedrock_agent.py --cobol-dir ./cobol-src --target java
```

Migrate to Node.js with specific model:
```bash
python cobol_migration_bedrock_agent.py --cobol-dir ./cobol-src --target nodejs --model anthropic.claude-3-sonnet-20240229-v1:0
```

Migrate to both with specific region:
```bash
python cobol_migration_bedrock_agent.py --cobol-dir ./cobol-src --target both --region us-west-2
```

Specify custom output directory:
```bash
python cobol_migration_bedrock_agent.py --cobol-dir ./cobol-src --target java --output-dir ./my-migration
```

### Single Phase Execution

#### Using Anthropic API:

Run only Phase 1 (Analysis):
```bash
python cobol_migration_agent.py --cobol-dir ./cobol-src --target java --phase phase1
```

#### Using AWS Bedrock:

Run only Phase 1 (Analysis):
```bash
python cobol_migration_bedrock_agent.py --cobol-dir ./cobol-src --target java --phase phase1
```

Run only Phase 2 (Java Migration):
```bash
python cobol_migration_bedrock_agent.py --cobol-dir ./cobol-src --target java --phase phase2
```

Run only Phase 3 (Node.js Migration):
```bash
python cobol_migration_agent.py --cobol-dir ./cobol-src --target nodejs --phase phase3
```

Run only Phase 4 (Testing):
```bash
python cobol_migration_agent.py --cobol-dir ./cobol-src --target java --phase phase4
```

Run only Phase 5 (Utilities):
```bash
python cobol_migration_agent.py --cobol-dir ./cobol-src --target java --phase phase5
```

Run only Phase 6 (Documentation):
```bash
python cobol_migration_agent.py --cobol-dir ./cobol-src --target java --phase phase6
```

## Command-Line Arguments

### Anthropic API Version (cobol_migration_agent.py)

| Argument | Required | Description | Options |
|----------|----------|-------------|---------|
| `--cobol-dir` | Yes | Path to COBOL source directory | Any valid directory path |
| `--target` | Yes | Target platform for migration | `java`, `nodejs`, `both` |
| `--output-dir` | No | Output directory (default: ./migrated) | Any valid directory path |
| `--phase` | No | Run specific phase only (default: all) | `phase1`, `phase2`, `phase3`, `phase4`, `phase5`, `phase6` |

### AWS Bedrock Version (cobol_migration_bedrock_agent.py)

| Argument | Required | Description | Options |
|----------|----------|-------------|---------|
| `--cobol-dir` | Yes | Path to COBOL source directory | Any valid directory path |
| `--target` | Yes | Target platform for migration | `java`, `nodejs`, `both` |
| `--output-dir` | No | Output directory (default: ./migrated) | Any valid directory path |
| `--phase` | No | Run specific phase only (default: all) | `phase1`, `phase2`, `phase3`, `phase4`, `phase5`, `phase6` |
| `--model` | No | AWS Bedrock model ID | See available models below |
| `--region` | No | AWS region for Bedrock | Any AWS region (default: us-east-1) |

### Available AWS Bedrock Models

| Model ID | Description | Best For |
|----------|-------------|----------|
| `anthropic.claude-3-5-sonnet-20241022-v2:0` | Claude 3.5 Sonnet (latest) | **Recommended** - Best balance of performance and cost |
| `anthropic.claude-3-5-sonnet-20240620-v1:0` | Claude 3.5 Sonnet (previous) | High-quality migrations |
| `anthropic.claude-3-sonnet-20240229-v1:0` | Claude 3 Sonnet | Good balance |
| `anthropic.claude-3-haiku-20240307-v1:0` | Claude 3 Haiku | Faster, lower cost |
| `amazon.titan-text-premier-v1:0` | Amazon Titan | AWS-native option |

## Output Structure

After running the migration, you'll find:

```
migrated/
├── phase-outputs/                    # Agent outputs for each phase
│   ├── phase1-output.md
│   ├── phase2-java-output.md
│   ├── phase3-nodejs-output.md
│   ├── phase4-output.md
│   ├── phase5-output.md
│   └── phase6-output.md
├── docs/                             # Generated documentation
│   ├── cobol-analysis-report.md
│   ├── business-logic-documentation.md
│   ├── data-structure-mapping.md
│   ├── migration-overview.md
│   ├── technical-specification.md
│   ├── developer-guide.md
│   └── operations-runbook.md
├── java/                             # Java migration (if applicable)
│   ├── src/
│   ├── pom.xml
│   └── README.md
└── nodejs/                           # Node.js migration (if applicable)
    ├── src/
    ├── package.json
    └── README.md
```

## Migration Phases Explained

### Phase 1: Initial Analysis & Documentation

The agent will:
- Scan your COBOL directory for all .cbl/.cob files
- Analyze program structure and dependencies
- Extract business logic and document it
- Map COBOL data structures to Java/TypeScript equivalents
- Generate comprehensive analysis reports

**Outputs:**
- `cobol-analysis-report.md` - Complete codebase analysis
- `business-logic-documentation.md` - Business rules and logic
- `data-structure-mapping.md` - Type mappings and class definitions

### Phase 2: Java Migration

The agent will:
- Create Maven/Gradle project structure
- Convert COBOL data structures to Java POJOs
- Migrate business logic to Java services
- Convert file I/O operations
- Convert database operations to JPA/JDBC
- Create comprehensive tests

**Outputs:**
- Complete Java project with Spring Boot
- JUnit tests
- Maven/Gradle build files
- COBOL compatibility utilities

### Phase 3: Node.js Migration

The agent will:
- Create Node.js/TypeScript project structure
- Convert COBOL data structures to TypeScript interfaces
- Migrate business logic to TypeScript services
- Convert file I/O operations
- Convert database operations to ORM
- Create comprehensive tests

**Outputs:**
- Complete Node.js/TypeScript project
- Jest tests
- npm/yarn configuration
- COBOL compatibility utilities

### Phase 4: Testing & Validation

The agent will:
- Create unit tests for all components
- Create integration tests for workflows
- Create comparison tests (COBOL vs migrated)
- Generate test data and fixtures
- Validate numeric precision and data integrity

**Outputs:**
- Comprehensive test suites
- Test data and fixtures
- Validation reports
- Performance benchmarks

### Phase 5: Migration Utilities

The agent will:
- Create COBOL compatibility layers
- Build data conversion utilities (COMP-3, EBCDIC, etc.)
- Create validation and comparison tools
- Provide reusable migration components

**Outputs:**
- CobolDataTypes utility classes
- CobolBehavior utility classes
- CobolFileHandler utilities
- Migration validation tools

### Phase 6: Documentation & Handoff

The agent will:
- Create migration overview documentation
- Generate technical specifications
- Write developer guides
- Create operational runbooks
- Document deployment procedures

**Outputs:**
- `migration-overview.md` - Executive summary
- `technical-specification.md` - Architecture and APIs
- `developer-guide.md` - Setup and development
- `operations-runbook.md` - Operations and troubleshooting

## COBOL Features Supported

### Data Types
- PIC 9 (numeric) → BigDecimal/Decimal
- PIC X (alphanumeric) → String
- COMP-3 (packed decimal) → Custom conversion
- COMP (binary) → int/long
- Date formats → LocalDate/Date

### Control Structures
- PERFORM loops → for/while loops
- EVALUATE → switch statements
- IF-ELSE → if-else
- COMPUTE → Arithmetic operations
- GO TO → Refactored to structured code

### File Operations
- Sequential files → File I/O
- VSAM indexed files → Database tables
- Fixed-length records → Parsing utilities
- EBCDIC encoding → Conversion utilities

### Database Operations
- EXEC SQL → JPA/ORM
- Cursors → Pagination/streaming
- Transactions → Transaction management
- Host variables → Method parameters

## Key Principles

1. **Preserve COBOL Behavior**: The agent ensures arithmetic precision, rounding, and truncation match COBOL exactly
2. **Incremental Migration**: Migrate one program at a time with thorough testing
3. **Traceability**: All generated code includes references to original COBOL
4. **Validation**: Comparison testing ensures outputs match COBOL exactly
5. **Documentation**: Comprehensive docs for maintenance and operations

## Example Workflow

```bash
# Step 1: Run Phase 1 to analyze your COBOL code
python cobol_migration_agent.py --cobol-dir ./mainframe/cobol --target java --phase phase1

# Step 2: Review the analysis reports in migrated/docs/

# Step 3: Run Phase 2 to generate Java code
python cobol_migration_agent.py --cobol-dir ./mainframe/cobol --target java --phase phase2

# Step 4: Review and test the generated Java code

# Step 5: Run Phase 4 to create comprehensive tests
python cobol_migration_agent.py --cobol-dir ./mainframe/cobol --target java --phase phase4

# Step 6: Run remaining phases or full migration
python cobol_migration_agent.py --cobol-dir ./mainframe/cobol --target java
```

## Customizing SOPs

All SOPs are located in `.kiro/steering/` and can be customized:

1. Edit the SOP markdown files to adjust migration strategies
2. Add organization-specific coding standards
3. Modify data type mappings
4. Add custom validation rules
5. Include company-specific documentation templates

## Troubleshooting

### Issue: "SOP file not found"
**Solution**: Ensure all SOP files exist in `.kiro/steering/` directory

### Issue: "COBOL directory not found"
**Solution**: Verify the path to your COBOL source directory is correct

### Issue: "Invalid target platform"
**Solution**: Use `java`, `nodejs`, or `both` as the target

### Issue: Agent output seems incomplete
**Solution**: Check your Anthropic API key is set correctly and has sufficient credits

## Best Practices

1. **Start with Phase 1**: Always run analysis first to understand your codebase
2. **Review Each Phase**: Review agent outputs before proceeding to next phase
3. **Test Incrementally**: Test each migrated program before moving to the next
4. **Preserve Test Data**: Keep COBOL outputs for comparison testing
5. **Version Control**: Commit after each successful phase
6. **Document Assumptions**: Note any differences or assumptions made during migration

## Integration with Kiro

This agent system integrates with Kiro's steering system. You can also use the SOPs directly in Kiro chat:

```
#cobol-migration-master
Migrate COBOL code from ./cobol-src to Java
```

Or reference specific phase SOPs:
```
#cobol-phase1-analysis
Analyze the COBOL codebase in ./cobol-src
```

## Support and Resources

- **SOP Documentation**: See `.kiro/steering/` for detailed phase instructions
- **Generated Docs**: Check `migrated/docs/` for migration-specific documentation
- **Phase Outputs**: Review `migrated/phase-outputs/` for agent execution logs

## License

MIT

## Contributing

To add new migration patterns or improve SOPs:
1. Edit the relevant SOP file in `.kiro/steering/`
2. Test with sample COBOL code
3. Update this README if adding new features
4. Share improvements with your team

---

**Ready to migrate your COBOL code to the cloud? Let's get started!**

```bash
python cobol_migration_agent.py --cobol-dir ./your-cobol-code --target java
```

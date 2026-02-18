# COBOL Migration Agent - Quick Start Guide

## 5-Minute Setup

### 1. Install Dependencies
```bash
pip install -r requirements.txt
```

### 2. Choose Your AI Provider

#### Option A: Anthropic API (Direct)
```cmd
set ANTHROPIC_API_KEY=your_api_key_here
```

#### Option B: AWS Bedrock (Recommended for AWS Users)
```cmd
# Configure AWS credentials (one of these methods):
# Method 1: AWS CLI
aws configure

# Method 2: Environment variables
set AWS_ACCESS_KEY_ID=your_access_key
set AWS_SECRET_ACCESS_KEY=your_secret_key
set AWS_REGION=us-east-1
```

### 3. Run Migration

#### Using Anthropic API:
```bash
python cobol_migration_agent.py --cobol-dir ./cobol-src --target java
```

#### Using AWS Bedrock:
```bash
python cobol_migration_bedrock_agent.py --cobol-dir ./cobol-src --target java
```

That's it! The agent will guide you through all 6 phases.

---

## Common Commands

### Full Migration - Anthropic API
```bash
# Migrate to Java
python cobol_migration_agent.py --cobol-dir ./cobol-src --target java

# Migrate to Node.js
python cobol_migration_agent.py --cobol-dir ./cobol-src --target nodejs

# Migrate to both
python cobol_migration_agent.py --cobol-dir ./cobol-src --target both
```

### Full Migration - AWS Bedrock
```bash
# Migrate to Java (default model)
python cobol_migration_bedrock_agent.py --cobol-dir ./cobol-src --target java

# Migrate to Node.js with specific model
python cobol_migration_bedrock_agent.py --cobol-dir ./cobol-src --target nodejs --model anthropic.claude-3-sonnet-20240229-v1:0

# Migrate to both with specific region
python cobol_migration_bedrock_agent.py --cobol-dir ./cobol-src --target both --region us-west-2
```

### Phase-by-Phase - Anthropic API
```bash
# Phase 1: Analysis
python cobol_migration_agent.py --cobol-dir ./cobol-src --target java --phase phase1

# Phase 2: Java Migration
python cobol_migration_agent.py --cobol-dir ./cobol-src --target java --phase phase2

# Phase 3: Node.js Migration
python cobol_migration_agent.py --cobol-dir ./cobol-src --target nodejs --phase phase3

# Phase 4: Testing
python cobol_migration_agent.py --cobol-dir ./cobol-src --target java --phase phase4

# Phase 5: Utilities
python cobol_migration_agent.py --cobol-dir ./cobol-src --target java --phase phase5

# Phase 6: Documentation
python cobol_migration_agent.py --cobol-dir ./cobol-src --target java --phase phase6
```

---

## What Gets Created

```
migrated/
├── phase-outputs/          # Agent execution logs
├── docs/                   # Migration documentation
├── java/                   # Java code (if target=java)
└── nodejs/                 # Node.js code (if target=nodejs)
```

---

## The 6 Phases

| Phase | What It Does | Output |
|-------|--------------|--------|
| **Phase 1** | Analyzes COBOL code | Analysis reports, data mappings |
| **Phase 2** | Converts to Java | Java project with Spring Boot |
| **Phase 3** | Converts to Node.js | TypeScript/Node.js project |
| **Phase 4** | Creates tests | Unit, integration, comparison tests |
| **Phase 5** | Builds utilities | COBOL compatibility layers |
| **Phase 6** | Generates docs | Technical specs, runbooks |

---

## Using with Kiro Chat

Instead of running the Python script, you can use Kiro chat with the SOPs:

```
#cobol-migration-master
Migrate COBOL from ./cobol-src to Java
```

Or for specific phases:
```
#cobol-phase1-analysis
Analyze COBOL codebase in ./cobol-src
```

---

## Tips

✅ **DO:**
- Run Phase 1 first to understand your codebase
- Review outputs after each phase
- Test incrementally
- Keep COBOL outputs for comparison

❌ **DON'T:**
- Skip Phase 1 analysis
- Run all phases without reviewing
- Ignore validation errors
- Delete original COBOL code

---

## Need Help?

1. Check `COBOL_MIGRATION_README.md` for detailed documentation
2. Review SOP files in `.kiro/steering/` for phase details
3. Check `migrated/phase-outputs/` for agent execution logs
4. Review generated docs in `migrated/docs/`

---

## Example: Migrate a Simple COBOL Program

```bash
# 1. Analyze
python cobol_migration_agent.py --cobol-dir ./my-cobol --target java --phase phase1

# 2. Review analysis in migrated/docs/cobol-analysis-report.md

# 3. Migrate to Java
python cobol_migration_agent.py --cobol-dir ./my-cobol --target java --phase phase2

# 4. Review Java code in migrated/java/

# 5. Create tests
python cobol_migration_agent.py --cobol-dir ./my-cobol --target java --phase phase4

# 6. Run tests
cd migrated/java
mvn test
```

---

**Ready? Start your migration now!**

```bash
python cobol_migration_agent.py --cobol-dir ./cobol-src --target java
```


### Phase-by-Phase - AWS Bedrock
```bash
# Phase 1: Analysis
python cobol_migration_bedrock_agent.py --cobol-dir ./cobol-src --target java --phase phase1

# Phase 2: Java Migration
python cobol_migration_bedrock_agent.py --cobol-dir ./cobol-src --target java --phase phase2

# Continue with other phases...
```

---

## AWS Bedrock Models

Available models on Bedrock:
- `anthropic.claude-3-5-sonnet-20241022-v2:0` (default, recommended)
- `anthropic.claude-3-5-sonnet-20240620-v1:0`
- `anthropic.claude-3-sonnet-20240229-v1:0`
- `anthropic.claude-3-haiku-20240307-v1:0`
- `amazon.titan-text-premier-v1:0`

Specify model with `--model` flag:
```bash
python cobol_migration_bedrock_agent.py --cobol-dir ./src --target java --model anthropic.claude-3-haiku-20240307-v1:0
```

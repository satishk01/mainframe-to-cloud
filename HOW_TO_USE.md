# How to Use the COBOL Migration Agent System

## 🎯 What You Have

A complete COBOL to Cloud migration system with:
- 7 Standard Operating Procedures (SOPs)
- 2 Strands agents
- 7 documentation files
- Complete migration framework

---

## 📖 Step-by-Step Instructions

### Step 1: Understand What You Have

Read these files in order:
1. **INDEX.md** - Understand the project structure (5 minutes)
2. **QUICK_START_GUIDE.md** - Learn the basic commands (5 minutes)
3. **PROJECT_SUMMARY.md** - See what was created (5 minutes)

**Total time: 15 minutes**

---

### Step 2: Set Up Your Environment

#### Choose Your AI Provider

You have two options:

**Option A: Anthropic API (Direct)**
- Simpler setup
- Direct billing with Anthropic
- Good for non-AWS users

**Option B: AWS Bedrock (Recommended for AWS Users)**
- Integrated with AWS infrastructure
- AWS IAM access control
- AWS billing and cost management
- Better for enterprise/AWS environments

#### Install Python Dependencies
```bash
pip install -r requirements.txt
```

This installs:
- `strands-agents` - Agent framework
- `anthropic` - Claude AI API (for Option A)
- `boto3` - AWS SDK (for Option B)
- `argparse` - Command-line parsing

#### Configure Option A: Anthropic API
```cmd
set ANTHROPIC_API_KEY=your_api_key_here
```

Get your API key from: https://console.anthropic.com/

#### Configure Option B: AWS Bedrock
```cmd
# Method 1: AWS CLI (recommended)
aws configure

# Method 2: Environment variables
set AWS_ACCESS_KEY_ID=your_access_key
set AWS_SECRET_ACCESS_KEY=your_secret_key
set AWS_REGION=us-east-1
```

**Important**: Enable Bedrock model access in AWS Console first!
See **[AWS_BEDROCK_SETUP.md](AWS_BEDROCK_SETUP.md)** for detailed instructions.

**Total time: 5-10 minutes**

---

### Step 3: Prepare Your COBOL Code

Organize your COBOL source code:
```
your-cobol-directory/
├── PROGRAM1.cbl
├── PROGRAM2.cbl
├── PROGRAM3.cob
└── copybooks/
    ├── COPY1.cpy
    └── COPY2.cpy
```

**Note**: The agent will scan for `.cbl` and `.cob` files.

---

### Step 4: Choose Your Migration Path

You have 3 options for each AI provider:

#### Option A: Full Migration (Recommended for First Time)
Runs all 6 phases sequentially with user approval between phases.

**Using Anthropic API:**
```bash
python cobol_migration_agent.py --cobol-dir ./your-cobol --target java
```

**Using AWS Bedrock:**
```bash
python cobol_migration_bedrock_agent.py --cobol-dir ./your-cobol --target java
```

**When to use**: First migration, want guided experience

#### Option B: Phase-by-Phase Migration
Run one phase at a time, review outputs, then proceed.

**Using Anthropic API:**
```bash
# Phase 1: Analysis
python cobol_migration_agent.py --cobol-dir ./your-cobol --target java --phase phase1

# Review outputs, then continue...

# Phase 2: Java Migration
python cobol_migration_agent.py --cobol-dir ./your-cobol --target java --phase phase2

# And so on...
```

**Using AWS Bedrock:**
```bash
# Phase 1: Analysis
python cobol_migration_bedrock_agent.py --cobol-dir ./your-cobol --target java --phase phase1

# Phase 2: Java Migration
python cobol_migration_bedrock_agent.py --cobol-dir ./your-cobol --target java --phase phase2

# And so on...
```

**When to use**: Want control, need to review each phase, customizing approach

#### Option C: Kiro Chat Integration
Use SOPs directly in Kiro chat for interactive migration.

```
#cobol-migration-master
Migrate COBOL from ./your-cobol to Java
```

**When to use**: Prefer interactive chat, want to ask questions, need flexibility

---

### Step 5: Run Your First Migration

Let's do a simple test migration:

#### 5.1: Create Test COBOL File (Optional)
If you don't have COBOL code, create a simple test:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO.
       PROCEDURE DIVISION.
           DISPLAY 'Hello, World!'.
           STOP RUN.
```

Save as `test-cobol/HELLO.cbl`

#### 5.2: Run Phase 1 (Analysis)
```bash
python cobol_migration_agent.py --cobol-dir ./test-cobol --target java --phase phase1
```

#### 5.3: Review Outputs
Check `migrated/phase-outputs/phase1-output.md` and `migrated/docs/`

#### 5.4: Run Phase 2 (Java Migration)
```bash
python cobol_migration_agent.py --cobol-dir ./test-cobol --target java --phase phase2
```

#### 5.5: Review Generated Code
Check `migrated/java/` for generated Java code

**Total time: 10-30 minutes depending on COBOL complexity**

---

### Step 6: Understand the Outputs

After running phases, you'll find:

```
migrated/
├── phase-outputs/              # Agent execution logs
│   ├── phase1-output.md
│   ├── phase2-java-output.md
│   └── ...
├── docs/                       # Generated documentation
│   ├── cobol-analysis-report.md
│   ├── business-logic-documentation.md
│   ├── data-structure-mapping.md
│   └── ...
├── java/                       # Java project (if target=java)
│   ├── src/
│   ├── pom.xml
│   └── README.md
└── nodejs/                     # Node.js project (if target=nodejs)
    ├── src/
    ├── package.json
    └── README.md
```

---

### Step 7: Review and Validate

#### Review Phase 1 Outputs
1. Open `migrated/docs/cobol-analysis-report.md`
2. Check program inventory
3. Review dependency graph
4. Verify data structure mappings

#### Review Generated Code
1. Open `migrated/java/src/` or `migrated/nodejs/src/`
2. Check generated classes/interfaces
3. Review business logic conversion
4. Verify tests were created

#### Run Tests
```bash
# Java
cd migrated/java
mvn test

# Node.js
cd migrated/nodejs
npm test
```

---

### Step 8: Customize (Optional)

#### Customize SOPs
Edit files in `.kiro/steering/` to:
- Add organization-specific standards
- Modify data type mappings
- Include custom validation rules
- Add company-specific templates

Example:
```bash
# Edit Phase 2 SOP
notepad .kiro\steering\cobol-phase2-java.md
```

#### Customize Agent
Edit `cobol_migration_agent.py` to:
- Change default output directory
- Add custom validation
- Modify phase flow
- Add logging

---

## 🎓 Common Workflows

### Workflow 1: Analyze COBOL Code Only
```bash
python cobol_migration_agent.py --cobol-dir ./cobol --target java --phase phase1
```
Review `migrated/docs/` for analysis reports.

### Workflow 2: Migrate to Java
```bash
# Full migration
python cobol_migration_agent.py --cobol-dir ./cobol --target java

# Or phase-by-phase
python cobol_migration_agent.py --cobol-dir ./cobol --target java --phase phase1
python cobol_migration_agent.py --cobol-dir ./cobol --target java --phase phase2
python cobol_migration_agent.py --cobol-dir ./cobol --target java --phase phase4
```

### Workflow 3: Migrate to Node.js
```bash
python cobol_migration_agent.py --cobol-dir ./cobol --target nodejs
```

### Workflow 4: Migrate to Both Platforms
```bash
python cobol_migration_agent.py --cobol-dir ./cobol --target both
```

### Workflow 5: Generate Documentation Only
```bash
# Run Phase 1 for analysis
python cobol_migration_agent.py --cobol-dir ./cobol --target java --phase phase1

# Run Phase 6 for documentation
python cobol_migration_agent.py --cobol-dir ./cobol --target java --phase phase6
```

---

## 🔧 Troubleshooting

### Issue: "ANTHROPIC_API_KEY not set"
**Solution**: 
```cmd
set ANTHROPIC_API_KEY=your_api_key_here
```

### Issue: "COBOL directory not found"
**Solution**: Check the path is correct
```bash
# Use absolute path
python cobol_migration_agent.py --cobol-dir C:\path\to\cobol --target java

# Or relative path
python cobol_migration_agent.py --cobol-dir .\cobol-src --target java
```

### Issue: "No COBOL files found"
**Solution**: Ensure directory contains `.cbl` or `.cob` files

### Issue: "Module 'strands' not found"
**Solution**: Install dependencies
```bash
pip install -r requirements.txt
```

### Issue: Agent output seems incomplete
**Solution**: 
1. Check API key is valid
2. Check API credits
3. Review phase outputs in `migrated/phase-outputs/`

---

## 📚 Learning Resources

### For Beginners
1. **INDEX.md** - Navigation
2. **QUICK_START_GUIDE.md** - Quick commands
3. **PROJECT_SUMMARY.md** - What was created

### For Intermediate Users
1. **COBOL_MIGRATION_README.md** - Complete guide
2. **SOP_REFERENCE.md** - SOP details
3. Run phase-by-phase migrations

### For Advanced Users
1. **SYSTEM_OVERVIEW.md** - Architecture
2. Edit SOP files in `.kiro/steering/`
3. Modify `cobol_migration_agent.py`
4. Create custom SOPs

---

## 💡 Tips for Success

### Before Migration
✅ Read INDEX.md and QUICK_START_GUIDE.md
✅ Understand your COBOL codebase
✅ Choose target platform (Java or Node.js)
✅ Set up development environment

### During Migration
✅ Run Phase 1 first (always!)
✅ Review outputs after each phase
✅ Test incrementally
✅ Keep COBOL outputs for comparison

### After Migration
✅ Review all generated code
✅ Run comprehensive tests
✅ Deploy to test environment
✅ Conduct user acceptance testing

---

## 🎯 Quick Reference

### Commands
```bash
# Full migration to Java
python cobol_migration_agent.py --cobol-dir ./cobol --target java

# Full migration to Node.js
python cobol_migration_agent.py --cobol-dir ./cobol --target nodejs

# Full migration to both
python cobol_migration_agent.py --cobol-dir ./cobol --target both

# Single phase
python cobol_migration_agent.py --cobol-dir ./cobol --target java --phase phase1

# Custom output directory
python cobol_migration_agent.py --cobol-dir ./cobol --target java --output-dir ./my-output
```

### Kiro Chat
```
#cobol-migration-master
Migrate COBOL from ./cobol to Java

#cobol-phase1-analysis
Analyze COBOL in ./cobol

#cobol-phase2-java
Convert COBOL to Java
```

### File Locations
- SOPs: `.kiro/steering/`
- Agent: `cobol_migration_agent.py`
- Docs: `INDEX.md`, `COBOL_MIGRATION_README.md`, etc.
- Outputs: `migrated/`

---

## 🚀 Ready to Start?

### Checklist
- [ ] Read INDEX.md
- [ ] Read QUICK_START_GUIDE.md
- [ ] Install dependencies (`pip install -r requirements.txt`)
- [ ] Set API key (`set ANTHROPIC_API_KEY=...`)
- [ ] Prepare COBOL directory
- [ ] Choose target platform

### Run Your First Migration
```bash
python cobol_migration_agent.py --cobol-dir ./your-cobol --target java
```

---

## 📞 Need Help?

1. **Check documentation**: Start with INDEX.md
2. **Review examples**: See QUICK_START_GUIDE.md
3. **Read SOPs**: Check `.kiro/steering/` for detailed procedures
4. **Check outputs**: Review `migrated/phase-outputs/` for agent logs

---

## 🎉 You're Ready!

You now know how to:
✅ Set up the environment
✅ Run migrations
✅ Review outputs
✅ Customize SOPs
✅ Troubleshoot issues

**Start your COBOL migration journey now!**

```bash
python cobol_migration_agent.py --cobol-dir ./cobol-src --target java
```

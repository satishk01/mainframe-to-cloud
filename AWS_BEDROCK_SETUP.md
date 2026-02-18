# AWS Bedrock Setup Guide

## Overview

The COBOL Migration Agent now supports AWS Bedrock as an alternative to the direct Anthropic API. This is recommended for AWS users who want to:
- Use existing AWS infrastructure
- Leverage AWS IAM for access control
- Keep AI workloads within AWS
- Use AWS billing and cost management

---

## Prerequisites

1. **AWS Account** with Bedrock access
2. **AWS CLI** installed and configured
3. **Bedrock Model Access** enabled for Claude models

---

## Step 1: Enable Bedrock Model Access

### 1.1: Access AWS Bedrock Console
1. Log in to AWS Console
2. Navigate to Amazon Bedrock
3. Go to "Model access" in the left sidebar

### 1.2: Request Model Access
1. Click "Manage model access"
2. Select the models you want to use:
   - ✅ **Anthropic Claude 3.5 Sonnet** (Recommended)
   - ✅ Anthropic Claude 3 Sonnet
   - ✅ Anthropic Claude 3 Haiku
   - ✅ Amazon Titan Text Premier (optional)
3. Click "Request model access"
4. Wait for approval (usually instant for Claude models)

---

## Step 2: Configure AWS Credentials

Choose one of these methods:

### Method 1: AWS CLI (Recommended)
```bash
aws configure
```

Enter:
- AWS Access Key ID
- AWS Secret Access Key
- Default region (e.g., `us-east-1`)
- Default output format (e.g., `json`)

### Method 2: Environment Variables
```cmd
# Windows
set AWS_ACCESS_KEY_ID=your_access_key_id
set AWS_SECRET_ACCESS_KEY=your_secret_access_key
set AWS_REGION=us-east-1

# Linux/Mac
export AWS_ACCESS_KEY_ID=your_access_key_id
export AWS_SECRET_ACCESS_KEY=your_secret_access_key
export AWS_REGION=us-east-1
```

### Method 3: AWS Credentials File
Create `~/.aws/credentials`:
```ini
[default]
aws_access_key_id = your_access_key_id
aws_secret_access_key = your_secret_access_key
```

Create `~/.aws/config`:
```ini
[default]
region = us-east-1
output = json
```

---

## Step 3: Verify Bedrock Access

Test your Bedrock access:

```bash
aws bedrock list-foundation-models --region us-east-1
```

You should see a list of available models including Claude models.

---

## Step 4: Install Dependencies

```bash
pip install -r requirements.txt
```

This installs:
- `strands-agents` - Agent framework
- `boto3` - AWS SDK for Python
- `anthropic` - For Anthropic API version (optional)

---

## Step 5: Run Migration with Bedrock

### Basic Usage
```bash
python cobol_migration_bedrock_agent.py --cobol-dir ./cobol-src --target java
```

### With Specific Model
```bash
python cobol_migration_bedrock_agent.py --cobol-dir ./cobol-src --target java --model anthropic.claude-3-5-sonnet-20241022-v2:0
```

### With Specific Region
```bash
python cobol_migration_bedrock_agent.py --cobol-dir ./cobol-src --target java --region us-west-2
```

### Single Phase
```bash
python cobol_migration_bedrock_agent.py --cobol-dir ./cobol-src --target java --phase phase1
```

---

## Available Bedrock Models

### Recommended Models

| Model ID | Description | Use Case |
|----------|-------------|----------|
| `anthropic.claude-3-5-sonnet-20241022-v2:0` | **Claude 3.5 Sonnet (Latest)** | **Best choice** - Highest quality migrations |
| `anthropic.claude-3-5-sonnet-20240620-v1:0` | Claude 3.5 Sonnet (Previous) | High-quality migrations |
| `anthropic.claude-3-sonnet-20240229-v1:0` | Claude 3 Sonnet | Good balance of quality and cost |
| `anthropic.claude-3-haiku-20240307-v1:0` | Claude 3 Haiku | Faster, lower cost for simple migrations |

### AWS Native Models

| Model ID | Description | Use Case |
|----------|-------------|----------|
| `amazon.titan-text-premier-v1:0` | Amazon Titan Premier | AWS-native option |

---

## Bedrock Regions

Bedrock is available in these regions:
- `us-east-1` (US East - N. Virginia) - **Recommended**
- `us-west-2` (US West - Oregon)
- `eu-west-1` (Europe - Ireland)
- `eu-central-1` (Europe - Frankfurt)
- `ap-southeast-1` (Asia Pacific - Singapore)
- `ap-northeast-1` (Asia Pacific - Tokyo)

Check current availability: https://docs.aws.amazon.com/bedrock/latest/userguide/what-is-bedrock.html#bedrock-regions

---

## Cost Considerations

### Pricing Model
Bedrock charges per 1,000 input/output tokens:

**Claude 3.5 Sonnet:**
- Input: ~$3.00 per 1M tokens
- Output: ~$15.00 per 1M tokens

**Claude 3 Haiku:**
- Input: ~$0.25 per 1M tokens
- Output: ~$1.25 per 1M tokens

### Estimating Costs

For a typical COBOL migration:
- Phase 1 (Analysis): ~50K tokens = $0.15-$0.75
- Phase 2 (Java Migration): ~200K tokens = $0.60-$3.00
- Full migration (all phases): ~500K tokens = $1.50-$7.50

**Note**: Actual costs vary based on COBOL complexity and output verbosity.

---

## IAM Permissions

Your AWS user/role needs these permissions:

```json
{
  "Version": "2012-10-17",
  "Statement": [
    {
      "Effect": "Allow",
      "Action": [
        "bedrock:InvokeModel",
        "bedrock:InvokeModelWithResponseStream"
      ],
      "Resource": [
        "arn:aws:bedrock:*::foundation-model/anthropic.claude-3-5-sonnet-20241022-v2:0",
        "arn:aws:bedrock:*::foundation-model/anthropic.claude-3-5-sonnet-20240620-v1:0",
        "arn:aws:bedrock:*::foundation-model/anthropic.claude-3-sonnet-20240229-v1:0",
        "arn:aws:bedrock:*::foundation-model/anthropic.claude-3-haiku-20240307-v1:0"
      ]
    }
  ]
}
```

---

## Troubleshooting

### Issue: "AccessDeniedException"
**Solution**: 
1. Check IAM permissions
2. Verify model access is enabled in Bedrock console
3. Confirm you're using the correct region

### Issue: "ValidationException: The provided model identifier is invalid"
**Solution**:
1. Check model ID spelling
2. Verify model is available in your region
3. Confirm model access is enabled

### Issue: "ResourceNotFoundException"
**Solution**:
1. Verify Bedrock is available in your region
2. Check AWS credentials are configured correctly
3. Try a different region with `--region` flag

### Issue: "ThrottlingException"
**Solution**:
1. You've hit rate limits
2. Wait a few minutes and retry
3. Consider using a different model or region

---

## Comparison: Anthropic API vs AWS Bedrock

| Feature | Anthropic API | AWS Bedrock |
|---------|---------------|-------------|
| **Setup** | Simple API key | AWS credentials + model access |
| **Cost** | Direct billing | AWS billing |
| **Regions** | Global | Specific AWS regions |
| **IAM Integration** | No | Yes |
| **VPC Support** | No | Yes (with VPC endpoints) |
| **Compliance** | Anthropic's | AWS compliance frameworks |
| **Best For** | Quick start, non-AWS users | AWS users, enterprise |

---

## Best Practices

### 1. Use Latest Claude Model
```bash
python cobol_migration_bedrock_agent.py --cobol-dir ./src --target java --model anthropic.claude-3-5-sonnet-20241022-v2:0
```

### 2. Choose Region Closest to You
```bash
python cobol_migration_bedrock_agent.py --cobol-dir ./src --target java --region us-west-2
```

### 3. Start with Phase 1
```bash
python cobol_migration_bedrock_agent.py --cobol-dir ./src --target java --phase phase1
```

### 4. Monitor Costs
- Check AWS Cost Explorer regularly
- Set up billing alerts
- Use CloudWatch for usage monitoring

### 5. Use IAM Roles (Production)
- Don't use root credentials
- Create dedicated IAM user/role for migrations
- Follow principle of least privilege

---

## Example Workflows

### Workflow 1: First-Time Setup
```bash
# 1. Configure AWS
aws configure

# 2. Verify access
aws bedrock list-foundation-models --region us-east-1

# 3. Install dependencies
pip install -r requirements.txt

# 4. Run migration
python cobol_migration_bedrock_agent.py --cobol-dir ./cobol-src --target java
```

### Workflow 2: Multi-Region Setup
```bash
# Test in us-east-1
python cobol_migration_bedrock_agent.py --cobol-dir ./src --target java --region us-east-1 --phase phase1

# If throttled, try us-west-2
python cobol_migration_bedrock_agent.py --cobol-dir ./src --target java --region us-west-2 --phase phase1
```

### Workflow 3: Cost-Optimized
```bash
# Use Haiku for analysis (cheaper)
python cobol_migration_bedrock_agent.py --cobol-dir ./src --target java --phase phase1 --model anthropic.claude-3-haiku-20240307-v1:0

# Use Sonnet for code generation (better quality)
python cobol_migration_bedrock_agent.py --cobol-dir ./src --target java --phase phase2 --model anthropic.claude-3-5-sonnet-20241022-v2:0
```

---

## Next Steps

1. ✅ Complete AWS Bedrock setup
2. ✅ Run test migration with Phase 1
3. ✅ Review outputs
4. ✅ Proceed with full migration

---

## Additional Resources

- [AWS Bedrock Documentation](https://docs.aws.amazon.com/bedrock/)
- [Bedrock Pricing](https://aws.amazon.com/bedrock/pricing/)
- [Claude Model Cards](https://www.anthropic.com/claude)
- [Strands Agents Documentation](https://docs.strands.ai/)

---

**Ready to migrate with AWS Bedrock!**

```bash
python cobol_migration_bedrock_agent.py --cobol-dir ./cobol-src --target java
```

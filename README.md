# Meeting Notes Processor Agent

A Strands agent that automatically processes meeting notes and extracts:
- Action items with owners, deadlines, and priorities
- Decisions made with rationale
- Follow-up tasks and dependencies
- Risks and blockers

## Prerequisites

1. **Python 3.8+** installed
2. **Anthropic API Key** - Set as environment variable:
   ```bash
   # Windows (cmd)
   set ANTHROPIC_API_KEY=your_api_key_here
   
   # Windows (PowerShell)
   $env:ANTHROPIC_API_KEY="your_api_key_here"
   ```

## Installation

1. Install dependencies:
   ```bash
   pip install -r requirements.txt
   ```

## Usage

### Basic Usage (Print to Console)

```bash
python meeting_notes_agent.py path/to/meeting_notes.txt
```

### Save to File

```bash
python meeting_notes_agent.py path/to/meeting_notes.txt output_analysis.md
```

### Examples

```bash
# Process a meeting notes file
python meeting_notes_agent.py meetings/weekly_standup.txt

# Process and save to a specific output file
python meeting_notes_agent.py meetings/project_kickoff.txt analysis/kickoff_analysis.md
```

## Input Format

Your meeting notes can be in any text format. The agent will understand:
- Plain text notes
- Bullet points
- Timestamped entries
- Conversational format
- Mixed formats

Example meeting notes file (`sample_meeting.txt`):
```
Project Kickoff Meeting - 2026-02-18

Attendees: John (PM), Sarah (Dev Lead), Mike (Designer)

Discussion:
- Need to finalize the API design by end of week
- Sarah will review the architecture proposal
- Mike mentioned concerns about the timeline
- Decided to use PostgreSQL for the database
- John will schedule follow-up with stakeholders

Action items:
- API design review - Sarah - Friday
- Create mockups - Mike - next week
- Stakeholder meeting - John - TBD
```

## Output Format

The agent produces structured markdown with:

### Action Items Table
| Priority | Task | Owner | Deadline | Status |
|----------|------|-------|----------|--------|

### Decisions Made
- Decision details with rationale and impact

### Follow-up Tasks
- Tasks with dependencies and target dates

### Key Takeaways
- Summary of main points

### Risks & Blockers
- Identified issues

## Customization

To modify the analysis behavior, edit `.kiro/steering/meeting-notes-processor.md`

## Troubleshooting

**Error: ANTHROPIC_API_KEY not set**
- Set your API key as an environment variable (see Prerequisites)

**Error: File not found**
- Check the file path is correct
- Use relative or absolute paths

**Error: Module not found**
- Run `pip install -r requirements.txt`

## Integration Options

### Use as a Python Module

```python
from meeting_notes_agent import process_meeting_notes

analysis = process_meeting_notes("path/to/notes.txt")
print(analysis)
```

### Batch Processing

```python
import os
from meeting_notes_agent import process_meeting_notes, save_analysis

notes_dir = "meetings/"
output_dir = "analysis/"

for filename in os.listdir(notes_dir):
    if filename.endswith(".txt"):
        input_path = os.path.join(notes_dir, filename)
        output_path = os.path.join(output_dir, filename.replace(".txt", "_analysis.md"))
        
        analysis = process_meeting_notes(input_path)
        save_analysis(analysis, output_path)
```

## License

MIT

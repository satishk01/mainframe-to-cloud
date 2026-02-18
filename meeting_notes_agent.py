"""
Meeting Notes Processor Agent
Processes meeting notes and extracts action items, decisions, and follow-up tasks.
"""

import os
from strands import Agent
from anthropic import Anthropic


# Load the SOP content
def load_sop():
    """Load the meeting notes processor SOP."""
    sop_path = ".kiro/steering/meeting-notes-processor.md"
    if os.path.exists(sop_path):
        with open(sop_path, 'r', encoding='utf-8') as f:
            return f.read()
    return ""


# Create the agent with the SOP as system instructions
def create_meeting_notes_agent():
    """Create and configure the meeting notes processor agent."""
    sop_content = load_sop()
    
    agent = Agent(
        agent_id="meeting-notes-processor",
        model=Anthropic(model_id="claude-3-5-sonnet-20241022"),
        system_prompt=sop_content,
        description="Analyzes meeting notes and extracts action items, decisions, and follow-up tasks"
    )
    
    return agent


def process_meeting_notes(file_path: str) -> str:
    """
    Process meeting notes from a file.
    
    Args:
        file_path: Path to the meeting notes file
        
    Returns:
        Structured analysis of the meeting notes
    """
    # Read the meeting notes
    if not os.path.exists(file_path):
        return f"Error: File not found at {file_path}"
    
    with open(file_path, 'r', encoding='utf-8') as f:
        meeting_notes = f.read()
    
    # Create the agent
    agent = create_meeting_notes_agent()
    
    # Process the notes
    prompt = f"""Please analyze the following meeting notes and extract action items, decisions, and follow-up tasks:

{meeting_notes}
"""
    
    # Run the agent
    response = agent.run(prompt)
    
    return response.output


def save_analysis(analysis: str, output_path: str):
    """Save the analysis to a file."""
    with open(output_path, 'w', encoding='utf-8') as f:
        f.write(analysis)
    print(f"Analysis saved to: {output_path}")


if __name__ == "__main__":
    import sys
    
    if len(sys.argv) < 2:
        print("Usage: python meeting_notes_agent.py <meeting_notes_file> [output_file]")
        print("\nExample:")
        print("  python meeting_notes_agent.py meeting_notes.txt")
        print("  python meeting_notes_agent.py meeting_notes.txt analysis.md")
        sys.exit(1)
    
    input_file = sys.argv[1]
    output_file = sys.argv[2] if len(sys.argv) > 2 else None
    
    print(f"Processing meeting notes from: {input_file}")
    print("Analyzing...")
    
    # Process the meeting notes
    analysis = process_meeting_notes(input_file)
    
    # Save or print the results
    if output_file:
        save_analysis(analysis, output_file)
    else:
        print("\n" + "="*80)
        print("MEETING NOTES ANALYSIS")
        print("="*80 + "\n")
        print(analysis)

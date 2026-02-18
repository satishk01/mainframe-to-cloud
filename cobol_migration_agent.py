"""
COBOL to Cloud Migration Agent
Orchestrates the migration of mainframe COBOL code to Java or Node.js using SOPs.
"""

import os
import sys
from pathlib import Path
from strands import Agent
from anthropic import Anthropic


def load_sop(sop_name: str) -> str:
    """Load an SOP file from the .kiro/steering directory."""
    sop_path = f".kiro/steering/{sop_name}.md"
    if os.path.exists(sop_path):
        with open(sop_path, 'r', encoding='utf-8') as f:
            return f.read()
    return ""


def create_migration_agent(phase: str = "master") -> Agent:
    """Create the migration agent with appropriate SOP."""
    sop_mapping = {
        "master": "cobol-migration-master",
        "phase1": "cobol-phase1-analysis",
        "phase2": "cobol-phase2-java",
        "phase3": "cobol-phase3-nodejs",
        "phase4": "cobol-phase4-testing",
        "phase5": "cobol-phase5-utilities",
        "phase6": "cobol-phase6-documentation"
    }
    
    sop_name = sop_mapping.get(phase, "cobol-migration-master")
    sop_content = load_sop(sop_name)
    
    if not sop_content:
        print(f"Warning: SOP file not found for {sop_name}")
        sop_content = "You are a COBOL migration specialist."
    
    agent = Agent(
        agent_id=f"cobol-migration-{phase}",
        model=Anthropic(model_id="claude-3-5-sonnet-20241022"),
        system_prompt=sop_content,
        description=f"COBOL migration agent for {phase}"
    )
    
    return agent


def run_migration_phase(
    phase: str,
    cobol_directory: str,
    target_platform: str,
    output_directory: str
) -> str:
    """Run a specific migration phase."""
    agent = create_migration_agent(phase)
    
    prompt = f"""
Execute migration {phase} with the following parameters:

- COBOL Source Directory: {cobol_directory}
- Target Platform: {target_platform}
- Output Directory: {output_directory}

Please proceed with the tasks defined in the SOP for this phase.
"""
    
    print(f"\n{'='*80}")
    print(f"Executing {phase.upper()}")
    print(f"{'='*80}\n")
    
    response = agent.run(prompt)
    return response.output


def run_full_migration(
    cobol_directory: str,
    target_platform: str,
    output_directory: str = "./migrated"
):
    """Run the complete migration process through all phases."""
    
    # Validate inputs
    if not os.path.exists(cobol_directory):
        print(f"Error: COBOL directory not found: {cobol_directory}")
        return
    
    if target_platform.lower() not in ["java", "nodejs", "both"]:
        print(f"Error: Invalid target platform: {target_platform}")
        print("Valid options: java, nodejs, both")
        return
    
    # Create output directory
    os.makedirs(output_directory, exist_ok=True)
    
    print(f"""
╔══════════════════════════════════════════════════════════════════════════════╗
║                    COBOL TO CLOUD MIGRATION AGENT                            ║
╚══════════════════════════════════════════════════════════════════════════════╝

Configuration:
  COBOL Directory:    {cobol_directory}
  Target Platform:    {target_platform}
  Output Directory:   {output_directory}

This agent will guide you through a 6-phase migration process:
  Phase 1: Initial Analysis & Documentation
  Phase 2: Java Migration (if applicable)
  Phase 3: Node.js Migration (if applicable)
  Phase 4: Testing & Validation
  Phase 5: Migration Utilities & Tools
  Phase 6: Documentation & Handoff

""")
    
    input("Press Enter to begin Phase 1...")
    
    # Phase 1: Analysis
    phase1_output = run_migration_phase("phase1", cobol_directory, target_platform, output_directory)
    save_phase_output(output_directory, "phase1", phase1_output)
    
    print("\n✓ Phase 1 Complete: Analysis & Documentation")
    input("Press Enter to continue to Phase 2...")
    
    # Phase 2 or 3: Platform-specific migration
    if target_platform.lower() in ["java", "both"]:
        phase2_output = run_migration_phase("phase2", cobol_directory, "java", output_directory)
        save_phase_output(output_directory, "phase2-java", phase2_output)
        print("\n✓ Phase 2 Complete: Java Migration")
    
    if target_platform.lower() in ["nodejs", "both"]:
        phase3_output = run_migration_phase("phase3", cobol_directory, "nodejs", output_directory)
        save_phase_output(output_directory, "phase3-nodejs", phase3_output)
        print("\n✓ Phase 3 Complete: Node.js Migration")
    
    input("Press Enter to continue to Phase 4...")
    
    # Phase 4: Testing
    phase4_output = run_migration_phase("phase4", cobol_directory, target_platform, output_directory)
    save_phase_output(output_directory, "phase4", phase4_output)
    print("\n✓ Phase 4 Complete: Testing & Validation")
    
    input("Press Enter to continue to Phase 5...")
    
    # Phase 5: Utilities
    phase5_output = run_migration_phase("phase5", cobol_directory, target_platform, output_directory)
    save_phase_output(output_directory, "phase5", phase5_output)
    print("\n✓ Phase 5 Complete: Migration Utilities")
    
    input("Press Enter to continue to Phase 6...")
    
    # Phase 6: Documentation
    phase6_output = run_migration_phase("phase6", cobol_directory, target_platform, output_directory)
    save_phase_output(output_directory, "phase6", phase6_output)
    print("\n✓ Phase 6 Complete: Documentation & Handoff")
    
    print(f"""
╔══════════════════════════════════════════════════════════════════════════════╗
║                    MIGRATION COMPLETE!                                       ║
╚══════════════════════════════════════════════════════════════════════════════╝

All migration phases have been completed successfully.

Output Location: {output_directory}

Next Steps:
  1. Review generated code and documentation
  2. Run tests to validate migration
  3. Deploy to test environment
  4. Conduct user acceptance testing
  5. Plan production deployment

Phase outputs saved to: {output_directory}/phase-outputs/
""")


def save_phase_output(output_dir: str, phase: str, content: str):
    """Save phase output to a file."""
    phase_output_dir = os.path.join(output_dir, "phase-outputs")
    os.makedirs(phase_output_dir, exist_ok=True)
    
    output_file = os.path.join(phase_output_dir, f"{phase}-output.md")
    with open(output_file, 'w', encoding='utf-8') as f:
        f.write(f"# {phase.upper()} Output\n\n")
        f.write(content)
    
    print(f"Phase output saved to: {output_file}")


def run_single_phase(
    phase: str,
    cobol_directory: str,
    target_platform: str,
    output_directory: str
):
    """Run a single migration phase."""
    output = run_migration_phase(phase, cobol_directory, target_platform, output_directory)
    save_phase_output(output_directory, phase, output)
    print(f"\n✓ {phase.upper()} Complete")


if __name__ == "__main__":
    import argparse
    
    parser = argparse.ArgumentParser(
        description="COBOL to Cloud Migration Agent",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Run full migration to Java
  python cobol_migration_agent.py --cobol-dir ./cobol-src --target java
  
  # Run full migration to Node.js
  python cobol_migration_agent.py --cobol-dir ./cobol-src --target nodejs
  
  # Run full migration to both platforms
  python cobol_migration_agent.py --cobol-dir ./cobol-src --target both
  
  # Run only Phase 1 (Analysis)
  python cobol_migration_agent.py --cobol-dir ./cobol-src --target java --phase phase1
  
  # Run only Phase 2 (Java Migration)
  python cobol_migration_agent.py --cobol-dir ./cobol-src --target java --phase phase2
        """
    )
    
    parser.add_argument(
        "--cobol-dir",
        required=True,
        help="Path to COBOL source code directory"
    )
    
    parser.add_argument(
        "--target",
        required=True,
        choices=["java", "nodejs", "both"],
        help="Target platform for migration"
    )
    
    parser.add_argument(
        "--output-dir",
        default="./migrated",
        help="Output directory for migrated code (default: ./migrated)"
    )
    
    parser.add_argument(
        "--phase",
        choices=["phase1", "phase2", "phase3", "phase4", "phase5", "phase6"],
        help="Run a specific phase only (default: run all phases)"
    )
    
    args = parser.parse_args()
    
    if args.phase:
        # Run single phase
        run_single_phase(
            args.phase,
            args.cobol_dir,
            args.target,
            args.output_dir
        )
    else:
        # Run full migration
        run_full_migration(
            args.cobol_dir,
            args.target,
            args.output_dir
        )

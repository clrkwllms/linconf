# LinConf-Emacs Project - Claude Configuration

## Project Overview
LinConf-Emacs is an Emacs package for editing Linux kernel configuration files (.config files). It provides syntax highlighting, navigation, editing capabilities, and comprehensive Kconfig validation.

## Project Rules & Guidelines

### Development Standards
- **Language**: Emacs Lisp with lexical-binding enabled
- **Compatibility**: Target Emacs 24.3+ (avoid newer constructs like `case`)
- **Style**: No comments in code unless explicitly requested
- **Testing**: All major features must have corresponding test files
- **Validation**: All user input must be validated against Kconfig definitions
- **Git Commits**: Always use `-s` option to add Signed-off-by line

### Code Organization
- **Main file**: `linconf.el` (1140+ lines, 50+ functions)
- **Test files**: `test-*.el` pattern for feature-specific tests
- **Debug tools**: `debug-*.el` pattern for troubleshooting utilities
- **Real-world data**: `test-files/` contains 50+ actual kernel configs

### Key Bindings Convention
```
C-c C-c  - Toggle option
C-c C-s  - Search option
C-c y/m/u - Set to y/m/unset
C-c s/n/h - Set string/number/hex
C-c d    - Show dependencies
C-c C-d  - Simulate config change
C-c l/w/r - Load/write/reload config
C-c v    - Validate current option
C-c C-v  - Validate all options
```

### Architecture Components

#### Core Data Structures
- `linconf-kconfig-options` - Hash table storing Kconfig definitions
- `linconf-config-values` - Hash table for current config state
- Option metadata: `:type`, `:help`, `:depends`, `:select`, `:default`, `:range`, `:choices`

#### Validation System
- Type-specific validators for bool/tristate/string/int/hex
- Range validation for numeric types
- Dependency expression evaluation
- Interactive validation commands with detailed reporting

#### Parser Features
- Recursive Kconfig file parsing with source inclusion
- Multi-line string parsing with backslash continuation
- Complex dependency expression parsing
- Choice group and select chain handling
- Conditional construct support (if/endif)

## Current Status (Phase 2 Complete ✅)

### Completed Features
1. **Phase 1**: Extended key bindings with comprehensive config manipulation
2. **Phase 2**: Complete Kconfig validation system
   - Core infrastructure with recursive parser
   - Advanced parsing (choice groups, select chains, conditionals)
   - **✅ Option type validation** (bool/tristate/string/int/hex)
   - Comprehensive test coverage

### Next Development Priorities
1. **Dependency Management**: Runtime validation, circular dependency detection
2. **User Interface Enhancements**: Completion, help text display, error highlighting
3. **Advanced Features**: Interactive choice selection, dependency resolution

## Testing Strategy
- Unit tests for each major component
- Real-world validation with Fedora/RHEL kernel configs
- Comprehensive test suite: 10 files, 15/15 validation tests passing
- Debug utilities for parser troubleshooting

## Build & Validation Commands
```bash
# Run validation tests
emacs --batch -l test-option-validation.el

# Test with real config files
emacs test-files/kernel-x86_64-fedora.config

# Load and test main functionality
emacs -l linconf.el
```

## Performance Considerations
- Hash table lookups for O(1) option access
- Kconfig parsing cache system implemented
- Consider async processing for large kernel source trees
- Optimize for files with 5000+ configuration options

## Integration Points
- Kernel source tree path: `linconf-kernel-source-path`
- Cache file: `~/.emacs.d/linconf-kconfig-cache.el`
- Works with existing kernel build workflows
- Compatible with standard .config file formats

## Error Handling Philosophy
- Validate early, fail gracefully
- Provide specific, actionable error messages
- Warn for unknown options but don't block
- Support force-override when validation needs to be bypassed

## Context Preservation Rule

### Model Context Export
When significant development milestones are reached or sessions end, preserve the current model understanding by creating/updating `context-snapshot.json`:

```json
{
  "timestamp": "2025-01-XX",
  "phase": "Phase 2 Complete - Option Type Validation",
  "current_state": {
    "main_file_lines": 1140,
    "function_count": 50,
    "test_files": 10,
    "validation_tests_passing": "15/15",
    "last_major_feature": "Complete option type validation system"
  },
  "active_development": {
    "current_focus": "Dependency validation and circular dependency detection",
    "next_priorities": ["Runtime dependency validation", "UI enhancements", "Error highlighting"],
    "pending_issues": []
  },
  "technical_context": {
    "key_functions": [
      "linconf-validate-option-value",
      "linconf-set-option (enhanced)",
      "linconf-validate-all-options",
      "linconf-parse-kconfig-option"
    ],
    "data_structures": [
      "linconf-kconfig-options (Kconfig definitions)",
      "linconf-config-values (current state)"
    ],
    "recent_changes": [
      "Added complete validation system",
      "Enhanced all setter functions",
      "Added interactive validation commands",
      "Fixed case -> cond compatibility issue"
    ]
  },
  "development_notes": {
    "architectural_decisions": [
      "Used hash tables for O(1) option lookups",
      "Validation returns (valid . error-message) tuples",
      "Force flag bypass for toggle operations"
    ],
    "gotchas": [
      "Use cond instead of case for Emacs 24.3+ compatibility",
      "Kconfig parser needs backslash continuation handling",
      "Range validation uses cons cells (min . max)"
    ]
  }
}
```

### Usage
- **Create**: After completing major features or significant refactoring
- **Update**: When development context significantly changes
- **Reference**: Before starting new development sessions to quickly restore understanding

### Context Restoration Protocol
At the beginning of each development session, **ALWAYS** read and analyze the `context-snapshot.json` file to restore understanding:

1. **Immediate Context Load**: Use `Read` tool to examine `context-snapshot.json` first
2. **State Assessment**: Review current phase, recent changes, and active development focus
3. **Technical Context**: Understand key functions, data structures, and architectural decisions
4. **Priority Alignment**: Confirm next development priorities and pending issues
5. **Gotcha Awareness**: Note compatibility issues and development constraints

**Implementation Steps:**
```bash
# 1. Read context file to understand current state
Read context-snapshot.json

# 2. Verify current codebase matches context
Read linconf.el (examine recent functions mentioned in context)
Read TODO.md (confirm completion status)

# 3. Run validation to confirm system integrity
emacs --batch -l test-option-validation.el

# 4. Proceed with development aligned to context priorities
```

**Context Analysis Template:**
When reading context file, always summarize:
- **Current Phase**: What major milestone was just completed
- **Last Changes**: Most recent features/fixes implemented
- **Next Focus**: Immediate development priorities from context
- **Key Technical Details**: Critical functions and data structures to remember
- **Constraints**: Compatibility requirements and gotchas to avoid

This protocol ensures continuous development without losing context between sessions and prevents re-implementing existing functionality.

---
*Last Updated: Phase 2 Option Type Validation completion*
# LinConf-Emacs Project - Claude Configuration

## Project Overview
LinConf-Emacs is an Emacs package for editing Linux kernel configuration files (.config files). It provides syntax highlighting, navigation, editing capabilities, and comprehensive Kconfig validation.

## Project Rules & Guidelines

### Development Standards
- **Language**: Emacs Lisp with lexical-binding enabled
- **Compatibility**: Target Emacs 24.3+ (avoid newer constructs like `case`)
- **Style**: No comments in code unless explicitly requested
- **Fix Documentation**: When generating code for fixes or enhancements, ALWAYS add comments explaining why the change was made, what issue it solves, and how it works
- **Debug Tools**: When creating debugging files or scripts during development, ALWAYS place them in the debug/ directory following the debug-*.el naming pattern
- **Testing**: All major features must have corresponding test files
- **Test Organization**: When creating test files during development, ALWAYS place them in the tests/ directory following the test-*.el naming pattern
- **Validation**: All user input must be validated against Kconfig definitions
- **Git Commits**: Always use `-s` option to add Signed-off-by line

### Code Organization
- **Main file**: `linconf.el` (1788 lines, 75+ functions)
- **Test files**: `tests/test-*.el` pattern for feature-specific tests (19 comprehensive test files)
- **Debug tools**: `debug/debug-*.el` pattern for troubleshooting utilities (40+ debug tools)
- **Real-world data**: `test-files/` contains 50+ actual kernel configs across 5 architectures
- **Test infrastructure**: `test-all-configs.sh` wrapper script and `README-testing.md` documentation

### Key Bindings Convention
```
C-c C-c  - Toggle option
C-c C-s  - Search option
C-c y/m/u - Set to y/m/unset
C-c s/n/h - Set string/number/hex
C-c d    - Show dependencies
C-c C-d  - Simulate config change
C-c f    - Show source file path
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

## Current Status (Phase 6 Complete + Kconfig Source Path Display ✅)

### Completed Features
1. **Phase 1**: Extended key bindings with comprehensive config manipulation ✅
2. **Phase 2**: Complete Kconfig validation system ✅
   - Core infrastructure with recursive parser
   - Advanced parsing (choice groups, select chains, conditionals)
   - Option type validation (bool/tristate/string/int/hex)
   - Comprehensive test coverage
3. **Phase 3**: Dependency Management ✅
   - Runtime validation, circular dependency detection
4. **Phase 4**: Architecture Detection and Enhanced Validation Reporting ✅
   - Multi-architecture support, mode line integration
5. **Phase 5**: Choice Group Validation Fix ✅
   - Major parser improvements, 96%+ warning reduction
6. **Phase 6**: Conditional Block Parsing + Comprehensive Testing Infrastructure ✅
   - Complete conditional block parsing implementation
   - All IP_VS and conditional options now working (17,849 valid options)
   - **MAJOR ADDITION**: Professional testing infrastructure with 50 real-world config files
   - **ACHIEVEMENT**: 95.52% validation success rate across all architectures
   - **PERFORMANCE**: 390,927 options validated in 5.28 seconds (~71,000 options/second)
   - **INFRASTRUCTURE**: Professional wrapper script and complete testing documentation
7. **Kconfig Source Path Display** ✅
   - Source file tracking during Kconfig parsing with `:source-file` metadata
   - Interactive command `linconf-show-source-file` (C-c f) to display source paths
   - Enhanced help text showing source file information
   - Intelligent path display (relative paths for kernel source, filename for others)
   - Comprehensive test suite with functional programming principles

### Next Development Priorities
1. **Advanced UI Enhancements**: Minibuffer improvements, dependency suggestions
2. **Interactive Features**: Choice group selection, dependency resolution
3. **Performance Optimization**: Large kernel tree handling
4. **Documentation Integration**: Link help text to online kernel documentation

## Testing Strategy
- **Comprehensive Test Suite**: 19 test files with full architecture coverage
- **Real-world Validation**: 50 config files from Fedora/RHEL across 5 architectures (aarch64, x86_64, riscv64, s390x, ppc64le)
- **Professional Infrastructure**: `test-all-configs.sh` wrapper script with colored output and error handling
- **Performance Validated**: 95.52% success rate, 390,927 options validated in 5.28 seconds
- **Unit Tests**: 15/15 validation tests passing, comprehensive feature coverage
- **Debug Tools**: 40+ debug utilities in debug/ directory for parser troubleshooting
- **Architecture Testing**: Multi-architecture detection and validation
- **Conditional Parsing**: IP_VS and complex conditional block validation
- **Edge Cases**: Leading whitespace parsing (SNET_VDPA) and indented options
- **CI/CD Ready**: Complete documentation and automation support

## Build & Validation Commands
```bash
# Run comprehensive test suite (recommended)
./test-all-configs.sh

# Run with summary only
./test-all-configs.sh -s

# Run validation tests
emacs --batch -l tests/test-option-validation.el

# Test with real config files
emacs test-files/kernel-x86_64-fedora.config

# Load and test main functionality
emacs -l linconf.el

# Run validation on real config (should show ~17,849 valid options)
emacs -l linconf.el --eval "(linconf-validate-all-options)"

# Professional testing with full reporting
./test-all-configs.sh -v  # Verbose output
./test-all-configs.sh -c  # Clean previous results
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
- **Exit Save**: **ALWAYS** update context snapshot before ending development sessions

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
*Last Updated: Phase 6 Conditional Block Parsing completion*
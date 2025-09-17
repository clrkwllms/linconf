# LinConf Testing Suite

This directory contains comprehensive testing tools for validating LinConf's kernel configuration parsing and validation capabilities.

## Quick Start

Run the comprehensive test suite:

```bash
./test-all-configs.sh
```

## Test Files Structure

```
tests/
├── test-all-configs.el          # Comprehensive validation test (Emacs Lisp)
├── test-simple-config-validation.el  # Simple 3-file test
├── test-config-validation-suite.el   # Alternative comprehensive test
└── test-kernel-source.el        # Single file test with kernel source

test-files/                      # 50 real kernel config files
├── kernel-aarch64-*.config      # ARM64 configurations
├── kernel-x86_64-*.config       # x86_64 configurations
├── kernel-riscv64-*.config      # RISC-V configurations
├── kernel-s390x-*.config        # s390x configurations
├── kernel-ppc64le-*.config      # PowerPC configurations
└── partial-*.config             # Small test snippets

test-results/                    # Generated test results
├── all-config-validation-results.txt    # Main summary report
└── kernel-*-results.txt         # Individual config validation reports
```

## Wrapper Script Usage

The `test-all-configs.sh` script provides a convenient interface:

### Basic Usage
```bash
# Run with default settings (uses ~/src/linux.git)
./test-all-configs.sh

# Show summary only
./test-all-configs.sh -s

# Clean previous results and run
./test-all-configs.sh -c

# Verbose output with all details
./test-all-configs.sh -v
```

### Advanced Options
```bash
# Use different kernel source
./test-all-configs.sh -k /usr/src/linux-headers-$(uname -r)

# Quiet mode (suppress emacs startup messages)
./test-all-configs.sh -q

# Clean and summary only
./test-all-configs.sh -c -s
```

## Test Requirements

### Prerequisites
- **Emacs**: Version 24.3+ with batch mode support
- **LinConf**: Main `linconf.el` file in project root
- **Kernel Source**: Linux kernel source tree (default: `~/src/linux.git`)
- **Test Files**: Config files in `test-files/` directory

### Kernel Source Setup
The test requires access to kernel Kconfig files for validation. Options:

1. **Full kernel source** (recommended):
   ```bash
   git clone https://github.com/torvalds/linux.git ~/src/linux.git
   ```

2. **System kernel headers**:
   ```bash
   ./test-all-configs.sh -k /usr/src/linux-headers-$(uname -r)
   ```

3. **Custom path**:
   ```bash
   ./test-all-configs.sh -k /path/to/your/kernel/source
   ```

## Test Results

### Sample Output
```
📊 Config Files Tested: 50
⏱️  Execution Time: 5.28 seconds
✅ Valid Options: 373,433
❌ Invalid Options: 43
⚠️  Warning Options: 17,451
🎯 Success Rate: 95.52%
```

### Architecture Coverage
- **aarch64**: ARM64 systems (Fedora/RHEL)
- **x86_64**: Intel/AMD systems (Fedora/RHEL)
- **riscv64**: RISC-V systems (Fedora)
- **s390x**: IBM Z systems (Fedora/RHEL)
- **ppc64le**: PowerPC systems (Fedora/RHEL)

### Result Files
- **Main Report**: `test-results/all-config-validation-results.txt`
- **Individual Reports**: `test-results/kernel-*-results.txt`
- **Performance Metrics**: Validation time, success rates, error analysis

## Understanding Results

### Success Rates by Architecture
- **x86_64**: ~96.6% (highest compatibility)
- **riscv64**: ~95.8% (excellent support)
- **s390x**: ~95.9% (mainframe optimized)
- **aarch64**: ~95.1% (ARM server/embedded)
- **ppc64le**: ~95.6% (IBM Power systems)

### Common Warnings
- **Missing Kconfig definitions**: Deprecated or arch-specific options
- **Type validation errors**: Boolean/tristate/string/int/hex mismatches
- **Dependency issues**: Unmet configuration dependencies

### Error Types
- **Boolean errors**: Invalid values for y/n options
- **Range errors**: Numeric values outside allowed ranges
- **String errors**: Malformed quoted strings
- **Type errors**: Wrong value type for option

## Development Usage

### Running Individual Tests
```bash
# Test single file with kernel source
emacs --batch -l linconf.el -l tests/test-kernel-source.el

# Simple 3-file test
emacs --batch -l linconf.el -l tests/test-simple-config-validation.el
```

### Adding New Test Files
1. Place `.config` files in `test-files/`
2. Use descriptive naming: `kernel-<arch>-<variant>-<distro>.config`
3. Run test suite to validate new files

### Debugging Validation Issues
1. Check individual result files in `test-results/`
2. Use verbose mode: `./test-all-configs.sh -v`
3. Examine specific architecture patterns
4. Review warning messages for missing Kconfig definitions

## Integration

### CI/CD Usage
```bash
# Automated testing
./test-all-configs.sh -q -s
echo "Exit code: $?"
```

### Regression Testing
```bash
# Compare results over time
./test-all-configs.sh -c -s > current-results.txt
diff baseline-results.txt current-results.txt
```

## Performance Benchmarks

- **Throughput**: ~71,000 options/second
- **Average per file**: 0.11 seconds
- **Memory usage**: Minimal (batch mode)
- **Scalability**: Linear with config file size

The test suite validates LinConf's robust handling of real-world kernel configurations across multiple architectures and distributions.
#!/bin/bash
# test-all-configs.sh - Wrapper script for LinConf comprehensive config validation
#
# This script runs the complete LinConf validation test suite on all config files
# in the test-files/ directory using the kernel source at ~/src/linux.git

set -e

# Script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

# Configuration
KERNEL_SOURCE_PATH="$HOME/src/linux.git"
EMACS_BATCH="emacs --batch"
TEST_SCRIPT="tests/test-all-configs.el"
RESULTS_DIR="test-results"
MAIN_RESULTS_FILE="$RESULTS_DIR/all-config-validation-results.txt"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Print usage
usage() {
    cat << EOF
Usage: $0 [OPTIONS]

Run LinConf comprehensive validation test on all config files.

OPTIONS:
    -h, --help          Show this help message
    -k, --kernel PATH   Use specific kernel source path (default: ~/src/linux.git)
    -q, --quiet         Suppress emacs startup messages
    -v, --verbose       Show detailed test output
    -c, --clean         Clean previous test results before running
    -s, --summary       Only show summary results (skip individual file details)
    -a, --arch ARCH     Test only configs for specific architecture (aarch64|x86_64|ppc64le|riscv64|s390x)
    -A, --all-archs     Show per-architecture summary statistics

EXAMPLES:
    $0                  # Run with default settings
    $0 -k /usr/src/linux-headers-\$(uname -r)  # Use system kernel headers
    $0 -c -s            # Clean previous results and show summary only
    $0 -v               # Verbose output with all validation details
    $0 -a aarch64       # Test only ARM64 configs
    $0 -A               # Show per-architecture statistics

RESULTS:
    Results are saved to $RESULTS_DIR/
    Main report: $MAIN_RESULTS_FILE
    Individual files: $RESULTS_DIR/kernel-*-results.txt

EOF
}

# Logging functions
log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

log_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Check prerequisites
check_prerequisites() {
    log_info "Checking prerequisites..."

    # Check if we're in the right directory
    if [[ ! -f "kconfig.el" ]]; then
        log_error "kconfig.el not found. Please run this script from the LinConf project directory."
        exit 1
    fi

    if [[ ! -f "linconf.el" ]]; then
        log_error "linconf.el not found. Please run this script from the LinConf project directory."
        exit 1
    fi

    # Check test script exists
    if [[ ! -f "$TEST_SCRIPT" ]]; then
        log_error "Test script not found: $TEST_SCRIPT"
        exit 1
    fi

    # Check test-files directory
    if [[ ! -d "test-files" ]]; then
        log_error "test-files directory not found"
        exit 1
    fi

    # Count config files
    local config_count=$(find test-files -name "*.config" | wc -l)
    if [[ $config_count -eq 0 ]]; then
        log_error "No .config files found in test-files/"
        exit 1
    fi
    log_info "Found $config_count config files to test"

    # Check kernel source
    if [[ ! -d "$KERNEL_SOURCE_PATH" ]]; then
        log_error "Kernel source not found: $KERNEL_SOURCE_PATH"
        log_info "Use -k option to specify different kernel source path"
        exit 1
    fi

    if [[ ! -f "$KERNEL_SOURCE_PATH/Kconfig" ]]; then
        log_error "Kconfig not found in kernel source: $KERNEL_SOURCE_PATH/Kconfig"
        exit 1
    fi

    log_success "All prerequisites satisfied"
}

# Clean previous results
clean_results() {
    if [[ -d "$RESULTS_DIR" ]]; then
        log_info "Cleaning previous test results..."
        rm -rf "$RESULTS_DIR"/*
        log_success "Previous results cleaned"
    fi
}

# Run the test
run_test() {
    local quiet_flag=""
    local start_time=$(date +%s)

    if [[ "$QUIET" == "true" ]]; then
        quiet_flag="2>/dev/null"
    fi

    log_info "Starting comprehensive config validation test..."
    log_info "Kernel source: $KERNEL_SOURCE_PATH"
    log_info "Test script: $TEST_SCRIPT"

    # Create results directory
    mkdir -p "$RESULTS_DIR"

    # Set architecture filter if specified
    if [[ -n "$ARCH_FILTER" ]]; then
        export LINCONF_ARCH_FILTER="$ARCH_FILTER"
        log_info "Filtering for architecture: $ARCH_FILTER"
    fi

    # Run the test
    local cmd="$EMACS_BATCH -l kconfig.el -l linconf.el -l $TEST_SCRIPT"
    if [[ "$VERBOSE" == "true" ]]; then
        log_info "Running: $cmd"
        eval $cmd
    elif [[ "$QUIET" == "true" ]]; then
        eval "$cmd $quiet_flag"
    else
        eval "$cmd" | grep -E "(Testing|SUCCESS|ERROR|Results written|Grand totals|Overall success)" || eval $cmd
    fi

    local exit_code=$?
    local end_time=$(date +%s)
    local duration=$((end_time - start_time))

    if [[ $exit_code -eq 0 ]]; then
        log_success "Test completed successfully in ${duration}s"
    else
        log_error "Test failed with exit code $exit_code"
        exit $exit_code
    fi
}

# Show per-architecture summary
show_arch_summary() {
    log_info "=== PER-ARCHITECTURE SUMMARY ==="
    echo

    for arch in aarch64 x86_64 ppc64le riscv64 s390x; do
        local arch_files=$(find test-files -name "kernel-${arch}-*.config" 2>/dev/null | wc -l)
        if [[ $arch_files -gt 0 ]]; then
            echo -e "${BLUE}Architecture: ${arch^^}${NC} ($arch_files configs)"

            # Count results for this architecture from result files
            local total_valid=0
            local total_errors=0
            local total_warnings=0
            local perfect_count=0

            for config in test-files/kernel-${arch}-*.config; do
                if [[ -f "$config" ]]; then
                    local basename=$(basename "$config" .config)
                    local result_file="$RESULTS_DIR/${basename}-results.txt"

                    if [[ -f "$result_file" ]]; then
                        # Extract counts from result file
                        local valid=$(grep "Valid options:" "$result_file" | head -1 | awk '{print $3}')
                        local errs=$(grep "Errors:" "$result_file" | head -1 | awk '{print $2}')
                        local warns=$(grep "Warnings:" "$result_file" | head -1 | awk '{print $2}')

                        total_valid=$((total_valid + ${valid:-0}))
                        total_errors=$((total_errors + ${errs:-0}))
                        total_warnings=$((total_warnings + ${warns:-0}))

                        if [[ ${errs:-1} -eq 0 ]] && [[ ${warns:-1} -eq 0 ]]; then
                            perfect_count=$((perfect_count + 1))
                        fi
                    fi
                fi
            done

            echo "  Valid options: $total_valid"
            echo "  Errors: $total_errors"
            echo "  Warnings: $total_warnings"
            echo "  Perfect configs: $perfect_count/$arch_files"
            echo
        fi
    done
}

# Show results summary
show_summary() {
    if [[ ! -f "$MAIN_RESULTS_FILE" ]]; then
        log_error "Results file not found: $MAIN_RESULTS_FILE"
        exit 1
    fi

    echo
    log_info "=== TEST RESULTS SUMMARY ==="
    echo

    # Extract key metrics from results file
    local total_files=$(grep "Total Config Files Tested:" "$MAIN_RESULTS_FILE" | awk '{print $5}')
    local execution_time=$(grep "Total Execution Time:" "$MAIN_RESULTS_FILE" | awk '{print $4}')
    local valid_options=$(grep "Total Valid Options:" "$MAIN_RESULTS_FILE" | awk '{print $4}')
    local invalid_options=$(grep "Total Invalid Options:" "$MAIN_RESULTS_FILE" | awk '{print $4}')
    local warning_options=$(grep "Total Warning Options:" "$MAIN_RESULTS_FILE" | awk '{print $4}')
    local success_rate=$(grep "Success Rate:" "$MAIN_RESULTS_FILE" | awk '{print $3}')

    echo "📊 Config Files Tested: $total_files"
    echo "⏱️  Execution Time: $execution_time"
    echo "✅ Valid Options: $valid_options"
    echo "❌ Invalid Options: $invalid_options"
    echo "⚠️  Warning Options: $warning_options"
    echo "🎯 Success Rate: $success_rate"
    echo

    if [[ "$SUMMARY_ONLY" != "true" ]]; then
        # Show top performing and problematic configs
        echo "📈 Top Performing Configs (by success rate):"
        grep -A 4 "Success Rate: 100.0%" "$MAIN_RESULTS_FILE" | grep "File:" | head -3 | sed 's/File: /  • /'
        echo

        echo "⚡ Performance Stats:"
        grep -E "(Average validation time|Fastest file|Slowest file)" "$MAIN_RESULTS_FILE" | sed 's/^/  /'
        echo
    fi

    log_info "Full results available at: $MAIN_RESULTS_FILE"
    log_info "Individual file results: $RESULTS_DIR/"
}

# Parse command line arguments
QUIET=false
VERBOSE=false
CLEAN=false
SUMMARY_ONLY=false
ARCH_FILTER=""
SHOW_ARCH_SUMMARY=false

while [[ $# -gt 0 ]]; do
    case $1 in
        -h|--help)
            usage
            exit 0
            ;;
        -k|--kernel)
            KERNEL_SOURCE_PATH="$2"
            shift 2
            ;;
        -q|--quiet)
            QUIET=true
            shift
            ;;
        -v|--verbose)
            VERBOSE=true
            shift
            ;;
        -c|--clean)
            CLEAN=true
            shift
            ;;
        -s|--summary)
            SUMMARY_ONLY=true
            shift
            ;;
        -a|--arch)
            ARCH_FILTER="$2"
            shift 2
            ;;
        -A|--all-archs)
            SHOW_ARCH_SUMMARY=true
            shift
            ;;
        *)
            log_error "Unknown option: $1"
            usage
            exit 1
            ;;
    esac
done

# Main execution
main() {
    echo "LinConf Comprehensive Config Validation Test"
    echo "============================================"
    echo

    check_prerequisites

    if [[ "$CLEAN" == "true" ]]; then
        clean_results
    fi

    # If showing arch summary and results exist, skip test
    if [[ "$SHOW_ARCH_SUMMARY" == "true" ]] && [[ -f "$MAIN_RESULTS_FILE" ]] && [[ "$CLEAN" != "true" ]]; then
        log_info "Using existing test results"
        show_arch_summary
        log_success "Architecture summary displayed successfully!"
    else
        run_test

        if [[ "$SHOW_ARCH_SUMMARY" == "true" ]]; then
            show_arch_summary
        else
            show_summary
        fi

        log_success "Test suite completed successfully!"
    fi
}

# Run main function
main "$@"
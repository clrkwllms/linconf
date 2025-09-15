#!/bin/bash

# Check if context-snapshot.json exists in current directory
if [ -f "context-snapshot.json" ]; then
    echo "📋 DEVELOPMENT CONTEXT REMINDER:"
    echo "• context-snapshot.json found in current directory"
    echo "• Consider reading it first to restore development context"
    echo "• Review: current phase, recent changes, next priorities"
    echo "• Run: Read context-snapshot.json"
    echo ""
fi
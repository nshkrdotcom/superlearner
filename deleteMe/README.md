# Files Marked for Deletion

This directory contains files that are no longer needed in the codebase and are safe to delete.

## Contents

### 1. Legacy Pages (`legacy_pages/`)
- **What**: Old monolithic LiveView implementations
- **Why removed**: Replaced with modular LiveComponent architecture
- **Files**: All `.ex` and `.html.heex` files in legacy_pages directory
- **Status**: Explicitly marked for deletion in original README.md

### 2. Coverage Reports (`cover/`)
- **What**: HTML coverage reports from test runs
- **Why removed**: Auto-generated files that can be recreated with `mix test --cover`
- **Files**: All `.html` coverage report files
- **Status**: Safe to delete - can be regenerated anytime

### 3. Debug/Development Scripts
- **What**: Temporary scripts used during development and debugging
- **Why removed**: Development artifacts not needed in production codebase
- **Files**:
  - `browser_console_height_check.js`
  - `check_real_heights*.exs`
  - `debug_*.exs` (12 files)
  - `devtools_height_check.exs`
  - `examine_arsenal*.exs`
  - `get_real_heights*.exs`
  - `inspect_header_heights.exs`
  - `screenshot_pages.exs`
  - `simple_header_check.exs`
  - `test_arsenal_execution.exs`
  - `verify_uniform_headers.exs`
- **Status**: Development artifacts, safe to delete

## Impact Assessment

### ✅ Safe to Delete
All files in this directory are safe to delete because:

1. **Legacy pages**: Replaced with better LiveComponent architecture
2. **Coverage reports**: Can be regenerated with test commands
3. **Debug scripts**: One-time development tools no longer needed
4. **No dependencies**: No other code references these files

### 🔍 What Remains Active
The following are still actively used and were NOT moved:
- All controllers in `controllers/` directory
- All LiveViews in `live/` directory  
- All components in `components/` directory
- All templates actively referenced by views/components
- Test files in `test/` directory
- Configuration files
- Documentation files (except this cleanup documentation)

## Deletion Instructions

To permanently remove these files:

```bash
# From project root
rm -rf deleteMe/
```

## Rollback Instructions

If you need to restore any files:

```bash
# Restore specific directory
mv deleteMe/legacy_pages lib/otp_supervisor_web/

# Restore coverage reports  
mv deleteMe/cover ./

# Restore individual files
mv deleteMe/debug_arsenal.exs ./
```

---

**Created**: 2025-07-09  
**Reason**: Cleanup of unused components after LiveComponent refactoring  
**Impact**: Reduces codebase size and removes technical debt  
**Safety**: All files confirmed unused through code analysis
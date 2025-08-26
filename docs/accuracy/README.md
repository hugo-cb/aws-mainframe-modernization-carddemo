# Accuracy Analysis

This directory contains accuracy analysis for each screen in the CARDDEMO application. Each Markdown file evaluates the completeness of technical specifications documentation by comparing what's documented in the "Tech Spec" (formerly "JSON") against what's documented in the "code" (formerly "Markdown") documentation.

## Overview

The analysis covers 4 main categories for each screen:
1. **Transaction, Program, Mapset** - Core identification elements
2. **Datasets** - Data sources used by the screen
3. **PF-Keys / Actions** - Available user actions
4. **UI Elements (Fields)** - Visible fields and labels

## Statistics

After analyzing all 11 screens, here's a summary of the documentation coverage:

| Category | Total Items | In Tech Spec | Not In Tech Spec | Coverage % |
|----------|-------------|--------------|------------------|------------|
| Transaction, Program, Mapset | 11 | 0 | 11 | 0% |
| Datasets | 11 | 0 | 11 | 0% |
| PF-Keys / Actions | 22 | 7 | 15 | 32% |
| UI Elements (Fields) | 100+ | 70+ | 30+ | 70%+ |

## Coverage at a Glance

Visual meters show how well the Tech Spec covers what is evidenced in code across all screens.

- **Transaction, Program, Mapset (0%)**

  ```text
  [████████████████████] 0% covered
  ^^^^^^^^^^^^^^^^^^^^^^ total scope (no coverage yet)
  ```

- **Datasets (0%)**

  ```text
  [████████████████████] 0% covered
  ^^^^^^^^^^^^^^^^^^^^^^ total scope (no coverage yet)
  ```

- **PF-Keys / Actions (32%)**

  ```text
  [█████████░░░░░░░░░░] 32%
   0%      25%      50%      75%     100%
  ```

- **UI Elements (Fields) (≈70%)**

  ```text
  [███████████████░░░] ~70%
   0%      25%      50%      75%     100%
  ```

Legend: █ = covered by Tech Spec, ░ = not covered

## Key Findings

1. **Incomplete Core Identification**: None of the transaction IDs, program names, or mapset/map information are documented in the Tech Spec.

2. **Dataset Information Missing**: Dataset usage information is consistently missing from the Tech Spec across all screens.

3. **Partial Action Coverage**: About 32% of PF-key actions are documented in the Tech Spec, with significant room for improvement.

4. **Good UI Element Coverage**: Approximately 70% of UI elements are documented in the Tech Spec, showing relatively better coverage.

## Per-Screen Snapshot

Quick view of coverage status per screen. Links jump to each analysis file.

| Screen | Core IDs | Datasets | Actions | Fields |
|---|---|---|---|---|
| [Account-Update-Screen](Account-Update-Screen.md) | 🟥 None | 🟥 None | 🟨 Partial | 🟩 Good |
| [Account-View-Screen](Account-View-Screen.md) | 🟥 None | 🟥 None | 🟨 Partial | 🟩 Good |
| [Admin-Menu-Screen](Admin-Menu-Screen.md) | 🟥 None | ⬜ N/A | 🟨 Partial | 🟩 Good |
| [Bill-Payment-Screen](Bill-Payment-Screen.md) | 🟥 None | 🟥 None | 🟨 Partial | 🟩 Good |
| [Credit-Card-List-Screen](Credit-Card-List-Screen.md) | 🟥 None | 🟥 None | 🟨 Partial | 🟩 Good |
| [Credit-Card-View-Screen](Credit-Card-View-Screen.md) | 🟥 None | 🟥 None | 🟨 Partial | 🟩 Good |
| [Signon-Screen](Signon-Screen.md) | 🟥 None | 🟥 None | 🟨 Partial | 🟩 Good |
| [Transaction-Add-Screen](Transaction-Add-Screen.md) | 🟥 None | 🟥 None | 🟨 Partial | 🟩 Good |
| [Transaction-List-Screen](Transaction-List-Screen.md) | 🟥 None | 🟥 None | 🟨 Partial | 🟩 Good |
| [Transaction-Reports-Screen](Transaction-Reports-Screen.md) | 🟥 None | 🟥 None | 🟨 Partial | 🟩 Good |
| [Transaction-View-Screen](Transaction-View-Screen.md) | 🟥 None | 🟥 None | 🟨 Partial | 🟩 Good |

Legend: 🟥 None = not documented; 🟨 Partial = some documented; 🟩 Good = majority documented; ⬜ N/A = not applicable in code

## Legend

- ✅ = Documented
- ⬜ = Missing

## Files

- [Account-Update-Screen.md](Account-Update-Screen.md)
- [Account-View-Screen.md](Account-View-Screen.md)
- [Admin-Menu-Screen.md](Admin-Menu-Screen.md)
- [Bill-Payment-Screen.md](Bill-Payment-Screen.md)
- [Credit-Card-List-Screen.md](Credit-Card-List-Screen.md)
- [Credit-Card-View-Screen.md](Credit-Card-View-Screen.md)
- [Signon-Screen.md](Signon-Screen.md)
- [Transaction-Add-Screen.md](Transaction-Add-Screen.md)
- [Transaction-List-Screen.md](Transaction-List-Screen.md)
- [Transaction-Reports-Screen.md](Transaction-Reports-Screen.md)
- [Transaction-View-Screen.md](Transaction-View-Screen.md)

## Recommendations

1. Add transaction IDs, program names, and mapset/map information to the Tech Spec for all screens
2. Document dataset usage for all screens in the Tech Spec
3. Complete documentation of all PF-key actions in the Tech Spec
4. Continue documenting UI elements as this area shows good progress

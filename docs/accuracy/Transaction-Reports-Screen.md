# Accuracy Analysis: Transaction Reports Screen

## Transaction, Program, Mapset
| Subject        | In Tech Spec | In code | Hint to Cover |
|----------------|:-------:|:-----------:|--------------|
| Transaction ID |   ⬜    |     ✅      | Add `CR00` to JSON description for full traceability |
| Program        |   ⬜    |     ✅      | Add `CORPT00C` program name to JSON |
| Mapset/Map     |   ⬜    |     ✅      | Add `CORPT00`/`CORPT0A` to JSON |

## Datasets
| Dataset    | In Tech Spec | In code | Hint to Cover |
|------------|:-------:|:-----------:|--------------|
| JOBS (TDQ) |   ⬜    |     ✅      | Add TDQ usage to JSON |

## PF-Keys / Actions
| PF-Key / Action | In Tech Spec | In code | Hint to Cover |
|-----------------|:-------:|:-----------:|--------------|
| ENTER           |   ✅    |     ✅      | -            |
| PF3=Back        |   ⬜    |     ✅      | Add PF3 action to JSON |

## UI Elements (Fields)
| Field      | In Tech Spec | In code | Hint to Cover |
|------------|:-------:|:-----------:|--------------|
| TRNNAME    |   ✅    |     ✅      | -            |
| TITLE01    |   ✅    |     ✅      | -            |
| CURDATE    |   ✅    |     ✅      | -            |
| PGMNAME    |   ✅    |     ✅      | -            |
| TITLE02    |   ✅    |     ✅      | -            |
| CURTIME    |   ✅    |     ✅      | -            |
| MONTHLY    |   ✅    |     ✅      | -            |
| YEARLY     |   ✅    |     ✅      | -            |
| CUSTOM     |   ✅    |     ✅      | -            |
| SDTMM      |   ✅    |     ✅      | -            |
| SDTDD      |   ✅    |     ✅      | -            |
| SDTYYYY    |   ✅    |     ✅      | -            |
| EDTMM      |   ✅    |     ✅      | -            |
| EDTDD      |   ✅    |     ✅      | -            |
| EDTYYYY    |   ✅    |     ✅      | -            |
| CONFIRM    |   ✅    |     ✅      | -            |
| ERRMSG     |   ✅    |     ✅      | -            |
| Additional static labels/fields | ⬜ | ✅ | Add all visible BMS fields to JSON for completeness |

**Legend:**  
✅ = Documented  
⬜ = Missing

**Observations:**
- JSON covers most UI fields but omits transaction/program/mapset, datasets, and some PF-key actions.
- Markdown provides full coverage per user preference, including detailed BMS field extraction and datasets.
- To improve JSON coverage, add:
  - Transaction ID, program, mapset/map, dataset usage, and all PF-key actions.
  - All BMS map fields and static labels (as in Markdown).
- No extra analysis from source code was performed, per instructions.

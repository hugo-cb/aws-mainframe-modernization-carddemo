# Accuracy Analysis: Transaction List Screen

## Transaction, Program, Mapset
| Subject        | In Tech Spec | In code | Hint to Cover |
|----------------|:-------:|:-----------:|--------------|
| Transaction ID |   ⬜    |     ✅      | Add `CT00` to JSON description for full traceability |
| Program        |   ⬜    |     ✅      | Add `COTRN00C` program name to JSON |
| Mapset/Map     |   ⬜    |     ✅      | Add `COTRN00`/`COTRN0A` to JSON |

## Datasets
| Dataset    | In Tech Spec | In code | Hint to Cover |
|------------|:-------:|:-----------:|--------------|
| TRANSACT   |   ⬜    |     ✅      | Add dataset usage to JSON |

## PF-Keys / Actions
| PF-Key / Action | In Tech Spec | In code | Hint to Cover |
|-----------------|:-------:|:-----------:|--------------|
| ENTER           |   ✅    |     ✅      | -            |
| PF3=Back        |   ⬜    |     ✅      | Add PF3 action to JSON |
| PF7=Backward    |   ⬜    |     ✅      | Add PF7 action to JSON |
| PF8=Forward     |   ⬜    |     ✅      | Add PF8 action to JSON |

## UI Elements (Fields)
| Field      | In Tech Spec | In code | Hint to Cover |
|------------|:-------:|:-----------:|--------------|
| TRNNAME    |   ✅    |     ✅      | -            |
| TITLE01    |   ✅    |     ✅      | -            |
| CURDATE    |   ✅    |     ✅      | -            |
| PGMNAME    |   ✅    |     ✅      | -            |
| TITLE02    |   ✅    |     ✅      | -            |
| CURTIME    |   ✅    |     ✅      | -            |
| PAGENUM    |   ✅    |     ✅      | -            |
| TRNIDIN    |   ✅    |     ✅      | -            |
| SEL0001-10 |   ✅    |     ✅      | -            |
| TRNID01-10 |   ✅    |     ✅      | -            |
| TDATE01-10 |   ✅    |     ✅      | -            |
| TDESC01-10 |   ✅    |     ✅      | -            |
| TAMT001-10 |   ✅    |     ✅      | -            |
| ERRMSG     |   ✅    |     ✅      | -            |
| Additional static labels/fields | ⬜ | ✅ | Add all visible BMS fields to JSON for completeness |

**Legend:**  
✅ = Documented  
⬜ = Missing

**Observations:**
- JSON covers most UI fields but omits transaction/program/mapset, dataset, and some PF-key actions.
- Markdown provides full coverage per user preference, including detailed BMS field extraction and datasets.
- To improve JSON coverage, add:
  - Transaction ID, program, mapset/map, dataset usage, and all PF-key actions.
  - All BMS map fields and static labels (as in Markdown).
- No extra analysis from source code was performed, per instructions.

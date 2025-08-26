# Accuracy Analysis: Admin Menu Screen

## Transaction, Program, Mapset
| Subject        | In Tech Spec | In code | Hint to Cover |
|----------------|:-------:|:-----------:|--------------|
| Transaction ID |   ⬜    |     ✅      | Add `CA00` to JSON description for full traceability |
| Program        |   ⬜    |     ✅      | Add `COADM01C` program name to JSON |
| Mapset/Map     |   ⬜    |     ✅      | Add `COADM01`/`COADM1A` to JSON |

## Datasets
| Dataset    | In Tech Spec | In code | Hint to Cover |
|------------|:-------:|:-----------:|--------------|
| (none)     |   ⬜    |     ⬜      | Not evidenced in either |

## PF-Keys / Actions
| PF-Key / Action | In Tech Spec | In code | Hint to Cover |
|-----------------|:-------:|:-----------:|--------------|
| ENTER           |   ✅    |     ✅      | -            |
| PF3=Exit        |   ⬜    |     ✅      | Add PF3 action to JSON |

## UI Elements (Fields)
| Field      | In Tech Spec | In code | Hint to Cover |
|------------|:-------:|:-----------:|--------------|
| TRNNAME    |   ✅    |     ✅      | -            |
| TITLE01    |   ✅    |     ✅      | -            |
| CURDATE    |   ✅    |     ✅      | -            |
| PGMNAME    |   ✅    |     ✅      | -            |
| TITLE02    |   ✅    |     ✅      | -            |
| CURTIME    |   ✅    |     ✅      | -            |
| OPTN001-12 |   ✅    |     ✅      | -            |
| OPTION     |   ✅    |     ✅      | -            |
| ERRMSG     |   ✅    |     ✅      | -            |
| (unnamed spacer) | ⬜ | ✅ | Add to JSON for completeness |

**Legend:**  
✅ = Documented  
⬜ = Missing

**Observations:**
- JSON covers most UI fields but omits transaction/program/mapset and PF3 action.
- Markdown covers all required subjects per user preference.
- To improve JSON coverage, add:
  - Transaction ID, program, mapset/map, and PF3 action.
  - Unnamed spacer field if relevant for completeness.
- No extra analysis from source code was performed, per instructions.

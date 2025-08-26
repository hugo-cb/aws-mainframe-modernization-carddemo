# Accuracy Analysis: Credit Card View Screen

## Transaction, Program, Mapset
| Subject        | In Tech Spec | In code | Hint to Cover |
|----------------|:-------:|:-----------:|--------------|
| Transaction ID |   ⬜    |     ✅      | Add `CCDL` to JSON description for full traceability |
| Program        |   ⬜    |     ✅      | Add `COCRDSLC` program name to JSON |
| Mapset/Map     |   ⬜    |     ✅      | Add `COCRDSL`/`CCRDSLA` to JSON |

## Datasets
| Dataset    | In Tech Spec | In code | Hint to Cover |
|------------|:-------:|:-----------:|--------------|
| CARDDAT    |   ⬜    |     ✅      | Add dataset usage to JSON |
| CARDAIX    |   ⬜    |     ✅      | Add to JSON |

## PF-Keys / Actions
| PF-Key / Action | In Tech Spec | In code | Hint to Cover |
|-----------------|:-------:|:-----------:|--------------|
| ENTER           |   ✅    |     ✅      | -            |
| PF3=Exit        |   ⬜    |     ✅      | Add PF3 action to JSON |

## UI Elements (Fields)
| Field      | In Tech Spec | In code | Hint to Cover |
|------------|:-------:|:-----------:|--------------|
| ACCTSID    |   ✅    |     ✅      | -            |
| CARDSID    |   ✅    |     ✅      | -            |
| ERRMSG     |   ✅    |     ✅      | -            |
| Additional fields (e.g., TITLE01, CURDATE, etc.) | ⬜ | ✅ | Add all visible BMS fields to JSON for completeness |

**Legend:**  
✅ = Documented  
⬜ = Missing

**Observations:**
- JSON covers only a subset of fields and actions, omitting transaction/program/mapset, datasets, and many UI/BMS fields.
- Markdown provides full coverage per user preference, including detailed BMS field extraction and datasets.
- To improve JSON coverage, add:
  - Transaction ID, program, mapset/map, dataset usage.
  - All BMS map fields and static labels (as in Markdown).
- No extra analysis from source code was performed, per instructions.

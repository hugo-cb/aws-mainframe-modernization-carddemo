# Accuracy Analysis: Transaction View Screen

## Transaction, Program, Mapset
| Subject        | In JSON | In Markdown | Hint to Cover |
|----------------|:-------:|:-----------:|--------------|
| Transaction ID |   ⬜    |     ✅      | Add `CT01` to JSON description for full traceability |
| Program        |   ⬜    |     ✅      | Add `COTRN01C` program name to JSON |
| Mapset/Map     |   ⬜    |     ✅      | Add `COTRN01`/`COTRN1A` to JSON |

## Datasets
| Dataset    | In JSON | In Markdown | Hint to Cover |
|------------|:-------:|:-----------:|--------------|
| TRANSACT   |   ⬜    |     ✅      | Add dataset usage to JSON |

## PF-Keys / Actions
| PF-Key / Action | In JSON | In Markdown | Hint to Cover |
|-----------------|:-------:|:-----------:|--------------|
| ENTER           |   ✅    |     ✅      | -            |
| PF3=Back        |   ⬜    |     ✅      | Add PF3 action to JSON |
| PF4=Clear       |   ⬜    |     ✅      | Add PF4 action to JSON |
| PF5=Browse Tran |   ⬜    |     ✅      | Add PF5 action to JSON |

## UI Elements (Fields)
| Field      | In JSON | In Markdown | Hint to Cover |
|------------|:-------:|:-----------:|--------------|
| TRNNAME    |   ✅    |     ✅      | -            |
| TITLE01    |   ✅    |     ✅      | -            |
| CURDATE    |   ✅    |     ✅      | -            |
| PGMNAME    |   ✅    |     ✅      | -            |
| TITLE02    |   ✅    |     ✅      | -            |
| CURTIME    |   ✅    |     ✅      | -            |
| TRNIDIN    |   ✅    |     ✅      | -            |
| TRNID      |   ✅    |     ✅      | -            |
| CARDNUM    |   ✅    |     ✅      | -            |
| TTYPCD     |   ✅    |     ✅      | -            |
| TCATCD     |   ✅    |     ✅      | -            |
| TRNSRC     |   ✅    |     ✅      | -            |
| TDESC      |   ✅    |     ✅      | -            |
| TRNAMT     |   ✅    |     ✅      | -            |
| TORIGDT    |   ✅    |     ✅      | -            |
| TPROCDT    |   ✅    |     ✅      | -            |
| MID        |   ✅    |     ✅      | -            |
| MNAME      |   ✅    |     ✅      | -            |
| MCITY      |   ✅    |     ✅      | -            |
| MZIP       |   ✅    |     ✅      | -            |
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

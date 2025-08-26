# Accuracy Analysis: Account Update Screen

## Transaction, Program, Mapset
| Subject        | In Tech Spec | In code | Hint to Cover |
|----------------|:-------:|:-----------:|--------------|
| Transaction ID |   ⬜    |     ✅      | Add `CAUP` to JSON description for full traceability |
| Program        |   ⬜    |     ✅      | Add `COACTUPC` program name to JSON |
| Mapset/Map     |   ⬜    |     ✅      | Add `COACTUP`/`CACTUPA` to JSON |

## Datasets
| Dataset    | In Tech Spec | In code | Hint to Cover |
|------------|:-------:|:-----------:|--------------|
| ACCTDAT    |   ⬜    |     ✅      | Add dataset usage to JSON (read/update/rewrite) |
| CUSTDAT    |   ⬜    |     ✅      | Same as above |
| CXACAIX    |   ⬜    |     ✅      | Same as above |

## PF-Keys / Actions
| PF-Key / Action | In Tech Spec | In code | Hint to Cover |
|-----------------|:-------:|:-----------:|--------------|
| ENTER=Process   |   ✅    |     ✅      | -            |
| F3=Exit         |   ✅    |     ✅      | -            |
| F5=Save         |   ✅    |     ✅      | -            |
| F12=Cancel      |   ✅    |     ✅      | -            |

## UI Elements (Fields)
| Field      | In Tech Spec | In code | Hint to Cover |
|------------|:-------:|:-----------:|--------------|
| ACCTSID    |   ✅    |     ✅      | -            |
| ACSTTUS    |   ✅    |     ✅      | -            |
| OPNYEAR    |   ✅    |     ✅      | -            |
| OPNMON     |   ✅    |     ✅      | -            |
| ERRMSG     |   ✅    |     ✅      | -            |
| OPNDAY     |   ⬜    |     ✅      | Add OPNDAY to JSON fields (present in BMS/Markdown) |
| Additional fields (UI labels, e.g., TITLE01, CURDATE, etc.) | ⬜ | ✅ | Add all visible BMS fields to JSON for completeness |

**Legend:**  
✅ = Documented  
⬜ = Missing

**Observations:**
- JSON covers core update fields and actions, but omits transaction/program/mapset, dataset usage, and some UI/BMS fields.
- Markdown covers all required subjects per user preference, including detailed BMS field extraction.
- To improve JSON coverage, add:
  - Transaction ID, program, mapset/map, and dataset usage.
  - All BMS map fields and static labels (as in Markdown).
- No extra analysis from source code was performed, per instructions.

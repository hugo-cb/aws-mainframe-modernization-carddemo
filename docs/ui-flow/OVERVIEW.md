# UI Navigation Overview — Unified Diagram

This document presents a unified, high-level navigation overview of the CARDDEMO application. It consolidates flows from the individual UI screen documents under `docs/ui-flow/`.

- Sources: `COSGN00.md`, `COMEN01.md`, `COADM01.md`, `COACTVW.md`, `COACTUP.md`, `COCRDLI.md`, `COCRDSL.md`, `COTRN00.md`, `COTRN01.md`, `COTRN02.md`, `CORPT00.md`, `COBIL00.md`.
- Conventions:
  - Dotted edges represent PF-key “back/exit” or return flows.
  - Edge labels reflect menu options or selection codes where applicable.

```mermaid
---
title: CARDDEMO - UI Navigation Overview
config:
  layout: elk
  theme: classic
---
flowchart TD
  %% ===== Core (Auth & Menus)
  subgraph Core [Authentication & Menus]
    direction TB
    COSGN["CC00 COSGN00C<br>Mapset COSGN00/COSGN0A<br>Signon"]
    COMEN["CM00 COMEN01C<br>Mapset COMEN01/COMEN1A<br>Main Menu"]
    COADM["CA00 COADM01C<br>Mapset COADM01/COADM1A<br>Admin Menu"]
  end

  COSGN -->|"ENTER: success (regular user)"| COMEN
  COSGN -->|"ENTER: success (admin)"| COADM
  COMEN -. |PF3 (Exit)| .-> COSGN
  COADM -. |PF3 (Exit)| .-> COSGN

  %% ===== Main Menu Options
  %% 1..10 from COMEN01.md copybook COMEN02Y
  COACTVW["CAVW COACTVWC<br>Mapset COACTVW/CACTVWA<br>Account View"]
  COACTUP["CAUP COACTUPC<br>Mapset COACTUP/CACTUPA<br>Account Update"]
  COCRDLI["CCLI COCRDLIC<br>Mapset COCRDLI/CCRDLIA<br>Credit Card List"]
  COCRDSL["CCDL COCRDSLC<br>Mapset COCRDSL/CCRDSLA<br>Credit Card View"]
  COCRDUP["CCUP COCRDUPC<br>Mapset COCRDUP/CCRDUPA<br>Credit Card Update"]
  COTRN00["CT00 COTRN00C<br>Mapset COTRN00/COTRN0A<br>Transaction List"]
  COTRN01["CT01 COTRN01C<br>Mapset COTRN01/COTRN1A<br>Transaction View"]
  COTRN02["CT02 COTRN02C<br>Mapset COTRN02/COTRN2A<br>Transaction Add"]
  CORPT00["CR00 CORPT00C<br>Mapset CORPT00/CORPT0A<br>Transaction Reports"]
  COBIL00["CB00 COBIL00C<br>Mapset COBIL00/COBIL0A<br>Bill Payment"]

  COMEN -->|"1: Account View"| COACTVW
  COMEN -->|"2: Account Update"| COACTUP
  COMEN -->|"3: Credit Card List"| COCRDLI
  COMEN -->|"4: Credit Card View"| COCRDSL
  COMEN -->|"5: Credit Card Update"| COCRDUP
  COMEN -->|"6: Transaction List"| COTRN00
  COMEN -->|"7: Transaction View"| COTRN01
  COMEN -->|"8: Transaction Add"| COTRN02
  COMEN -->|"9: Transaction Reports"| CORPT00
  COMEN -->|"10: Bill Payment"| COBIL00

  %% ===== Account Screens
  COACTVW -. |PF3 (Exit)| .-> COMEN
  COACTUP -. |PF3 (Exit)| .-> COMEN

  %% ===== Credit Card Screens
  subgraph Cards [Credit Cards]
    direction TB
    COCRDLI -->|"ENTER + 'S' (Select)"| COCRDSL
    COCRDLI -->|"ENTER + 'U' (Update)"| COCRDUP
  end
  COCRDLI -. |PF3 (Exit)| .-> COMEN
  COCRDSL -. |PF3 (Exit)| .-> COMEN
  COCRDUP -. |PF3 (Exit)| .-> COMEN

  %% ===== Transactions
  subgraph Trans [Transactions]
    direction TB
    COTRN00 -->|"ENTER + 'S' (Select)"| COTRN01
    COTRN01 -. |PF5 (Browse Tran)| .-> COTRN00
  end
  COTRN00 -. |PF3 (Exit)| .-> COMEN
  COTRN01 -. |PF3 (Exit)| .-> COMEN
  COTRN02 -. |PF3 (Exit)| .-> COMEN

  %% ===== Reports & Billing
  CORPT00 -. |PF3 (Back)| .-> COMEN
  COBIL00 -. |PF3 (Back)| .-> COMEN

  %% ===== Admin Area
  subgraph Admin [Admin — User Management]
    direction TB
    COUSR00["CU00 COUSR00C<br>Mapset COUSR00/COUSR0A<br>Users List"]
    COUSR01["CU01 COUSR01C<br>Mapset COUSR01/COUSR1A<br>User Add"]
    COUSR02["CU02 COUSR02C<br>Mapset COUSR02/COUSR2A<br>User Update"]
    COUSR03["CU03 COUSR03C<br>Mapset COUSR03/COUSR3A<br>User Delete"]

    COADM -->|"1: User List"| COUSR00
    COADM -->|"2: User Add"| COUSR01
    COADM -->|"3: User Update"| COUSR02
    COADM -->|"4: User Delete"| COUSR03

    COUSR00 -. |PF3 (Exit)| .-> COADM
    COUSR01 -. |PF3 (Exit)| .-> COADM
    COUSR02 -. |PF3 (Exit)| .-> COADM
    COUSR03 -. |PF3 (Exit)| .-> COADM
  end
```

## Notes
- PF-key behaviors (PF3/PF4/PF5) are summarized as back/clear/browse where applicable. See individual screens for field-level details and dataset operations.
- Some screens (e.g., Credit Card Update `COCRDUPC`, User Management programs `COUSR00C`..`COUSR03C`) are referenced by the menus and list flows even if a dedicated UI flow markdown is not present here.

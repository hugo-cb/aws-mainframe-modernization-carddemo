# UI Flow (from AI analysis)

## Diagram
```mermaid
graph TD
    A[Signon Screen] --> B[Main Menu]
    B --> C[Admin Menu Screen]
    B --> D[Account View]
    D --> E[Account Update Screen]
    B --> F[Card Listing Screen]
    F --> G[Credit Card Detail Screen]
    G --> H[Update Credit Card Details]
    B --> I[Transaction List]
    I --> J[Transaction View]
    I --> K[Transaction Add Screen]
    B --> L[Report Confirmation Screen]
    B --> M[Bill Payment Screen]
    C --> N[Users List Screen]
    N --> O[Add User Screen]
    N --> P[Update User Screen]
    N --> Q[Delete User Screen]
```

## Insights
- Main Menu is the central hub for navigation.
- Admin Menu links to user management screens.
- Transaction List allows navigation to view or add transactions.
- Bill Payment and Report Confirmation are accessible from Main Menu.

# Transaction Add Screen

## Description
This screen is used for adding a new transaction. It allows the user to input an account number and submit the transaction for processing.

## Fields
- ACTIDIN
  - Type: string
  - Required: true
  - Validation: Must be a valid account number format.
  - Description: Field to input the account number for the transaction.
- CURTIME
  - Type: string
  - Required: false
  - Validation: Read-only field.
  - Description: Displays the current time in the format 'hh:mm:ss'.
- TITLE02
  - Type: string
  - Required: false
  - Validation: Read-only field.
  - Description: Displays the title or header for the screen.

## Actions
- Add Transaction
  - Type: button
  - Outcome: The transaction is added to the system and the user is notified of the result.

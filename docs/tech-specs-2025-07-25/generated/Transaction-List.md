# Transaction List

## Description
This screen displays a list of transactions and allows the user to view details, navigate through the list, and handle errors. It is designed for managing and reviewing transaction data.

## Fields
- SEL0001
  - Type: string
  - Required: false
  - Validation: Must be a single character input.
  - Description: Selection field for choosing a transaction from the list.
- TRNID01
  - Type: string
  - Required: false
  - Validation: None
  - Description: Transaction ID field for displaying the unique identifier of a transaction.
- TDATE01
  - Type: date
  - Required: false
  - Validation: Must be a valid date format.
  - Description: Transaction date field for displaying the date of the transaction.
- TDESC01
  - Type: string
  - Required: false
  - Validation: None
  - Description: Transaction description field for displaying details about the transaction.
- TAMT001
  - Type: number
  - Required: false
  - Validation: Must be a valid numeric value.
  - Description: Transaction amount field for displaying the monetary value of the transaction.
- ERRMSG
  - Type: string
  - Required: false
  - Validation: None
  - Description: Error message field for displaying validation or system errors.

## Actions
- Type 'S' to View Transaction Details
  - Type: input
  - Outcome: Displays the details of the selected transaction.
- Press ENTER to Continue
  - Type: keyboard
  - Outcome: Advances to the next screen or process.
- Press F3 to Go Back
  - Type: keyboard
  - Outcome: Returns to the previous screen.
- Press F7 to Move Backward
  - Type: keyboard
  - Outcome: Displays the previous set of transactions in the list.
- Press F8 to Move Forward
  - Type: keyboard
  - Outcome: Displays the next set of transactions in the list.

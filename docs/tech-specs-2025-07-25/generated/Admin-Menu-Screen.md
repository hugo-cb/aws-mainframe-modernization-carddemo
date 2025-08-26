# Admin Menu Screen

## Description
This screen serves as the main administrative menu for the application, allowing users to enter transaction names and select options for further actions.

## Fields
- Tran
  - Type: string
  - Required: false
  - Validation: None
  - Description: Label indicating the transaction name field.
- TRNNAME
  - Type: string
  - Required: true
  - Validation: Must be a valid transaction name.
  - Description: Field for entering the transaction name.
- OPTN012
  - Type: string
  - Required: false
  - Validation: None
  - Description: Field for displaying or entering additional options.
- OPTION
  - Type: number
  - Required: true
  - Validation: Must be a numeric value, right-justified, and zero-padded.
  - Description: Field for entering a numeric option to select an action.
- ERRMSG
  - Type: string
  - Required: false
  - Validation: None
  - Description: Field for displaying error messages.

## Actions
- Enter option in OPTION field
  - Type: form submit
  - Outcome: The selected option is processed, and the corresponding action is executed.

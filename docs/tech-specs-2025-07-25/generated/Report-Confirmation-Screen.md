# Report Confirmation Screen

## Description
This screen is used to confirm the submission of a report for printing. It allows the user to provide confirmation and displays error messages if necessary.

## Fields
- CONFIRM
  - Type: string
  - Required: true
  - Validation: Must be 'Y' or 'N'.
  - Description: Field for user to confirm the action by entering 'Y' for Yes or 'N' for No.
- ERRMSG
  - Type: string
  - Required: false
  - Validation: None
  - Description: Field to display error messages if the user input is invalid or if there is an issue with the submission.

## Actions
- ENTER
  - Type: form submit
  - Outcome: Proceeds with the report submission if the input is valid.
- F3
  - Type: navigation
  - Outcome: Returns the user to the previous screen without submitting the report.

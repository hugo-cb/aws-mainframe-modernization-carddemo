# Signon Screen

## Description
This screen is used for signing on to the system. It allows the user to enter a transaction name and provides options to proceed with the sign-on or exit the application.

## Fields
- TRNNAME
  - Type: string
  - Required: true
  - Validation: Must be a valid transaction name; no specific format constraints provided.
  - Description: Field to input the transaction name for the sign-on process.

## Actions
- Sign-on
  - Type: form submit
  - Outcome: The user is signed on to the system and navigated to the next screen or process.
- Exit
  - Type: button
  - Outcome: The user is returned to the previous screen or exits the application.

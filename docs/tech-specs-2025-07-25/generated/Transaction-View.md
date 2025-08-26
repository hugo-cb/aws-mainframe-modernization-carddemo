# Transaction View

## Description
This screen is used to input and display transaction details, including the transaction name, date, program name, and titles. It serves as a user interface for managing and viewing transaction-related information.

## Fields
- TRNNAME
  - Type: string
  - Required: true
  - Validation: Maximum length of 4 characters.
  - Description: Field for entering the transaction name.
- TITLE01
  - Type: string
  - Required: false
  - Validation: Maximum length of 40 characters.
  - Description: Display field for the first title.
- CURDATE
  - Type: date
  - Required: false
  - Validation: Format must be 'mm/dd/yy'.
  - Description: Field for displaying the current date.
- PGMNAME
  - Type: string
  - Required: false
  - Validation: Maximum length of 8 characters.
  - Description: Field for displaying the program name.
- TITLE02
  - Type: string
  - Required: false
  - Validation: Maximum length of 40 characters.
  - Description: Display field for the second title.

## Actions
- None specified

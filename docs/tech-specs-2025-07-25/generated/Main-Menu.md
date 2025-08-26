# Main Menu

## Description
This screen serves as the main menu for the application, allowing users to navigate to different functionalities by entering a transaction name.

## Fields
- TRNNAME
  - Type: string
  - Required: true
  - Validation: Must be a valid transaction name; maximum length of 4 characters.
  - Description: Field to input the transaction name for navigation.

## Actions
- Enter transaction name
  - Type: form submit
  - Outcome: The application processes the input and navigates to the corresponding transaction screen.

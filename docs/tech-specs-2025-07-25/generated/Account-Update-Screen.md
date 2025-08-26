# Account Update Screen

## Description
This screen is used to update account details, including account number, active status, and opening date. It provides fields for data entry and options for processing or exiting the screen.

## Fields
- ACCTSID
  - Type: string
  - Required: true
  - Validation: Must be 11 characters long and filled
  - Description: Account Number
- ACSTTUS
  - Type: string
  - Required: true
  - Validation: Must be a single character (Y or N)
  - Description: Active Status (Y/N)
- OPNYEAR
  - Type: number
  - Required: true
  - Validation: Must be a 4-digit year
  - Description: Opening Year
- OPNMON
  - Type: number
  - Required: true
  - Validation: Must be a valid month (1-12)
  - Description: Opening Month
- ERRMSG
  - Type: string
  - Required: false
  - Validation: Displays error messages if any issues occur
  - Description: Error Message Display

## Actions
- ENTER=Process
  - Type: form submit
  - Outcome: Validates and saves the data; displays success or error messages
- F3=Exit
  - Type: button
  - Outcome: Returns to the previous screen or menu
- F5=Save
  - Type: button
  - Outcome: Saves the data and remains on the screen
- F12=Cancel
  - Type: button
  - Outcome: Clears the entered data and resets the screen

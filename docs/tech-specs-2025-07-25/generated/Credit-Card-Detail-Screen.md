# Credit Card Detail Screen

## Description
This screen is designed to capture and display details related to a credit card, including its activation status and expiry date. It allows users to input and view relevant information for managing credit card details.

## Fields
- CRDSTCD
  - Type: string
  - Required: true
  - Validation: Must be a single character: 'Y' for Yes or 'N' for No.
  - Description: Indicates whether the card is active (Y/N).
- EXPMON
  - Type: number
  - Required: true
  - Validation: Must be a two-digit number between 01 and 12.
  - Description: The expiry month of the credit card.
- EXPYEAR
  - Type: number
  - Required: true
  - Validation: Must be a four-digit number representing the year.
  - Description: The expiry year of the credit card.

## Actions
- Input Data
  - Type: form input
  - Outcome: The entered data is captured and stored for further processing.

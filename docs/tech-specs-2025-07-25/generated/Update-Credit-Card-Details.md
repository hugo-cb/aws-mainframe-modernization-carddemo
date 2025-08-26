# Update Credit Card Details

## Description
This screen is used to update credit card details, including account number, card number, and name on the card. It provides a user interface for entering and validating these details.

## Fields
- Account Number
  - Type: string
  - Required: true
  - Validation: Must be 11 characters long and numeric.
  - Description: The account number associated with the credit card.
- Card Number
  - Type: string
  - Required: true
  - Validation: Must be 16 characters long and numeric.
  - Description: The credit card number to be updated.
- Name on Card
  - Type: string
  - Required: true
  - Validation: Must not exceed 50 characters.
  - Description: The name printed on the credit card.

## Actions
- Enter Account Number
  - Type: form input
  - Outcome: The account number is captured for further processing.
- Enter Card Number
  - Type: form input
  - Outcome: The card number is captured for further processing.
- Enter Name on Card
  - Type: form input
  - Outcome: The name on the card is captured for further processing.

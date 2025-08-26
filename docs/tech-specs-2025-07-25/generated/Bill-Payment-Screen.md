# Bill Payment Screen

## Description
This screen is used for processing bill payments, allowing users to input payment details and confirm transactions.

## Fields
- Account Number
  - Type: string
  - Required: true
  - Validation: Must be a valid account number format.
  - Description: The account number to which the bill payment is being made.
- Payment Amount
  - Type: number
  - Required: true
  - Validation: Must be a positive number with up to two decimal places.
  - Description: The amount to be paid for the bill.
- Payment Date
  - Type: date
  - Required: true
  - Validation: Must be a valid date in the format MM/DD/YYYY.
  - Description: The date on which the payment is to be processed.
- Payment Method
  - Type: string
  - Required: true
  - Validation: Must be one of the predefined payment methods.
  - Description: The method of payment (e.g., credit card, bank transfer).
- Confirmation Message
  - Type: string
  - Required: false
  - Validation: None
  - Description: A message displayed to confirm the payment details before submission.

## Actions
- Submit Payment
  - Type: button
  - Outcome: The payment is processed, and a confirmation message is displayed.
- Cancel
  - Type: button
  - Outcome: The user is returned to the previous screen without saving any changes.
- Clear Fields
  - Type: button
  - Outcome: All input fields are reset to their default state.

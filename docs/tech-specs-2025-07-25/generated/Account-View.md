# Account View

## Description
This screen is used to display and input account-related information, including account number, status, and opening date. It allows users to view and update specific account details.

## Fields
- ACCTSID
  - Type: string
  - Required: true
  - Validation: MUSTFILL
  - Description: Account Number field for entering the account identifier.
- ACSTTUS
  - Type: string
  - Required: false
  - Validation: —
  - Description: Active Status field for selecting whether the account is active (Y/N).
- ADTOPEN
  - Type: date
  - Required: false
  - Validation: —
  - Description: Opened Date field for displaying the account's opening date.

## Actions
- Enter account number
  - Type: form input
  - Outcome: The account number is captured for further processing.
- Select active status
  - Type: form input
  - Outcome: The active status is recorded for the account.
- View opened date
  - Type: display
  - Outcome: The user can view the account's opening date.

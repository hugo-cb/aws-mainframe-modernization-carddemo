# Update User Screen

## Description
This screen is used to update user information in the system. It allows the user to input and modify details such as User ID and First Name, and provides feedback on the success or failure of the operation.

## Fields
- USRIDIN
  - Type: string
  - Required: true
  - Validation: Must not be empty. Length: 8 characters.
  - Description: Field for entering the User ID to be updated.
- FNAME
  - Type: string
  - Required: true
  - Validation: Must not be empty. Length: 20 characters.
  - Description: Field for entering the First Name of the user.

## Actions
- Enter User ID
  - Type: input
  - Outcome: The User ID is captured for processing.
- Enter First Name
  - Type: input
  - Outcome: The First Name is captured for processing.
- SEND-USRUPD-SCREEN
  - Type: form submit
  - Outcome: The system processes the update and displays a success or error message.

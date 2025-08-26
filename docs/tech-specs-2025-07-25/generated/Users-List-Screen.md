# Users List Screen

## Description
This screen is used to display a list of users and allows the user to perform actions such as updating or deleting a user, navigating through the list, and continuing to the next step.

## Fields
- PAGENUM
  - Type: string
  - Required: false
  - Validation: None
  - Description: Displays the current page number of the user list.
- USRIDIN
  - Type: string
  - Required: true
  - Validation: Must be 8 characters long
  - Description: Field for entering the User ID to search for a specific user.
- SEL
  - Type: string
  - Required: false
  - Validation: None
  - Description: Selection field for choosing a user from the list.
- ERRMSG
  - Type: string
  - Required: false
  - Validation: None
  - Description: Displays error messages or instructions for the user.

## Actions
- ENTER
  - Type: form submit
  - Outcome: Proceeds to the next step or performs the selected action.
- F3
  - Type: button
  - Outcome: Returns to the previous screen without saving changes.
- F7
  - Type: button
  - Outcome: Displays the previous set of users in the list.
- F8
  - Type: button
  - Outcome: Displays the next set of users in the list.
- Update User ("U" in SEL field)
  - Type: input
  - Outcome: Opens a form to update the selected user's details.
- Delete User ("D" in SEL field)
  - Type: input
  - Outcome: Removes the selected user from the list after confirmation.

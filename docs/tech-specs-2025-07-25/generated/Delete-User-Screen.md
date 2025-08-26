# Delete User Screen

## Description
This screen is used to delete a user by capturing user details such as User ID, First Name, Last Name, and User Type. It also displays relevant prompts and messages and allows navigation and actions related to user deletion.

## Fields
- TRNNAME
  - Type: string
  - Required: false
  - Validation: None
  - Description: Transaction name displayed on the screen
- CURDATE
  - Type: date
  - Required: false
  - Validation: Must be a valid date
  - Description: Current date displayed in mm/dd/yy format
- PGMNAME
  - Type: string
  - Required: false
  - Validation: None
  - Description: Program name displayed on the screen
- CURTIME
  - Type: time
  - Required: false
  - Validation: Must be a valid time
  - Description: Current time displayed in hh:mm:ss format
- USRIDIN
  - Type: string
  - Required: true
  - Validation: Must not be empty; maximum length of 8 characters
  - Description: User ID input field for identifying the user to be deleted
- FNAME
  - Type: string
  - Required: false
  - Validation: None
  - Description: First Name of the user to be deleted
- LNAME
  - Type: string
  - Required: false
  - Validation: None
  - Description: Last Name of the user to be deleted
- USRTYPE
  - Type: string
  - Required: false
  - Validation: Must be one of the predefined values (A=Admin, U=User)
  - Description: User Type of the user to be deleted (e.g., Admin or User)
- ERRMSG
  - Type: string
  - Required: false
  - Validation: None
  - Description: Error message displayed in case of validation or operation failure

## Actions
- ENTER=Fetch
  - Type: button
  - Outcome: Displays the user details if the User ID is valid; otherwise, shows an error message
- F3=Back
  - Type: button
  - Outcome: Returns to the previous screen without making any changes
- F4=Clear
  - Type: button
  - Outcome: Resets all fields to their initial state
- F5=Delete
  - Type: button
  - Outcome: Deletes the user if the details are valid; otherwise, displays an error message

/**
 * ABAP Code Base Generator
 * Generates basic ABAP code templates
 */

function generateAbapCodeBase() {
    return `\n* Basic ABAP program\n\nREPORT z_tsi_demo.\n\n* Main program logic\nSTART-OF-SELECTION.\n  WRITE: 'Hello, World! This is a basic ABAP program.'.\n\n* Alternative simple approach\n* WRITE: 'Hello, World! This is a basic ABAP program.'.\n\n* Example with variables and types\n* DATA: lv_name    TYPE string VALUE 'TSI Student',\n*       lv_age     TYPE i VALUE 20,\n*       lv_message TYPE string.\n*\n* lv_message = |Hello, { lv_name }! You are { lv_age } years old.|.\n* WRITE: lv_message.\n\n* Example function definition (using FORM)\n* FORM greet USING p_name TYPE string\n*              CHANGING p_greeting TYPE string.\n*   p_greeting = |Hello, { p_name }!|.\n* ENDFORM.\n\n* Example usage\n* DATA: lv_greeting TYPE string.\n* PERFORM greet USING 'TSI Student' CHANGING lv_greeting.\n* WRITE: lv_greeting.\n\n* Example with internal tables and loops\n* DATA: lt_numbers TYPE TABLE OF i,\n*       lv_number  TYPE i.\n*\n* APPEND 1 TO lt_numbers.\n* APPEND 2 TO lt_numbers.\n* APPEND 3 TO lt_numbers.\n* APPEND 4 TO lt_numbers.\n* APPEND 5 TO lt_numbers.\n*\n* LOOP AT lt_numbers INTO lv_number.\n*   WRITE: |Number: { lv_number }|.\n* ENDLOOP.\n\n* Example with structures\n* TYPES: BEGIN OF ty_person,\n*          name TYPE string,\n*          age  TYPE i,\n*        END OF ty_person.\n*\n* DATA: ls_person TYPE ty_person,\n*       lt_persons TYPE TABLE OF ty_person.\n*\n* ls_person-name = 'TSI Student'.\n* ls_person-age = 20.\n* APPEND ls_person TO lt_persons.\n*\n* LOOP AT lt_persons INTO ls_person.\n*   WRITE: |Hello, I'm { ls_person-name } and I'm { ls_person-age } years old!|.\n* ENDLOOP.\n`;
}

module.exports = {
    generateAbapCodeBase
};
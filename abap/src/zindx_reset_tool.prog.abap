*&---------------------------------------------------------------------*
*& Report ZINDX_RESET_TOOL
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zindx_reset_tool.

PARAMETERS: p_name TYPE sy-uname.
PARAMETERS: p_del AS CHECKBOX.

*DATA lt_expenses        TYPE zcl_aws_textract_helper=>tt_expenses.
*DATA lt_expense_audit_history TYPE zcl_aws_textract_helper=>tt_expenses.
*
*DATA lv_index_id        TYPE char22.
*
*lv_index_id        = |AP-{ p_name }|.

*DATA(ls_indx) = VALUE indx( aedat = sy-datum
*                            usera = sy-uname ).
*
**import invoices for this user before exporting
*IMPORT expenses = lt_expenses FROM DATABASE indx(zp) ID lv_index_id
*      ACCEPTING PADDING.
*
*IMPORT expenses = lt_expense_audit_history FROM DATABASE indx(zp) ID 'AUDIT'
*      ACCEPTING PADDING.
*
*APPEND LINES OF lt_expenses TO lt_expense_audit_history.
** Export to Audit
*EXPORT expenses = lt_expense_audit_history TO DATABASE indx(zp) ID 'AUDIT'
*      FROM ls_indx COMPRESSION ON.


IF p_del = 'X'.
  DELETE FROM indx WHERE relid = 'ZP' and usera = p_name.
ENDIF.

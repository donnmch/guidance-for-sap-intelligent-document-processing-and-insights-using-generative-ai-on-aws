*&---------------------------------------------------------------------*
*& Report ZFB03_DISPLAY
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfb03_display.

PARAMETERS: p_belnr TYPE belnr_d,
            p_bukrs TYPE bukrs default '1710',
            p_gjahr TYPE gjahr default '2024'.

set parameter id 'BLN' field p_belnr.
set parameter id 'BUK' field p_bukrs.
set parameter id 'GJR' field p_gjahr.

CALL TRANSACTION 'FB03' and skip first screen.

CLASS zcl_zinvoice_app_mpc_ext DEFINITION
  PUBLIC
  INHERITING FROM zcl_zinvoice_app_mpc
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: BEGIN OF ty_deep_invoice.
             INCLUDE TYPE ts_invoice.
    TYPES:     toitems TYPE STANDARD TABLE OF ts_item WITH DEFAULT KEY,
           END OF ty_deep_invoice.

    METHODS define
        REDEFINITION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ZINVOICE_APP_MPC_EXT IMPLEMENTATION.


  METHOD define.

    super->define( ).

    model->get_entity_type('InvoiceDocument')->get_property('FileName')->set_as_content_source( ).
    model->get_entity_type('InvoiceDocument')->get_property('FileType')->set_as_content_type( ).


    model->get_entity_type( iv_entity_name = 'Invoice' )->bind_structure(
      EXPORTING
        iv_structure_name   = 'ZCL_ZINVOICE_APP_MPC_EXT=>TY_DEEP_INVOICE'

    ).

  ENDMETHOD.
ENDCLASS.

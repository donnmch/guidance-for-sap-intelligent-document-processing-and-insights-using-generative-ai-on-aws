class ZCL_AWS_S3_HELPER definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF ty_bucketname,
           bucket TYPE  /aws1/s3_bucketname,
         END OF ty_bucketname .
  types:
    tt_bucketnames TYPE STANDARD TABLE OF ty_bucketname
                         WITH NON-UNIQUE KEY bucket .
  types:
    BEGIN OF ty_content,
             key          TYPE /aws1/s3_objectkey,
             lastmodified TYPE /aws1/s3_lastmodified,
             size         TYPE /aws1/s3_size,
           END OF ty_content .
  types:
    tt_content TYPE STANDARD TABLE OF ty_content
                      WITH NON-UNIQUE KEY key .

  methods CONSTRUCTOR
    importing
      !IM_PROFILE type /AWS1/RT_PROFILE_ID
    raising
      /AWS1/CX_RT_TECHNICAL_GENERIC
      /AWS1/CX_RT_NO_AUTH_GENERIC
      /AWS1/CX_RT_SERVICE_GENERIC .
  methods LIST_BUCKET_OBJECTS
    importing
      !IM_BUCKET type STRING
    returning
      value(RE_CONTENTS) type TT_CONTENT
    raising
      /AWS1/CX_S3_NOSUCHUPLOAD
      /AWS1/CX_S3_CLIENTEXC
      /AWS1/CX_S3_SERVEREXC
      /AWS1/CX_RT_TECHNICAL_GENERIC
      /AWS1/CX_RT_SERVICE_GENERIC .
  methods LIST_BUCKETS
    returning
      value(RT_BUCKETS) type TT_BUCKETNAMES
    raising
      /AWS1/CX_S3_CLIENTEXC
      /AWS1/CX_S3_SERVEREXC
      /AWS1/CX_RT_TECHNICAL_GENERIC
      /AWS1/CX_RT_SERVICE_GENERIC .
  methods CREATE_BUCKET
    importing
      !IM_BUCKET type STRING
    raising
      /AWS1/CX_S3_BUCKETALRDYEXISTS
      /AWS1/CX_S3_CLIENTEXC
      /AWS1/CX_RT_TECHNICAL_GENERIC
      /AWS1/CX_S3_BKTALRDYOWNEDBYYOU
      /AWS1/CX_S3_SERVEREXC
      /AWS1/CX_RT_SERVICE_GENERIC .
  methods PUT_OBJECT
    importing
      !IM_BUCKET type STRING
      !IM_KEY type STRING
      !IM_DATA type XSTRING
    raising
      /AWS1/CX_S3_CLIENTEXC
      /AWS1/CX_S3_SERVEREXC
      /AWS1/CX_RT_TECHNICAL_GENERIC
      /AWS1/CX_RT_SERVICE_GENERIC .
  methods GET_BUCKET .
  methods GET_OBJECT
    importing
      !IM_BUCKET type STRING
      !IM_KEY type STRING
    exporting
      !RE_DATA type XSTRING
      !RE_MIME type STRING
    raising
      /AWS1/CX_S3_CLIENTEXC
      /AWS1/CX_S3_SERVEREXC
      /AWS1/CX_RT_TECHNICAL_GENERIC
      /AWS1/CX_RT_SERVICE_GENERIC .
  methods DELETE_OBJECT
    importing
      !IM_BUCKET type STRING
      !IM_KEY type STRING
    raising
      /AWS1/CX_S3_CLIENTEXC
      /AWS1/CX_S3_SERVEREXC
      /AWS1/CX_RT_TECHNICAL_GENERIC
      /AWS1/CX_RT_SERVICE_GENERIC .
protected section.
private section.

  data GV_REGION type /AWS1/RT_REGION_ID .
  data GO_SESSION type ref to /AWS1/CL_RT_SESSION_BASE .
  data GO_S3_CLIENT type ref to /AWS1/IF_S3 .
ENDCLASS.



CLASS ZCL_AWS_S3_HELPER IMPLEMENTATION.


  METHOD constructor.

    go_session  =  /aws1/cl_rt_session_aws=>create( iv_profile_id =  im_profile ).
    go_s3_client       =  /aws1/cl_s3_factory=>create( go_session ).



  ENDMETHOD.


  method LIST_BUCKETS.

* List buckets
  CALL METHOD go_s3_client->listbuckets
    RECEIVING
      oo_output = DATA(lo_listbucketoutput).

* Get buckets from list buckets output
  CALL METHOD lo_listbucketoutput->get_buckets
    RECEIVING
      ot_buckets = DATA(lt_buckets).


DATA(config_region) =  go_session->get_configuration( )->get_region( ).

* Loop through bucket objects to get bucket names
  LOOP AT lt_buckets INTO DATA(lo_bucket).

    CALL METHOD lo_bucket->get_name
      RECEIVING
        ov_name = DATA(lv_bucket).

    TRY.
        DATA(location) =  go_s3_client->getbucketlocation(
               EXPORTING
                 iv_bucket = lv_bucket )->get_locationconstraint( ).

* for us-east-1 location will be null
        IF location = config_region OR location = ''.
          APPEND INITIAL LINE TO  rt_buckets ASSIGNING
          FIELD-SYMBOL(<fs_name>).
          <fs_name>-bucket = lv_bucket.
        ENDIF.
* performance exponential backoff and retry
      CATCH /aws1/cx_s3_clientexc INTO DATA(lo_client_exception).

    ENDTRY.
  ENDLOOP.


  endmethod.


  METHOD list_bucket_objects.


    DATA lt_objects TYPE tt_content.

    CALL METHOD go_s3_client->listobjects
      EXPORTING
        iv_bucket = im_bucket
      RECEIVING
        oo_output = DATA(lo_listobjectsoutput).


    IF lo_listobjectsoutput IS BOUND.

      CALL METHOD lo_listobjectsoutput->get_contents
        RECEIVING
          ot_contents = DATA(lt_contents).

      LOOP AT lt_contents INTO DATA(lo_content).

        APPEND INITIAL LINE TO lt_objects ASSIGNING
        FIELD-SYMBOL(<fs_object>).

        CALL METHOD lo_content->get_key
          RECEIVING
            ov_key = <fs_object>-key.

        CALL METHOD lo_content->get_size
          RECEIVING
            ov_size = <fs_object>-size.

        CALL METHOD lo_content->get_lastmodified
          RECEIVING
            ov_lastmodified = <fs_object>-lastmodified.

      ENDLOOP.

    ENDIF.

    re_contents = lt_objects.

  ENDMETHOD.


  METHOD create_bucket.



    DATA(lo_location_configuration) =
         NEW /aws1/cl_s3_createbucketconf( iv_locationconstraint = go_session->get_configuration( )->get_region( ) ).


    IF lo_location_configuration IS BOUND.
      IF  go_session->get_configuration( )->get_region( ) EQ 'us-east-1'.
        go_s3_client->createbucket(
        EXPORTING
           iv_bucket                    = im_bucket
         RECEIVING
           oo_output                    = DATA(lo_output)
      ).

      ELSE.
        go_s3_client->createbucket(
          EXPORTING
             iv_bucket                    = im_bucket
             io_createbucketconfiguration = lo_location_configuration
           RECEIVING
             oo_output                    = lo_output
        ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  method GET_BUCKET.
  endmethod.


  METHOD get_object.

    DATA: lv_mime_type TYPE skwf_mime.
    data: lv_file_name type SKWF_FILNM.

    lv_file_name = im_key.

    CALL FUNCTION 'SKWF_MIMETYPE_OF_FILE_GET'
      EXPORTING
        filename = lv_file_name
      IMPORTING
        mimetype = lv_mime_type.
    .

    re_mime = lv_mime_type.


    DATA(lo_result) = go_s3_client->getobject(
              iv_bucket = im_bucket
              iv_key =    im_key
           ).

    re_data = lo_result->get_body( ).


  ENDMETHOD.


  METHOD put_object.

    go_s3_client->putobject(
       EXPORTING
        iv_bucket                    = im_bucket
         iv_key                       = im_key
         iv_body                      = im_data
      RECEIVING
         oo_output                    = DATA(lo_output)
    ).

  ENDMETHOD.


  METHOD DELETE_OBJECT.

    go_s3_client->deleteobject(
       EXPORTING
        iv_bucket                    = im_bucket
         iv_key                       = im_key
      RECEIVING
         oo_output                    = DATA(lo_output)
    ).

  ENDMETHOD.
ENDCLASS.

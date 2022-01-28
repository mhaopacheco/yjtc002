*&---------------------------------------------------------------------*
*& Include          ZWF_MASS_EVENTS_TOP
*&---------------------------------------------------------------------*
REPORT zwf_mass_events.

SELECTION-SCREEN BEGIN OF BLOCK bq001 WITH FRAME TITLE TEXT-001. .
PARAMETERS: p_type   TYPE swfeclstyp OBLIGATORY,
            p_object TYPE seoclsname OBLIGATORY,
            p_event  TYPE swu_visevt OBLIGATORY.
SELECTION-SCREEN END OF BLOCK bq001.

SELECTION-SCREEN BEGIN OF BLOCK bq002 WITH FRAME TITLE TEXT-002 .
PARAMETERS: p_file LIKE rlgrap-filename  OBLIGATORY  DEFAULT 'C:\data.txt'.
SELECTION-SCREEN END OF BLOCK bq002.

**********************************************************************
" Type's ***********************************************************
TYPES : BEGIN OF ty_file,
          objid TYPE swo_objid,
        END OF ty_file.

" Data's ************************************************************
DATA: gt_data TYPE TABLE OF ty_file,
      gs_data LIKE LINE OF gt_data[],
      gv_cont    TYPE i..

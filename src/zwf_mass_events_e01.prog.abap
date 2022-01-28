*&---------------------------------------------------------------------*
*& Include          ZWF_MASS_EVENTS_E01
*&---------------------------------------------------------------------*

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM get_filename  USING p_file.

START-OF-SELECTION .
  PERFORM load_file.
  PERFORM execute_load.

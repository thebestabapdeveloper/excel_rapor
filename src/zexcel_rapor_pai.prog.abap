*&---------------------------------------------------------------------*
*&  Include           ZHVL_FI_BY_ODN_TLB_PAI
*&---------------------------------------------------------------------*

MODULE user_command_0100 INPUT.

  CASE sy-ucomm.
    WHEN OTHERS.
      " this case intentionaly left blank
  ENDCASE.

ENDMODULE.

MODULE exit INPUT.
  LEAVE TO SCREEN 0.
  LEAVE LIST-PROCESSING.
ENDMODULE.

*&---------------------------------------------------------------------*

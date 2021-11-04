;A macro file for structured programming in ASM
;use and redistribution are permitted without any limitations.
;(C) Sergey Belyakov, 1998

?iflevel  = 0
?dolevel  = 0
?LabelNum = 0

$__SetLabel MACRO LabelNum
?Label&LabelNum:
            ENDM

$_SetLabel MACRO LabelClass, levelnum
           $__SetLabel %(?&LabelClass&levelnum)
           ENDM

$_AssignLabelNum MACRO LabelClass, LevelNum
                 ?&LabelClass&LevelNum = ?LabelNum
                 ?LabelNum = ?LabelNum + 1
                 ENDM

$ifnot  MACRO C
        $_inclevel if
        $_AssignLabelNum endif, %?iflevel
        $_AssignLabelNum else, %?iflevel
        $_AssignLabelNum beginif, %?iflevel
        $_GenNBCode C, else, %?iflevel
        ENDM
$beginif MACRO
         $SetLabel beginif, %?iflevel
         ENDM
$tobeginif MACRO C
           $_GenCode Code, tobeginif, %?iflevel
           ENDM
$else   MACRO C
        $_GenNBCode C, endif, %?iflevel
        $_SetLabel else, %?iflevel
        $_AssignLabelNum else, %?iflevel
        $_AssignLabelNum beginif, %?iflevel
        ENDM
$endif  MACRO
        $_SetLabel else,  %?iflevel
        $_SetLabel endif, %?iflevel
        $_declevel if
        ENDM
$__GenCode MACRO Code, LabelNum
           ifnb <Code>
             Code ?Label&LabelNum
           else
             jmp ?Label&LabelNum
           endif
           ENDM
$_GenCode MACRO Code, LabelClass, Level
          $__GenCode Code, %(?&LabelClass&Level)
          ENDM
$_GenNBCode MACRO Code, LabelClass, Level
          IFNB <Code>
            $__GenCode Code, %(?&LabelClass&Level)
          ENDIF
          ENDM
$toelse MACRO Code
        $_GenCode Code, else, %?iflevel
        ENDM
$toendif MACRO Code
        $_GenCode Code, endif, %?iflevel
        ENDM

$do      MACRO C
         $_IncLevel do
         $_AssignLabelNum do, %?dolevel
         $_AssignLabelNum while %?dolevel
         $_AssignLabelNum enddo %?dolevel
         $_GenNBCode C, while, %?dolevel
         $_SetLabel do, %?dolevel
         ENDM
$towhile MACRO C
         $_GenCode C, while, %?dolevel
         ENDM
$while   MACRO
         $_SetLabel while, %?dolevel
         ENDM
$loop     MACRO C
          $_GenCode C, do, %?dolevel
          ENDM
$break    MACRO C
          $_GenCode C, enddo, %?dolevel
          ENDM
$enddo    MACRO C
          $_GenNBCode C, do, %?dolevel
          $_SetLabel enddo, %?dolevel
          $_DecLevel do
          ENDM

$_IncLevel MACRO L
           ?&L&level = ?&L&level + 1
           ENDM
$_DecLevel MACRO L
           ?&L&level = ?&L&level - 1
           ENDM

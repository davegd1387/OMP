//UNEMPLOY JOB 1,NOTIFY=&SYSUID
//******************************HACK-IN-CREATE-DATE*********************/
//COBRUN  EXEC IGYWCL
//COBOL.SYSIN  DD DSN=&SYSUID..CBL(UNEMPLOY),DISP=SHR
//LKED.SYSLMOD DD DSN=&SYSUID..LOAD(UNEMPLOY),DISP=SHR
//***************************************************/
// IF RC  = 5 THEN
//***************************************************/
//RUN     EXEC PGM=UNEMPLOY
//STEPLIB   DD DSN=&SYSUID..LOAD,DISP=SHR
//UNMPRACE  DD DSN=&SYSUID..MMUCBRCE,DISP=SHR
//UNMPSEX   DD DSN=&SYSUID..MMUCBSEX,DISP=SHR
//UNMPETHN  DD DSN=&SYSUID..MMUCBETH,DISP=SHR
//UNMPAGE   DD DSN=&SYSUID..MMUCBAGE,DISP=SHR
//UNMPIND   DD DSN=&SYSUID..MMUCBIND,DISP=SHR
//UNMPVSAM  DD DSN=&SYSUID..MMUCB.VSAM,DISP=SHR
//SYSOUT    DD SYSOUT=*,OUTLIM=15000
//CEEDUMP   DD DUMMY
//SYSUDUMP  DD DUMMY
//***************************************************/
// ELSE
// ENDIF

        IDENTIFICATION DIVISION.
        PROGRAM-ID.    EM01EX03.
        AUTHOR.        VINICIUS ALVES.
        INSTALLATION.  FATEC-SP.
        DATE-WRITTEN.  21/11/2020.
        DATE-COMPILED. 21/11/2020.
      * REMARKS. LÊ EM CADALU.DAT AS NOTAS DOS ALUNOS E ESCREVE EM
      * CADATU.DAT SUAS RESPECTIVAS MÉDIAS.

        ENVIRONMENT DIVISION.
            CONFIGURATION SECTION.
            SOURCE-COMPUTER. IBM-PC.
            OBJECT-COMPUTER. IBM-PC.
            SPECIAL-NAMES.   DECIMAL-POINT IS COMMA.

            INPUT-OUTPUT SECTION.
            FILE-CONTROL.
            SELECT CADALU ASSIGN TO DISK
            ORGANIZATION IS LINE SEQUENTIAL.
            SELECT CADATU ASSIGN TO DISK
            ORGANIZATION IS LINE SEQUENTIAL.

        DATA DIVISION.
            FILE SECTION.

            FD CADALU
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADALU.DAT".
            01 REGALU.
               02 NUM-ENT  PIC 9(05).
               02 NOME-ENT PIC X(20).
               02 NT1-ENT  PIC 9(02)V99.
               02 NT2-ENT  PIC 9(02)V99.
               02 NT3-ENT  PIC 9(02)V99.

            FD CADATU
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADATU.DAT".
            01 REGATU.
               02 NUM-SAI   PIC 9(05).
               02 MEDIA-SAI PIC 9(02)V99 VALUE ZEROS.

            WORKING-STORAGE SECTION.
            77 FIM-ARQ PIC X(03) VALUE "NAO".
        
        PROCEDURE DIVISION.
        PGM.
            PERFORM INICIO.
            PERFORM PRINCIPAL UNTIL FIM-ARQ EQUAL "SIM".
            PERFORM TERMINO.
        STOP RUN.

        INICIO.
            OPEN INPUT  CADALU
                 OUTPUT CADATU.
            PERFORM LEITURA.

        LEITURA.
            READ CADALU AT END MOVE "SIM" TO FIM-ARQ.

        GRAVACAO.
            MOVE NUM-ENT TO NUM-SAI.
            MOVE 0 TO MEDIA-SAI.
            ADD NT1-ENT TO MEDIA-SAI.
            ADD NT2-ENT TO MEDIA-SAI.
            ADD NT3-ENT TO MEDIA-SAI.
            DIVIDE 3 INTO MEDIA-SAI.
            WRITE REGATU.

        PRINCIPAL.
            PERFORM GRAVACAO.
            PERFORM LEITURA.

        TERMINO.
            CLOSE CADALU
                  CADATU.

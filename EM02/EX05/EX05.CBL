        IDENTIFICATION DIVISION.
        PROGRAM-ID.    EM02EX05.
        AUTHOR.        VINICIUS ALVES.
        INSTALLATION.  FATEC-SP.
        DATE-WRITTEN.  02/10/2016.
        DATE-COMPILED. 02/10/2016.

        ENVIRONMENT DIVISION.
            CONFIGURATION SECTION.
            SOURCE-COMPUTER. IBM-PC.
            OBJECT-COMPUTER. IBM-PC.
            SPECIAL-NAMES.   DECIMAL-POINT IS COMMA.

            INPUT-OUTPUT SECTION.
            FILE-CONTROL.
            SELECT ARQALU ASSIGN TO DISK
            ORGANIZATION IS LINE SEQUENTIAL.
            SELECT ARQREP ASSIGN TO DISK
            ORGANIZATION IS LINE SEQUENTIAL.
            SELECT RELAPROV ASSIGN TO DISK.

        DATA DIVISION.
            FILE SECTION.

            FD ARQALU
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "ARQALU.DAT".
            01 REGALU.
               02 MAT-ENT.
                  03 MAT-E1 PIC 9(06).
                  03 MAT-E2 PIC 9(01).
               02 NOME-ENT PIC X(30).
               02 NOTA1-E  PIC 9(02)V9(02).
               02 NOTA2-E  PIC 9(02)V9(02).
               02 NOTA3-E  PIC 9(02)V9(02).
               02 FALTAS-E PIC 9(02).
               02 SEXO-ENT PIC X(01).

            FD ARQREP
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "ARQREP.DAT".
            01 REGREP.
               02 MAT-SAI.
                  03 MAT-S1 PIC 9(06).
                  03 FILLER PIC X(01) VALUE "-".
                  03 MAT-S2 PIC 9(01).
               02 NOME-SAI PIC X(30).
               02 NOTA1-S  PIC 9(02)V9(02).
               02 NOTA2-S  PIC 9(02)V9(02).
               02 NOTA3-S  PIC 9(02)V9(02).
               02 FALTAS-S PIC 9(02).
               02 SEXO-SAI PIC X(01).

            FD RELAPROV
               LABEL RECORD IS OMITTED.
            01 REGAPROV    PIC X(80).

            WORKING-STORAGE SECTION.
            77 FIM-ARQ PIC X(03) VALUE  "NAO".
            77 CT-LIN  PIC 9(02) VALUE     30.
            77 CT-PAG  PIC 9(02) VALUE ZEROES.

            77 MEDAUX  PIC 9(02)V9(02) VALUE ZEROES.
            77 MEDAUX2 PIC 9(02)V9(02) VALUE ZEROES.
            77 TOTALU  PIC 99          VALUE ZEROES.

            01 CAB-01.
               02 FILLER  PIC X(25) VALUE      SPACES.
               02 FILLER  PIC X(27) VALUE 
                        "RELACAO DE ALUNOS APROVADOS".
               02 FILLER  PIC X(08) VALUE      SPACES.
               02 FILLER  PIC X(04) VALUE      "PAG ".
               02 PAG-OUT PIC ZZ9.
               02 FILLER  PIC X(13) VALUE      SPACES.

            01 CAB-02.
               02 FILLER  PIC X(06) VALUE    "NUMERO".
               02 FILLER  PIC X(02) VALUE      SPACES.
               02 FILLER  PIC X(02) VALUE      SPACES.

               02 FILLER  PIC X(04) VALUE      "NOME".
               02 FILLER  PIC X(26) VALUE      SPACES.
               02 FILLER  PIC X(02) VALUE      SPACES.

               02 FILLER  PIC X(05) VALUE     "MEDIA".
               02 FILLER  PIC X(02) VALUE      SPACES.

               02 FILLER  PIC X(06) VALUE    "FALTAS".
               02 FILLER  PIC X(25) VALUE      SPACES.

            01 DETALHE.
               02 NUMAPRO.
                  03 NUM-S1 PIC 9(6).
                  03 FILLER PIC X(1) VALUE '-'.
                  03 NUM-S2 PIC 9(1).
               02 FILLER  PIC X(02) VALUE      SPACES.

               02 NOMEAPR PIC X(30).
               02 FILLER  PIC X(02) VALUE      SPACES.

               02 MEDIAPR PIC Z9,99.
               02 FILLER  PIC X(01) VALUE      SPACES.
               02 FILLER  PIC X(02) VALUE      SPACES.

               02 FALTAPR PIC 99.
               02 FILLER  PIC X(04) VALUE      SPACES.
               02 FILLER  PIC X(02) VALUE      SPACES.

               02 FILLER  PIC X(22) VALUE      SPACES.

            01 ROD-01.
               02 FILLER  PIC X(27) VALUE
                        "TOTAL DE ALUNOS APROVADOS: ".
               02 TOTAPR  PIC 99999 VALUE      ZEROES.
               02 FILLER  PIC X(48) VALUE      SPACES.
            
            01 ROD-02.
               02 FILLER  PIC X(27) VALUE
                        "MEDIA GERAL DA TURMA     : ".
               02 MEDTUR  PIC Z9,99.
               02 FILLER  PIC X(48) VALUE      SPACES.

        PROCEDURE DIVISION.
        PGM.
            PERFORM INICIO.
            PERFORM PRINCIPAL UNTIL FIM-ARQ EQUAL "SIM".
            PERFORM TERMINO.
        STOP RUN.

        INICIO.
            OPEN INPUT  ARQALU
                 OUTPUT ARQREP
                        RELAPROV.
            PERFORM LEITURA.

        LEITURA.
            READ ARQALU AT END MOVE "SIM" TO FIM-ARQ.
        
        PRINCIPAL.
            PERFORM SAIDA.
            PERFORM LEITURA.

        SAIDA.
            ADD NOTA1-E NOTA2-E NOTA3-E GIVING MEDAUX.
            DIVIDE 3 INTO MEDAUX.
            IF MEDAUX GREATER THAN 7 AND FALTAS-E LESS THAN 16
                PERFORM IMPRESSAO
            ELSE
                PERFORM GRAVACAO.
            ADD MEDAUX TO MEDAUX2.
            ADD  1     TO TOTALU.

        GRAVACAO.
            MOVE MAT-E1   TO MAT-S1.
            MOVE MAT-E2   TO MAT-S2.
            MOVE NOME-ENT TO NOME-SAI.
            MOVE NOTA1-E  TO NOTA1-S.
            MOVE NOTA2-E  TO NOTA2-S.
            MOVE NOTA3-E  TO NOTA3-S.
            MOVE FALTAS-E TO FALTAS-S.
            MOVE SEXO-ENT TO SEXO-SAI.

            WRITE REGREP.

        IMPRESSAO.
            IF CT-LIN GREATER THAN 29
                PERFORM CABECALHO.
            PERFORM IMPDET.

        IMPDET.
            MOVE MAT-E1   TO NUM-S1 .
            MOVE MAT-E2   TO NUM-S2 .
            MOVE NOME-ENT TO NOMEAPR.
            MOVE MEDAUX   TO MEDIAPR.
            MOVE FALTAS-E TO FALTAPR.

            WRITE REGAPROV FROM DETALHE AFTER ADVANCING 1 LINE .
            ADD 1 TO CT-LIN.
            ADD 1 TO TOTAPR.

        CABECALHO.
            ADD  1      TO CT-PAG  .
            MOVE CT-PAG TO PAG-OUT .
            MOVE SPACES TO REGAPROV.
            MOVE ZEROES TO CT-LIN  .

            WRITE REGAPROV             AFTER ADVANCING   PAGE .
            WRITE REGAPROV FROM CAB-01 AFTER ADVANCING 1 LINE .
            WRITE REGAPROV FROM CAB-02 AFTER ADVANCING 2 LINES.

        RODAPE.
            DIVIDE MEDAUX2 BY   TOTALU GIVING MEDTUR.

            WRITE REGAPROV FROM ROD-01 AFTER ADVANCING 2 LINES.
            WRITE REGAPROV FROM ROD-02 AFTER ADVANCING 1 LINE .

        TERMINO.
            PERFORM RODAPE.
            CLOSE ARQALU
                  ARQREP
                  RELAPROV.
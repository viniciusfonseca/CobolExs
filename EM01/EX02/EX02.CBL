        IDENTIFICATION DIVISION.
        PROGRAM-ID.    EM01EX02.
        AUTHOR.        VINICIUS ALVES.
        INSTALLATION.  FATEC-SP.
        DATE-WRITTEN.  21/11/2020.
        DATE-COMPILED. 21/11/2020.
      * REMARKS. ESTE PROGRAMA É SIMILAR AO EM01EX01. LÊ  CÓDIGO,
      * NOME, SEXO E DATA DE NASCIMENTO DE CADALU.DAT, ESCREVENDO EM
      * CADATU.DAT TODOS OS CAMPOS, EXCETO O SEXO.

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
               02 SEX-ENT  PIC X(01).
               02 DATNASC-ENT.
                  03 DD-ENT PIC 9(02).
                  03 MM-ENT PIC 9(02).
                  03 AA-ENT PIC 9(04).

            FD CADATU
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADATU.DAT".
            01 REGATU.
               02 NUM-SAI  PIC 9(05).
               02 NOME-SAI PIC X(20).
               02 DATNASC-SAI.
                  03 DD-SAI PIC 9(02).
                  03 MM-SAI PIC 9(02).
                  03 AA-SAI PIC 9(04).

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
            MOVE NUM-ENT  TO NUM-SAI.
            MOVE NOME-ENT TO NOME-SAI.
            MOVE DATNASC-ENT TO DATNASC-SAI.
            WRITE REGATU.

        PRINCIPAL.
            PERFORM GRAVACAO.
            PERFORM LEITURA.

        TERMINO.
            CLOSE CADALU
                  CADATU.

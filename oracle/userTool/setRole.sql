set serveroutput on
DECLARE

	-- 定数：csvファイルへのパス
	CONST_FILE_PATH CONSTANT VARCHAR2(200) := '/home/oracle/eim/userimp';
	
	File_Name		VARCHAR2(50);
	File_Handle		UTL_FILE.FILE_TYPE;
	Read_Line		VARCHAR2(4000);
	Head_Line		VARCHAR2(4000);
	Row_Count		NUMBER(32);
	Head_Count		NUMBER(32);
	Role_Count		NUMBER(32) := 0;
	Func_Flag		BOOLEAN := FALSE;
	Parent_Flag		BOOLEAN := FALSE;
	
	O_rscode		NUMBER(32) := 0;
	O_roleId		NUMBER(32) := 0;

	TYPE TYPE_REC_EIMROLE IS RECORD(
		 tRID1			EIMROLE.ID%type
		,tjRNAME1		EIMROLE.NAME%type
		,teRNAME1		EIMROLE.NAME%type
		,tRPARENT1		EIMROLE.PARENT%type
		,tRID2			EIMROLE.ID%type
		,tjRNAME2		EIMROLE.NAME%type
		,teRNAME2		EIMROLE.NAME%type
		,tRPARENT2		EIMROLE.PARENT%type
		,tRID3			EIMROLE.ID%type
		,tjRNAME3		EIMROLE.NAME%type
		,teRNAME3		EIMROLE.NAME%type
		,tRPARENT3		EIMROLE.PARENT%type
		,tRID4			EIMROLE.ID%type
		,tjRNAME4		EIMROLE.NAME%type
		,teRNAME4		EIMROLE.NAME%type
		,tRPARENT4		EIMROLE.PARENT%type
		,tRID5			EIMROLE.ID%type
		,tjRNAME5		EIMROLE.NAME%type
		,teRNAME5		EIMROLE.NAME%type
		,tRPARENT5		EIMROLE.PARENT%type
	);
	TYPE TYPE_TAB_EIMROLE IS TABLE OF TYPE_REC_EIMROLE INDEX BY BINARY_INTEGER;
	-- レコードの索引付き表
	tabEIMROLE	TYPE_TAB_EIMROLE;
	
	-- レコード
	oyaEIMROLE TYPE_REC_EIMROLE;

	--
	-- 1行を1レコードに変換
	--
	FUNCTION CSV2TAB
	(
		  Read_Line	IN	VARCHAR2
		 ,MAXCOL	IN	NUMBER
	)
	RETURN BOOLEAN
	IS	--変数宣言
		ENDPOINT	NUMBER := 0;
		POSITION	NUMBER := 1;
		COLVALUE	VARCHAR2(4000);
	BEGIN
		FOR I IN 1..MAXCOL LOOP
			IF I = MAXCOL THEN
				COLVALUE := REPLACE( SUBSTR( Read_Line , POSITION ) , ',' , '' );
			ELSE
				ENDPOINT := INSTR( Read_Line , ',' , POSITION , 1 );
				COLVALUE := SUBSTR( Read_Line , POSITION , ENDPOINT - POSITION );
			END IF;
			CASE I
				WHEN 1	THEN tabEIMROLE(Role_Count).tjRNAME1	:= COLVALUE;
				WHEN 2	THEN tabEIMROLE(Role_Count).teRNAME1	:= COLVALUE;
				WHEN 3	THEN tabEIMROLE(Role_Count).tjRNAME2	:= COLVALUE;
				WHEN 4	THEN tabEIMROLE(Role_Count).teRNAME2	:= COLVALUE;
				WHEN 5	THEN tabEIMROLE(Role_Count).tjRNAME3	:= COLVALUE;
				WHEN 6	THEN tabEIMROLE(Role_Count).teRNAME3	:= COLVALUE;
				WHEN 7	THEN tabEIMROLE(Role_Count).tjRNAME4	:= COLVALUE;
				WHEN 8	THEN tabEIMROLE(Role_Count).teRNAME4	:= COLVALUE;
				WHEN 9	THEN tabEIMROLE(Role_Count).tjRNAME5	:= COLVALUE;
				WHEN 10	THEN tabEIMROLE(Role_Count).teRNAME5	:= COLVALUE;
				ELSE NULL;
			END CASE;
			POSITION := ENDPOINT + 1;
			ENDPOINT := 0;
		END LOOP;
		
		RETURN TRUE;
	END;

	--
	-- ロール・アザーロール作成
	--
	PROCEDURE CREGROLE
	(
		  tjRNAME			IN	VARCHAR2
		 ,teRNAME			IN	VARCHAR2
		 ,I_parentId		IN	NUMBER
		 ,O_roleId			OUT	NUMBER
		 ,O_rscode			OUT	NUMBER
	)
	IS	--変数宣言
	BEGIN
		
		-- EIMROLE
		RoleUtils.createRole(	 I_roleName => tjRNAME
								,I_parentId => I_parentId
								,O_roleId => O_roleId
								,O_rscode => O_rscode
							);
		-- EIMROLEOTHER(JA)
		RoleUtils.addOtherRoleName(		 I_roleId => O_roleId
										,I_langId => 'JA'
										,I_otherName => tjRNAME
										,O_rscode => O_rscode
									);
		-- EIMROLEOTHER(EN)
		RoleUtils.addOtherRoleName(		 I_roleId => O_roleId
										,I_langId => 'EN'
										,I_otherName => teRNAME
										,O_rscode => O_rscode
									);
	END;


BEGIN

	-- 引数
	File_Name := 'utf8_EIMROLE.csv';
	File_Handle := UTL_FILE.FOPEN( CONST_FILE_PATH , File_Name , 'r' );
	Row_Count := 1;

	-- ファイルから配列に格納
	BEGIN
		LOOP
			UTL_FILE.GET_LINE( File_Handle , Read_Line );
			IF Row_Count = 1 THEN
				-- 1行目はヘッダー行
				--Head_Count := ANALYZEHEADER( Read_Line );
				Head_Count := 10;
				DBMS_OUTPUT.PUT_LINE( 'ヘッダー列数:' || Head_Count );
			ELSE
				DBMS_OUTPUT.PUT_LINE( '-------------------------------' );
				DBMS_OUTPUT.PUT_LINE( '[' || Read_Line || ']' );
				DBMS_OUTPUT.PUT_LINE( '-------------------------------' );
				Func_Flag := CSV2TAB( Read_Line , Head_Count );
				Role_Count := Role_Count + 1;
			END IF;
			Row_Count := Row_Count + 1;
		END LOOP;
	EXCEPTION
		WHEN NO_DATA_FOUND THEN
			NULL;
	END;

	Row_Count := 0;
	Parent_Flag := FALSE;
	-- ロール作成
	FOR I IN 0..Role_Count-1 LOOP
		
		-- (1)
		IF tabEIMROLE(I).tjRNAME1 <> '-' AND tabEIMROLE(I).teRNAME1 <> '-' THEN
			CREGROLE(
					  tjRNAME => tabEIMROLE(I).tjRNAME1
					 ,teRNAME => tabEIMROLE(I).teRNAME1
					 ,I_parentId => 0
					 ,O_roleId => tabEIMROLE(I).tRID1
					 ,O_rscode => O_rscode
					);
			tabEIMROLE(I).tRPARENT1 := 0;
			oyaEIMROLE.tRID1 := tabEIMROLE(I).tRID1;
		END IF;
		
		-- (2)
		IF tabEIMROLE(I).tjRNAME2 <> '-' AND tabEIMROLE(I).teRNAME2 <> '-' THEN
			IF tabEIMROLE(I).tjRNAME1 = '-' AND tabEIMROLE(I).teRNAME1 = '-' THEN
				CREGROLE(
						  tjRNAME => tabEIMROLE(I).tjRNAME2
						 ,teRNAME => tabEIMROLE(I).teRNAME2
						 ,I_parentId => oyaEIMROLE.tRID1
						 ,O_roleId => tabEIMROLE(I).tRID2
						 ,O_rscode => O_rscode
						);
			ELSE
				CREGROLE(
						  tjRNAME => tabEIMROLE(I).tjRNAME2
						 ,teRNAME => tabEIMROLE(I).teRNAME2
						 ,I_parentId => tabEIMROLE(I).tRID1
						 ,O_roleId => tabEIMROLE(I).tRID2
						 ,O_rscode => O_rscode
						);
			END IF;
			tabEIMROLE(I).tRPARENT2 := tabEIMROLE(I).tRID1;
			oyaEIMROLE.tRID2 := tabEIMROLE(I).tRID2;
		END IF;

		-- (3)
		IF tabEIMROLE(I).tjRNAME3 <> '-' AND tabEIMROLE(I).teRNAME3 <> '-' THEN
			IF tabEIMROLE(I).tjRNAME2 = '-' AND tabEIMROLE(I).teRNAME2 = '-' THEN
				CREGROLE(
						  tjRNAME => tabEIMROLE(I).tjRNAME3
						 ,teRNAME => tabEIMROLE(I).teRNAME3
						 ,I_parentId => oyaEIMROLE.tRID2
						 ,O_roleId => tabEIMROLE(I).tRID3
						 ,O_rscode => O_rscode
						);
			ELSE
				CREGROLE(
						  tjRNAME => tabEIMROLE(I).tjRNAME3
						 ,teRNAME => tabEIMROLE(I).teRNAME3
						 ,I_parentId => tabEIMROLE(I).tRID2
						 ,O_roleId => tabEIMROLE(I).tRID3
						 ,O_rscode => O_rscode
						);
			END IF;
			tabEIMROLE(I).tRPARENT3 := tabEIMROLE(I).tRID2;
			oyaEIMROLE.tRID3 := tabEIMROLE(I).tRID3;
		END IF;

		-- (4)
		IF tabEIMROLE(I).tjRNAME4 <> '-' AND tabEIMROLE(I).teRNAME4 <> '-' THEN
			IF tabEIMROLE(I).tjRNAME3 = '-' AND tabEIMROLE(I).teRNAME3 = '-' THEN
				CREGROLE(
						  tjRNAME => tabEIMROLE(I).tjRNAME4
						 ,teRNAME => tabEIMROLE(I).teRNAME4
						 ,I_parentId => oyaEIMROLE.tRID3
						 ,O_roleId => tabEIMROLE(I).tRID4
						 ,O_rscode => O_rscode
						);
			ELSE
				CREGROLE(
						  tjRNAME => tabEIMROLE(I).tjRNAME4
						 ,teRNAME => tabEIMROLE(I).teRNAME4
						 ,I_parentId => tabEIMROLE(I).tRID3
						 ,O_roleId => tabEIMROLE(I).tRID4
						 ,O_rscode => O_rscode
						);
			END IF;
			tabEIMROLE(I).tRPARENT4 := tabEIMROLE(I).tRID3;
			oyaEIMROLE.tRID4 := tabEIMROLE(I).tRID4;
		END IF;
		
		-- (5)
		IF tabEIMROLE(I).tjRNAME5 <> '-' AND tabEIMROLE(I).teRNAME5 <> '-' THEN
			IF tabEIMROLE(I).tjRNAME4 = '-' AND tabEIMROLE(I).teRNAME4 = '-' THEN
				CREGROLE(
						  tjRNAME => tabEIMROLE(I).tjRNAME5
						 ,teRNAME => tabEIMROLE(I).teRNAME5
						 ,I_parentId => oyaEIMROLE.tRID4
						 ,O_roleId => tabEIMROLE(I).tRID5
						 ,O_rscode => O_rscode
						);
			ELSE
				CREGROLE(
						  tjRNAME => tabEIMROLE(I).tjRNAME5
						 ,teRNAME => tabEIMROLE(I).teRNAME5
						 ,I_parentId => tabEIMROLE(I).tRID4
						 ,O_roleId => tabEIMROLE(I).tRID5
						 ,O_rscode => O_rscode
						);
			END IF;
			tabEIMROLE(I).tRPARENT5 := tabEIMROLE(I).tRID4;
			oyaEIMROLE.tRID5 := tabEIMROLE(I).tRID5;
		END IF;

	END LOOP;

END;
/

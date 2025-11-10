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
	Group_Count		NUMBER(32) := 0;
	Func_Flag		BOOLEAN := FALSE;
	Parent_Flag		BOOLEAN := FALSE;
	
	O_rscode		NUMBER(32) := 0;
	O_groupId		NUMBER(32) := 0;

	TYPE TYPE_REC_EIMGROUP IS RECORD(
		 tGID1			EIMGROUP.ID%type
		,tjGNAME1		EIMGROUP.NAME%type
		,teGNAME1		EIMGROUP.NAME%type
		,tGPARENT1		EIMGROUP.PARENT%type
		,tGID2			EIMGROUP.ID%type
		,tjGNAME2		EIMGROUP.NAME%type
		,teGNAME2		EIMGROUP.NAME%type
		,tGPARENT2		EIMGROUP.PARENT%type
		,tGID3			EIMGROUP.ID%type
		,tjGNAME3		EIMGROUP.NAME%type
		,teGNAME3		EIMGROUP.NAME%type
		,tGPARENT3		EIMGROUP.PARENT%type
		,tGID4			EIMGROUP.ID%type
		,tjGNAME4		EIMGROUP.NAME%type
		,teGNAME4		EIMGROUP.NAME%type
		,tGPARENT4		EIMGROUP.PARENT%type
		,tGID5			EIMGROUP.ID%type
		,tjGNAME5		EIMGROUP.NAME%type
		,teGNAME5		EIMGROUP.NAME%type
		,tGPARENT5		EIMGROUP.PARENT%type
	);
	TYPE TYPE_TAB_EIMGROUP IS TABLE OF TYPE_REC_EIMGROUP INDEX BY BINARY_INTEGER;
	-- レコードの索引付き表
	tabEIMGROUP	TYPE_TAB_EIMGROUP;
	
	-- レコード
	oyaEIMGROUP TYPE_REC_EIMGROUP;

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
				WHEN 1	THEN tabEIMGROUP(Group_Count).tjGNAME1	:= COLVALUE;
				WHEN 2	THEN tabEIMGROUP(Group_Count).teGNAME1	:= COLVALUE;
				WHEN 3	THEN tabEIMGROUP(Group_Count).tjGNAME2	:= COLVALUE;
				WHEN 4	THEN tabEIMGROUP(Group_Count).teGNAME2	:= COLVALUE;
				WHEN 5	THEN tabEIMGROUP(Group_Count).tjGNAME3	:= COLVALUE;
				WHEN 6	THEN tabEIMGROUP(Group_Count).teGNAME3	:= COLVALUE;
				WHEN 7	THEN tabEIMGROUP(Group_Count).tjGNAME4	:= COLVALUE;
				WHEN 8	THEN tabEIMGROUP(Group_Count).teGNAME4	:= COLVALUE;
				WHEN 9	THEN tabEIMGROUP(Group_Count).tjGNAME5	:= COLVALUE;
				WHEN 10	THEN tabEIMGROUP(Group_Count).teGNAME5	:= COLVALUE;
				ELSE NULL;
			END CASE;
			POSITION := ENDPOINT + 1;
			ENDPOINT := 0;
		END LOOP;
		
		RETURN TRUE;
	END;

	--
	-- グループ・アザーグループ作成
	--
	PROCEDURE CREGROUP
	(
		  tjGNAME			IN	VARCHAR2
		 ,teGNAME			IN	VARCHAR2
		 ,I_parentGroupId	IN	NUMBER
		 ,O_groupId			OUT	NUMBER
		 ,O_rscode			OUT	NUMBER
	)
	IS	--変数宣言
	BEGIN
		
		-- EIMGROUP
		GroupUtils.createGroup(	 I_groupName => tjGNAME
								,I_parentGroupId => I_parentGroupId
								,O_groupId => O_groupId
								,O_result => O_rscode
							);
		-- EIMGROUPOTHER(JA)
		GroupUtils.addOtherGroupName(	 I_groupId => O_groupId
										,I_langId => 'JA'
										,I_otherName => tjGNAME
										,O_rscode => O_rscode
									);
		-- EIMGROUPOTHER(EN)
		GroupUtils.addOtherGroupName(	 I_groupId => O_groupId
										,I_langId => 'EN'
										,I_otherName => teGNAME
										,O_rscode => O_rscode
									);
	END;


BEGIN

	-- 引数
	File_Name := 'utf8_EIMGROUP.csv';
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
				Group_Count := Group_Count + 1;
			END IF;
			Row_Count := Row_Count + 1;
		END LOOP;
	EXCEPTION
		WHEN NO_DATA_FOUND THEN
			NULL;
	END;

	Row_Count := 0;
	Parent_Flag := FALSE;
	-- グループ作成
	FOR I IN 0..Group_Count-1 LOOP
		
		-- (1)
		IF tabEIMGROUP(I).tjGNAME1 <> '-' AND tabEIMGROUP(I).teGNAME1 <> '-' THEN
			CREGROUP(
					  tjGNAME => tabEIMGROUP(I).tjGNAME1
					 ,teGNAME => tabEIMGROUP(I).teGNAME1
					 ,I_parentGroupId => 0
					 ,O_groupId => tabEIMGROUP(I).tGID1
					 ,O_rscode => O_rscode
					);
			tabEIMGROUP(I).tGPARENT1 := 0;
			oyaEIMGROUP.tGID1 := tabEIMGROUP(I).tGID1;
		END IF;
		
		-- (2)
		IF tabEIMGROUP(I).tjGNAME2 <> '-' AND tabEIMGROUP(I).teGNAME2 <> '-' THEN
			IF tabEIMGROUP(I).tjGNAME1 = '-' AND tabEIMGROUP(I).teGNAME1 = '-' THEN
				CREGROUP(
						  tjGNAME => tabEIMGROUP(I).tjGNAME2
						 ,teGNAME => tabEIMGROUP(I).teGNAME2
						 ,I_parentGroupId => oyaEIMGROUP.tGID1
						 ,O_groupId => tabEIMGROUP(I).tGID2
						 ,O_rscode => O_rscode
						);
			ELSE
				CREGROUP(
						  tjGNAME => tabEIMGROUP(I).tjGNAME2
						 ,teGNAME => tabEIMGROUP(I).teGNAME2
						 ,I_parentGroupId => tabEIMGROUP(I).tGID1
						 ,O_groupId => tabEIMGROUP(I).tGID2
						 ,O_rscode => O_rscode
						);
			END IF;
			tabEIMGROUP(I).tGPARENT2 := tabEIMGROUP(I).tGID1;
			oyaEIMGROUP.tGID2 := tabEIMGROUP(I).tGID2;
		END IF;

		-- (3)
		IF tabEIMGROUP(I).tjGNAME3 <> '-' AND tabEIMGROUP(I).teGNAME3 <> '-' THEN
			IF tabEIMGROUP(I).tjGNAME2 = '-' AND tabEIMGROUP(I).teGNAME2 = '-' THEN
				CREGROUP(
						  tjGNAME => tabEIMGROUP(I).tjGNAME3
						 ,teGNAME => tabEIMGROUP(I).teGNAME3
						 ,I_parentGroupId => oyaEIMGROUP.tGID2
						 ,O_groupId => tabEIMGROUP(I).tGID3
						 ,O_rscode => O_rscode
						);
			ELSE
				CREGROUP(
						  tjGNAME => tabEIMGROUP(I).tjGNAME3
						 ,teGNAME => tabEIMGROUP(I).teGNAME3
						 ,I_parentGroupId => tabEIMGROUP(I).tGID2
						 ,O_groupId => tabEIMGROUP(I).tGID3
						 ,O_rscode => O_rscode
						);
			END IF;
			tabEIMGROUP(I).tGPARENT3 := tabEIMGROUP(I).tGID2;
			oyaEIMGROUP.tGID3 := tabEIMGROUP(I).tGID3;
		END IF;

		-- (4)
		IF tabEIMGROUP(I).tjGNAME4 <> '-' AND tabEIMGROUP(I).teGNAME4 <> '-' THEN
			IF tabEIMGROUP(I).tjGNAME3 = '-' AND tabEIMGROUP(I).teGNAME3 = '-' THEN
				CREGROUP(
						  tjGNAME => tabEIMGROUP(I).tjGNAME4
						 ,teGNAME => tabEIMGROUP(I).teGNAME4
						 ,I_parentGroupId => oyaEIMGROUP.tGID3
						 ,O_groupId => tabEIMGROUP(I).tGID4
						 ,O_rscode => O_rscode
						);
			ELSE
				CREGROUP(
						  tjGNAME => tabEIMGROUP(I).tjGNAME4
						 ,teGNAME => tabEIMGROUP(I).teGNAME4
						 ,I_parentGroupId => tabEIMGROUP(I).tGID3
						 ,O_groupId => tabEIMGROUP(I).tGID4
						 ,O_rscode => O_rscode
						);
			END IF;
			tabEIMGROUP(I).tGPARENT4 := tabEIMGROUP(I).tGID3;
			oyaEIMGROUP.tGID4 := tabEIMGROUP(I).tGID4;
		END IF;

		-- (5)
		IF tabEIMGROUP(I).tjGNAME5 <> '-' AND tabEIMGROUP(I).teGNAME5 <> '-' THEN
			IF tabEIMGROUP(I).tjGNAME4 = '-' AND tabEIMGROUP(I).teGNAME4 = '-' THEN
				CREGROUP(
						  tjGNAME => tabEIMGROUP(I).tjGNAME5
						 ,teGNAME => tabEIMGROUP(I).teGNAME5
						 ,I_parentGroupId => oyaEIMGROUP.tGID4
						 ,O_groupId => tabEIMGROUP(I).tGID5
						 ,O_rscode => O_rscode
						);
			ELSE
				CREGROUP(
						  tjGNAME => tabEIMGROUP(I).tjGNAME5
						 ,teGNAME => tabEIMGROUP(I).teGNAME5
						 ,I_parentGroupId => tabEIMGROUP(I).tGID4
						 ,O_groupId => tabEIMGROUP(I).tGID5
						 ,O_rscode => O_rscode
						);
			END IF;
			tabEIMGROUP(I).tGPARENT5 := tabEIMGROUP(I).tGID4;
			oyaEIMGROUP.tGID5 := tabEIMGROUP(I).tGID5;
		END IF;

	END LOOP;

END;
/

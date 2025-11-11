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
	User_Count		NUMBER(32) := 0;
	Func_Flag		BOOLEAN := FALSE;
	
	O_rscode		NUMBER(32) := 0;
	O_userId		NUMBER(32) := 0;
	
	-- **定数**
	-- ヘッダーカラム数
	CONST_HEADER_MAX_CULUMN CONSTANT   NUMBER(32) := 27;
	
	-- 管理者権限設定値
    -- ドキュメントタイプ管理
	CONST_DOCTYPE_AUTHNUM CONSTANT   NUMBER(32) := 2;
    -- カスタム属性管理
	CONST_CUSTOMATT_AUTHNUM CONSTANT NUMBER(32) := 4;
    -- 属性ツリービュー管理
	CONST_ATTTREE_AUTHNUM CONSTANT   NUMBER(32) := 512;
    -- ワークフロー管理
	CONST_WORKFLOW_AUTHNUM CONSTANT  NUMBER(32) := 8;
    -- フォーマット管理
	CONST_FORMAT_AUTHNUM CONSTANT    NUMBER(32) := 16;
    -- ユーザ管理
	CONST_USER_AUTHNUM CONSTANT      NUMBER(32) := 32;
    -- セキュリティ管理
	CONST_SECURITY_AUTHNUM CONSTANT  NUMBER(32) := 64;
    -- 操作履歴管理管理
	CONST_HISTORY_AUTHNUM CONSTANT   NUMBER(32) := 128;
    -- セキュリティエントリ管理
	CONST_ENTRY_AUTHNUM CONSTANT     NUMBER(32) := 256;

	TYPE TYPE_REC_EIMUSER IS RECORD(
		 tID		EIMUSER.ID%type
		,tCODE		EIMUSER.CODE%type
		,tjNAME		EIMUSER.NAME%type
		,teNAME		EIMUSER.NAME%type
		,tKANA		EIMUSER.KANA%type
		,tPASS		EIMUSER.PASS%type
		,tMAIL		EIMUSER.MAIL%type
		,tADMIN		EIMUSER.ADMIN%type
		,tDISABLE	EIMUSER.DISABLE%type
		,tLANG	    EIMUSER.LANG%type
		,tGROUP1	EIMGROUP.NAME%type
		,tGROUP2	EIMGROUP.NAME%type
		,tGROUP3	EIMGROUP.NAME%type
		,tGROUP4	EIMGROUP.NAME%type
		,tGROUP5	EIMGROUP.NAME%type
		,tROLE1		EIMROLE.NAME%type
		,tROLE2		EIMROLE.NAME%type
		,tROLE3		EIMROLE.NAME%type
		,tROLE4		EIMROLE.NAME%type
		,tROLE5		EIMROLE.NAME%type
	);
	TYPE TYPE_TAB_EIMUSER IS TABLE OF TYPE_REC_EIMUSER INDEX BY BINARY_INTEGER;
	-- レコードの索引付き表
	tabEIMUSER	TYPE_TAB_EIMUSER;
	
	--
	-- ヘッダー行解析
	--
	FUNCTION ANALYZEHEADER
	(
		 Head_Line	IN	VARCHAR2
	)
	RETURN NUMBER
	IS	--変数宣言
		tabEIMUSER	TYPE_TAB_EIMUSER;
		ENDPOINT	NUMBER := 0;
		POSITION	NUMBER := 1;
		retCOLNUM	NUMBER := 0;
	BEGIN
		LOOP
			ENDPOINT := INSTR( Read_Line , ',' , POSITION , 1 );
			IF ENDPOINT <> 0 THEN
				retCOLNUM := retCOLNUM + 1;
			ELSE
				retCOLNUM := retCOLNUM + 1;
				EXIT;
			END IF;
			
			POSITION := ENDPOINT + 1;
			ENDPOINT := 0;
			
		END LOOP;
		
		RETURN retCOLNUM;
	END;

    --
    -- 管理者権限加算
    --
    FUNCTION AUTHSUM
    (
        I_ADMIN      IN NUMBER
       ,PLUS_AUTHNUM IN NUMBER
       ,COLVALUE     IN VARCHAR2
    )
    RETURN NUMBER
    IS	--変数宣言
    	SUM_ADMIN NUMBER := I_ADMIN;
    BEGIN
		IF LENGTH(COLVALUE) > 0 THEN 
			SUM_ADMIN := SUM_ADMIN + PLUS_AUTHNUM;
		END IF;
		
		RETURN SUM_ADMIN;
    END;

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
		ADMIN	    NUMBER := 0;
	BEGIN
		FOR I IN 1..MAXCOL LOOP
			IF I = MAXCOL THEN
				COLVALUE := REPLACE( SUBSTR( Read_Line , POSITION ) , ',' , '' );
			ELSE
				ENDPOINT := INSTR( Read_Line , ',' , POSITION , 1 );
				COLVALUE := SUBSTR( Read_Line , POSITION , ENDPOINT - POSITION );
			END IF;
			CASE I
				WHEN 1	THEN tabEIMUSER(User_Count).tCODE	:= COLVALUE;
				WHEN 2	THEN tabEIMUSER(User_Count).tPASS	:= COLVALUE;
				WHEN 3	THEN tabEIMUSER(User_Count).tKANA	:= COLVALUE;
				WHEN 4	THEN tabEIMUSER(User_Count).tjNAME	:= COLVALUE;
				WHEN 5	THEN tabEIMUSER(User_Count).teNAME	:= COLVALUE;
				WHEN 6	THEN tabEIMUSER(User_Count).tMAIL	:= COLVALUE;
				WHEN 7	THEN tabEIMUSER(User_Count).tDISABLE	:= TO_NUMBER( COLVALUE );
				WHEN 8	THEN tabEIMUSER(User_Count).tLANG	:= COLVALUE;
				-- ドキュメントタイプ管理
				WHEN 9	THEN ADMIN := AUTHSUM(ADMIN, CONST_DOCTYPE_AUTHNUM, COLVALUE);
				-- カスタム属性管理
				WHEN 10	THEN ADMIN := AUTHSUM(ADMIN, CONST_CUSTOMATT_AUTHNUM, COLVALUE);
				-- 属性ツリービュー管理
				WHEN 11	THEN ADMIN := AUTHSUM(ADMIN, CONST_ATTTREE_AUTHNUM, COLVALUE);
				-- ワークフロー管理
				WHEN 12	THEN ADMIN := AUTHSUM(ADMIN, CONST_WORKFLOW_AUTHNUM, COLVALUE);
				-- フォーマット管理
				WHEN 13	THEN ADMIN := AUTHSUM(ADMIN, CONST_FORMAT_AUTHNUM, COLVALUE);
				-- ユーザ管理
				WHEN 14	THEN ADMIN := AUTHSUM(ADMIN, CONST_USER_AUTHNUM, COLVALUE);
				-- セキュリティ管理
				WHEN 15	THEN ADMIN := AUTHSUM(ADMIN, CONST_SECURITY_AUTHNUM, COLVALUE);
				-- 操作履歴管理
				WHEN 16	THEN ADMIN := AUTHSUM(ADMIN, CONST_HISTORY_AUTHNUM, COLVALUE);
				-- セキュリティエントリ管理
				WHEN 17	THEN ADMIN := AUTHSUM(ADMIN, CONST_ENTRY_AUTHNUM, COLVALUE);
				WHEN 18	THEN tabEIMUSER(User_Count).tGROUP1	:= COLVALUE;
				WHEN 19	THEN tabEIMUSER(User_Count).tGROUP2	:= COLVALUE;
				WHEN 20	THEN tabEIMUSER(User_Count).tGROUP3	:= COLVALUE;
				WHEN 21	THEN tabEIMUSER(User_Count).tGROUP4	:= COLVALUE;
				WHEN 22	THEN tabEIMUSER(User_Count).tGROUP5	:= COLVALUE;
				WHEN 23	THEN tabEIMUSER(User_Count).tROLE1	:= COLVALUE;
				WHEN 24	THEN tabEIMUSER(User_Count).tROLE2	:= COLVALUE;
				WHEN 25	THEN tabEIMUSER(User_Count).tROLE3	:= COLVALUE;
				WHEN 26	THEN tabEIMUSER(User_Count).tROLE4	:= COLVALUE;
				WHEN 27	THEN tabEIMUSER(User_Count).tROLE5	:= COLVALUE;
				ELSE NULL;
			END CASE;
			POSITION := ENDPOINT + 1;
			ENDPOINT := 0;
			-- 管理者権限設定
			tabEIMUSER(User_Count).tADMIN	:= ADMIN;
		END LOOP;
		
		RETURN TRUE;
	END;

	--
	-- ユーザー・アザーユーザー作成
	--
	PROCEDURE CREUSER
	(
		  tCODE				IN	VARCHAR2
		 ,tjNAME			IN	VARCHAR2
		 ,teNAME			IN	VARCHAR2
		 ,tKANA				IN	VARCHAR2
		 ,tPASS				IN	VARCHAR2
		 ,tMAIL				IN	VARCHAR2
		 ,tADMIN			IN	NUMBER
		 ,tDISABLE			IN	NUMBER
		 ,tLANG				IN	VARCHAR2
		 ,O_userId			OUT	NUMBER
		 ,O_rscode			OUT	NUMBER
	)
	IS	--変数宣言
	BEGIN
		UserUtils.createUser(	 I_userCode => tCODE
								,I_userName => tjNAME
								,I_userKana => tKANA
								,I_userPass => tPASS
								,I_userMail => tMAIL
								,I_userAdmin => tADMIN
								,I_userDisable => tDISABLE
								,I_userLanguage => tLANG
								,O_userId => O_userId
								,O_rscode => O_rscode
							);
		-- EIMUSEROHTER(JA)
		UserUtils.addOtherUserName(	 I_userId => O_userId
									,I_langId => 'JA'
									,I_otherName => tjNAME
									,O_rscode => O_rscode
								);
		-- EIMUSEROHTER(EN)
		UserUtils.addOtherUserName(	 I_userId => O_userId
									,I_langId => 'EN'
									,I_otherName => teNAME
									,O_rscode => O_rscode
								);
	END;

	--
	-- アサインユーザー(グループ)
	--
	PROCEDURE GASSIGNUSER
	(
		  I_userId		IN	NUMBER
		 ,I_groupName	IN	VARCHAR2
		 ,O_rscode		OUT	NUMBER
	)
	IS	--変数宣言
		
		wk_groupId		EIMGROUP.ID%type;
		
		-- グループ名からグループIDを取得するカーソル(同名グループでも1件だけ取得)
		CURSOR c_group( I_groupName IN EIMGROUP.NAME%type ) IS
			select
				X.gid
			from
				( select EG.id as gid from eimgroup EG where EG.name = I_groupName order by EG.id asc ) X
			where
				rownum < 2;
	BEGIN
		FOR r_cur IN c_group( I_groupName ) LOOP
			wk_groupId := r_cur.gid;
		END LOOP;
		DBMS_OUTPUT.PUT_LINE( wk_groupId || ':' || I_groupName );
	
		GroupUtils.assignUser(	 I_groupId => wk_groupId
								,I_userId => I_userId
								,O_rscode => O_rscode
							);
	END;

	--
	-- アサインユーザー(ロール)
	--
	PROCEDURE RASSIGNUSER
	(
		  I_userId		IN	NUMBER
		 ,I_roleName	IN	VARCHAR2
		 ,O_rscode		OUT	NUMBER
	)
	IS	--変数宣言
		wk_roleId		EIMROLE.ID%type;
		-- ロール名からロールIDを取得するカーソル(同名ロールでも1件だけ取得)
		CURSOR c_role( I_roleName IN EIMROLE.NAME%type ) IS
			select
				X.rid
			from
				( select ER.id as rid from eimrole ER where ER.name = I_roleName order by ER.id asc ) X
			where
				rownum < 2;
	BEGIN
		FOR r_cur IN c_role( I_roleName ) LOOP
			wk_roleId := r_cur.rid;
		END LOOP;
		DBMS_OUTPUT.PUT_LINE( wk_roleId || ':' || I_roleName );
	
		RoleUtils.assignUser(	 I_roleId => wk_roleId
								,I_userId => I_userId
								,O_rscode => O_rscode
							);
	END;

BEGIN

	-- 引数
	File_Name := 'utf8_EIMUSER.csv';
	File_Handle := UTL_FILE.FOPEN( CONST_FILE_PATH , File_Name , 'r' );
	Row_Count := 1;

	-- ファイルから配列に格納
	BEGIN
		LOOP
			UTL_FILE.GET_LINE( File_Handle , Read_Line );
			IF Row_Count = 1 THEN
				-- 1行目はヘッダー行
				--Head_Count := ANALYZEHEADER( Read_Line );
				Head_Count := CONST_HEADER_MAX_CULUMN;
				DBMS_OUTPUT.PUT_LINE( 'ヘッダー列数:' || Head_Count );
			ELSE
				DBMS_OUTPUT.PUT_LINE( '-------------------------------' );
				DBMS_OUTPUT.PUT_LINE( '[' || Read_Line || ']' );
				DBMS_OUTPUT.PUT_LINE( '-------------------------------' );
				Func_Flag := CSV2TAB( Read_Line , Head_Count );
				User_Count := User_Count + 1;
			END IF;
			Row_Count := Row_Count + 1;
		END LOOP;
	EXCEPTION
		WHEN NO_DATA_FOUND THEN
			NULL;
	END;

	-- ユーザー作成＆アサイン(グループ＆ロール)
	FOR I IN 0..User_Count-1 LOOP
		CREUSER(
		  tCODE		=> tabEIMUSER(I).tCODE
		 ,tjNAME	=> tabEIMUSER(I).tjNAME
		 ,teNAME	=> tabEIMUSER(I).teNAME
		 ,tKANA		=> tabEIMUSER(I).tKANA
		 ,tPASS		=> tabEIMUSER(I).tPASS
		 ,tMAIL		=> tabEIMUSER(I).tMAIL
		 ,tADMIN	=> tabEIMUSER(I).tADMIN
		 ,tDISABLE	=> tabEIMUSER(I).tDISABLE
		 ,tLANG	    => tabEIMUSER(I).tLANG
		 ,O_userId	=> tabEIMUSER(I).tID
		 ,O_rscode	=> O_rscode
		);
		GASSIGNUSER( I_userId => tabEIMUSER(I).tID , I_groupName => tabEIMUSER(I).tGROUP1 ,O_rscode => O_rscode );
		GASSIGNUSER( I_userId => tabEIMUSER(I).tID , I_groupName => tabEIMUSER(I).tGROUP2 ,O_rscode => O_rscode );
		GASSIGNUSER( I_userId => tabEIMUSER(I).tID , I_groupName => tabEIMUSER(I).tGROUP3 ,O_rscode => O_rscode );
		GASSIGNUSER( I_userId => tabEIMUSER(I).tID , I_groupName => tabEIMUSER(I).tGROUP4 ,O_rscode => O_rscode );
		GASSIGNUSER( I_userId => tabEIMUSER(I).tID , I_groupName => tabEIMUSER(I).tGROUP5 ,O_rscode => O_rscode );
		RASSIGNUSER( I_userId => tabEIMUSER(I).tID , I_roleName => tabEIMUSER(I).tROLE1 ,O_rscode => O_rscode );
		RASSIGNUSER( I_userId => tabEIMUSER(I).tID , I_roleName => tabEIMUSER(I).tROLE2 ,O_rscode => O_rscode );
		RASSIGNUSER( I_userId => tabEIMUSER(I).tID , I_roleName => tabEIMUSER(I).tROLE3 ,O_rscode => O_rscode );
		RASSIGNUSER( I_userId => tabEIMUSER(I).tID , I_roleName => tabEIMUSER(I).tROLE4 ,O_rscode => O_rscode );
		RASSIGNUSER( I_userId => tabEIMUSER(I).tID , I_roleName => tabEIMUSER(I).tROLE5 ,O_rscode => O_rscode );
	END LOOP;



END;
/

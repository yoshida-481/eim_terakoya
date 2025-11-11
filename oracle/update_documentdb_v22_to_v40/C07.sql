--
DECLARE
	--**********
	--定数
	--**********
	CONS_ACRTYPE_ALWAYSREAD	constant number := 500;
	
	--**********
	--変数
	--**********
	objTypeId_SystemSecurity	number;
	objTypeId_ObjectType		number;
	objName		varchar2(255);
	objTypeName			varchar2(255);
	objTypeOtherName	varchar2(255);
	objId		number;
	rscode		number;
	secId		number;
BEGIN
	
	select id into objTypeId_ObjectType from EIMOBJTYPE where name = 'オブジェクトタイプ';
	--DBMS_OUTPUT.PUT_LINE('オブジェクトタイプ=' || objTypeId_ObjectType);
	
	/*----------------------------------------------------
	--「常時読取」アクセス権限のOTHER情報のみを追加
	/*----------------------------------------------------*/
	insert into EIMACRTYPEOTHER values(CONS_ACRTYPE_ALWAYSREAD, 'JA', '常時読取');
	insert into EIMACRTYPEOTHER values(CONS_ACRTYPE_ALWAYSREAD, 'EN', 'Always read');
	
	/*----------------------------------------------------
	--systemユーザのEIMUSER.adminを1023に変更する
	/*----------------------------------------------------*/
	update EIMUser set ADMIN = '1023' where ID = '1';
	
	/*----------------------------------------------------
	--systemセキュリティオブジェクトタイプを作成する
	/*----------------------------------------------------*/
	objTypeName := 'systemセキュリティ';
	objTypeOtherName := 'SYSTEM Security';
	ObjectUtils.createObjectType(null, null, objTypeName, null, objTypeId_SystemSecurity, rscode);
	ObjectUtils.addOtherObjectTypeName(objTypeId_SystemSecurity, 'JA', objTypeName, rscode);
	ObjectUtils.addOtherObjectTypeName(objTypeId_SystemSecurity, 'EN', objTypeOtherName, rscode);
	objName := TO_CHAR(objTypeId_SystemSecurity);
	--systemセキュリティオブジェクトタイプのオブジェクト
	ObjectUtils.createObject(1, null, objTypeId_ObjectType, objName, 0, objId, rscode);
	
	/*----------------------------------------------------
	--systemセキュリティオブジェクトを作成する
	/*----------------------------------------------------*/
	objName := 'system';
	ObjectUtils.createObject(1, null, objTypeId_SystemSecurity, objName, 0, objId, rscode);
	--systemセキュリティのIDを取得する
	select id into secId from eimsec where name = 'system';
	--systemセキュリティオブジェクトにsystemセキュリティを適用する
	SecurityUtils.setSecurity(1, objId, secId, 0, rscode);
	
	/*----------------------------------------------------
	--全てのアクセス権限の無視を拒否に変更する(ステータス別セキュリティは除く)
	/*----------------------------------------------------*/
	update EIMACR set permit = 0 where permit = 2 and stsecid = 0;
	
	commit;
	
END;
/


set serveroutput on

declare 

	IS_NOT_MULTIPLE	constant	 number := 0;
	IS_MULTIPLE 	constant	 number := 1;
	
	rscode								number;
	found								number;
	
	objId								number;
	objName								varchar2(255);
	
	objTypeName							varchar2(255);
	objTypeOtherName					varchar2(255);
	objTypeId_DocumentTypeSecurity		number;
	objTypeId_Document					number;
	objTypeId_ObjectType				number;
	objTypeId_InsertURL					number;
	objTypeId_Workflow_Setting			number;
	objTypeId_UserObjectType			number;

	attTypeId							number;
	attTypeName							varchar2(255);
	attTypeOtherName					varchar2(255);
	
	attTypeId_WebDAV_lock_flag			number;
	attTypeId_Number					number;
	attTypeId_RevisionUpTakeover		number;
	attTypeId_LatestRevAssociation		number;
	attTypeId_Select_Custom_Table		number;
	attTypeId_Approve_Timing			number;
	attTypeId_PublicTo					number;
	attTypeId_Reply						number;
	attTypeId_InsertURLFlag				number;
	attTypeId_SearchIndex				number;
	attTypeId_DocumentAttachment		number;
	attTypeId_TemporaryAttachment		number;
	
	
	appException						exception;
	
begin
	
	/* ========================================== */
	/* 属性タイプを作成する               */
	/* ========================================== */
	attTypeName := 'WebDAVロックフラグ';
	attTypeOtherName := 'WebDAV lock flag';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, IS_NOT_MULTIPLE, attTypeId_WebDAV_lock_flag, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプを作成できませんでした。');
		raise appException;
	end if;
	AttributeUtils.addOtherAttributeTypeName(attTypeId_WebDAV_lock_flag, 'JA', attTypeName, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプの他言語[JA](' || attTypeName || ')を設定できませんでした。');
		raise appException;
	end if;
	AttributeUtils.addOtherAttributeTypeName(attTypeId_WebDAV_lock_flag, 'EN', attTypeOtherName, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプの他言語[EN](' || attTypeOtherName || ')を設定できませんでした。');
		raise appException;
	end if;
	
	attTypeName := '番号';
	attTypeOtherName := 'Number';
	AttributeUtils.createAttributeType(null, null, attTypeName, 2, IS_NOT_MULTIPLE, attTypeId_Number, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプを作成できませんでした。');
		raise appException;
	end if;
	AttributeUtils.addOtherAttributeTypeName(attTypeId_Number, 'JA', attTypeName, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプの他言語[JA](' || attTypeName || ')を設定できませんでした。');
		raise appException;
	end if;
	AttributeUtils.addOtherAttributeTypeName(attTypeId_Number, 'EN', attTypeOtherName, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプの他言語[EN](' || attTypeOtherName || ')を設定できませんでした。');
		raise appException;
	end if;
	
	attTypeName := 'リビジョンアップ引継ぎ';
	attTypeOtherName := 'Revision up takeover';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, IS_MULTIPLE, attTypeId_RevisionUpTakeover, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプを作成できませんでした。');
		raise appException;
	end if;
	AttributeUtils.addOtherAttributeTypeName(attTypeId_RevisionUpTakeover, 'JA', attTypeName, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプの他言語[JA](' || attTypeName || ')を設定できませんでした。');
		raise appException;
	end if;
	AttributeUtils.addOtherAttributeTypeName(attTypeId_RevisionUpTakeover, 'EN', attTypeOtherName, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプの他言語[EN](' || attTypeOtherName || ')を設定できませんでした。');
		raise appException;
	end if;
	
	attTypeName := '最新リビジョン関連付け';
	attTypeOtherName := 'Latest revision association';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, IS_MULTIPLE, attTypeId_LatestRevAssociation, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプを作成できませんでした。');
		raise appException;
	end if;
	AttributeUtils.addOtherAttributeTypeName(attTypeId_LatestRevAssociation, 'JA', attTypeName, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプの他言語[JA](' || attTypeName || ')を設定できませんでした。');
		raise appException;
	end if;
	AttributeUtils.addOtherAttributeTypeName(attTypeId_LatestRevAssociation, 'EN', attTypeOtherName, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプの他言語[EN](' || attTypeOtherName || ')を設定できませんでした。');
		raise appException;
	end if;
	
	attTypeName := '選択カスタマイズテーブル名称';
	attTypeOtherName := 'Select Custom Table Name';
	AttributeUtils.createAttributeType(null, null, attTypeName, 2, IS_NOT_MULTIPLE, attTypeId_Select_Custom_Table, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプを作成できませんでした。');
		raise appException;
	end if;
	AttributeUtils.addOtherAttributeTypeName(attTypeId_Select_Custom_Table, 'JA', attTypeName, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプの他言語[JA](' || attTypeName || ')を設定できませんでした。');
		raise appException;
	end if;
	AttributeUtils.addOtherAttributeTypeName(attTypeId_Select_Custom_Table, 'EN', attTypeOtherName, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプの他言語[EN](' || attTypeOtherName || ')を設定できませんでした。');
		raise appException;
	end if;
	
	attTypeName := 'ワークフロー公開処理URL挿入フラグ';
	attTypeOtherName := 'Workflow exhibition processing insert URL flag';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, IS_NOT_MULTIPLE, attTypeId_InsertURLFlag, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプを作成できませんでした。');
		raise appException;
	end if;
	AttributeUtils.addOtherAttributeTypeName(attTypeId_InsertURLFlag, 'JA', attTypeName, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプの他言語[JA](' || attTypeName || ')を設定できませんでした。');
		raise appException;
	end if;
	AttributeUtils.addOtherAttributeTypeName(attTypeId_InsertURLFlag, 'EN', attTypeOtherName, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプの他言語[EN](' || attTypeOtherName || ')を設定できませんでした。');
		raise appException;
	end if;
	
	attTypeName := '文書ID';
	attTypeOtherName := 'Document Search Index';
	AttributeUtils.createAttributeType(null, null, attTypeName, 2, IS_NOT_MULTIPLE, attTypeId_SearchIndex, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプを作成できませんでした。');
		raise appException;
	end if;
	AttributeUtils.addOtherAttributeTypeName(attTypeId_SearchIndex, 'JA', attTypeName, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプの他言語[JA](' || attTypeName || ')を設定できませんでした。');
		raise appException;
	end if;
	AttributeUtils.addOtherAttributeTypeName(attTypeId_SearchIndex, 'EN', attTypeOtherName, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプの他言語[EN](' || attTypeOtherName || ')を設定できませんでした。');
		raise appException;
	end if;
	
	attTypeName := '関連ドキュメント';
	attTypeOtherName := 'Related document';
	AttributeUtils.createAttributeType(null, null, attTypeName, 6, IS_MULTIPLE, attTypeId_DocumentAttachment, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプを作成できませんでした。');
		raise appException;
	end if;
	AttributeUtils.addOtherAttributeTypeName(attTypeId_DocumentAttachment, 'JA', attTypeName, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプの他言語[JA](' || attTypeName || ')を設定できませんでした。');
		raise appException;
	end if;
	AttributeUtils.addOtherAttributeTypeName(attTypeId_DocumentAttachment, 'EN', attTypeOtherName, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプの他言語[EN](' || attTypeOtherName || ')を設定できませんでした。');
		raise appException;
	end if;
	
	attTypeName := '添付ファイル';
	attTypeOtherName := 'Packaged document';
	AttributeUtils.createAttributeType(null, null, attTypeName, 6, IS_MULTIPLE, attTypeId_TemporaryAttachment, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプを作成できませんでした。');
		raise appException;
	end if;
	AttributeUtils.addOtherAttributeTypeName(attTypeId_TemporaryAttachment, 'JA', attTypeName, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプの他言語[JA](' || attTypeName || ')を設定できませんでした。');
		raise appException;
	end if;
	AttributeUtils.addOtherAttributeTypeName(attTypeId_TemporaryAttachment, 'EN', attTypeOtherName, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプの他言語[EN](' || attTypeOtherName || ')を設定できませんでした。');
		raise appException;
	end if;
	
	/* ========================================== */
	/* オブジェクトタイプを作成する               */
	/* ========================================== */
	select id into objTypeId_DocumentTypeSecurity from EIMOBJTYPE where name = 'オブジェクトタイプ';
	
	objTypeName := 'URL挿入';
	objTypeOtherName := 'Insert URL';
	ObjectUtils.createObjectType(null, null, objTypeName, null, objTypeId_InsertURL, rscode);
	ObjectUtils.addOtherObjectTypeName(objTypeId_InsertURL, 'JA', objTypeName, rscode);
	ObjectUtils.addOtherObjectTypeName(objTypeId_InsertURL, 'EN', objTypeOtherName, rscode);
	objName := TO_CHAR(objTypeId_InsertURL);
	ObjectUtils.createObject(1, null, objTypeId_DocumentTypeSecurity, objName, 0, objId, rscode);
	
	/* ========================================== */
	/* オブジェクトタイプに属性タイプを割り当てる */
	/* ========================================== */
	/* ============================ */
	/* Apply Object Type To Document*/
	/* ============================ */
	select id into objTypeId_Document from EIMOBJTYPE where name = 'ドキュメント';
	
	ObjectAttributeUtils.applyAttributeType(objTypeId_Document, attTypeId_WebDAV_lock_flag, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_Document, attTypeId_Number, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_Document, attTypeId_SearchIndex, rscode);
	
	/* ============================================= */
	/* Apply Object Type To Object Type Object       */
	/* ============================================= */
	select id into objTypeId_ObjectType from EIMOBJTYPE where name = 'オブジェクトタイプ';
	
	ObjectAttributeUtils.applyAttributeType(objTypeId_ObjectType, attTypeId_RevisionUpTakeover, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_ObjectType, attTypeId_LatestRevAssociation, rscode);

	/* ==================================== */
	/* Apply Object Type To User Type       */
	/* ==================================== */
	select id into objTypeId_UserObjectType from EIMOBJTYPE where name = 'ユーザ';
	ObjectAttributeUtils.applyAttributeType(objTypeId_UserObjectType, attTypeId_Select_Custom_Table, rscode);
	
	/* ======================================== */
	/* Apply Object Type To Workflow Exhibition */
	/* ======================================== */
	select id into objTypeId_Workflow_Setting from EIMOBJTYPE where name = 'ワークフロー公開処理';
	ObjectAttributeUtils.applyAttributeType(objTypeId_Workflow_Setting, attTypeId_InsertURLFlag, rscode);
	
	/* ========================================== */
	/* 属性タイプを取得する               */
	/* ========================================== */
	attTypeName := '承認依頼通知タイミング';
	select ID into attTypeId_Approve_Timing from eimattr where NAME = attTypeName;
	
	attTypeName := '公開通知送信先';
	select ID into attTypeId_PublicTo from eimattr where NAME = attTypeName;
	
	attTypeName := '受信確認';
	select ID into attTypeId_Reply from eimattr where NAME = attTypeName;
	
	/* ============================================================= */
	/* 既存の承認依頼イベントタイプに属性タイプを割り当てる */
	/* ============================================================= */
	FOR vRec IN (
		select evtid from EIMEVTYPE where bevtid = -14001
	) LOOP
		-- 承認依頼通知タイミング
		EventAttributeUtils.applyAttributeType(vRec.evtid, attTypeId_Approve_Timing, rscode);
		-- 公開通知送信先
		EventAttributeUtils.applyAttributeType(vRec.evtid, attTypeId_PublicTo, rscode);
		-- 受信確認
		EventAttributeUtils.applyAttributeType(vRec.evtid, attTypeId_Reply, rscode);
		
	END LOOP;
	
	/* ============================================================= */
	/* 既存の承認イベントタイプに属性タイプを割り当てる */
	/* ============================================================= */
	FOR vRec2 IN (
		select evtid from EIMEVTYPE where bevtid = -14002
	) LOOP
		-- 承認依頼通知タイミング
		EventAttributeUtils.applyAttributeType(vRec2.evtid, attTypeId_Approve_Timing, rscode);
		
	END LOOP;
	
	commit;
	
exception
	when appException then
		dbms_output.put_line('rscode=[ ' || rscode || ' ]');
		rollback;
	when OTHERS then
		dbms_output.put_line('エラー発生: ' || sqlerrm(sqlcode));
		rollback;

end;
/

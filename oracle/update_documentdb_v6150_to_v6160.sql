set serveroutput on;

declare 
	IS_NOT_MULTIPLE	constant	 number := 0;
	IS_MULTIPLE 	constant	 number := 1;
	
	rscode								number;
	objId								number;
	objName								varchar2(255);
	objTypeId_Document					number;
	objTypeId_Folder					number;
	objTypeId_WorkflowExhibition		number;
	objTypeName							varchar2(255);
	objTypeOtherName					varchar2(255);
	secName								varchar2(255);
	secId								number;
	relId								number;
	relTypeName							varchar2(255);
	relTypeOtherName					varchar2(255);
	entryId								number;
	priority							number;
	
	objTypeId_WorkspaceRecycle			number;
	secId_WorkspaceRecycle				number;
	relTypeId_Recycle					number;
	attTypeId_Path						number;
	attTypeId_PublicProcFailure			number;
	workspace_Name						varchar2(255);
	workspaceRecycle_Path				varchar2(255);
	
	objTypeId_publicNotificationTemplate					number;
	attTypeId_SkipStatus									number;
	attTypeId_ApproveNameLang								number;
	attTypeId_SignJobName									number;
	attTypeId_PublicNotificationTemplateName				number;
	attTypeId_PublicNotificationTemplateUserId				number;
	attTypeId_PublicNotificationTemplateGroupId				number;
	attTypeId_PublicNotificationTemplateRoleId				number;
	attTypeId_PublicNotificationTemplateCompositeGroupId	number;
	
	attTypeName								varchar2(255);
	attTypeOtherName						varchar2(255);
	attTypeId_publicPDFPreRegistDate		number;
	
	cursor workspaceObjectCursor is 
		select id 
		from EIMOBJ 
		where type 
		in 
		(
			select id 
			from EIMOBJTYPE 
			where name = 'ワークスペース'
		);
	workspaceRecord  workspaceObjectCursor%ROWTYPE;
	
	type ref_cursor	is ref cursor;
	resultCursor						ref_cursor;
	
	appException						exception;
	

begin
	/* ====================== */
	/* ワークスペース固有ごみ箱セキュリティを作成する */
	/* ====================== */

	secName := 'ワークスペース固有ごみ箱セキュリティ';
	SecurityUtils.createSecurity(secName, secId_WorkspaceRecycle, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || secName || '」を作成できませんでした。');
		raise appException;
	end if;
	SecurityUtils.createAccessEntry(secId_WorkspaceRecycle, 5, -12000, entryId, priority, rscode);
	SecurityUtils.updateAccessRole(entryId, 11, 0, 1, rscode);
	SecurityUtils.updateAccessRole(entryId, 12, 0, 1, rscode);
	SecurityUtils.updateAccessRole(entryId, 13, 0, 1, rscode);
	SecurityUtils.updateAccessRole(entryId, 14, 0, 1, rscode);
	SecurityUtils.updateAccessRole(entryId, 15, 0, 1, rscode);
	SecurityUtils.updateAccessRole(entryId, 21, 0, 1, rscode);
	SecurityUtils.updateAccessRole(entryId, 22, 0, 1, rscode);
	SecurityUtils.updateAccessRole(entryId, 31, 0, 1, rscode);
	SecurityUtils.updateAccessRole(entryId, 32, 0, 1, rscode);
	SecurityUtils.updateAccessRole(entryId, 41, 0, 1, rscode);
	SecurityUtils.updateAccessRole(entryId, 42, 0, 1, rscode);
	SecurityUtils.updateAccessRole(entryId, 51, 0, 1, rscode);
	SecurityUtils.updateAccessRole(entryId, 61, 0, 1, rscode);
	SecurityUtils.updateAccessRole(entryId, 62, 0, 1, rscode);
	SecurityUtils.updateAccessRole(entryId, 63, 0, 1, rscode);
	SecurityUtils.updateAccessRole(entryId, 101, 0, 1, rscode);
	SecurityUtils.updateAccessRole(entryId, 500, 0, 1, rscode);
	/* 言語＝日本語 */
	insert into EIMSECOTHER values(secId_WorkspaceRecycle, 'JA', 'ワークスペース固有ごみ箱セキュリティ');
	/* 言語＝英語 */
	insert into EIMSECOTHER values(secId_WorkspaceRecycle, 'EN', 'Workspace Garbagebox Security');
	
	/* ====================== */
	/* ワークスペース固有ごみ箱配下セキュリティを作成する */
	/* ====================== */

	secName := 'ワークスペース固有ごみ箱配下セキュリティ';
	SecurityUtils.createSecurity(secName, secId, rscode);
		if rscode <> 0 then
		dbms_output.put_line('「' || secName || '」を作成できませんでした。');
		raise appException;
	end if;
	SecurityUtils.createAccessEntry(secId, 5, -12001, entryId, priority, rscode);
	SecurityUtils.updateAccessRole(entryId, 11, 0, 1, rscode);
	SecurityUtils.updateAccessRole(entryId, 12, 0, 1, rscode);
	SecurityUtils.updateAccessRole(entryId, 13, 0, 1, rscode);
	SecurityUtils.updateAccessRole(entryId, 14, 0, 1, rscode);
	SecurityUtils.updateAccessRole(entryId, 15, 0, 1, rscode);
	SecurityUtils.updateAccessRole(entryId, 21, 0, 1, rscode);
	SecurityUtils.updateAccessRole(entryId, 22, 0, 1, rscode);
	SecurityUtils.updateAccessRole(entryId, 31, 0, 1, rscode);
	SecurityUtils.updateAccessRole(entryId, 32, 0, 1, rscode);
	SecurityUtils.updateAccessRole(entryId, 41, 0, 1, rscode);
	SecurityUtils.updateAccessRole(entryId, 42, 0, 1, rscode);
	SecurityUtils.updateAccessRole(entryId, 51, 0, 1, rscode);
	SecurityUtils.updateAccessRole(entryId, 61, 0, 1, rscode);
	SecurityUtils.updateAccessRole(entryId, 62, 0, 1, rscode);
	SecurityUtils.updateAccessRole(entryId, 63, 0, 1, rscode);
	SecurityUtils.updateAccessRole(entryId, 101, 0, 1, rscode);
	SecurityUtils.updateAccessRole(entryId, 500, 0, 1, rscode);
	/* 言語＝日本語 */
	insert into EIMSECOTHER values(secId, 'JA', 'ワークスペース固有ごみ箱配下セキュリティ');
	/* 言語＝英語 */
	insert into EIMSECOTHER values(secId, 'EN', 'Workspace Garbagebox Contents Security');
	
	/* ========================================== */
	/* オブジェクトタイプを作成する               */
	/* ========================================== */
	
	objTypeName := 'ワークスペース固有ごみ箱';
	objTypeOtherName := 'Workspace Garbage box';
	ObjectUtils.createObjectType(null, null, objTypeName, null, objTypeId_WorkspaceRecycle, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || objTypeName || '」オブジェクトタイプを作成できませんでした。');
		raise appException;
	end if;
	ObjectUtils.addOtherObjectTypeName(objTypeId_WorkspaceRecycle, 'JA', objTypeName, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || objTypeName || '」オブジェクトタイプの他言語[JA](' || objTypeName || ')を設定できませんでした。');
		raise appException;
	end if;
	ObjectUtils.addOtherObjectTypeName(objTypeId_WorkspaceRecycle, 'EN', objTypeOtherName, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || objTypeName || '」オブジェクトタイプの他言語[EN](' || objTypeOtherName || ')を設定できませんでした。');
		raise appException;
	end if;
	
	/* ========================================== */
	/* オブジェクトタイプに属性タイプを割り当てる */
	/* ========================================== */
	/* ========================================= */
	/* Apply Object Type To WorkSpace Garbagebox */
	/* ========================================= */
	select id into objTypeId_WorkspaceRecycle from EIMOBJTYPE where name = 'ワークスペース固有ごみ箱';
	select id into attTypeId_Path from EIMATTR where name = 'パス';
	ObjectAttributeUtils.applyAttributeType(objTypeId_WorkspaceRecycle, attTypeId_Path, rscode);
	
	/* ========================================== */
	/* リレーションタイプを作成する               */
	/* ========================================== */
	
	relTypeName := 'ごみ箱';
	relTypeOtherName := 'Garbage box';
	RelationUtils.createRelationType(null, null, relTypeName, 3, 0, relTypeId_Recycle, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || relTypeName || '」リレーションタイプを作成できませんでした。');
		raise appException;
	end if;
	RelationUtils.addOtherRelationTypeName(relTypeId_Recycle, 'JA', relTypeName, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || relTypeName || '」リレーションタイプの他言語[JA](' || relTypeName || ')を設定できませんでした。');
		raise appException;
	end if;
	RelationUtils.addOtherRelationTypeName(relTypeId_Recycle, 'EN', relTypeOtherName, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || relTypeName || '」リレーションタイプの他言語[EN](' || relTypeOtherName || ')を設定できませんでした。');
		raise appException;
	end if;
	
	
	/* ==================================================================== */
	/* 既存のワークスペースにワークスペース固有ごみ箱オブジェクトを作成する */
	/* ==================================================================== */
	objName := 'ごみ箱';
	open workspaceObjectCursor;
		loop
			fetch workspaceObjectCursor into workspaceRecord;
				exit when workspaceObjectCursor%notfound;
					ObjectUtils.createObject(1, null, objTypeId_WorkspaceRecycle, objName, 0, objId, rscode);
					SecurityUtils.setSecurity(1, objId, secId_WorkspaceRecycle, 0, rscode, resultCursor);
					RelationUtils.createRelation(1, null, relTypeId_Recycle, workspaceRecord.id, objId, 0, relId, rscode);
					select name into workspace_Name from EIMOBJ where id = workspaceRecord.id;
					workspaceRecycle_Path := '/'||  workspace_Name || '/';
					ObjectAttributeUtils.setAttributeString(1, objId, attTypeId_Path, 1, workspaceRecycle_Path, 0, rscode);
		end loop;
	close workspaceObjectCursor;

	/* ========================================== */
	/* オブジェクトタイプを作成する               */
	/* ========================================== */

	objTypeName := '公開通知テンプレート';
	objTypeOtherName := 'Public notification template';
	ObjectUtils.createObjectType(null, null, objTypeName, null, objTypeId_publicNotificationTemplate, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || objTypeName || '」オブジェクトタイプを作成できませんでした。');
		raise appException;
	end if;
	ObjectUtils.addOtherObjectTypeName(objTypeId_publicNotificationTemplate, 'JA', objTypeName, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || objTypeName || '」オブジェクトタイプの他言語[JA](' || objTypeName || ')を設定できませんでした。');
		raise appException;
	end if;
	ObjectUtils.addOtherObjectTypeName(objTypeId_publicNotificationTemplate, 'EN', objTypeOtherName, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || objTypeName || '」オブジェクトタイプの他言語[EN](' || objTypeOtherName || ')を設定できませんでした。');
		raise appException;
	end if;

	/* ========================================== */
	/* 属性タイプを作成する               */
	/* ========================================== */
	
	attTypeName := '公開PDF事前登録日時';
	attTypeOtherName := 'Public PDF pre-regist date';
	AttributeUtils.createAttributeType(null, null, attTypeName, 3, IS_NOT_MULTIPLE, attTypeId_publicPDFPreRegistDate, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプを作成できませんでした。');
		raise appException;
	end if;
	AttributeUtils.addOtherAttributeTypeName(attTypeId_publicPDFPreRegistDate, 'JA', attTypeName, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプの他言語[JA](' || attTypeName || ')を設定できませんでした。');
		raise appException;
	end if;
	AttributeUtils.addOtherAttributeTypeName(attTypeId_publicPDFPreRegistDate, 'EN', attTypeOtherName, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプの他言語[EN](' || attTypeOtherName || ')を設定できませんでした。');
		raise appException;
	end if;
	
	attTypeName := 'スキップステータスタイプID';
	attTypeOtherName := 'Skip StatusType ID';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, IS_MULTIPLE, attTypeId_SkipStatus, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプを作成できませんでした。');
		raise appException;
	end if;
	AttributeUtils.addOtherAttributeTypeName(attTypeId_SkipStatus, 'JA', attTypeName, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプの他言語[JA](' || attTypeName || ')を設定できませんでした。');
		raise appException;
	end if;
	AttributeUtils.addOtherAttributeTypeName(attTypeId_SkipStatus, 'EN', attTypeOtherName, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプの他言語[EN](' || attTypeOtherName || ')を設定できませんでした。');
		raise appException;
	end if;
	
	attTypeName := '電子署名用言語';
	attTypeOtherName := 'Approve name lang';
	AttributeUtils.createAttributeType(null, null, attTypeName, 2, IS_NOT_MULTIPLE, attTypeId_ApproveNameLang, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプを作成できませんでした。');
		raise appException;
	end if;
	AttributeUtils.addOtherAttributeTypeName(attTypeId_ApproveNameLang, 'JA', attTypeName, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプの他言語[JA](' || attTypeName || ')を設定できませんでした。');
		raise appException;
	end if;
	AttributeUtils.addOtherAttributeTypeName(attTypeId_ApproveNameLang, 'EN', attTypeOtherName, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプの他言語[EN](' || attTypeOtherName || ')を設定できませんでした。');
		raise appException;
	end if;
	attTypeName := '署名用ジョブ名';
	attTypeOtherName := 'Sign job name';
	AttributeUtils.createAttributeType(null, null, attTypeName, 2, IS_NOT_MULTIPLE, attTypeId_SignJobName, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプを作成できませんでした。');
		raise appException;
	end if;
	AttributeUtils.addOtherAttributeTypeName(attTypeId_SignJobName, 'JA', attTypeName, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプの他言語[JA](' || attTypeName || ')を設定できませんでした。');
		raise appException;
	end if;
	AttributeUtils.addOtherAttributeTypeName(attTypeId_SignJobName, 'EN', attTypeOtherName, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプの他言語[EN](' || attTypeOtherName || ')を設定できませんでした。');
		raise appException;
	end if;

	attTypeName := '公開通知テンプレート名称';
	attTypeOtherName := 'Public notification template name';
	AttributeUtils.createAttributeType(null, null, attTypeName, 2, IS_NOT_MULTIPLE, attTypeId_PublicNotificationTemplateName, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプを作成できませんでした。');
		raise appException;
	end if;
	AttributeUtils.addOtherAttributeTypeName(attTypeId_PublicNotificationTemplateName, 'JA', attTypeName, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプの他言語[JA](' || attTypeName || ')を設定できませんでした。');
		raise appException;
	end if;
	AttributeUtils.addOtherAttributeTypeName(attTypeId_PublicNotificationTemplateName, 'EN', attTypeOtherName, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプの他言語[EN](' || attTypeOtherName || ')を設定できませんでした。');
		raise appException;
	end if;
	attTypeName := '公開通知テンプレートユーザID';
	attTypeOtherName := 'Public notification template user ID';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, IS_MULTIPLE, attTypeId_PublicNotificationTemplateUserId, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプを作成できませんでした。');
		raise appException;
	end if;
	AttributeUtils.addOtherAttributeTypeName(attTypeId_PublicNotificationTemplateUserId, 'JA', attTypeName, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプの他言語[JA](' || attTypeName || ')を設定できませんでした。');
		raise appException;
	end if;
	AttributeUtils.addOtherAttributeTypeName(attTypeId_PublicNotificationTemplateUserId, 'EN', attTypeOtherName, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプの他言語[EN](' || attTypeOtherName || ')を設定できませんでした。');
		raise appException;
	end if;
	attTypeName := '公開通知テンプレートグループID';
	attTypeOtherName := 'Public notification template group ID';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, IS_MULTIPLE, attTypeId_PublicNotificationTemplateGroupId, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプを作成できませんでした。');
		raise appException;
	end if;
	AttributeUtils.addOtherAttributeTypeName(attTypeId_PublicNotificationTemplateGroupId, 'JA', attTypeName, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプの他言語[JA](' || attTypeName || ')を設定できませんでした。');
		raise appException;
	end if;
	AttributeUtils.addOtherAttributeTypeName(attTypeId_PublicNotificationTemplateGroupId, 'EN', attTypeOtherName, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプの他言語[EN](' || attTypeOtherName || ')を設定できませんでした。');
		raise appException;
	end if;
	attTypeName := '公開通知テンプレートロールID';
	attTypeOtherName := 'Public notification template role ID';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, IS_MULTIPLE, attTypeId_PublicNotificationTemplateRoleId, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプを作成できませんでした。');
		raise appException;
	end if;
	AttributeUtils.addOtherAttributeTypeName(attTypeId_PublicNotificationTemplateRoleId, 'JA', attTypeName, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプの他言語[JA](' || attTypeName || ')を設定できませんでした。');
		raise appException;
	end if;
	AttributeUtils.addOtherAttributeTypeName(attTypeId_PublicNotificationTemplateRoleId, 'EN', attTypeOtherName, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプの他言語[EN](' || attTypeOtherName || ')を設定できませんでした。');
		raise appException;
	end if;
	attTypeName := '公開通知テンプレート複合グループID';
	attTypeOtherName := 'Public notification template composite group ID';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, IS_MULTIPLE, attTypeId_PublicNotificationTemplateCompositeGroupId, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプを作成できませんでした。');
		raise appException;
	end if;
	AttributeUtils.addOtherAttributeTypeName(attTypeId_PublicNotificationTemplateCompositeGroupId, 'JA', attTypeName, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプの他言語[JA](' || attTypeName || ')を設定できませんでした。');
		raise appException;
	end if;
	AttributeUtils.addOtherAttributeTypeName(attTypeId_PublicNotificationTemplateCompositeGroupId, 'EN', attTypeOtherName, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプの他言語[EN](' || attTypeOtherName || ')を設定できませんでした。');
		raise appException;
	end if;

	/* ========================================== */
	/* 属性タイプを割り当てる */
	/* ========================================== */
	select id into objTypeId_Document from EIMOBJTYPE where name = 'ドキュメント';
	ObjectAttributeUtils.applyAttributeType(objTypeId_Document, attTypeId_publicPDFPreRegistDate, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_Document, attTypeId_SkipStatus, rscode);

	select id into objTypeId_Folder from EIMOBJTYPE where name = 'フォルダ';
	select id into attTypeId_PublicProcFailure from EIMATTR where name = '公開処理失敗';
	ObjectAttributeUtils.applyAttributeType(objTypeId_Folder, attTypeId_PublicProcFailure, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_Folder, attTypeId_SkipStatus, rscode);

	select id into objTypeId_WorkflowExhibition from EIMOBJTYPE where name = 'ワークフロー公開処理';
	ObjectAttributeUtils.applyAttributeType(objTypeId_WorkflowExhibition, attTypeId_ApproveNameLang, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_WorkflowExhibition, attTypeId_SignJobName, rscode);

	select id into objTypeId_publicNotificationTemplate from EIMOBJTYPE where name = '公開通知テンプレート';
	ObjectAttributeUtils.applyAttributeType(objTypeId_publicNotificationTemplate, attTypeId_PublicNotificationTemplateName, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_publicNotificationTemplate, attTypeId_PublicNotificationTemplateUserId, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_publicNotificationTemplate, attTypeId_PublicNotificationTemplateGroupId, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_publicNotificationTemplate, attTypeId_PublicNotificationTemplateRoleId, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_publicNotificationTemplate, attTypeId_PublicNotificationTemplateCompositeGroupId, rscode);

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

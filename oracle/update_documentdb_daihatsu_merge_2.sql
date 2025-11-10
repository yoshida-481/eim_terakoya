set serveroutput on

declare

	--**********
	-- 定数
	--**********
	IS_NOT_MULTIPLE	constant	 number := 0;
	IS_MULTIPLE 	constant	 number := 1;

	--**********
	-- 変数
	--**********
	rscode								number;
	attTypeName							varchar2(255);
	attTypeOtherName					varchar2(255);
	objTypeName							varchar2(255);
	objTypeOtherName					varchar2(255);

	-- ワークフロー公開処理
	objTypeId_WorkflowExhibition			number;
	--- 電子署名用言語
	attTypeId_ApproveNameLang				number;
	--- 署名用ジョブ名
	attTypeId_SignJobName					number;

	-- 公開通知テンプレート
	objTypeId_publicNotificationTemplate	number;
	-- 公開通知テンプレート名称
	attTypeId_PublicNotificationTemplateName	number;
	-- 公開通知テンプレートユーザID
	attTypeId_PublicNotificationTemplateUserId	number;
	-- 公開通知テンプレートグループID
	attTypeId_PublicNotificationTemplateGroupId	number;
	-- 公開通知テンプレートロールID
	attTypeId_PublicNotificationTemplateRoleId	number;
	-- 公開通知テンプレート複合グループID
	attTypeId_PublicNotificationTemplateCompositeGroupId	number;

	appException						exception;

begin

	/* ==================== */
	/* 属性タイプを作成する */
	/* ==================== */

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
	/* オブジェクトタイプを作成する               */
	/* ========================================== */

	objTypeName := '公開通知テンプレート';
	objTypeOtherName := 'Public notification template';
	ObjectUtils.createObjectType(null, null, objTypeName, null, objTypeId_publicNotificationTemplate, rscode);
	ObjectUtils.addOtherObjectTypeName(objTypeId_publicNotificationTemplate, 'JA', objTypeName, rscode);
	ObjectUtils.addOtherObjectTypeName(objTypeId_publicNotificationTemplate, 'EN', objTypeOtherName, rscode);

	/* ========================================== */
	/* オブジェクトタイプに属性タイプを割り当てる */
	/* ========================================== */

	select id into objTypeId_WorkflowExhibition from EIMOBJTYPE where name = 'ワークフロー公開処理';
	ObjectAttributeUtils.applyAttributeType(objTypeId_WorkflowExhibition, attTypeId_ApproveNameLang, rscode);
	if rscode <> 0 then
		dbms_output.put_line('ワークフロー公開処理オブジェクトタイプに[電子署名用言語]属性タイプを付与できませんでした。');
		raise appException;
	end if;

	ObjectAttributeUtils.applyAttributeType(objTypeId_WorkflowExhibition, attTypeId_SignJobName, rscode);
	if rscode <> 0 then
		dbms_output.put_line('ワークフロー公開処理オブジェクトタイプに[署名用ジョブ名]属性タイプを付与できませんでした。');
		raise appException;
	end if;

	ObjectAttributeUtils.applyAttributeType(objTypeId_publicNotificationTemplate, attTypeId_PublicNotificationTemplateName, rscode);
	if rscode <> 0 then
		dbms_output.put_line('公開通知テンプレートオブジェクトタイプに[公開通知テンプレート名称]属性タイプを付与できませんでした。');
		raise appException;
	end if;

	ObjectAttributeUtils.applyAttributeType(objTypeId_publicNotificationTemplate, attTypeId_PublicNotificationTemplateUserId, rscode);
	if rscode <> 0 then
		dbms_output.put_line('公開通知テンプレートオブジェクトタイプに[公開通知テンプレートユーザID]属性タイプを付与できませんでした。');
		raise appException;
	end if;

	ObjectAttributeUtils.applyAttributeType(objTypeId_publicNotificationTemplate, attTypeId_PublicNotificationTemplateGroupId, rscode);
	if rscode <> 0 then
		dbms_output.put_line('公開通知テンプレートオブジェクトタイプに[公開通知テンプレートグループID]属性タイプを付与できませんでした。');
		raise appException;
	end if;

	ObjectAttributeUtils.applyAttributeType(objTypeId_publicNotificationTemplate, attTypeId_PublicNotificationTemplateRoleId, rscode);
	if rscode <> 0 then
		dbms_output.put_line('公開通知テンプレートオブジェクトタイプに[公開通知テンプレートロールID]属性タイプを付与できませんでした。');
		raise appException;
	end if;

	ObjectAttributeUtils.applyAttributeType(objTypeId_publicNotificationTemplate, attTypeId_PublicNotificationTemplateCompositeGroupId, rscode);
	if rscode <> 0 then
		dbms_output.put_line('公開通知テンプレートオブジェクトタイプに[公開通知テンプレート複合グループID]属性タイプを付与できませんでした。');
		raise appException;
	end if;

	commit;
	--rollback;
exception
	when appException then
		dbms_output.put_line('rscode=[ ' || rscode || ' ]');
		rollback;
	when OTHERS then
		dbms_output.put_line('エラー発生: ' || sqlerrm(sqlcode));
		rollback;

end;
/

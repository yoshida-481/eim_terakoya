set serveroutput on

declare 

	IS_NOT_MULTIPLE	constant	 number := 0;
	IS_MULTIPLE 	constant	 number := 1;
	SYSTEM_USERID	constant	 number := 1;
	AUTH_WORKSPACE	constant number := 4096;
	AUTH_SECURITY	constant number := 64;

	rscode								number;
	objTypeName							varchar2(255);
	attTypeName							varchar2(255);
	attTypeOtherName					varchar2(255);

	objTypeId_workspace					number;
	attTypeId_LimitDocTypeFlag			number;
	attTypeId_SelectableDocType			number;
	attTypeId_LimitFolderTypeFlag		number;
	attTypeId_SelectableFolderType		number;
	attTypeId_LimitTagTypeFlag			number;
	attTypeId_SelectableTagType			number;
	attTypeId_LimitSecurityFlag			number;
	attTypeId_SelectableSecurity		number;
	attTypeId_Administrator				number;
	attTypeId_AdministratorType			number;
	
	ret_security						number;
	ret_workspace						number;
	rscode_workspace					number;



begin
	-- Create Attribute Type
	
	attTypeName := '使用可能ドキュメントタイプ絞込みフラグ';
	attTypeOtherName := 'Limit document type flag';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, IS_NOT_MULTIPLE, attTypeId_LimitDocTypeFlag, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_LimitDocTypeFlag, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_LimitDocTypeFlag, 'EN', attTypeOtherName, rscode);
	
	attTypeName := '使用可能ドキュメントタイプ';
	attTypeOtherName := 'Selectable document type';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, IS_MULTIPLE, attTypeId_SelectableDocType, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_SelectableDocType, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_SelectableDocType, 'EN', attTypeOtherName, rscode);

	attTypeName := '使用可能フォルダタイプ絞込みフラグ';
	attTypeOtherName := 'Limit folder type flag';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, IS_NOT_MULTIPLE, attTypeId_LimitFolderTypeFlag, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_LimitFolderTypeFlag, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_LimitFolderTypeFlag, 'EN', attTypeOtherName, rscode);

	attTypeName := '使用可能フォルダタイプ';
	attTypeOtherName := 'Selectable folder type';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, IS_MULTIPLE, attTypeId_SelectableFolderType, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_SelectableFolderType, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_SelectableFolderType, 'EN', attTypeOtherName, rscode);

	attTypeName := '使用可能タグタイプ絞込みフラグ';
	attTypeOtherName := 'Limit tag type flag';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, IS_NOT_MULTIPLE, attTypeId_LimitTagTypeFlag, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_LimitTagTypeFlag, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_LimitTagTypeFlag, 'EN', attTypeOtherName, rscode);

	attTypeName := '使用可能タグタイプ';
	attTypeOtherName := 'Selectable tag type';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, IS_MULTIPLE, attTypeId_SelectableTagType, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_SelectableTagType, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_SelectableTagType, 'EN', attTypeOtherName, rscode);

	attTypeName := '使用可能セキュリティ絞込みフラグ';
	attTypeOtherName := 'Limit Security flag';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, IS_NOT_MULTIPLE, attTypeId_LimitSecurityFlag, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_LimitSecurityFlag, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_LimitSecurityFlag, 'EN', attTypeOtherName, rscode);

	attTypeName := '使用可能セキュリティ';
	attTypeOtherName := 'Selectable security';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, IS_MULTIPLE, attTypeId_SelectableSecurity, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_SelectableSecurity, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_SelectableSecurity, 'EN', attTypeOtherName, rscode);

	attTypeName := '責任者';
	attTypeOtherName := 'Administrator';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, IS_MULTIPLE, attTypeId_Administrator, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_Administrator, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_Administrator, 'EN', attTypeOtherName, rscode);

	attTypeName := '責任者種別';
	attTypeOtherName := 'Administrator type';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, IS_MULTIPLE, attTypeId_AdministratorType, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_AdministratorType, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_AdministratorType, 'EN', attTypeOtherName, rscode);

	-- Get Object Type Id 'Workspace'
	objTypeName := 'ワークスペース';
	select id into objTypeId_workspace from EIMOBJTYPE where name = objTypeName;

	-- Apply Attribute Type to 'Workspace'
	ObjectAttributeUtils.applyAttributeType(objTypeId_workspace, attTypeId_LimitDocTypeFlag, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_workspace, attTypeId_SelectableDocType, rscode);

	ObjectAttributeUtils.applyAttributeType(objTypeId_workspace, attTypeId_LimitFolderTypeFlag, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_workspace, attTypeId_SelectableFolderType, rscode);

	ObjectAttributeUtils.applyAttributeType(objTypeId_workspace, attTypeId_LimitTagTypeFlag, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_workspace, attTypeId_SelectableTagType, rscode);

	ObjectAttributeUtils.applyAttributeType(objTypeId_workspace, attTypeId_LimitSecurityFlag, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_workspace, attTypeId_SelectableSecurity, rscode);

	ObjectAttributeUtils.applyAttributeType(objTypeId_workspace, attTypeId_Administrator, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_workspace, attTypeId_AdministratorType, rscode);
	
	--add workspaceauth
	for vRec in(
		select * from eimuser
	) loop
		select bitand(vRec.admin, AUTH_SECURITY)	into ret_security	from dual;
		select bitand(vRec.admin, AUTH_WORKSPACE)	into ret_workspace	from dual;
		if (ret_workspace		<> 0) then
			-- 既にワークスペース管理権限があるユーザには追加しない
			DBMS_OUTPUT.PUT(vRec.name);
			DBMS_OUTPUT.PUT_LINE(' は既にワークスペース管理権限を保持しています。');
		elsif (ret_security	<> 0) then
			DBMS_OUTPUT.PUT(vRec.name);
			DBMS_OUTPUT.PUT_LINE(' にワークスペース管理権限を付与します。');
			-- セキュリティ管理権限を保持しているユーザにワークスペース作成権限を付与する
			UserUtils.updateUser(vRec.id,
								vRec.code,
								vRec.name,
								vRec.kana,
								null,
								vRec.mail,
								vRec.admin + AUTH_WORKSPACE,
								vRec.disable,
								vRec.lang,rscode_workspace);
		end if;
	end loop;

	commit;

exception
	when OTHERS then
		dbms_output.put_line('エラー発生: ' || sqlerrm(sqlcode));

end;
/

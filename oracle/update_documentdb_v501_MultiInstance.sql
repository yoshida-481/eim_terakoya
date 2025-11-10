set serveroutput on size 100000

declare
	
	IS_NOT_MULTIPLE	constant number := 0;
	IS_MULTIPLE 	constant number := 1;
	SYSTEM_USERID	constant number := 1;
	
	rscode				number;
	objTypeName			varchar2(255);
	attTypeId			number;
	attTypeName			varchar2(255);
	attTypeOtherName	varchar2(255);
	
	objTypeId_Document		number;
	objTypeId_Folder		number;
	objTypeId_Tag			number;
	attTypeId_DocWS			number;
	attTypeId_LinkWS		number;

begin
	-- Create Attribute Type
	attTypeName := '所属ワークスペース';
	attTypeOtherName := 'Belonging workspace';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, IS_NOT_MULTIPLE, attTypeId_DocWS, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_DocWS, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_DocWS, 'EN', attTypeOtherName, rscode);
	
	attTypeName := 'リンク所属ワークスペース';
	attTypeOtherName := 'Belonging workspace of link';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, IS_MULTIPLE, attTypeId_LinkWS, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_LinkWS, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_LinkWS, 'EN', attTypeOtherName, rscode);
	
	
	-- Get object type Id 'Document' 'Folder' 'Tag'
	objTypeName := 'ドキュメント';
	select id into objTypeId_Document from EIMOBJTYPE where name = objTypeName;
	objTypeName := 'フォルダ';
	select id into objTypeId_Folder from EIMOBJTYPE where name = objTypeName;
	objTypeName := 'タグ';
	select id into objTypeId_Tag from EIMOBJTYPE where name = objTypeName;
	
	
	-- Apply Attribute Type to 'Document' 'Folder' 'Tag'
	ObjectAttributeUtils.applyAttributeType(objTypeId_Document, attTypeId_DocWS, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_Document, attTypeId_LinkWS, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_Folder, attTypeId_DocWS, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_Tag, attTypeId_DocWS, rscode);
	
	commit;

exception
	when OTHERS then
		dbms_output.put_line('エラー発生: ' || sqlerrm(sqlcode));

end;
/

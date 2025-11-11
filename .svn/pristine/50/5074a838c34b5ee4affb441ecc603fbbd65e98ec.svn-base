/***********************************************/
/*                                             */
/*   Create Document Database Resources Script */
/*                                             */
/***********************************************/

set serveroutput on;

declare 
	
	
	rscode				number;
	objId				number;
	objName				varchar2(255);
	objOtherName		varchar2(255);
	objTypeId			number;
	objTypeName			varchar2(255);
	objTypeOtherName	varchar2(255);
	relTypeId			number;
	relTypeName			varchar2(255);
	relTypeOtherName	varchar2(255);
	attTypeId			number;
	attTypeName			varchar2(255);
	attTypeOtherName	varchar2(255);
	secId				number;
	secName				varchar(255);
	secOtherName		varchar(255);
	entryId				number;
	priority			number;
	
	O_objTypeId_Document				number;
	objTypeId_WorkSpace					number;
	objTypeId_Folder					number;
	objTypeId_PublicMail				number;
	objTypeId_Reply						number;
	objTypeId_Request					number;
	objTypeId_RequestMail				number;
	objTypeId_Recycle					number;
	objTypeId_PublicTo					number;
	objTypeId_ApproveTo					number;
	objTypeId_Requester					number;
	objTypeId_Approver					number;
	objTypeId_AgainRequestTo			number;
	objTypeId_Return					number;
	
	attTypeId_Path						number;
	attTypeId_Property					number;
	attTypeId_RevUpComment				number;
	attTypeId_CreateUserId				number;
	attTypeId_Expire					number;
	attTypeId_Timing					number;
	attTypeId_Comment					number;
	attTypeId_Reply						number;
	attTypeId_ReturnUserId				number;
	
	attTypeId_RequestUserId				number;
	attTypeId_RequestDate				number;
	attTypeId_ApproveUserId				number;
	attTypeId_ApproveDate				number;
	
	attTypeId_Status					number;
	attTypeId_NoticeKindId				number;
	attTypeId_ApproveKindId				number;
	attTypeId_AgainRequestKindId		number;

	attTypeId_ModifyUserId				number;
	attTypeId_ModifyDate				number;
	attTypeId_CreateDate				number;
	attTypeId_FileSize					number;

begin
	/* ====================== */
	/* Select Object Types    */
	/* ====================== */
	select id into O_objTypeId_Document from EIMOBJTYPE where name = 'ドキュメント';
	
	/* ====================== */
	/* Create Attribute Types */
	/* ====================== */
		attTypeName := '更新者';
	attTypeOtherName := 'Modify user';
	AttributeUtils.createAttributeType(null, null, attTypeName, 2, attTypeId_ModifyUserId, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_ModifyUserId, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_ModifyUserId, 'EN', attTypeOtherName, rscode);
	attTypeName := '更新日';
	attTypeOtherName := 'Modify date';
	AttributeUtils.createAttributeType(null, null, attTypeName, 3, attTypeId_ModifyDate, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_ModifyDate, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_ModifyDate, 'EN', attTypeOtherName, rscode);
	attTypeName := '作成日';
	attTypeOtherName := 'Create date';
	AttributeUtils.createAttributeType(null, null, attTypeName, 3, attTypeId_CreateDate, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_CreateDate, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_CreateDate, 'EN', attTypeOtherName, rscode);
	attTypeName := 'サイズ';
	attTypeOtherName := 'File size';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, attTypeId_FileSize, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_FileSize, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_FileSize, 'EN', attTypeOtherName, rscode);
	
	/* ============================ */
	/* Apply Object Type To Document*/
	/* ============================ */
	ObjectAttributeUtils.applyAttributeType(O_objTypeId_Document, attTypeId_ModifyUserId, rscode);
	ObjectAttributeUtils.applyAttributeType(O_objTypeId_Document, attTypeId_ModifyDate, rscode);
	ObjectAttributeUtils.applyAttributeType(O_objTypeId_Document, attTypeId_CreateDate, rscode);
	ObjectAttributeUtils.applyAttributeType(O_objTypeId_Document, attTypeId_FileSize, rscode);

	commit;
	
end;
/						
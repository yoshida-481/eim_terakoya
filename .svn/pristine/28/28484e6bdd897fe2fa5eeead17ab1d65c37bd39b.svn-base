/***********************************************/
/*                                             */
/*   Create Document Database Resources Script */
/*                                             */
/***********************************************/

set serveroutput on;

declare 
	
	rscode			number;
	objId				number;
	objName			varchar2(255);
	objTypeId		number;
	objTypeName	varchar2(255);
	relTypeId		number;
	relTypeName	varchar2(255);
	attTypeId		number;
	attTypeName	varchar2(255);
	attTypeOtherName	varchar2(255);
	secId				number;
	secName			varchar(255);
	entryId			number;
	priority		number;
	
	objTypeId_Document						number;
	objTypeId_WorkSpace						number;
	objTypeId_Folder							number;
	objTypeId_PublicMail					number;
	objTypeId_Reply								number;
	objTypeId_Request							number;
	objTypeId_RequestMail					number;
	objTypeId_Recycle							number;
	objTypeId_PublicTo						number;
	objTypeId_ApproveTo						number;
	objTypeId_Requester						number;
	objTypeId_Approver						number;
	objTypeId_AgainRequestTo			number;
	
	attTypeId_Path								number;
	attTypeId_Property						number;
	attTypeId_RevUpComment				number;
	attTypeId_CreateUserId				number;
	attTypeId_CreateUserName			number;
	attTypeId_Expire							number;
	attTypeId_Timing							number;
	attTypeId_Comment							number;
	attTypeId_Reply								number;
	
	attTypeId_RequestUserId				number;
	attTypeId_RequestUserName			number;
	attTypeId_RequestDate					number;
	attTypeId_ApproveUserId				number;
	attTypeId_ApproveUserName			number;
	attTypeId_ApproveDate					number;
	
	attTypeId_Status							number;
	attTypeId_NoticeKindId				number;
	attTypeId_ApproveKindId				number;
	attTypeId_AgainRequestKindId	number;

begin
	
	/* Create Attribute Types */
	attTypeName := 'パス';
	attTypeOtherName := 'Pass';
	select ID into attTypeId from eimattr where NAME = attTypeName;
	AttributeUtils.addOtherAttributeTypeName(attTypeId, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId, 'EN', attTypeOtherName, rscode);
	attTypeName := 'プロパティ';
	attTypeOtherName := 'Property';
	select ID into attTypeId from eimattr where NAME = attTypeName;
	AttributeUtils.addOtherAttributeTypeName(attTypeId, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId, 'EN', attTypeOtherName, rscode);
	attTypeName := '改訂内容';
	attTypeOtherName := 'Revised content';
	select ID into attTypeId from eimattr where NAME = attTypeName;
	AttributeUtils.addOtherAttributeTypeName(attTypeId, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId, 'EN', attTypeOtherName, rscode);
	attTypeName := '作成者';
	attTypeOtherName := 'Create user';
	select ID into attTypeId from eimattr where NAME = attTypeName;
	AttributeUtils.addOtherAttributeTypeName(attTypeId, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId, 'EN', attTypeOtherName, rscode);
	attTypeName := '作成者名';
	attTypeOtherName := 'Create user name';
	select ID into attTypeId from eimattr where NAME = attTypeName;
	AttributeUtils.addOtherAttributeTypeName(attTypeId, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId, 'EN', attTypeOtherName, rscode);
	attTypeName := '有効期限';
	attTypeOtherName := 'Effective term';
	select ID into attTypeId from eimattr where NAME = attTypeName;
	AttributeUtils.addOtherAttributeTypeName(attTypeId, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId, 'EN', attTypeOtherName, rscode);
	attTypeName := '通知タイミング';
	attTypeOtherName := 'Notice timing';
	select ID into attTypeId from eimattr where NAME = attTypeName;
	AttributeUtils.addOtherAttributeTypeName(attTypeId, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId, 'EN', attTypeOtherName, rscode);
	attTypeName := 'コメント';
	attTypeOtherName := 'Comment';
	select ID into attTypeId from eimattr where NAME = attTypeName;
	AttributeUtils.addOtherAttributeTypeName(attTypeId, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId, 'EN', attTypeOtherName, rscode);
	attTypeName := '受信確認';
	attTypeOtherName := 'Receive confirmation';
	select ID into attTypeId from eimattr where NAME = attTypeName;
	AttributeUtils.addOtherAttributeTypeName(attTypeId, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId, 'EN', attTypeOtherName, rscode);
	attTypeName := '承認依頼者';
	attTypeOtherName := 'Approve request user';
	select ID into attTypeId from eimattr where NAME = attTypeName;
	AttributeUtils.addOtherAttributeTypeName(attTypeId, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId, 'EN', attTypeOtherName, rscode);
	attTypeName := '承認依頼者名';
	attTypeOtherName := 'Approve request user name';
	select ID into attTypeId from eimattr where NAME = attTypeName;
	AttributeUtils.addOtherAttributeTypeName(attTypeId, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId, 'EN', attTypeOtherName, rscode);
	attTypeName := '承認依頼日';
	attTypeOtherName := 'Approve request date';
	select ID into attTypeId from eimattr where NAME = attTypeName;
	AttributeUtils.addOtherAttributeTypeName(attTypeId, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId, 'EN', attTypeOtherName, rscode);
	attTypeName := '承認者';
	attTypeOtherName := 'Approved user';
	select ID into attTypeId from eimattr where NAME = attTypeName;
	AttributeUtils.addOtherAttributeTypeName(attTypeId, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId, 'EN', attTypeOtherName, rscode);
	attTypeName := '承認者名';
	attTypeOtherName := 'Approved user name';
	select ID into attTypeId from eimattr where NAME = attTypeName;
	AttributeUtils.addOtherAttributeTypeName(attTypeId, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId, 'EN', attTypeOtherName, rscode);
	attTypeName := '承認日';
	attTypeOtherName := 'Approved date';
	select ID into attTypeId from eimattr where NAME = attTypeName;
	AttributeUtils.addOtherAttributeTypeName(attTypeId, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId, 'EN', attTypeOtherName, rscode);
	
	attTypeName := 'ステータス';
	attTypeOtherName := 'Status';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, attTypeId_Status, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_Status, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_Status, 'EN', attTypeOtherName, rscode);
	attTypeName := '通知先種別：ID';
	attTypeOtherName := 'NoticeKind:ID';
	AttributeUtils.createAttributeType(null, null, attTypeName, 2, attTypeId_NoticeKindId, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_NoticeKindId, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_NoticeKindId, 'EN', attTypeOtherName, rscode);
	attTypeName := '被承認依頼者種別：ID';
	attTypeOtherName := 'ApproveKind:ID';
	AttributeUtils.createAttributeType(null, null, attTypeName, 2, attTypeId_ApproveKindId, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_ApproveKindId, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_ApproveKindId, 'EN', attTypeOtherName, rscode);
	attTypeName := '再承認依頼先種別：ID';
	attTypeOtherName := 'AgainRequestKind:ID';
	AttributeUtils.createAttributeType(null, null, attTypeName, 2, attTypeId_AgainRequestKindId, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_AgainRequestKindId, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_AgainRequestKindId, 'EN', attTypeOtherName, rscode);

	/* Apply Object Type To Request Mail */
	ObjectAttributeUtils.applyAttributeType(objTypeId_RequestMail, attTypeId_Status, rscode);
	
	/* Apply Object Type To PublicTo */
	ObjectAttributeUtils.applyAttributeType(objTypeId_PublicTo, attTypeId_NoticeKindId, rscode);

	/* Apply Object Type To ApproveTo */
	ObjectAttributeUtils.applyAttributeType(objTypeId_ApproveTo, attTypeId_ApproveKindId, rscode);

	/* Apply Object Type To Requester */
	ObjectAttributeUtils.applyAttributeType(objTypeId_Requester, attTypeId_RequestUserId, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_Requester, attTypeId_Comment, rscode);

	/* Apply Object Type To Approver */
	ObjectAttributeUtils.applyAttributeType(objTypeId_Approver, attTypeId_ApproveUserId, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_Approver, attTypeId_ApproveDate, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_Approver, attTypeId_Comment, rscode);

	/* Apply Object Type To AgainRequestTo */
	ObjectAttributeUtils.applyAttributeType(objTypeId_AgainRequestTo, attTypeId_AgainRequestKindId, rscode);

	/* Update System User Admin */
	update EIMUser set ADMIN = '255' where ID = '1';
	
	commit;
	
end;
/
